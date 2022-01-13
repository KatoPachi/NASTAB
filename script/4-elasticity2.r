#' ---
#' title: |
#'   Estimating Conventional Price Elasticity of Charitable Giving
#' author: Hiroki Kato
#' bibliography: ../Rmarkdown/ref_main.bib
#' output:
#'   bookdown::html_document2:
#'     toc: true
#'     toc_float: true
#'     number_sections: false
#' params:
#'   preview: true
#' ---
#'
#+ include = FALSE, eval = params$preview
knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  echo = FALSE,
  cache = FALSE,
  include = TRUE,
  fig.width = 10
)

library(here)
knitr::opts_knit$set(
  root.dir = here::here()
)

options(
  knitr.kable.NA = " ",
  knitr.table.format = "html",
  modelsummary_stars_note = FALSE
)


#'
#+ include = FALSE, eval = params$preview
library(xfun)
xfun::pkg_attach2(c(
  "tidyverse", "rlist", "modelsummary", "kableExtra",
  "estimatr", "fixest"
))

lapply(Sys.glob(file.path("script/functions", "*.r")), source)

#'
#+ include = FALSE
book <- readr::read_csv("data/codebook/shaped2_description.csv"); View(book)
df <- readr::read_csv("data/shaped2.csv")

#'
#'
#' ## Conventional Method to Estimate Tax-Price Elasticity
#'
#+ MainElasticity
fixest::setFixest_fml(
  ..cov = ~ linc_ln + sqage | year + pid + indust + area
)

lastmod <- list(
  "(1)" = list(
    mod = donate_ln ~ lprice_ln:d_relief_donate + ..cov,
    data = subset(df, d_donate == 1)
  ),
  "(2)" = list(
    mod = donate_ln ~ ..cov | lprice_ln:d_relief_donate ~ price_ln,
    data = subset(df, d_donate == 1)
  ),
  "(3)" = list(
    mod = d_donate ~ lprice_ln:d_relief_donate + ..cov,
    data = df
  ),
  "(4)" = list(
    mod = d_donate ~ ..cov | lprice_ln:d_relief_donate ~ price_ln,
    data = df
  )
)

est_lastmod <- lastmod %>%
  purrr::map(~ fixest::feols(
    xpd(.$mod), data = .$data, cluster = ~ pid
  ))

impelast_lastmod <- est_lastmod[c(3, 4)] %>%
  purrr::map(function(x) {
    dbar <- mean(x$fitted.values + x$residuals)

    tidy(x) %>%
      filter(str_detect(term, "price")) %>%
      mutate(
        estimate = case_when(
          p.value <= .01 ~ sprintf("%1.3f***", estimate / dbar),
          p.value <= .05 ~ sprintf("%1.3f**", estimate / dbar),
          p.value <= .1 ~ sprintf("%1.3f*", estimate / dbar),
          TRUE ~ sprintf("%1.3f", estimate / dbar),
        ),
        std.error = sprintf("(%1.3f)", std.error / dbar)
      ) %>%
      select(estimate, std.error) %>%
      pivot_longer(everything())
  }) %>%
  reduce(left_join, by = "name") %>%
  bind_cols(tribble(
    ~value.a, ~value.b,
    "", "",
    "", ""
  )) %>%
  select(name, value.a, value.b, value.x, value.y) %>%
  setNames(c("term", sprintf("(%1d)", 1:6))) %>%
  mutate(term = recode(
    term,
    "estimate" = "Implied price elasticity", .default = ""
  ))

stage1_lastmod <- est_lastmod[c(2, 4)] %>%
  purrr::map(function(x) {
    coef <- x$iv_first_stage[["lprice_ln:d_relief_donate"]]$coeftable[1, 1]
    ivwald <- fitstat(x, "ivwald")[[1]]$stat

    tibble(coef = coef, wald = ivwald) %>%
      pivot_longer(everything()) %>%
      mutate(value = case_when(
        name == "coef" ~ sprintf("%1.3f", value),
        name == "wald" ~ sprintf("[%1.1f]", value)
      ))
  }) %>%
  reduce(left_join, by = "name") %>%
  bind_cols(tribble(
    ~value.a, ~value.b,
    "", "",
    "", ""
  )) %>%
  select(name, value.a, value.x, value.b, value.y) %>%
  setNames(c("term", sprintf("(%1d)", 1:6))) %>%
  mutate(term = recode(
    term, "coef" = "First-stage: log(first price)", .default = ""
  ))

addtab <- impelast_lastmod %>%
  bind_rows(stage1_lastmod) #%>%
  # bind_rows(tribble(
  #   ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)",
  #   "Square of age", "X", "X", "X", "X"
  # ))

attr(addtab, "position") <- 3:6

est_lastmod %>%
  modelsummary(
    title = "Estimation of Last-Unit Price Elasticities",
    coef_map = c(
      "lprice_ln:d_relief_donate" = "log(last price)",
      "fit_lprice_ln:d_relief_donate" = "log(last price)"#,
      # "linc_ln" = "log(income)"
    ),
    gof_omit = "R2|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling(font_size = 7) %>%
  kableExtra::add_header_above(c(
    " ", "FE", "FE-2SLS",
    "FE", "FE-2SLS"
  )) %>%
  kableExtra::add_header_above(c(
    " " = 1, "Intensive margin" = 2, "Extensive margin" = 2
  )) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at individual level.",
      "A square bracket is wald statistics of instrument."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#'
#' ## Conventional Method to Estimate Tax-Price Elasticity (2)
#'
#+ WoAnnoucementElasticity
est_rob_lastmod <- lastmod %>%
  purrr::map(~ fixest::feols(
    xpd(.$mod), data = subset(.$data, year < 2013 | 2014 < year),
    cluster = ~ pid
  ))

impelast_rob_lastmod <- est_rob_lastmod[c(3, 4)] %>%
  purrr::map(function(x) {
    dbar <- mean(x$fitted.values + x$residuals)

    tidy(x) %>%
      filter(str_detect(term, "price")) %>%
      mutate(
        estimate = case_when(
          p.value <= .01 ~ sprintf("%1.3f***", estimate / dbar),
          p.value <= .05 ~ sprintf("%1.3f**", estimate / dbar),
          p.value <= .1 ~ sprintf("%1.3f*", estimate / dbar),
          TRUE ~ sprintf("%1.3f", estimate / dbar),
        ),
        std.error = sprintf("(%1.3f)", std.error / dbar)
      ) %>%
      select(estimate, std.error) %>%
      pivot_longer(everything())
  }) %>%
  reduce(left_join, by = "name") %>%
  bind_cols(tribble(
    ~value.a, ~value.b,
    "", "",
    "", ""
  )) %>%
  select(name, value.a, value.b, value.x, value.y) %>%
  setNames(c("term", sprintf("(%1d)", 1:6))) %>%
  mutate(term = recode(
    term,
    "estimate" = "Implied price elasticity", .default = ""
  ))

stage1_rob_lastmod <- est_rob_lastmod[c(2, 4)] %>%
  purrr::map(function(x) {
    coef <- x$iv_first_stage[["lprice_ln:d_relief_donate"]]$coeftable[1, 1]
    ivwald <- fitstat(x, "ivwald")[[1]]$stat

    tibble(coef = coef, wald = ivwald) %>%
      pivot_longer(everything()) %>%
      mutate(value = case_when(
        name == "coef" ~ sprintf("%1.3f", value),
        name == "wald" ~ sprintf("[%1.1f]", value)
      ))
  }) %>%
  reduce(left_join, by = "name") %>%
  bind_cols(tribble(
    ~value.a, ~value.b,
    "", "",
    "", ""
  )) %>%
  select(name, value.a, value.x, value.b, value.y) %>%
  setNames(c("term", sprintf("(%1d)", 1:6))) %>%
  mutate(term = recode(
    term,
    "coef" = "First-stage: log(first price)", .default = ""
  ))

addtab <- impelast_rob_lastmod %>%
  bind_rows(stage1_rob_lastmod) #%>%
  # bind_rows(tribble(
  #   ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)",
  #   "Square of age", "X", "X", "X", "X"
  # ))

attr(addtab, "position") <- 3:6

est_rob_lastmod %>%
  modelsummary(
    title = paste(
      "Estimation of Last-Unit Price Elasticities",
      "Excluding 2013 and 2014 data"
    ),
    coef_map = c(
      "lprice_ln:d_relief_donate" = "log(last price)",
      "fit_lprice_ln:d_relief_donate" = "log(last price)"#,
      # "linc_ln" = "log(income)"
    ),
    gof_omit = "R2|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling(font_size = 7) %>%
  kableExtra::add_header_above(c(
    " ", "FE", "FE-2SLS",
    "FE", "FE-2SLS"
  )) %>%
  kableExtra::add_header_above(c(
    " " = 1, "Intensive margin" = 2, "Extensive margin" = 2
  )) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at individual level.",
      "A square bracket is wald statistics of instrument."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#'
#' ## Estimating Price Elasticity Using Compliers
#'
#+ R1Elasticity
r1mod <- list(
  "(1)" = fixest::xpd(donate_ln ~ price_ln + ..cov),
  "(2)" = fixest::xpd(
    donate_ln ~ price_ln + d(price_ln, 1) + d(price_ln, -1) +
    d(linc_ln, 1) + d(linc_ln, -1) + ..cov
  ),
  "(3)" = fixest::xpd(donate_ln ~ ..cov | lprice_ln ~ price_ln),
  "(4)" = fixest::xpd(
    donate_ln ~ d(lprice_ln, 1) + d(lprice_ln, -1) +
    d(linc_ln, 1) + d(linc_ln, -1) + ..cov | lprice_ln ~ price_ln
  )
)

est_r1mod <- r1mod %>%
  purrr::map(~ fixest::feols(
    ., data = subset(df, d_relief_donate == 1),
    panel.id = ~ pid + year, cluster = ~ pid
  ))

addtab <- tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~ "(4)",
  "Instrument: log(first price)", "", "",
  sprintf("%1.3f", est_r1mod[[3]]$iv_first_stage$lprice_ln$coeftable[1, 1]),
  sprintf("%1.3f", est_r1mod[[4]]$iv_first_stage$lprice_ln$coeftable[1, 1]),
  "", "", "",
  sprintf("[%1.1f]", fitstat(est_r1mod[[3]], "ivwald")[[1]]$stat),
  sprintf("[%1.1f]", fitstat(est_r1mod[[4]], "ivwald")[[1]]$stat)#,
  # "Square of age", "X", "X", "X", "X"
)

attr(addtab, "position") <- c(13, 14)

est_r1mod %>%
  modelsummary(
    title = paste(
      "Estimating Intensive-Margin Price Elasticities", 
      "for Those Who Applied for Tax Relief"
    ),
    coef_map = c(
      "price_ln" = "log(first price)",
      "fit_lprice_ln" = "log(last price)",
      "linc_ln" = "log(income)",
      "d(price_ln, 1)" = "1-year lag of price",
      "d(price_ln, -1)" = "1-year lead of price",
      "d(lprice_ln, 1)" = "1-year lag of price",
      "d(lprice_ln, -1)" = "1-year lead of price",
      "d(linc_ln, 1)" = "1-year lag of income",
      "d(linc_ln, -1)" = "1-year lead of income"
    ),
    gof_omit = "^(?!N)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling(font_size = 5) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at individual level.",
      "1-year lead of price cannot be estimated because of collinearity."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#'
#' ## $k$-th Difference Model
#'
#+ KdiffElasticity
fixest::setFixest_fml(
  ..kdiff1 = ~ linc_ln_d1 + sqage_d1,
  ..kdiff2 = ~ linc_ln_d2 + sqage_d2,
  ..kdiff3 = ~ linc_ln_d3 + sqage_d3,
  ..kdifffe = ~ year + area + indust
)

kdiffmod <- list(
  "(1)" = fixest::xpd(
    donate_ln_d1 ~ ..kdiff1 | ..kdifffe | price_ln_d1 ~ log(price_iv1)
  ),
  "(2)" = fixest::xpd(
    donate_ln_d2 ~ ..kdiff2 | ..kdifffe | price_ln_d2 ~ log(price_iv2)
  ),
  "(3)" = fixest::xpd(
    donate_ln_d3 ~ ..kdiff3 | ..kdifffe | price_ln_d3 ~ log(price_iv3)
  )
)

est_kdiffmod <- kdiffmod %>%
  purrr::map(~fixest::feols(
    ., data = subset(df, d_relief_donate == 1),
    cluster = ~pid
  ))

stage1_kdiffmod <- 1:3 %>%
  purrr::map(function(i) {
    x <- est_kdiffmod[[i]]
    coef <- x$iv_first_stage[[paste("price_ln_d", i, sep = "")]]$coeftable[1, 1]
    ivwald <- fitstat(x, "ivwald")[[1]]$stat

    tibble(coef = coef, wald = ivwald) %>%
      pivot_longer(everything()) %>%
      mutate(value = case_when(
        name == "coef" ~ sprintf("%1.3f", value),
        name == "wald" ~ sprintf("[%1.1f]", value)
      ))
  }) %>%
  reduce(left_join, by = "name") %>%
  setNames(c("term", sprintf("(%1d)", 1:3))) %>%
  mutate(term = recode(
    term,
    "coef" = "First-stage: Instrument", .default = ""
  ))

addtab <- stage1_kdiffmod #%>%
  # bind_rows(tribble(
  #   ~term, ~"(1)", ~"(2)", ~"(3)",
  #   "Difference of square age", "X", "X", "X"
  # ))

attr(addtab, "position") <- c(3, 4)

est_kdiffmod %>%
  modelsummary(
    title = paste(
      "$k$-th Difference Model Using Those Who Applied for Tax Relief"
    ),
    coef_map = c(
      "fit_price_ln_d1" = "Difference of logged first price",
      # "linc_ln_d1" = "Difference of logged income",
      "fit_price_ln_d2" = "Difference of logged first price",
      # "linc_ln_d2" = "Difference of logged income",
      "fit_price_ln_d3" = "Difference of logged first price"#,
      # "linc_ln_d3" = "Difference of logged income"
    ),
    gof_omit = "^(?!FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling(font_size = 8) %>%
  kableExtra::add_header_above(c(
    " " = 1, "1-year lag" = 1,
    "2-year lag" = 1, "3-year lag" = 1
  )) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at individual level.",
      "Instrument is difference between lagged first price in year $t$",
      "and in year $t - k$ fixing income in year $t - k$."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#'
#'
#' <!-- ## Heterogenous Price Elasticity (1) -->
#'
#+ CovHeteroElasticity, eval = FALSE
covdt <- list(
  female = subset(df, sex == 1),
  male = subset(df, sex == 0),
  univ = subset(df, univ == 1),
  highschool = subset(df, highschool == 1),
  junior = subset(df, junior == 1),
  gen_less40 = subset(df, age < 40),
  gen40 = subset(df, 40 <= age & age <= 50),
  gen_over50 = subset(df, 50 < age),
  wage = subset(df, employee == 1),
  nonwage = subset(df, employee == 0)
)

intcov <- covdt %>%
  purrr::map(~ fixest::feols(
    donate_ln ~ linc_ln + sqage | pid + year + indust + area |
      d_relief_donate:lprice_ln ~ price_ln,
    data = subset(., d_donate == 1), cluster = ~ pid
  )) %>%
  modelsummary(
    stars = c("*" = .1, "**" = .05, "***" = .01),
    output = "data.frame"
  ) %>%
  filter(str_detect(term, "fit_d_relief_donate|Num.Obs.")) %>%
  dplyr::select(-term, -statistic) %>%
  dplyr::mutate(part = c("coef", "se", "N")) %>%
  pivot_longer(-part, names_to = "cov", values_to = "intensive") %>%
  pivot_wider(names_from = part, values_from = intensive)

extcov <- covdt %>%
  purrr::map(~ fixest::feols(
    d_donate ~ linc_ln + sqage | pid + year + indust + area |
      d_relief_donate:lprice_ln ~ price_ln,
    data = ., cluster = ~pid
  )) %>%
  purrr::map(function(x) {
    dbar <- mean(x$fitted.values + x$residuals)

    tidy(x) %>%
      filter(str_detect(term, "price")) %>%
      mutate(
        coef = case_when(
          p.value <= .01 ~ sprintf("%1.3f***", estimate / dbar),
          p.value <= .05 ~ sprintf("%1.3f**", estimate / dbar),
          p.value <= .1 ~ sprintf("%1.3f*", estimate / dbar),
          TRUE ~ sprintf("%1.3f", estimate / dbar),
        ),
        se = sprintf("(%1.3f)", std.error / dbar),
        N = sprintf("%1d", glance(x)$nobs)
      ) %>%
      select(coef, se, N) %>%
      pivot_longer(everything())
  }) %>%
  reduce(left_join, by = "name") %>%
  setNames(c("part", names(covdt))) %>%
  pivot_longer(-part, names_to = "cov", values_to = "extensive") %>%
  pivot_wider(names_from = part, values_from = extensive)

intcov %>%
  dplyr::left_join(extcov, by = "cov") %>%
  dplyr::mutate(cov = recode(
    cov,
    "female" = "Female",
    "male" = "Male",
    "univ" = "University graduate",
    "highschool" = "High school graduate",
    "junior" = "Less than junior high school graduate",
    "gen_less40" = "Age < 40",
    "gen40" = "40 $\\le$ Age $\\le$ 50",
    "gen_over50" = "50 < Age",
    "wage" = "Wage earner",
    "nonwage" = "Non wage earner"
  )) %>%
  kable(
    caption = paste(
      "Heterogenous Last-Unit Price Elasticities",
      "in terms of Individual Characteristics"
    ),
    col.names = c(
      "Covariate", "Estimate", "S.E.", "N",
      "Estimate", "S.E.", "N"
    ),
    align = "lcccccc",
    escape = FALSE, booktabs = TRUE, linesep = ""
  ) %>%
  kableExtra::kable_styling(font_size = 8) %>%
  kableExtra::add_header_above(c(
    " " = 1, "Intensive margin" = 3, "Extensive margin" = 3
  )) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at individual level."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#'
#' <!-- ## Heterogenous Price Elasticity (2) -->
#'
#+ TypeHeteroElasticity, eval = FALSE
donate_type <- list(
  welfare = "donate_welfare",
  educ = "donate_educ",
  culture = "donate_culture",
  poliparty = "donate_poliparty",
  religious = "donate_religious",
  religious_action = "donate_religious_action",
  other = "donate_others"
)

int_type <- donate_type[c(1, 2, 4, 5, 6, 7)] %>%
  purrr::map(function(x) {
    mod <- paste(
      paste(x, "_ln", sep = ""),
      "~ linc_ln + sqage | pid + year + indust + area |",
      "d_relief_donate:lprice_ln ~ price_ln"
    )

    cond <- paste("d_", x, sep = "")

    fixest::feols(
      formula(mod), data = df[df[, cond] == 1, ], cluster = ~ pid
    )
  }) %>%
  modelsummary(
    stars = c("*" = .1, "**" = .05, "***" = .01),
    output = "data.frame"
  ) %>%
  filter(str_detect(term, "fit_d_relief_donate|Num.Obs.")) %>%
  dplyr::select(-term, -statistic) %>%
  dplyr::mutate(part = c("coef", "se", "N")) %>%
  pivot_longer(-part, names_to = "type", values_to = "intensive") %>%
  pivot_wider(names_from = part, values_from = intensive)

ext_type <- donate_type %>%
  purrr::map(function(x) {
    mod <- paste(
      paste("d_", x, sep = ""),
      "~ linc_ln + sqage | pid + year + indust + area |",
      "d_relief_donate:lprice_ln ~ price_ln"
    )

    fixest::feols(formula(mod), data = df, cluster = ~pid)
  }) %>%
  purrr::map(function(x) {
    dbar <- mean(x$fitted.values + x$residuals)

    tidy(x) %>%
      filter(str_detect(term, "price")) %>%
      mutate(
        coef = case_when(
          p.value <= .01 ~ sprintf("%1.3f***", estimate / dbar),
          p.value <= .05 ~ sprintf("%1.3f**", estimate / dbar),
          p.value <= .1 ~ sprintf("%1.3f*", estimate / dbar),
          TRUE ~ sprintf("%1.3f", estimate / dbar),
        ),
        se = sprintf("(%1.3f)", std.error / dbar),
        N = sprintf("%1d", glance(x)$nobs)
      ) %>%
      select(coef, se, N) %>%
      pivot_longer(everything())
  }) %>%
  reduce(left_join, by = "name") %>%
  setNames(c("part", names(donate_type))) %>%
  pivot_longer(-part, names_to = "type", values_to = "extensive") %>%
  pivot_wider(names_from = part, values_from = extensive)

int_type %>%
  dplyr::right_join(ext_type, by = "type") %>%
  dplyr::mutate(type = recode(
    type,
    "welfare" = "Social welfare",
    "educ" = "Education",
    "culture" = "Culture",
    "poliparty" = "Political party",
    "religious" = "Religious institution",
    "religious_action" = "Relief activities by religious institution",
    "other" = "Others"
  )) %>%
  kable(
    caption = paste(
      "Estimating Last-Unit Price Elasticities for Each Oraganization Type"
    ),
    col.names = c(
      "Type", "Estimate", "S.E.", "N",
      "Estimate", "S.E.", "N"
    ),
    align = "lcccccc",
    booktabs = TRUE, linesep = ""
  ) %>%
  kableExtra::kable_styling(font_size = 8) %>%
  kableExtra::add_header_above(c(
    " " = 1, "Intensive margin" = 3, "Extensive margin" = 3
  )) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at individual level.",
      "We cannot the intensive-margin price elasticity",
      "for donations for culture due to small sample."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#'
#'
# /*
#+
rmarkdown::render(
  "script/3-elasticity.r",
  output_dir = "report/view"
)
# */