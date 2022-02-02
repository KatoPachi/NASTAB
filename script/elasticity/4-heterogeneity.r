#' ---
#' title: |
#'   Estimating Conventional Price Elasticity of Charitable Giving
#' author: Hiroki Kato
#' bibliography: ../../Rmarkdown/reference.bib
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
estdf <- readr::read_csv("data/shaped2_propensity.csv", guess_max = 30000)

#'
#'
#' ## Heterogenous Price Elasticity in Covariates
#'
#' <!---
#' //NOTE: 共変量による異質性分析
#' --->
#+ CovHeteroElasticity
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
#' ## Heterogenous Price Elasticity in Charity Types
#'
#' <!---
#' //NOTE: 寄付の支出先による異質性分析
#' --->
#+ TypeHeteroElasticity
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
#' ## Heterogeneous Price Elasticity in Income
#'
#' <!---
#' //NOTE: 所得による異質性分析（Intensive）
#' --->
#+
fixest::setFixest_fml(
  ..stage2 = ~ linc_ln + sqage | year + pid + indust + area
)

intmod <- list(
  "(1)" = donate_ln ~ d_relief_donate:price_ln + ..stage2,
  "(2)" = donate_ln ~ psc_pool:price_ln + ..stage2,
  "(3)" = donate_ln ~ psc_sep:price_ln + ..stage2,
  "(4)" = donate_ln ~ ..stage2 | d_relief_donate:price_ln ~ employee:price_ln,
  "(5)" = donate_ln ~ ..stage2 | d_relief_donate:price_ln ~ psc_pool:price_ln,
  "(6)" = donate_ln ~ ..stage2 | d_relief_donate:price_ln ~ psc_sep:price_ln
)

est_intmod1 <- intmod %>%
  purrr::map(~ fixest::feols(
    xpd(.),
    data = subset(estdf, credit_treat != 3 & d_donate == 1),
    cluster = ~ pid
  ))

stage1_intmod1 <- est_intmod1[4:6] %>%
  purrr::map(function(x) {
    coef <- x$iv_first_stage[["d_relief_donate:price_ln"]]$coeftable[1, 1]
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
    ~value.a, ~value.b, ~value.c,
    "", "", "",
    "", "", ""
  ), .) %>%
  select(name, value.a, value.b, value.c, value.x, value.y, value) %>%
  setNames(c("term", sprintf("(%1d)", 1:6))) %>%
  mutate(term = recode(
    term,
    "coef" = "First-stage: Instrument", .default = ""
  ))

addtab <- stage1_intmod1 %>%
  bind_rows(tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
    "Square of age", "X", "X", "X", "X", "X", "X",
    "Instrument", "", "", "", "WE x Price",
    "PS x Price", "PS x Price",
    "Method of PS", "", "Pool", "Separate", "", "Pool", "Separate"
  ))

attr(addtab, "position") <- 7:8

est_intmod1 %>%
  modelsummary(
    title = "Intensive-Margin Tax-Price Elasticity among Income <= 4600",
    coef_map = c(
      "d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "fit_d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "psc_pool:price_ln" =
        "PS of applying tax relief x log(first price)",
      "psc_sep:price_ln" =
        "PS of applying tax relief x log(first price)",
      "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    " ", "FE" = 3, "FE-2SLS" = 3
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
#+
est_intmod2 <- intmod %>%
  purrr::map(~ fixest::feols(
    xpd(.),
    data = subset(estdf, credit_treat != 1 & d_donate == 1),
    cluster = ~ pid
  ))

stage1_intmod2 <- est_intmod2[4:6] %>%
  purrr::map(function(x) {
    coef <- x$iv_first_stage[["d_relief_donate:price_ln"]]$coeftable[1, 1]
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
    ~value.a, ~value.b, ~value.c,
    "", "", "",
    "", "", ""
  ), .) %>%
  select(name, value.a, value.b, value.c, value.x, value.y, value) %>%
  setNames(c("term", sprintf("(%1d)", 1:6))) %>%
  mutate(term = recode(
    term,
    "coef" = "First-stage: Instrument", .default = ""
  ))

addtab <- stage1_intmod2 %>%
  bind_rows(tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
    "Square of age", "X", "X", "X", "X", "X", "X",
    "Instrument", "", "", "", "WE x Price",
    "PS x Price", "PS x Price",
    "Method of PS", "", "Pool", "Separate", "", "Pool", "Separate"
  ))

attr(addtab, "position") <- 7:8

est_intmod2 %>%
  modelsummary(
    title = "Intensive-Margin Tax-Price Elasticity among Income >= 1200",
    coef_map = c(
      "d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "fit_d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "psc_pool:price_ln" =
        "PS of applying tax relief x log(first price)",
      "psc_sep:price_ln" =
        "PS of applying tax relief x log(first price)",
      "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    " ", "FE" = 3, "FE-2SLS" = 3
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

#' <!---
#' //NOTE: 所得による異質性分析（Extensive）
#' --->
#+
extmod <- list(
  "(1)" = d_donate ~ d_relief_donate:price_ln + ..stage2,
  "(2)" = d_donate ~ psc_pool:price_ln + ..stage2,
  "(3)" = d_donate ~ psc_sep:price_ln + ..stage2,
  "(4)" = d_donate ~ ..stage2 | d_relief_donate:price_ln ~ employee:price_ln,
  "(5)" = d_donate ~ ..stage2 | d_relief_donate:price_ln ~ psc_pool:price_ln,
  "(6)" = d_donate ~ ..stage2 | d_relief_donate:price_ln ~ psc_sep:price_ln
)

est_extmod1 <- extmod %>%
  purrr::map(~ fixest::feols(
    xpd(.),
    data = subset(estdf, credit_treat != 3),
    cluster = ~pid
  ))

stage1_extmod1 <- est_extmod1[4:6] %>%
  purrr::map(function(x) {
    coef <- x$iv_first_stage[["d_relief_donate:price_ln"]]$coeftable[1, 1]
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
    ~value.a, ~value.b, ~value.c,
    "", "", "",
    "", "", ""
  ), .) %>%
  select(name, value.a, value.b, value.c, value.x, value.y, value) %>%
  setNames(c("term", sprintf("(%1d)", 1:6))) %>%
  mutate(term = recode(
    term,
    "coef" = "First-stage: Instrument", .default = ""
  ))

impelast_extmod1 <- est_extmod1 %>%
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
  setNames(c("term", sprintf("(%1d)", 1:6))) %>%
  mutate(term = recode(
    term,
    "estimate" = "Implied price elasticity", .default = ""
  ))

addtab <- impelast_extmod1 %>%
  bind_rows(stage1_extmod1) %>%
  bind_rows(tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
    "Square of age", "X", "X", "X", "X", "X", "X",
    "Instrument", "", "", "", "WE x Price",
    "PS x Price", "PS x Price",
    "Method of PS", "", "Pool", "Separate", "", "Pool", "Separate"
  ))

attr(addtab, "position") <- 7:10

est_extmod1 %>%
  modelsummary(
    title = "Extensive-Margin Tax-Price Elasticity among Income <= 4600",
    coef_map = c(
      "d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "fit_d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "psc_pool:price_ln" =
        "PS of applying tax relief x log(first price)",
      "psc_sep:price_ln" =
        "PS of applying tax relief x log(first price)",
      "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    " ", "FE" = 3, "FE-2SLS" = 3
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
#+
est_extmod2 <- extmod %>%
  purrr::map(~ fixest::feols(
    xpd(.),
    data = subset(estdf, credit_treat != 1),
    cluster = ~pid
  ))

stage1_extmod2 <- est_extmod2[4:6] %>%
  purrr::map(function(x) {
    coef <- x$iv_first_stage[["d_relief_donate:price_ln"]]$coeftable[1, 1]
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
    ~value.a, ~value.b, ~value.c,
    "", "", "",
    "", "", ""
  ), .) %>%
  select(name, value.a, value.b, value.c, value.x, value.y, value) %>%
  setNames(c("term", sprintf("(%1d)", 1:6))) %>%
  mutate(term = recode(
    term,
    "coef" = "First-stage: Instrument", .default = ""
  ))

impelast_extmod2 <- est_extmod2 %>%
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
  setNames(c("term", sprintf("(%1d)", 1:6))) %>%
  mutate(term = recode(
    term,
    "estimate" = "Implied price elasticity", .default = ""
  ))

addtab <- impelast_extmod2 %>%
  bind_rows(stage1_extmod2) %>%
  bind_rows(tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
    "Square of age", "X", "X", "X", "X", "X", "X",
    "Instrument", "", "", "", "WE x Price",
    "PS x Price", "PS x Price",
    "Method of PS", "", "Pool", "Separate", "", "Pool", "Separate"
  ))

attr(addtab, "position") <- 7:10

est_extmod2 %>%
  modelsummary(
    title = "Extensive-Margin Tax-Price Elasticity among Income >= 1200",
    coef_map = c(
      "d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "fit_d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "psc_pool:price_ln" =
        "PS of applying tax relief x log(first price)",
      "psc_sep:price_ln" =
        "PS of applying tax relief x log(first price)",
      "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    " ", "FE" = 3, "FE-2SLS" = 3
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

#' ## Heterogeneous Price Elasticity in Income (2)
#'
#' - Restriction: Only donors who applied for tax relief
#'
#' <!---
#' //NOTE: 申告者限定の所得による異質性分析（Intensive）
#' --->
#+
fixest::setFixest_fml(
  ..stage2 = ~ linc_ln + sqage | year + pid + indust + area
)

intmod <- list(
  "(1)" = donate_ln ~ price_ln + ..stage2,
  "(2)" = donate_ln ~ price_ln + gr_pool + ..stage2,
  "(3)" = donate_ln ~ price_ln + gr_sep + ..stage2
)

est_intmod1 <- intmod %>%
  purrr::map(~ fixest::feols(
    xpd(.),
    data = subset(estdf, credit_treat != 3 & d_relief_donate == 1),
    cluster = ~pid
  ))

addtab <- tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)",
  "Square of age", "X", "X", "X",
  "Method of PS", "", "Pool", "Separate"
)

attr(addtab, "position") <- 7:8

est_intmod1 %>%
  modelsummary(
    title = paste(
      "Intensive-Margin Tax-Price Elasticity among Income <= 4600",
      "(Only Donors Who Did Applied for Tax Relief)"
    ),
    coef_map = c(
      "price_ln" = "log(first price)",
      "linc_ln" = "log(income)",
      "gr_pool" = "correction term",
      "gr_sep" = "correction term"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling() %>%
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
#+
est_intmod2 <- intmod %>%
  purrr::map(~ fixest::feols(
    xpd(.),
    data = subset(estdf, credit_treat != 1 & d_relief_donate == 1),
    cluster = ~pid
  ))

addtab <- tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)",
  "Square of age", "X", "X", "X",
  "Method of PS", "", "Pool", "Separate"
)

attr(addtab, "position") <- 7:8

est_intmod2 %>%
  modelsummary(
    title = paste(
      "Intensive-Margin Tax-Price Elasticity among Income >= 1200",
      "(Only Donors Who Did Applied for Tax Relief)"
    ),
    coef_map = c(
      "price_ln" = "log(first price)",
      "linc_ln" = "log(income)",
      "gr_pool" = "correction term",
      "gr_sep" = "correction term"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling() %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at individual level."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#' ## Heterogeneous Partial Effect of Price in Income
#'
#' - Restriction: Only donors who did not applied for tax relief
#'
#' <!---
#' //NOTE: 無申告者の所得による異質性分析（Intensive）
#' --->
#+
fixest::setFixest_fml(
  ..stage2 = ~ linc + sqage | year + pid + indust + area
)

intmod <- list(
  "(1)" = donate ~ price + ..stage2,
  "(2)" = donate ~ price + gr_pool + ..stage2,
  "(3)" = donate ~ price + gr_sep + ..stage2
)

est_intmod1 <- intmod %>%
  purrr::map(~ fixest::feols(
    xpd(.),
    data = subset(
      estdf,
      credit_treat != 3 & d_relief_donate == 0 & d_donate == 1
    ),
    cluster = ~pid
  ))

addtab <- tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)",
  "Square of age", "X", "X", "X",
  "Method of PS", "", "Pool", "Separate"
)

attr(addtab, "position") <- 7:8

est_intmod1 %>%
  modelsummary(
    title = paste(
      "Partial Intensive-Margin Effect of Price among Income <= 4600",
      "(Only Donors Who Did Not Applied for Tax Relief)"
    ),
    coef_map = c(
      "price" = "first price",
      "linc" = "income",
      "gr_pool" = "correction term",
      "gr_sep" = "correction term"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling() %>%
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
#+
est_intmod2 <- intmod %>%
  purrr::map(~ fixest::feols(
    xpd(.),
    data = subset(
      estdf,
      credit_treat != 1 & d_relief_donate == 1 & d_donate == 1
    ),
    cluster = ~pid
  ))

addtab <- tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)",
  "Square of age", "X", "X", "X",
  "Method of PS", "", "Pool", "Separate"
)

attr(addtab, "position") <- 7:8

est_intmod2 %>%
  modelsummary(
    title = paste(
      "Partial Intensive-Margin Effect of Price among Income >= 1200",
      "(Only Donors Who Did Not Applied for Tax Relief)"
    ),
    coef_map = c(
      "price" = "first price",
      "linc" = "income",
      "gr_pool" = "correction term",
      "gr_sep" = "correction term"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling() %>%
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
# /*
#+
rmarkdown::render(
  "script/elasticity/4-heterogeneity.r",
  output_dir = "report/view/elasticity"
)
# */