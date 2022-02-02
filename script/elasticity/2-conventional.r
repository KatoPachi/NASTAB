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
#'
# /*
#+
rmarkdown::render(
  "script/elasticity/2-conventional.r",
  output_dir = "report/view/elasticity"
)
# */