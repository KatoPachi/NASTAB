#' ---
#' title: |
#'   Price Elasticity of Charitable Giving
#' author: Hiroki Kato
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
df <- readr::read_csv("data/shaped2.csv")

#'
#' # Estimating Conventional Price Elasticity of Charitable Giving
#'
#+ MainElasticity
fixest::setFixest_fml(
  ..cov = ~ linc_ln + sqage | year + pid + indust + area
)

lastmod <- list(
  "(1)" = list(
    mod = donate_ln ~ lprice_ln:d_relief_donate + ..cov,
    data = df
  ),
  "(2)" = list(
    mod = donate_ln ~ ..cov | lprice_ln:d_relief_donate ~ price_ln,
    data = df
  ),
  "(3)" = list(
    mod = donate_ln ~ lprice_ln:d_relief_donate + ..cov,
    data = subset(df, d_donate == 1)
  ),
  "(4)" = list(
    mod = donate_ln ~ ..cov | lprice_ln:d_relief_donate ~ price_ln,
    data = subset(df, d_donate == 1)
  ),
  "(5)" = list(
    mod = d_donate ~ lprice_ln:d_relief_donate + ..cov,
    data = df
  ),
  "(6)" = list(
    mod = d_donate ~ ..cov | lprice_ln:d_relief_donate ~ price_ln,
    data = df
  )
)

est_lastmod <- lastmod %>%
  purrr::map(~ fixest::feols(
    xpd(.$mod), data = .$data, cluster = ~ pid
  ))

impelast_lastmod <- est_lastmod[c(5, 6)] %>%
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
    ~value.a, ~value.b, ~value.c, ~value.d,
    "", "", "", "",
    "", "", "", ""
  )) %>%
  select(name, value.a:value.d, value.x:value.y) %>%
  setNames(c("term", sprintf("(%1d)", 1:6))) %>%
  mutate(term = recode(
    term,
    "estimate" = "Implied price elasticity", .default = ""
  ))

stage1_lastmod <- est_lastmod[c(2, 4, 6)] %>%
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
    ~value.a, ~value.b, ~value.c,
    "", "", "",
    "", "", ""
  )) %>%
  select(name, value.a, value.x, value.b, value.y, value.c, value) %>%
  setNames(c("term", sprintf("(%1d)", 1:6))) %>%
  mutate(term = recode(
    term, "coef" = "First-stage: log(first price)", .default = ""
  ))

addtab <- impelast_lastmod %>%
  bind_rows(stage1_lastmod) %>%
  bind_rows(tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~ "(6)",
    "Square of age", "X", "X", "X", "X", "X", "X"
  ))

attr(addtab, "position") <- 5:8

est_lastmod %>%
  modelsummary(
    coef_map = c(
      "lprice_ln:d_relief_donate" = "log(last price)",
      "fit_lprice_ln:d_relief_donate" = "log(last price)",
      "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    " ", "FE", "FE-2SLS",
    "FE", "FE-2SLS", "FE", "FE-2SLS"
  )) %>%
  kableExtra::add_header_above(c(
    " " = 1, "Overall" = 2,
    "Intensive margin" = 2, "Extensive margin" = 2
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
#+ Robust1Elasticity
est_rob_lastmod <- lastmod %>%
  purrr::map(~ fixest::feols(
    xpd(.$mod), data = subset(.$data, year < 2013 | 2014 < year),
    cluster = ~ pid
  ))

impelast_rob_lastmod <- est_rob_lastmod[c(5, 6)] %>%
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
    ~value.a, ~value.b, ~value.c, ~value.d,
    "", "", "", "",
    "", "", "", ""
  )) %>%
  select(name, value.a:value.d, value.x:value.y) %>%
  setNames(c("term", sprintf("(%1d)", 1:6))) %>%
  mutate(term = recode(
    term,
    "estimate" = "Implied price elasticity", .default = ""
  ))

stage1_rob_lastmod <- est_rob_lastmod[c(2, 4, 6)] %>%
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
    ~value.a, ~value.b, ~value.c,
    "", "", "",
    "", "", ""
  )) %>%
  select(name, value.a, value.x, value.b, value.y, value.c, value) %>%
  setNames(c("term", sprintf("(%1d)", 1:6))) %>%
  mutate(term = recode(
    term, "coef" = "First-stage: log(first price)", .default = ""
  ))

addtab <- impelast_rob_lastmod %>%
  bind_rows(stage1_rob_lastmod) %>%
  bind_rows(tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~ "(6)",
    "Square of age", "X", "X", "X", "X", "X", "X"
  ))

attr(addtab, "position") <- 5:8

est_rob_lastmod %>%
  modelsummary(
    coef_map = c(
      "lprice_ln:d_relief_donate" = "log(last price)",
      "fit_lprice_ln:d_relief_donate" = "log(last price)",
      "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    " ", "FE", "FE-2SLS",
    "FE", "FE-2SLS", "FE", "FE-2SLS"
  )) %>%
  kableExtra::add_header_above(c(
    " " = 1, "Overall" = 2,
    "Intensive margin" = 2, "Extensive margin" = 2
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


# /*
#+
rmarkdown::render(
  "script/3-elasticity.r",
  output_dir = "report/view"
)
# */