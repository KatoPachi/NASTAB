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

lapply(Sys.glob(file.path("script/R/functions", "*.r")), source)

#'
#+ include = FALSE
df <- readr::read_csv(
  "data/shaped2_propensity.csv",
  col_types = cols(
    ext_credit_giving = col_double(),
    krw_credit_giving = col_double(),
    trust_politician = col_double(),
    political_pref = col_double(),
    addtax = col_double(),
    avg_welfare_tax = col_double(),
    opt_welfare_tax = col_double(),
    now_balance = col_double(),
    ideal_balance = col_double(),
    accountant = col_double(),
    consult = col_double()
  )
) %>%
dplyr::filter(
  ext_benefit_tl == 0 | (ext_benefit_tl == 1 & i_ext_giving == 1)
)

#'
#' # Estimating Conventional Price Elasticity of Charitable Giving
#'
#+
fixest::setFixest_fml(
  ..cov = ~ log_pinc_all + sqage | year + panelid + industry + area
)

int_first <- list(
  "(1)" = log_total_g ~ log_price:ext_benefit_tl + ..cov,
  "(2)" = log_total_g ~ log_price:psc_pool + ..cov,
  "(3)" = log_total_g ~ log_price:psc_sep + ..cov,
  "(4)" = log_total_g ~ ..cov | log_price:ext_benefit_tl ~ psc_pool,
  "(5)" = log_total_g ~ ..cov | log_price:ext_benefit_tl ~ psc_sep
)

est_int_first <- int_first %>%
  purrr::map(~ fixest::feols(
    xpd(.), subset(df, i_ext_giving == 1), cluster = ~panelid
  ))

stage1_int_first <- est_int_first[c(4, 5)] %>%
  purrr::map(function(x) {
    coef <- x$iv_first_stage[["log_price:ext_benefit_tl"]]$coeftable[1, 1]
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
  select(name, value.a, value.b, value.c, value.x, value.y) %>%
  setNames(c("term", sprintf("(%1d)", 1:5))) %>%
  mutate(term = recode(
    term, "coef" = "First-stage: Instrument", .default = ""
  ))

addtab <- stage1_int_first %>%
  bind_rows(tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)",
    "Square of age", "X", "X", "X", "X", "X",
    "Method of Propensity Score", "", "Pooled", "Separated",
    "Pooled", "Separated"
  ))

attr(addtab, "position") <- c(7, 8)

est_int_first %>%
  modelsummary(
    coef_map = c(
      "log_price:ext_benefit_tl" = "Application x log(first price)",
      "log_price:psc_pool" = "PS of application x log(first price)",
      "log_price:psc_sep" = "PS of application x log(first price)",
      "fit_log_price:ext_benefit_tl" = "Application x log(first price)",
      "log_pinc_all" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    " " = 1, "FE" = 3, "FE-2SLS" = 2
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
ext_first <- list(
  "(1)" = i_ext_giving ~ log_price:ext_benefit_tl + ..cov,
  "(2)" = i_ext_giving ~ log_price:psc_pool + ..cov,
  "(3)" = i_ext_giving ~ log_price:psc_sep + ..cov,
  "(4)" = i_ext_giving ~ ..cov | log_price:ext_benefit_tl ~ psc_pool,
  "(5)" = i_ext_giving ~ ..cov | log_price:ext_benefit_tl ~ psc_sep
)

est_ext_first <- ext_first %>%
  purrr::map(~ fixest::feols(xpd(.), df, cluster = ~panelid))

impelast <- est_ext_first %>%
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
  setNames(c("term", sprintf("(%1d)", 1:5))) %>%
  mutate(term = recode(
    term, "estimate" = "Implied price elasticity", .default = ""
  ))

stage1_ext_first <- est_ext_first[c(4, 5)] %>%
  purrr::map(function(x) {
    coef <- x$iv_first_stage[["log_price:ext_benefit_tl"]]$coeftable[1, 1]
    ivwald <- fitstat(x, "ivwald")[[1]]$stat

    tibble(coef = coef, wald = ivwald) %>%
      pivot_longer(everything()) %>%
      mutate(value = case_when(
        name == "coef" ~ sprintf("%1.3f", value),
        name == "wald" ~ sprintf("[%1.1f]", value)
      )) %>%
      mutate(value2 = c("", "")) %>%
      select(name, value2, value)
  }) %>%
  reduce(left_join, by = "name") %>%
  bind_cols(tribble(
    ~value.a, ~value.b, ~value.c,
    "", "", "",
    "", "", ""
  )) %>%
  select(name, value.a, value.b, value.c, value.x, value.y) %>%
  setNames(c("term", sprintf("(%1d)", 1:5))) %>%
  mutate(term = recode(
    term,
    "coef" = "First-stage: Instrument", .default = ""
  ))

addtab <- stage1_ext_first %>%
  bind_rows(impelast) %>%
  bind_rows(tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)",
    "Square of age", "X", "X", "X", "X", "X"
  ))

attr(addtab, "position") <- c(7:10)

est_ext_first %>%
  modelsummary(
    coef_map = c(
      "log_price:ext_benefit_tl" = "Application x log(first price)",
      "log_price:psc_pool" = "PS of application x log(first price)",
      "log_price:psc_sep" = "PS of application x log(first price)",
      "fit_log_price:ext_benefit_tl" = "Application x log(first price)",
      "log_pinc_all" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    " " = 1, "FE" = 3, "FE-2SLS" = 2
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
  "script/R/3-elasticity.r",
  output_dir = "report/view"
)
# */