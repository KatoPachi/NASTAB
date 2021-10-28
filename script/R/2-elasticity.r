#' ---
#' title: |
#'   Preview: Income and Price Elasiticity for Those Who Apply to Tax Relief
#' author: Hiroki Kato
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
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
#+ include = FALSE, eval = params$preview
df <- readr::read_csv(
  "data/shaped2.csv",
  col_types = cols(
    ext_credit_giving = col_double(),
    krw_credit_giving = col_double(),
    trust_politician = col_double(),
    political_pref = col_double(),
    addtax = col_double(),
    avg_welfare_tax = col_double(),
    opt_welfare_tax = col_double(),
    now_balance = col_double(),
    ideal_balance = col_double()
  )
)

#'
#' ## Price and Income Elasticity
#'
#+
subdf <- df %>%
  dplyr::filter(ext_benefit_tl == 1)

#'
#+
fixest::setFixest_fml(
  ..first1 = ~ log_pinc_all | year + pid,
  ..first2 = ~ log_pinc_all + age + sqage | year + pid,
  ..first3 = ~ log_pinc_all + age + sqage +
    factor(year):factor(educ) | year + pid,
  ..first4 = ~ log_pinc_all + age + sqage +
    factor(year):factor(educ) + factor(year):factor(gender) | year + pid,
  ..first5 = ~ log_pinc_all + age + sqage + factor(year):factor(educ) +
    factor(year):factor(gender) + factor(year):factor(living_area) | year + pid
)

xtab <- tribble(
  ~terms, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)",
  "Age (with squared age)", "", "X", "X", "X", "X",
  "Year x Education", "", "", "X", "X", "X",
  "Year x Gender", "", "", "", "X", "X",
  "Year x Resident Area", "", "", "", "", "X"
)

firstmod <- list(
  "(1)" = fixest::xpd(log_total_g ~ log_price + ..first1),
  "(2)" = fixest::xpd(log_total_g ~ log_price + ..first2),
  "(3)" = fixest::xpd(log_total_g ~ log_price + ..first3),
  "(4)" = fixest::xpd(log_total_g ~ log_price + ..first4),
  "(5)" = fixest::xpd(log_total_g ~ log_price + ..first5)
)

firstmod %>%
  purrr::map(~ fixest::feols(
    .,
    data = subset(subdf, i_ext_giving == 1),
    cluster = ~pid, se = "cluster"
  )) %>%
  modelsummary(
    coef_rename = c(
      "log_price" = "log(first giving price)",
      "log_pinc_all" = "log(annual taxable income)"
    ),
    coef_omit = "^(?!log)",
    gof_omit = "^(?!R2 Adj.|R2 Within|FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = xtab
  )

#'
#' ## Robustness Check
#'
#+
firstmod %>%
  purrr::map(~ fixest::feols(
    .,
    data = subset(subdf, i_ext_giving == 1 & (year < 2013 | year > 2014)),
    cluster = ~pid, se = "cluster"
  )) %>%
  modelsummary(
    coef_rename = c(
      "log_price" = "log(first giving price)",
      "log_pinc_all" = "log(annual taxable income)"
    ),
    coef_omit = "^(?!log)",
    gof_omit = "^(?!R2 Adj.|R2 Within|FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = xtab
  )

#'
#+
lastmod <- list(
  "(1)" = fixest::xpd(log_total_g ~ ..first1 | log_lprice ~ log_price),
  "(2)" = fixest::xpd(log_total_g ~ ..first2 | log_lprice ~ log_price),
  "(3)" = fixest::xpd(log_total_g ~ ..first3 | log_lprice ~ log_price),
  "(4)" = fixest::xpd(log_total_g ~ ..first4 | log_lprice ~ log_price),
  "(5)" = fixest::xpd(log_total_g ~ ..first5 | log_lprice ~ log_price)
)

lastmod %>%
  purrr::map(~ fixest::feols(
    .,
    data = subset(subdf, i_ext_giving == 1),
    cluster = ~pid, se = "cluster"
  )) %>%
  modelsummary(
    coef_rename = c(
      "fit_log_lprice" = "log(last giving price)",
      "log_pinc_all" = "log(annual taxable income)"
    ),
    coef_omit = "^(?!log|fit)",
    gof_omit = "^(?!R2 Adj.|R2 Within|FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = xlist_tab
  )

#'
#+
fixest::setFixest_fml(
  ..kdiff1 = ~ log_iv1price + log_diff1I + diff1_age + diff1_sqage,
  ..kdiff2 = ~ log_iv2price + log_diff2I + diff2_age + diff2_sqage,
  ..kdiff3 = ~ log_iv3price + log_diff3I + diff3_age + diff3_sqage,
  ..kdiffbase = ~ factor(year):factor(educ) + factor(year):factor(gender) +
    factor(year):factor(living_area) | year + pid
)

kdiff_tab <- tibble::tribble(
  ~terms, ~"(1)", ~"(2)", ~"(3)",
  "Age (with squared age)", "X", "X", "X",
  "Year x Education", "X", "X", "X",
  "Year x Gender", "X", "X", "X",
  "Year x Resident Area", "X", "X", "X"
)

kdiffmod <- list(
  "(1)" = fixest::xpd(log_diff1g ~ ..kdiff1 + ..kdiffbase),
  "(2)" = fixest::xpd(log_diff2g ~ ..kdiff2 + ..kdiffbase),
  "(3)" = fixest::xpd(log_diff3g ~ ..kdiff3 + ..kdiffbase)
)

kdiffmod %>%
  purrr::map(~ fixest::feols(
    .,
    cluster = ~ pid, se = "cluster",
    data = subset(subdf, i_ext_giving == 1)
  )) %>%
  modelsummary(
    coef_rename = c(
      "log_iv1price" = "1-year lagged difference of first price (log)",
      "log_iv2price" = "2-year lagged difference of first price (log)",
      "log_iv3price" = "3-year lagged difference of first price (log)",
      "log_diff1I" = "1-year lagged difference of annual income (log)",
      "log_diff2I" = "2-year lagged difference of annual income (log)",
      "log_diff3I" = "3-year lagged difference of annual income (log)"
    ),
    coef_omit = "^(?!log)",
    gof_omit = "^(?!R2 Adj.|R2 Within|FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = kdiff_tab
  )

# /*
#+
rmarkdown::render(
  "script/R/2-elasticity.r",
  output_dir = "report/view"
)
# */