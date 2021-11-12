#' ---
#' title: |
#'   Price Elasticities with Self-Selection of Tax Relief
#' subtitle: Not intended for publication
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
#' # Results
#'
#' ## Intensive Margin
#'
#+
fixest::setFixest_fml(
  ..stage21 = ~ log_pinc_all | year + panelid,
  ..stage22 = ~ log_pinc_all + sqage | year + panelid,
  ..stage23 = ~ log_pinc_all + sqage | year + panelid + area,
  ..stage24 = ~ log_pinc_all + sqage | year + panelid + area + industry,
  ..stage1 = ~ log_pinc_all + sqage + factor(area) + factor(industry)
)

poolstage1 <- fixest::feglm(
  fixest::xpd(ext_benefit_tl ~ employee + log_price + ..stage1),
  family = binomial(link = "probit"),
  data = subset(df, year <= 2017)
)

sepstage1 <- df %>%
  dplyr::filter(year <= 2017) %>%
  group_by(year) %>%
  do(sepmod = fixest::feglm(
    fixest::xpd(ext_benefit_tl ~ employee + log_price + ..stage1),
    family = binomial(link = "probit"),
    data = .
  ))

estdf <- df %>%
  dplyr::filter(year <= 2017 & !is.na(ext_benefit_tl)) %>%
  mutate(poolmod = list(poolstage1)) %>%
  left_join(sepstage1, by = "year") %>%
  group_by(year) %>%
  do(modelr::spread_predictions(
    ., first(.$poolmod), first(.$sepmod),
    type = "link"
  )) %>%
  rename(lpred_pool = "first(.$poolmod)", lpred_sep = "first(.$sepmod)") %>%
  mutate(
    # propensity score
    psc_pool = pnorm(lpred_pool), psc_sep = pnorm(lpred_sep),

    # inverse mills ratio
    imr_pool = dnorm(lpred_pool) / pnorm(lpred_pool),
    imr_sep = dnorm(lpred_sep) / pnorm(lpred_sep),
  ) %>%
  ungroup() %>%
  select(-poolmod, -sepmod)

#'
#+
sepstage1 %>%
  pull(sepmod, name = year) %>%
  list.merge(list("Pool" = poolstage1), .) %>%
  modelsummary(
    title = "Probit Estimation of Selection Equation",
    coef_omit = "factor",
    coef_rename = c(
      "employee" = "1 = Wage earner",
      "log_price" = "log(first giving price)",
      "log_pinc_all" = "log(income)",
      "sqage" = "Square of age"
    ),
    gof_omit = "R2|AIC|BIC",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = tribble(
      ~"term", ~"Pool", ~"2012", ~"2013", ~"2014", ~"2015", ~"2016", ~"2017",
      "Dummy of area", "X", "X", "X", "X", "X", "X", "X",
      "Dummy of industry", "X", "X", "X", "X", "X", "X", "X"
    )
  )

#'
#+
stage2 <- list(
  "(1)" = fixest::xpd(
    log_total_g ~ ..stage24 | ext_benefit_tl:log_price ~ employee:log_price
  ),
  "(2)" = fixest::xpd(
    log_total_g ~ ..stage24 | ext_benefit_tl:log_price ~ psc_pool:log_price
  ),
  "(3)" = fixest::xpd(
    log_total_g ~ ..stage24 | ext_benefit_tl:log_price ~ psc_sep:log_price
  ),
  "(4)" = fixest::xpd(
    log_total_g ~ psc_pool:log_price + ..stage24
  ),
  "(5)" = fixest::xpd(
    log_total_g ~ psc_sep:log_price + ..stage24
  )
)

stage2 %>%
  purrr::map(~ fixest::feols(
    ., cluster = ~ panelid,
    data = subset(estdf, i_ext_giving == 1)
  )) %>%
  modelsummary(
    title = paste(
      "First-Price Elasticities (Intenstive Margin)"
    ),
    coef_map = c(
      "fit_ext_benefit_tl:log_price" =
        "Applying tax relief x log(first price)",
      "psc_pool:log_price" =
        "PS of applying tax relief x log(first price)",
      "psc_sep:log_price" =
        "PS of applying tax relief x log(first price)",
      "log_pinc_all" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = tribble(
      ~"term", ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)",
      "Square of age", "X", "X", "X", "X", "X",
      "Instrument", "Wage earner x Price",
      "PS x Price", "PS x Price", "", "",
      "Method of PS", "", "Pool", "Separate", "Pool", "Separate"
    )
  ) %>%
  kableExtra::add_header_above(c(" " = 1, "2SLS" = 3, "OLS" = 2))

#'
#+
stage2 %>%
  purrr::map(~ fixest::feols(
    ., cluster = ~ panelid,
    data = subset(estdf, i_ext_giving == 1 & (year < 2013 | 2014 < year))
  )) %>%
  modelsummary(
    title = paste(
      "Robustness of First-Price Elasticities (Intenstive Margin)"
    ),
    coef_map = c(
      "fit_ext_benefit_tl:log_price" =
        "Applying tax relief x log(first price)",
      "psc_pool:log_price" =
        "PS of applying tax relief x log(first price)",
      "psc_sep:log_price" =
        "PS of applying tax relief x log(first price)",
      "log_pinc_all" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = tribble(
      ~"term", ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)",
      "Square of age", "X", "X", "X", "X", "X",
      "Instrument", "Wage earner x Price",
      "PS x Price", "PS x Price", "", "",
      "Method of PS", "", "Pool", "Separate", "Pool", "Separate"
    )
  ) %>%
  kableExtra::add_header_above(c(" " = 1, "2SLS" = 3, "OLS" = 2))

#'
#+
rob1_stage2 <- list(
  "(1)" = fixest::xpd(
    log_total_g ~ ..stage24 | ext_benefit_tl:log_lprice ~ employee:log_price
  ),
  "(2)" = fixest::xpd(
    log_total_g ~ ..stage24 | ext_benefit_tl:log_lprice ~ psc_pool:log_price
  ),
  "(3)" = fixest::xpd(
    log_total_g ~ ..stage24 | ext_benefit_tl:log_lprice ~ psc_sep:log_price
  )
)

rob1_stage2 %>%
  purrr::map(~ fixest::feols(
    .,
    cluster = ~panelid,
    data = subset(estdf, i_ext_giving == 1)
  )) %>%
  modelsummary(
    title = paste(
      "Last-Price Elasticities (Intensive Margin)"
    ),
    coef_map = c(
      "fit_ext_benefit_tl:log_lprice" =
        "Applying tax relief x log(last price)",
      "log_pinc_all" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = tribble(
      ~"term", ~"(1)", ~"(2)", ~"(3)",
      "Square of age", "X", "X", "X",
      "Instrument", "Wage earner x Price",
      "PS x Price", "PS x Price",
      "Method of PS", "", "Pool", "Separate"
    )
  )

#'
#' ## Extensive Margin
#'
#+
ext_stage2 <- list(
  "(1)" = fixest::xpd(
    i_ext_giving ~ ..stage24 | ext_benefit_tl:log_price ~ employee:log_price
  ),
  "(2)" = fixest::xpd(
    i_ext_giving ~ ..stage24 | ext_benefit_tl:log_price ~ psc_pool:log_price
  ),
  "(3)" = fixest::xpd(
    i_ext_giving ~ ..stage24 | ext_benefit_tl:log_price ~ psc_sep:log_price
  ),
  "(4)" = fixest::xpd(
    i_ext_giving ~ psc_pool:log_price + ..stage24
  ),
  "(5)" = fixest::xpd(
    i_ext_giving ~ psc_sep:log_price + ..stage24
  )
)

ext_stage2 %>%
  purrr::map(~ fixest::feols(
    ., cluster = ~ panelid,
    data = estdf
  )) %>%
  modelsummary(
    title = paste(
      "First-Price Elasticities (Extenstive Margin)"
    ),
    coef_map = c(
      "fit_ext_benefit_tl:log_price" =
        "Applying tax relief x log(first price)",
      "psc_pool:log_price" =
        "PS of applying tax relief x log(first price)",
      "psc_sep:log_price" =
        "PS of applying tax relief x log(first price)",
      "log_pinc_all" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = tribble(
      ~"term", ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)",
      "Square of age", "X", "X", "X", "X", "X",
      "Instrument", "Wage earner x Price",
      "PS x Price", "PS x Price", "", "",
      "Method of PS", "", "Pool", "Separate", "Pool", "Separate"
    )
  ) %>%
  kableExtra::add_header_above(c(" " = 1, "2SLS" = 3, "OLS" = 2))

#'
#+
ext_stage2 %>%
  purrr::map(~ fixest::feols(
    .,
    cluster = ~panelid,
    data = subset(estdf, year < 2013 | 2014 < year)
  )) %>%
  modelsummary(
    title = paste(
      "Robustness of First-Price Elasticities (Extenstive Margin)"
    ),
    coef_map = c(
      "fit_ext_benefit_tl:log_price" =
        "Applying tax relief x log(first price)",
      "psc_pool:log_price" =
        "PS of applying tax relief x log(first price)",
      "psc_sep:log_price" =
        "PS of applying tax relief x log(first price)",
      "log_pinc_all" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = tribble(
      ~"term", ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)",
      "Square of age", "X", "X", "X", "X", "X",
      "Instrument", "Wage earner x Price",
      "PS x Price", "PS x Price", "", "",
      "Method of PS", "", "Pool", "Separate", "Pool", "Separate"
    )
  ) %>%
  kableExtra::add_header_above(c(" " = 1, "2SLS" = 3, "OLS" = 2))

# /*
#+
rmarkdown::render(
  "script/R/3-2sls_cf.r",
  output_dir = "report/view"
)
# */