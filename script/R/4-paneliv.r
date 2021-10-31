#' ---
#' title: |
#'   Preview: Recovering Population Elasticities
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

fixest::setFixest_fml(
  ..first1 = ~ log_pinc_all | year + panelid,
  ..first2 = ~ log_pinc_all + sqage | year + panelid,
  ..first3 = ~ log_pinc_all + sqage | year + panelid + area,
  ..first4 = ~ log_pinc_all + sqage | year + panelid + area + industry
)


#'
#' ## Recovering Population Elasticities
#'
#'
#+
popmod <- list(
  "(1)" = fixest::xpd(log_total_g ~ log_price:ext_benefit_tl + ..first1),
  "(2)" = fixest::xpd(log_total_g ~ log_price:ext_benefit_tl + ..first2),
  "(3)" = fixest::xpd(log_total_g ~ log_price:ext_benefit_tl + ..first3),
  "(4)" = fixest::xpd(log_total_g ~ log_price:ext_benefit_tl + ..first4)
)

popmod %>%
  purrr::map(~ fixest::feols(
    .,
    data = subset(df, i_ext_giving == 1 & year <= 2017),
    cluster = ~ panelid, se = "cluster"
  )) %>%
  modelsummary(
    title = "Population First-Price Elasiticities",
    coef_map = c(
      "log_price:ext_benefit_tl" =
        "log(first giving price) x Applying tax relief",
      "log_pinc_all" =
        "log(annual taxable income)"
    ),
    gof_omit = "^(?!R2 Adj.|FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = tribble(
      ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)",
      "Square age", "", "X", "X", "X"
    )
  )

#'
#+
fixest::setFixest_fml(
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
  dplyr::filter(year <= 2017) %>%
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

    # reverse of inverse mills ratio
    rimr_pool = -dnorm(lpred_pool) / (1 - pnorm(lpred_pool)),
    rimr_sep = -dnorm(lpred_sep) / (1 - pnorm(lpred_sep)),

    # generalized rediduals
    gr_pool = ext_benefit_tl * imr_pool + (1 - ext_benefit_tl) * rimr_pool,
    gr_sep = ext_benefit_tl * imr_sep + (1 - ext_benefit_tl) * rimr_sep,
  ) %>%
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
    log_total_g ~ ..stage1 | ext_benefit_tl:log_price ~ psc_pool:log_price
  ),
  "(2)" = fixest::xpd(
    log_total_g ~ ..stage1 | ext_benefit_tl:log_price ~ psc_sep:log_price
  ),
  "(3)" = fixest::xpd(
    log_total_g ~ ext_benefit_tl:log_price + gr_pool + ..stage1
  ),
  "(4)" = fixest::xpd(
    log_total_g ~ ext_benefit_tl:log_price + gr_sep + ..stage1
  ),
  "(5)" = fixest::xpd(
    log_total_g ~ ext_benefit_tl:log_price +
      gr_pool + gr_pool:ext_benefit_tl + ..stage1
  ),
  "(6)" = fixest::xpd(
    log_total_g ~ ext_benefit_tl:log_price +
      gr_sep + gr_sep:ext_benefit_tl + ..stage1
  )
)

stage2 %>%
  purrr::map(~ fixest::feols(
    ., cluster = ~ panelid,
    data = subset(estdf, i_ext_giving == 1)
  )) %>%
  modelsummary(
    title = paste(
      "Population First-Price Elasticities:",
      "Pooled 2SLS and Control Function Approach"
    ),
    coef_map = c(
      "fit_ext_benefit_tl:log_price" =
        "Propensity x log(first giving price)",
      "ext_benefit_tl:log_price" =
        "Applying tax relief x log(first giving price)",
      "log_pinc_all" = "log(income)",
      "gr_pool" = "Generalized residuals (pool)",
      "ext_benefit_tl:gr_pool" =
        "Applying tax relief x Generalized residuals (pool)",
      "gr_sep" = "Generalized residuals (separate)",
      "ext_benefit_tl:gr_sep" =
        "Applying tax relief x Generalized residuals (separate)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = tribble(
      ~"term", ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
      "Dummy of area", "X", "X", "X", "X", "X", "X",
      "Dummy of industry", "X", "X", "X", "X", "X", "X",
      "Square of age", "X", "X", "X", "X", "X", "X"
    )
  ) %>%
  kableExtra::add_header_above(c(
    " " = 1, "Pooled 2SLS" = 2, "CF Approach" = 4
  ))

# /*
#+
rmarkdown::render(
  "script/R/4-paneliv.r",
  output_dir = "report/view"
)
# */