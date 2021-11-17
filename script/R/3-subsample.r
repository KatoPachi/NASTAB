#' ---
#' title: |
#'   Appendix B: Estimate Elasiticity Using Subsample
#' subtitle: Not intended for publication
#' author: Hiroki Kato
#' bibliography: "../../Rmarkdown/ref_appx.bib"
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
) %>%
dplyr::filter(
  ext_benefit_tl == 0 | (ext_benefit_tl == 1 & i_ext_giving == 1)
)

#'
#+ include = FALSE, eval = params$preview
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
    psc2_pool = pnorm(lpred_pool), psc2_sep = pnorm(lpred_sep),

    # inverse mills ratio for R = 1
    imr1_pool = dnorm(lpred_pool) / pnorm(lpred_pool),
    imr1_sep = dnorm(lpred_sep) / pnorm(lpred_sep),

    # inverse mills ratio for R = 0
    imr0_pool = dnorm(lpred_pool) / (1 - pnorm(lpred_pool)),
    imr0_sep = dnorm(lpred_sep) / (1 - pnorm(lpred_sep)),

    # generalized error
    gr_pool = if_else(ext_benefit_tl == 1, imr1_pool, imr0_pool),
    gr_sep = if_else(ext_benefit_tl == 1, imr1_sep, imr0_sep)

  ) %>%
  ungroup() %>%
  select(-poolmod, -sepmod)

#'
#' # Estimate Elasiticity Using Subsample
#'
#' ## Sample Selection Bias Correction
#'
#' This supplement estimates price elasticity
#' using data from only those who have applied for tax relief.
#' If there is both
#' a year in which the same individual applied for tax relief
#' and a year when it did not,
#' we use only the year when the person applied for the relief.
#'
#' Since deduction applications are endogenous as described in this paper,
#' subsample estimation involves a sample selection bias.
#' Following @Wooldridge2010, we formally show this problem and its solution.
#' Consider the following model:
#' \begin{align}
#'   &Y_{it} = \beta  X_{it} + \mu_{i1} + \lambda_{t1} + e_{it1}, \\
#'   &R_{it} = 1[\gamma_1 Z_{it} + \gamma_2 X_{it} + \mu_{i2} + \lambda_{t2} + e_{it2} > 0].
#' \end{align}
#' where $Y_{it}$ is the logged value of giving amount ($\ln g_{it}$),
#' $X_{it}$ is the logged value of first giving price ($\ln p^f(y_{it})$),
#' and $R_{it}$ is the application dummy.
#' Thus, $\beta$ represents the price elasticity of giving,
#' which is our parameter of interest.
#' $Z_{it}$ is the wage earner dummy,
#' an instrument that allows arbitrary corrleation
#' with $\mu_{i1}$ and $\lambda_{i1}$ but
#' holds that exogeneity with respect to $u_{i1}$.
#' $\mu_i$ and $\eta_t$ is individual and time fixed effect, respectively.
#' $e_{it}$ is error term.
#' Assume that $E(e_{it1} |Z_{it}, X_{it}, \mu_{i1}, \lambda_{i1}) = 0$.
#' Note that, to clarify the problem,
#' we intentionally do not model covariates such as income.
#'
#' Since we estimate the model only for those who applied for the deduction,
#' the conditional expectation of the outcome equation is as follows:
#' \begin{align}
#'   E(Y_{it} |Z_{it}, X_{it}, \mu_{i1}, \lambda_{i1}, R_{it} = 1)
#'   = \beta  X_{it} + \mu_{i1} + \lambda_{t1}
#'   + E(e_{it1} |Z_{it}, X_{it}, \mu_{i1}, \lambda_{i1}, R_{it} = 1).
#' \end{align}
#'
#' The fixed effect estimator of $\beta$ is unbiased only if
#' $E(e_{it1} |Z_{it}, X_{it}, \mu_{i1}, \lambda_{i1}, R_{it} = 1) = 0$.
#' However, it is difficult to assume that
#' the idiosyncratic error of the donation amount
#' is independent of the tax deduction application
#' due to the simultaneous determination of
#' the tax deduction application and the donation amount.
#' Therefore, using the control function approach,
#' we eliminate this selection bias.
#'
#' This approach makes the following assumptions
#' about the error term of the outcome variable:
#' \begin{align}
#'   E(e_{it1} | Z_{it}, X_{it}, \mu_{i1}, \lambda_{t1}, e_{it2})
#'   = E(e_{it1} | e_{it2}) = \rho e_{it2}.
#' \end{align}
#' This equation suggests two assumptions.
#' First, the two fixed effects, $\mu_{i1}$ and $\lambda_{t1}$,
#' and observables, $(X_{it}, Z_{it})$ are independent of
#' the two error terms, $(e_{it1}, e_{it2})$.
#' Second, $e_{it1}$ is linearly correlated with $e_{it2}$,
#' the degree of which is constant with respect to time.
#'
#' Under this assumption,
#' we can write the conditional expectation of the error term, $e_{it1}$,
#' as follows:
#' \begin{align}
#'   E(e_{it1} | Z_{it}, X_{it}, \mu_{i1}, \lambda_{t1}, R_{it} = 1)
#'   = \rho E(e_{it2} | Z_{it}, X_{it}, R_{it} = 1).
#' \end{align}
#' Thus, the estimation model that eliminates the selection bias is as follows:
#' \begin{align}
#'   Y_{it} = \beta  X_{it} + \mu_{i1} + \lambda_{t1}
#'   + \rho E(e_{it2} | Z_{it}, X_{it}, R_{it} = 1) + u_{it1},
#' \end{align}
#' where, by construction, $E(u_{it1} | Z_{it}, X_{it}, R_{it} = 1) = 0$.
#' If we knew $E(e_{it2} | Z_{it}, X_{it}, R_{it} = 1)$,
#' then we can obtaine unbiased estimator of $\beta$.
#' The correction term $E(e_{it2} | Z_{it}, X_{it}, R_{it} = 1)$
#' can be obtained by the inverse Mills ratio (IMR).
#' To calculate the inverse Mills ratio, we use the probit estimation shown in
#' \@ref(tab:stage1) in Appendix A (pooled model and separate model).
#'
#' ## Results
#'
#+ benchmark
basemod <- list(
  "(1)" = fixest::xpd(log_total_g ~ log_price + ..stage24),
  "(2)" = fixest::xpd(log_total_g ~ log_price + gr_pool + ..stage24),
  "(3)" = fixest::xpd(log_total_g ~ log_price * gr_pool + ..stage24),
  "(4)" = fixest::xpd(log_total_g ~ log_price + gr_sep + ..stage24),
  "(5)" = fixest::xpd(log_total_g ~ log_price * gr_sep + ..stage24)
)

basemod %>%
  purrr::map(~ fixest::feols(
    .,
    data = subset(estdf, ext_benefit_tl == 1),
    cluster = ~ panelid, se = "cluster"
  )) %>%
  modelsummary(
    title = "First Price Intensive-Margin Elasiticity (Subsample Analysis)",
    coef_map = c(
      "log_price" = "log(first price)",
      "log_pinc_all" = "log(annual taxable income)",
      "log_price:gr_pool" = "log(first price) x IMR",
      "log_price:gr_sep" = "log(first price) x IMR",
      "gr_pool" = "Correction term",
      "gr_sep" = "Correction term"
    ),
    gof_omit = "^(?!R2 Adj.|FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = tribble(
      ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)",
      "Square age", "X", "X", "X", "X", "X",
      "Method of IMR", "", "Pooled", "Pooled", "Separate", "Separate"
    )
  ) %>%
  kableExtra::kable_styling(font_size = 9) %>%
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
#' Table \@ref(tab:benchmark) shows the estimation results of price elasticity.
#' Model (1) does not add a selection correction term,
#' while models (2) and (4) add it to the explanatory variables.
#' We obtain the inverse mills ratio from the pooled probit model
#' and the separated probit model, respectively.
#' Since the coefficient of the correction term is statistically insignificant,
#' the selection bias of the application of tax relief is not severe.
#' Therefore, the estimated elasticity is in the range of -1.3 to -1.6
#' with or without the correction term.
#' The estimated value is very close to the result of this paper.
#'
#' Models (3) and (5) considered
#' the heterogeneity of price elasticity among individuals.
#' Based on the model in the previous subsection,
#' we can write a (correlated) random coefficient model
#' that allows this heterogeneity as follows:
#' \begin{align}
#'   Y_{it} = \bar{\beta} X_{it} + \mu_{i1} + \lambda_{t1}
#'   + \rho E(e_{it2} | Z_{it}, X_{it}, R_{it} = 1)
#'   + \{(\beta_i - \bar{\beta}) X_{it} +  u_{it1}\},
#' \end{align}
#' where $\bar{\beta} = E(\beta_i | R_i = 1)$.
#' Then, since $(\beta_i - \bar{\beta}) X_{it}$ is included in the error term,
#' we cannot obtain unbiased estimator of $\bar{\beta}$,
#' which is a parameter of our interest,
#' by controlling only the selection correction term.
#'
#' @Wooldridge2015 proposes an estimation method that solves this problem
#' by making the following assumptions in the elements of this new error term:
#' \begin{align}
#'   E(\beta_i - \bar{\beta} | Z_{it}, X_{it}, \mu_{i1}, \lambda_{t1}, e_{it2})
#'   = E(\beta_i - \bar{\beta} | e_{it2}) = \eta e_{it2}.
#' \end{align}
#' Thus, the estimation model that eliminates both the selection bias and
#' the bias from random coefficient is as follows:
#' \begin{align}
#'   Y_{it} =& \beta  X_{it} + \mu_{i1} + \lambda_{t1} \\
#'   &+ \rho \lambda(Z_{it}, X_{it}) + \eta \lambda(Z_{it}, X_{it}) \times X_{it}
#'   + \tilde{u}_{it1},
#' \end{align}
#' where $\lambda(Z_{it}, X_{it})$ is the inverse Mills ratio.
#' Note that $E(\tilde{u}_{it1} | Z_{it}, X_{it}, R_{it} = 1) = 0$
#' by construction.
#' Therefore,
#' by simply adding an intersection
#' between the correction term and the giving price to models (3) and (4),
#' we can eliminate the bias resulting from the heterogeneous elasticity
#' and estimate the unbiased average elasticity.
#'
#' The average elasticity estimated by models (3) and (5) is about -1.5,
#' which is consistent with the results of this paper.
#' Also, since the coefficients of the intersection term
#' between the correction term and the giving price
#' are statistically insignificant,
#' the price elasticity is unlikely to vary significantly among individuals.
#'
#+ robustbenchmark1
basemod %>%
  purrr::map(~ fixest::feols(
    .,
    data = subset(estdf, ext_benefit_tl == 1 & (year < 2013 | year > 2014)),
    cluster = ~ panelid, se = "cluster"
  )) %>%
  modelsummary(
    title = paste(
      "First Price Intensive-Margin Elasiticity without Annoucment Effect",
      "(Subsample Analysis)"
    ),
    coef_map = c(
      "log_price" = "log(first price)",
      "log_pinc_all" = "log(annual taxable income)",
      "log_price:gr_pool" = "log(first price) x IMR",
      "log_price:gr_sep" = "log(first price) x IMR",
      "gr_pool" = "Correction term",
      "gr_sep" = "Correction term"
    ),
    gof_omit = "^(?!R2 Adj.|FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = tribble(
      ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)",
      "Square age", "X", "X", "X", "X", "X",
      "Method of IMR", "", "Pooled", "Pooled", "Separate", "Separate"
    )
  ) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at individual level.",
      "We exclude observations in 2013 and 2014."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#'
#+ robustbenchmark2
lastmod <- list(
  "(1)" = fixest::xpd(log_total_g ~ ..stage24 | log_lprice ~ log_price),
  "(2)" = fixest::xpd(
    log_total_g ~ gr_pool + ..stage24 | log_lprice ~ log_price
  ),
  "(3)" = fixest::xpd(
    log_total_g ~ gr_sep + ..stage24 | log_lprice ~ log_price
  )
)

lastmod %>%
  purrr::map(~ fixest::feols(
    .,
    data = subset(estdf, ext_benefit_tl == 1),
    cluster = ~ panelid, se = "cluster"
  )) %>%
  modelsummary(
    title = paste(
      "Last Price Intensive-Margin Elasiticity (Subsample Analysis)"
    ),
    coef_map = c(
      "fit_log_lprice" = "log(last price)",
      "log_pinc_all" = "log(annual taxable income)",
      "gr_pool" = "Correction term",
      "gr_sep" = "Correction term"
    ),
    gof_omit = "^(?!R2 Adj.|FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = tribble(
      ~term, ~"(1)", ~"(2)", ~"(3)",
      "Square age", "X", "X", "X",
      "Method of IMR", "", "Pool", "Separate"
    )
  ) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at individual level.",
      "The instrument of last-price is the first-price of giving."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#'
#' We show the results of the same robustness test as in this paper
#' in Tables \@ref(tab:robustbenchmark1) and \@ref(tab:robustbenchmark2).
#' To eliminate the announcement effect of the 2014 tax reform,
#' Table \@ref(tab:robustbenchmark1) shows
#' estimates excluding 2013 and 2014 data.
#' As a result, the selection bias and
#' the bias from the heterogeneous elasticity are not large.
#' Using the inverse mills ratio by the pooled probit model,
#' the price elasticity is about -1.2.
#' Given the heterogeneity of elasticity,
#' this price elasticity is statistically insignificant.
#' Moreover,
#' when we use the inverse mills ratio by the separated probit model,
#' the price elasticity is about -1.4, which is statistically significant.
#' Table \@ref(tab:robustbenchmark2) shows
#' the estimation results of the last-price elasticity.
#' As a result, the elasticity is in the range of -1.4 to -1.8,
#' which is similar to the result shown in this paper.
#'
#+ kdiffbenchmark
fixest::setFixest_fml(
  ..kdiff1 = ~ log_diff1I + diff1_sqage,
  ..kdiff2 = ~ log_diff2I + diff2_sqage,
  ..kdiff3 = ~ log_diff3I + diff3_sqage,
  ..kdifffe = ~ year + area + industry
)

kdiffmod <- list(
  "(1)" = fixest::xpd(
    log_diff1g ~ ..kdiff1 | ..kdifffe | log_diff1p ~ log_iv1price
  ),
  "(2)" = fixest::xpd(
    log_diff1g ~ ..kdiff1 + d(gr_pool, 1) |
    ..kdifffe | log_diff1p ~ log_iv1price
  ),
  "(3)" = fixest::xpd(
    log_diff1g ~ ..kdiff1 + d(gr_sep, 1) |
    ..kdifffe | log_diff1p ~ log_iv1price
  ),
  "(4)" = fixest::xpd(
    log_diff2g ~ ..kdiff2 | ..kdifffe | log_diff2p ~ log_iv2price
  ),
  "(5)" = fixest::xpd(
    log_diff2g ~ ..kdiff2 + d(gr_pool, 2) |
    ..kdifffe | log_diff2p ~ log_iv2price
  ),
  "(6)" = fixest::xpd(
    log_diff2g ~ ..kdiff2 + d(gr_sep, 2) |
    ..kdifffe | log_diff2p ~ log_iv2price
  ),
  "(7)" = fixest::xpd(
    log_diff3g ~ ..kdiff3 | ..kdifffe | log_diff3p ~ log_iv3price
  ),
  "(8)" = fixest::xpd(
    log_diff3g ~ ..kdiff3 + d(gr_pool, 3) |
    ..kdifffe | log_diff3p ~ log_iv3price
  ),
  "(9)" = fixest::xpd(
    log_diff3g ~ ..kdiff3 + d(gr_sep, 3) |
    ..kdifffe | log_diff3p ~ log_iv3price
  )
)

kdiffmod %>%
  purrr::map(~ fixest::feols(
    .,
    cluster = ~ panelid, se = "cluster",
    data = subset(estdf, ext_benefit_tl == 1),
    panel.id = ~ panelid + year
  )) %>%
  modelsummary(
    title = "$k$-th Difference Model with Instrument",
    coef_map = c(
      "fit_log_diff1p" = "Difference of first price",
      "log_diff1I" = "Difference of annual income",
      "fit_log_diff2p" = "Difference of first price",
      "log_diff2I" = "Difference of annual income",
      "fit_log_diff3p" = "Difference of first price",
      "log_diff3I" = "Difference of annual income",
      "d(gr_pool, 1)" = "Correction term",
      "d(gr_pool, 2)" = "Correction term",
      "d(gr_pool, 3)" = "Correction term",
      "d(gr_sep, 1)" = "Correction term",
      "d(gr_sep, 2)" = "Correction term",
      "d(gr_sep, 3)" = "Correction term"
    ),
    gof_omit = "^(?!R2 Adj.|FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = tribble(
      ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
      ~"(7)", ~"(8)", ~"(9)",
      "Difference of square age", "X", "X", "X", "X", "X", "X",
      "X", "X", "X",
      "Lag", "1-year", "1-year", "1-year", "2-year", "2-year", "2-year",
      "3-year", "3-year", "3-year",
      "Method of IMR", "", "Pool", "Separate", "", "Pool", "Separate",
      "", "Pool", "Separate"
    )
  ) %>%
  kableExtra::kable_styling(font_size = 9, latex_options = "scale_down") %>%
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
#+ leadlagbenchmark
fixest::setFixest_fml(
  ..stage1ll = ~ log_pinc_all + sqage +
    d(log_price, 1) + d(log_price, -1) +
    d(log_pinc_all, 1) + d(log_pinc_all, -1) +
    factor(area) + factor(industry)
)

poolstage1_ll <- fixest::feglm(
  fixest::xpd(ext_benefit_tl ~ employee + log_price + ..stage1ll),
  family = binomial(link = "probit"),
  data = subset(df, year <= 2017),
  panel.id = ~ panelid + year
)

estdf2 <- df %>%
  dplyr::filter(year <= 2017 & !is.na(ext_benefit_tl)) %>%
  modelr::add_predictions(poolstage1_ll, type = "link", var = "lpred_pool") %>%
  mutate(
    # propensity score
    psc_pool = pnorm(lpred_pool),
    # inverse mills ratio
    imr_pool = dnorm(lpred_pool) / pnorm(lpred_pool)
  ) %>%
  dplyr::filter(ext_benefit_tl == 1 & i_ext_giving == 1)

leadlagmod <- list(
  "(1)" = fixest::xpd(
    log_total_g ~ log_price + d(log_price, 1) + d(log_price, -1) +
    d(log_pinc_all, 1) + d(log_pinc_all, -1) + ..stage24
  ),
  "(2)" = fixest::xpd(
    log_total_g ~ log_price + imr_pool +
    d(log_price, 1) + d(log_price, -1) +
    d(log_pinc_all, 1) + d(log_pinc_all, -1) + ..stage24
  )
)

leadlagmod %>%
  purrr::map(~ fixest::feols(
    .,
    data = subdf2, panel.id = ~ panelid + year,
    cluster = ~panelid, se = "cluster"
  )) %>%
  modelsummary(
    title = paste(
      "First Price Intensive-Margin Elasiticities Including Lead and Lag"
    ),
    coef_map = c(
      "log_price" = "log(first giving price)",
      "log_pinc_all" = "log(annual taxable income)",
      "d(log_price, 1)" = "1-year lag of price",
      "d(log_price, -1)" = "1-year lead of price",
      "d(log_pinc_all, 1)" = "1-year lag of income",
      "d(log_pinc_all, -1)" = "1-year lead of income",
      "imr_pool" = "Correction term"
    ),
    gof_omit = "^(?!R2 Adj.|FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = tribble(
      ~term, ~"(1)", ~"(2)",
      "Square age", "X", "X",
      "Method of IMR", "", "Pool"
    )
  ) %>%
  kableExtra::kable_styling(font_size = 9) %>%
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
#' In addition to the same robustness test as in this paper,
#' the results of the other two robustness tests
#' are shown in Tables \@ref(tab:kdiffbenchmark) and \@ref(tab:leadlagbenchmark).
#' Table \@ref(tab:kdiffbenchmark) is an analysis dealing with
#' the endogeneity of first-price by income manipulation.
#' Since income is generally endogenous,
#' the first-price of giving is also an endogenous variable.
#' Under the income deduction system,
#' changes in income affect donations through the income effect
#' and the giving price through marginal tax rates
#' [@Randolph1995; @Auten2002; @Bakija2011].
#' Therefore, following @Saez2002 and @Almunia2020,
#' we estimate the following k-th difference model:
#' \begin{align}
#'   \Delta^k \ln g_{it} = \varepsilon_p \Delta^k \ln p^f_{it}(y_{it})
#'   + \varepsilon_y \Delta^k \ln y_{it} + \Delta^k X_{it} \beta
#'   + \mu_i + \iota_t + v_{it},
#' \end{align}
#' where $\Delta^k \ln g_{it} = \ln (g_{it} / g_{it-k})$,
#' and $\Delta^k \ln y_{it} = \ln (y_{it} / y_{it-k})$.
#' The variable
#' $\Delta^k p^f_{it}(y_{it}) = \ln (p^f_{it}(y_{it}) / p^f_{it-k}(y_{it-k}))$
#' is instumented by $\ln (p^f_{it}(y_{it-k})/p^f_{it-k}(y_{it-k}))$.
#'
#' As a result,
#' the price elasticity changes greatly
#' depending on the correction term of the selection,
#' but the degree of the selection bias is not large.
#' In addition, when we add the correction term,
#' the price elasticity is about -1.5 in the 3-year difference model,
#' which is similar to the result of this paper.
#' However, in the 1-year difference model and 2-year difference model,
#' the absolute value of price elasticity is less than 1,
#' which is statistically insignificant.
#' Therefore, the value of price elasticity is unstable
#' for the number of years of lag.
#'
#' Table \@ref(tab:leadlagbenchmark) adds
#' lagged and future changes of giving price and income
#' to the explanatory variables
#' to directly control the dynamic effects of
#' price and income changes on donations (proposed by @Bakija2011).
#' As a result, price elasticity is statistically insignificant.
#' However, because our data is unbalanced panel data,
#' the sample size is quite small.
#' In that respect, the results of this analysis are unreliable.
#'
#' ## Additional References {-}
#'
#' ::: {#refs_appx}
#' :::
#'
# /*
#+
rmarkdown::render(
  "script/R/3-subsample.r",
  output_dir = "report/view"
)
# */