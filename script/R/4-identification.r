#' ---
#' title: |
#'   2SLS and CF Approach in Panel Data
#' subtitle: Not intended for publication
#' author: Hiroki Kato
#' bibliography: ../../Rmarkdown/ref_appx.bib
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
#' # Conceptual Framework
#' ## Optimization Problem
#'
#' Following @Almunia2020, we formulate the optimaization problem as follows:
#'
#' \begin{align*}
#'   \max_{x_{it}, g_{it}, R_{it}} &U(x_{it}, g_{it}, G_t)
#'   = x_{it} - R_{it}K_{it} + \theta_i u(g_{it}) + V(G_t), \\
#'   \text{s.t.}\:\:
#'   &x_{it} + g_{it} = y_{it} - T_{it}(y_{it}, R_{it} g_{it}), \\
#'   &G_t = g_{it} + G_{-it}, \\
#'   &T_{it} = \begin{cases}
#'     \tau_t(y_{it} - R_{it} g_{it}) (y_{it} - R_{it} g_{it}) &t < 2014 \\
#'     \tau_t(y_{it}) y_{it} - m R_{it} g_{it} &t \ge 2014
#'   \end{cases}
#' \end{align*}
#'
#' This model implicitly assume no income effect and no saving.
#' By this assumption,
#' a dynamic optimization reduces to a repeated static optimaization problem.
#' We further assume that
#' (i) decision-making before 2014 is based on $\tau(y_{it})$ (first-price);
#' and (ii) $G_{-it}$ is large enough to $V'(G_t) \approx 0$.
#' Especially, by the second assumption,
#' we do not need to take $\partial V(G_t) / \partial g_{it}$ into accuount
#' when we derive the optimal donation levels.
#'
#' Under these assumptions,
#' we can reformulate the optimization problem as follows:
#'
#' \begin{align*}
#'   \max_{g_{it}, R_{it}} &U(g_{it}) =
#'   - (1 - R_{it}s_{it}) g_{it} + \theta_i u(g_{it}), \\
#'   \text{s.t.}\:\:
#'   &s_{it} = \begin{cases}
#'     \tau(y_{it}) &t < 2014 \\
#'     m &t \ge 2014
#'   \end{cases},
#' \end{align*}
#'
#' where $s_{it}$ is tax incentive of monetary donations
#' for individual $i$ in year $t$.
#'
#' Let $g(1-s_{it};\theta_i)$ and $g(1;\theta_i)$
#' be the optimal donation levels when $R_{it} = 1$ and $R_{it} = 0$,
#' respectively.
#' Each indirect utility function is
#'
#' \begin{align*}
#'   v(1 - s_{it}; \theta_i)
#'   &= -(1 - s_{it})g(1 - s_{it}; \theta_i)
#'   + \theta_i u(g(1 - s_{it}; \theta_i)), \\
#'   v(1; \theta_i)
#'   &= -g(1;\theta_i) + \theta_i u(g(1;\theta_i)).
#' \end{align*}
#'
#' Thus, individual $i$ applies for tax relief in year $t$ if and only if
#'
#' \begin{align*}
#'   \Delta v \equiv v(1 - s_{it};\theta_i) - v(1;\theta_i) \ge K_{it}.
#' \end{align*}
#'
#' ## Specification of Warm-Glow Utility
#'
#' When conduction the structual estimation,
#' @Almunia2020 specify the warm-glow utility $u$ as follows:
#'
#' \begin{align*}
#'   u(g) = \frac{g^{1 - 1/\gamma_i}}{1 - \frac{1}{\gamma_i}}.
#' \end{align*}
#'
#' where $\gamma_i > 0$.
#'
#' If we follow this specification,
#' we can derive the optimal donation level as follows:
#'
#' \begin{align*}
#'   g(1 - s_{it};\theta_i)
#'   &= \left( \theta_i/(1 - s_{it}) \right)^{\gamma_i}, \\
#'   g(1;\theta_i)
#'   &= \left( \theta_i \right)^{\gamma_i}.
#' \end{align*}
#'
#' Moreover, these logarithm values are
#'
#' \begin{align*}
#'   \ln g(1 - s_{it};\theta_i)
#'   &= \gamma_i \ln \theta_i - \gamma_i \ln (1 - s_{it}), \\
#'   \ln g(1;\theta_i)
#'   &= \gamma_i \ln \theta_i.
#' \end{align*}
#'
#' Thus, individual $i$ applies for tax relief in year $t$ if and only if
#'
#' \begin{align*}
#'   \theta_i \frac{(\theta_i / q^f_{it})^{\gamma_i(1 - 1/\gamma_i)}
#'   - \theta_i^{\gamma_i(1 - 1/\gamma_i)}}{1 - 1 / \gamma_i} -
#'   (\theta_i^{\gamma_i} (q^f_{it})^{1 - \gamma_i} - \theta_i^{\gamma_i})
#'   &\ge K_{it}  \\
#'   \theta_i \frac{\theta_i^{\gamma_i - 1}((q^f_{it})^{1 - \gamma_i} - 1)}
#'   {1 - 1 / \gamma_i} - \theta_i^{\gamma_i}((q^f_{it})^{1 - \gamma_i} - 1)
#'   &\ge K_{it}  \\
#'   \theta_i^{\gamma_i}
#'   \frac{(q^f_{it})^{1 - \gamma_i} - 1}{1 - 1 / \gamma_i} -
#'   \theta_i^{\gamma_i}((q^f_{it})^{1 - \gamma_i} - 1)
#'   &\ge K_{it} \\
#'   \frac{\theta_i^{\gamma_i}}{\gamma_i - 1}((q^f_{it})^{1 - \gamma_i} - 1)
#'   &\ge K_{it}
#' \end{align*}
#'
#' # Identification
#'
#' ## Statistical Model
#'
#' We assume that expectation of
#' $\ln g(1-s_{it};\theta_i)$ and $\ln g(1;/\theta_i)$
#' conditional on $s_{it}$ and $\theta_i$ has the following functional forms:
#'
#' \begin{align*}
#'   &E[\ln g(1-s_{it};\theta_i)|s_{it}, \theta_i]
#'   = \theta_i + \gamma_i \ln (1 - s_{it}), \\
#'   &E[\ln g(1;\theta_i) | \theta_i] = \theta_i. \\
#' \end{align*}
#'
#' Thus, realized value of
#' $\ln g(1-s_{it};\theta_i)$ and $\ln g(1;/\theta_i)$ are
#'
#' \begin{align*}
#'   &\ln g(1-s_{it};\theta_i) =
#'   \theta_i + \gamma_i \ln (1 - s_{it}) + \epsilon_{1it}, \\
#'   &\ln g(1;\theta_i) = \theta_i + \epsilon_{0it}. \\
#' \end{align*}
#'
#' where $E[\epsilon_{1it}|s_{it}, \theta_i]
#' = E[\ln g(1-s_{it};\theta_i)
#' - E[\ln g(1-s_{it};\theta_i)|s_{it}, \theta_i]|s_{it}, \theta_i] = 0$,
#' and $E[\epsilon_{0it}|\theta_i]
#' = E[\ln g(1;\theta_i) - E[\ln g(1;\theta_i) | \theta_i]|\theta_i] = 0$,
#' by construction.
#'
#' Since we cannot both $g(1 - s_{it};\theta_i)$ and $g(1;\theta_i)$
#' for individual $i$ in year $t$,
#' the observed outcome $\ln g_{it}$ is
#'
#' \begin{align*}
#'   \ln g_{it}
#'   &= \theta_i + \gamma_i R_{it} \ln (1 - s_{it})
#'   + (\epsilon_{0it} + R_{it}(\epsilon_{1it} - \epsilon_{0it})) \\
#'   &= \theta_i + \bar{\gamma} R_{it} \ln (1 - s_{it})
#'   + \{(\gamma_i - \bar{\gamma})R_{it}\ln (1 - s_{it})
#'   + (\epsilon_{0it} + R_{it}(\epsilon_{1it} - \epsilon_{0it}))\},
#' \end{align*}
#'
#' where $\bar{\gamma} = E(\gamma_i)$.
#'
#+
fixest::setFixest_fml(
  ..fullx = ~ log_pinc_all + sqage | year + panelid + area + industry
)

reducemod <- list(
  "(1)" = list(
    model = fixest::xpd(log_total_g ~ ext_benefit_tl:log_price + ..fullx),
    data = subset(df, i_ext_giving == 1)
  ),
  "(2)" = list(
    model = fixest::xpd(log_total_g ~ log_price + ..fullx),
    data = subset(df, ext_benefit_tl == 1 & i_ext_giving == 1)
  ),
  "(3)" = list(
    model = fixest::xpd(log_total_g ~ ..fullx),
    data = subset(df, ext_benefit_tl == 0 & i_ext_giving == 1)
  ),
  "(4)" = list(
    model = fixest::xpd(log_total_g ~ log_price + ..fullx),
    data = subset(df, ext_benefit_tl == 0 & i_ext_giving == 1)
  )
)

reducemod %>%
  purrr::map(~ fixest::feols(
    .$model, data = .$data,
    cluster = ~ panelid, se = "cluster"
  )) %>%
  modelsummary(
    title = "FE Estimation of Logged Donations (Intensive-margin)",
    coef_map = c(
      "ext_benefit_tl:log_price" = "Applying tax relief x log(first price)",
      "log_price" = "log(first price)",
      "log_pinc_all" = "log(income)"
    ),
    gof_omit = "^(?!R2 Adj.|FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01)
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(
    c(" " = 1, "Pooling" = 1, "R = 1" = 1, "R = 0" = 2)
  ) %>%
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
#' # First-Stage Estimation
#'
#+ include = FALSE, eval = params$preview
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
#+ eval = params$preview
sepstage1 %>%
  pull(sepmod, name = year) %>%
  list.merge(list("Pooled" = poolstage1), .) %>%
  modelsummary(
    title = "Probit Estimation of Selection of Applying for Tax Relief",
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
      ~"term", ~"Pooled", ~"2012", ~"2013", ~"2014", ~"2015", ~"2016", ~"2017",
      "Dummy of area", "X", "X", "X", "X", "X", "X", "X",
      "Dummy of industry", "X", "X", "X", "X", "X", "X", "X"
    )
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    " " = 2, "Separated Probit Model" = 6
  )) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#'
#' # FE-2SLS Approach
#'
#+ intensive
stage2 <- list(
  "(1)" = fixest::xpd(
    log_total_g ~ ..fullx | ext_benefit_tl:log_price ~ employee:log_price
  ),
  "(2)" = fixest::xpd(
    log_total_g ~ ..fullx | ext_benefit_tl:log_price ~ psc_pool:log_price
  ),
  "(3)" = fixest::xpd(
    log_total_g ~ ..fullx | ext_benefit_tl:log_price ~ psc_sep:log_price
  ),
  "(4)" = fixest::xpd(
    log_total_g ~ psc_pool:log_price + ..fullx
  ),
  "(5)" = fixest::xpd(
    log_total_g ~ psc_sep:log_price + ..fullx
  )
)

stage2 %>%
  purrr::map(~ fixest::feols(
    .,
    cluster = ~ panelid,
    data = subset(estdf, i_ext_giving == 1)
  )) %>%
  modelsummary(
    title = paste(
      "FE-2SLS Approach of Intensive-Margin"
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
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(" " = 1, "FE-2SLS" = 3, "OLS" = 2)) %>%
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
#' # Subsample Approach ($R_{it} = 1$)
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

est_basemod1 <- basemod %>%
  purrr::map(~ fixest::feols(
    .,
    data = subset(estdf, ext_benefit_tl == 1 & i_ext_giving == 1),
    cluster = ~ panelid, se = "cluster"
  ))

est_basemod1 %>%
  modelsummary(
    title = "Intensive-Margin Model for R = 1",
    coef_map = c(
      "log_price" = "log(first price)",
      "log_pinc_all" = "log(income)",
      "log_price:gr_pool" = "log(first price) x Correction term",
      "log_price:gr_sep" = "log(first price) x Correction term",
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
#' # Subsample Approach ($R_{it} = 0$)
#'
#+
est_basemod0 <- basemod %>%
  purrr::map(~ fixest::feols(
    .,
    data = subset(estdf, ext_benefit_tl == 0 & i_ext_giving == 1),
    cluster = ~ panelid, se = "cluster"
  ))

est_basemod0 %>%
  modelsummary(
    title = "Intensive-Margin Model for R = 0",
    coef_map = c(
      "log_price" = "log(first price)",
      "log_pinc_all" = "log(income)",
      "log_price:gr_pool" = "log(first price) x Correction term",
      "log_price:gr_sep" = "log(first price) x Correction term",
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
#' # Effect of Applying for Tax Relief
#'
#+
rawvalue <- function(x) x

preddt <- estdf %>%
  modelr::add_predictions(est_basemod1[["(1)"]], var = "pred11") %>%
  modelr::add_predictions(est_basemod1[["(2)"]], var = "pred12") %>%
  modelr::add_predictions(est_basemod1[["(3)"]], var = "pred13") %>%
  modelr::add_predictions(est_basemod1[["(4)"]], var = "pred14") %>%
  modelr::add_predictions(est_basemod1[["(5)"]], var = "pred15") %>%
  modelr::add_predictions(est_basemod0[["(1)"]], var = "pred01") %>%
  modelr::add_predictions(est_basemod0[["(2)"]], var = "pred02") %>%
  modelr::add_predictions(est_basemod0[["(3)"]], var = "pred03") %>%
  modelr::add_predictions(est_basemod0[["(4)"]], var = "pred04") %>%
  modelr::add_predictions(est_basemod0[["(5)"]], var = "pred05") %>%
  mutate(
    efmod1 = pred11 - pred01,
    efmod2 = pred12 - pred02,
    efmod3 = pred13 - pred03,
    efmod4 = pred14 - pred04,
    efmod5 = pred14 - pred05
  )

ate <- preddt %>%
  summarize_at(
    vars(starts_with("efmod")),
    list(~ mean(., na.rm = TRUE))
  ) %>%
  mutate(estimand = "ATE")

preddt %>%
  group_by(ext_benefit_tl) %>%
  summarize_at(
    vars(starts_with("efmod")),
    list(~mean(., na.rm = TRUE))
  ) %>%
  mutate(estimand = if_else(ext_benefit_tl == 1, "ATT", "ATU")) %>%
  select(-ext_benefit_tl) %>%
  bind_rows(ate) %>%
  pivot_longer(
    -estimand,
    names_to = "model", names_prefix = "efmod",
    values_to = "effect"
  ) %>%
  mutate(model = sprintf("(%1s)", model)) %>%
  datasummary(
    model * effect ~ rawvalue * estimand,
    data = .,
    fmt = 3
  ) %>%
  kableExtra::kable_styling()

#'
#' # Welfare Analysis by @Almunia2020
#'
#' The partial effect of $(1 - s_{it})$ on the indirect utility function is
#'
#' \begin{align*}
#'   \frac{\partial v(1 - s_{it};\theta_i)}{\partial (1 - s_{it})}
#'   &= -g(1 - s_{it};\theta_i)
#'   - (1 - s_{it}) \frac{\partial g(1-s_{it};\theta_i)}{\partial (1 - s_{it})}
#'   + \theta_i \frac{\partial u}{\partial g}
#'   \frac{\partial g(1-s_{it};\theta_i)}{\partial (1 - s_{it})} \\
#'   &= -g(1 - s_{it};\theta_i)
#'   + \left(\theta_i \frac{\partial u}{\partial g} - (1 - s_{it}) \right)
#'   \frac{\partial g(1-s_{it};\theta_i)}{\partial (1 - s_{it})} \\
#'   &= -g(1 - s_{it}; \theta_i)
#' \end{align*}
#'
#' The partial effect of $\theta_i$ on $\Delta v$ is
#'
#' \begin{align*}
#'   \frac{\partial \Delta v}{\partial \theta_i}
#'   &= 
#' \end{align*}
#'
#+
apemod <- list(
  "(1)" = fixest::xpd(i_total_giving ~ price + ..stage24),
  "(2)" = fixest::xpd(i_total_giving ~ price + gr_pool + ..stage24),
  "(3)" = fixest::xpd(i_total_giving ~ price * gr_pool + ..stage24),
  "(4)" = fixest::xpd(i_total_giving ~ price + gr_sep + ..stage24),
  "(5)" = fixest::xpd(i_total_giving ~ price * gr_sep + ..stage24)
)

est_apemod0 <- apemod %>%
  purrr::map(~ fixest::feols(
    .,
    data = subset(estdf, ext_benefit_tl == 0 & i_ext_giving == 1),
    cluster = ~panelid, se = "cluster"
  ))

est_apemod0 %>%
  modelsummary(
    title = "Partial Effect of Price (Subsets with R = 0)",
    coef_map = c(
      "price" = "First price",
      "log_pinc_all" = "log(income)",
      "price:gr_pool" = "First price x Correction term",
      "price:gr_sep" = "First price x Correction term",
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
elast <- est_basemod1 %>%
  purrr::map(function(x) {
    tidy(x) %>%
      dplyr::filter(term == "log_price") %>%
      dplyr::select("elast" = estimate)
  }) %>%
  reduce(bind_rows) %>%
  mutate(model = names(est_basemod1))

partef <- est_apemod0 %>%
  purrr::map(function(x) {
    est <- tidy(x) %>%
      dplyr::filter(term == "price") %>%
      dplyr::select("partef" = estimate)

    n <- glance(x) %>% select(nobs)

    return(bind_cols(est, n))
  }) %>%
  reduce(bind_rows) %>%
  mutate(
    model = names(est_apemod0),
    partef = partef * nobs
  ) %>%
  dplyr::select(-nobs)

elast %>%
  left_join(partef, by = "model") %>%
  mutate(g1 = with(
    subset(df, ext_benefit_tl == 1),
    sum(i_total_giving, na.rm = TRUE)
  )) %>%
  mutate(
    devide = partef / g1,
    elast = abs(elast)
  ) %>%
  datasummary(
    model ~ (` ` = rawvalue) * (
      (`Elasticity (R = 1)` = elast) +
      (`Partial Effect (R = 0)` = partef) +
      (`Sum of Giving (R = 1)` = g1) +
      (`Partial Effect (R = 0) / Sum of Giving (R = 1)` = devide)),
    data = .,
    fmt = 3
  ) %>%
  kableExtra::kable_styling() %>%
  footnote(
    general_title = "",
    general = paste(
      "Partial effect for R = 0 is multiplied by observations."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#'
# /*
#+
rmarkdown::render(
  "script/R/4-identification.r",
  output_dir = "report/view"
)
# */