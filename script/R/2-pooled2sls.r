#' ---
#' title: |
#'   Main Results
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
) %>%
dplyr::filter(
  ext_benefit_tl == 0 | (ext_benefit_tl == 1 & i_ext_giving == 1)
)

#'
#' # Results
#'
#' In this section, we report the price elasticity of intensive-margin and
#' extensive-margin, respectively.
#' Note that we provide an appendix with the first-stage estimation results
#' used to calculate the propensity score.
#' As a basic result, even if we control covariates such as income,
#' giving price, and industry dummy,
#' the wage earner dummy is strongly and positively correlated with
#' the application of donation deduction/credit.
#' However, when the sample is divided by year,
#' the partial correlation between the wage earner dummy and
#' the application of donation deduction/credit in 2013 is
#' statistically insignificant.
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
#+ stage1, results = if(params$preview) "markup" else "hide"
sepstage1 %>%
  pull(sepmod, name = year) %>%
  list.merge(list("Pooled" = poolstage1), .) %>%
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
      ~"term", ~"Pooled", ~"2012", ~"2013", ~"2014", ~"2015", ~"2016", ~"2017",
      "Dummy of area", "X", "X", "X", "X", "X", "X", "X",
      "Dummy of industry", "X", "X", "X", "X", "X", "X", "X"
    )
  ) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::add_header_above(c(
    " " = 2, "Separated Probit Model" = 6
  ))

#'
#' ## Intensive Margin
#'
#+ intensive
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
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::add_header_above(c(" " = 1, "FE-2SLS" = 3, "OLS" = 2))

#'
#' Table \@ref(tab:intensive) shows
#' the estimation results of price elasticity of intensive-margin.
#' Model (1) uses the intersection of the wage earner dummy and
#' the giving (first) price as an instrumental variable.
#' Models (2) and (3) use
#' the intersection of the propensity score of application and
#' the giving (first) price as an instrumental variable.
#' We use pooled model and separate model to calculate propensity scores,
#' respectively.
#' In models (4) and (5), we add the intersection between
#' the propensity score of application and the giving (first) price
#' directly to the explanatory variables.
#' The estimated value varies slightly depending on the estimation method,
#' but it is in the range of -1.5 to -1.8.
#' Therefore, a 1% price reduction will increase the donation amount by 1.5-1.8%
#' for those who apply for a donation deduction.
#'
#+ rob1intensive, results = if(params$preview) "markup" else "hide"
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
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::add_header_above(c(" " = 1, "FE-2SLS" = 3, "OLS" = 2))

#'
#+ rob2intensive, results = if(params$preview) "markup" else "hide"
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
  ) %>%
  kableExtra::kable_styling(font_size = 9)

#'
#' We performed some analyzes for the robustness of this result.
#' The regression table is shown in the appendix,
#' but we will briefly describe the results.
#' We show the results of estimating elasticity excluding 2013 and 2014 data
#' in Table \@ref(tab:rob1intensive) of the Appendix A
#' to eliminate the effects of tax reform announcements.
#' If individuals are aware of the 2014 tax reform in advance,
#' those who make the relative price of giving higher (cheaper)
#' by the reform should increase (decrease) donations before the reform.
#' Therefore, the price elasticity is under-biased
#' due to the announcement effect of tax reform.
#' As a result, as we expected,
#' the price elasticity ranges from -1.7 to -1.9,
#' which is a statistically significant result.
#'
#' Table \@ref(tab:rob2intensive) of the Appendix A shows
#' the estimation results of the last-price elasticity.
#' Under the income deduction system,
#' the relative price of giving that an individual
#' does not face the first price, but the last price.
#' Therefore, it is more realistic to estimate the elasticity
#' using the last price.
#' However, the last price is an endogenous variable
#' because it depends on the donation amount.
#' Therefore, 2SLS estimation was performed using the instrumental variables
#' used in Table \@ref(tab:intensive) as the instruments of
#' the intersection of application dummy and the last price.
#' As a result, the price elasticity of donations ranges from -1.7 to -2.1,
#' which is statistically significant.
#'
#' In addition,
#' we estimated price elasticity
#' using a sample limited to those who applied for tax relief.
#' In this section, we only outline and provide detailed results in Appendix B.
#' Correcting the bias due to sample selection by
#' adding the inverse Mills ratio calculated in the model
#' shown in Table 1 of Appendix A directly to the explanatory variables,
#' the estimated price elasticity ranges from -1.3 to -1.6,
#' which is similar to the main result.
#' We also confirmed that
#' this result is robust
#' even if the announcement effect of tax reform is eliminated
#' and that the last-price elasticity takes a similar value.
#'
#' This approach also solves the endogenous nature of the application
#' by correcting the sample selection bias,
#' making it simpler to perform two further robustness tests
#' on the relative price of giving.
#' First, the first-price depends only on income.
#' Therefore, if income is endogenous,
#' the first-price is also an endogenous variable.
#' A and B proposed to deal with it by $k$-th order difference estimation.
#' In this model, the k-th lagged variable of the giving price,
#' $\ln p^f_{it}(y_{it}) - \ln p^f_{it-k}(y_{it-k})$,
#' depends on the income for two periods.
#' Using the income for $t-k$ year, we calculate the giving price for $t$ year
#' and $t-k$ year, that is,
#' $\ln p^f_{it}(y_{it-k}) - \ln p^f_{it-k}(y_{it-k})$,
#' and use it as an instrumental variable.
#' This avoids the endogenous problem of income manipulation
#' because the variation described by the instrumental variable
#' is independent of income.
#' As a result,
#' the price elasticity by the 1-year and 2-year difference estimation i
#' statistically insignificant,
#' but the price elasticity by the 3-year difference estimation
#' is in the range of -1.5 to -1.7, which is statistically significant.
#'
#' Second,
#' to directly control the dynamic effects of
#' price and income changes on donations,
#' C proposes to add lagged and future changes of these variables
#' to the explanatory variables.
#' As a result, price elasticity is statistically insignificant.
#' However, because our data is unbalanced panel data,
#' the sample size is quite small.
#' In that respect, the results of this analysis are unreliable.
#'
#' ## Extensive Margin
#'
#' By changing the outcome variable
#' from the logarithmic value of giving
#' to a dummy variable that takes one when donated,
#' we estimate the extensive-margin price elasticity
#' with a linear probability model.
#' The estimated price coefficient value
#' does not directly reflect the price elasticity,
#' but we can obtain the price elasticity
#' by dividing the estimated coefficient value
#' by the average of the outcome variables.
#' Also, we focus only on the first-price elasticity
#' since the decision to donate is the same as
#' the decision to donate the first unit.
#'
#+ extensive
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

est_ext_stage2 <- ext_stage2 %>%
  purrr::map(~ fixest::feols(
    ., cluster = ~ panelid,
    data = estdf
  ))

addtab <- est_ext_stage2 %>%
  purrr::map(~ tidy(.) %>% filter(str_detect(term, "price"))) %>%
  purrr::map(function(x) {

    dbar <- mean(estdf$i_ext_giving, na.rm = TRUE)

    x %>%
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
  setNames(c("term", sprintf("(%1d)", seq_len(length(est_ext_stage2))))) %>%
  mutate(term = recode(
    term, "estimate" = "Implied price elasticity", .default = ""
  )) %>%
  bind_rows(tribble(
    ~"term", ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)",
    "Square of age", "X", "X", "X", "X", "X",
    "Instrument", "Wage earner x Price",
    "PS x Price", "PS x Price", "", "",
    "Method of PS", "", "Pool", "Separate", "Pool", "Separate"
  ))

attr(addtab, "position") <- c(7:9)

est_ext_stage2 %>%
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
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::add_header_above(c(" " = 1, "FE-2SLS" = 3, "OLS" = 2))

#'
#+ robextensive, results = if(params$preview) "markup" else "hide"
rob_ext_stage2 <- ext_stage2 %>%
  purrr::map(~ fixest::feols(
    .,
    cluster = ~panelid,
    data = subset(estdf, year < 2013 | 2014 < year)
  ))

addtab <- rob_ext_stage2 %>%
  purrr::map(~ tidy(.) %>% filter(str_detect(term, "price"))) %>%
  purrr::map(function(x) {

    dbar <- mean(estdf$i_ext_giving, na.rm = TRUE)

    x %>%
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
  setNames(c("term", sprintf("(%1d)", seq_len(length(est_ext_stage2))))) %>%
  mutate(term = recode(
    term,
    "estimate" = "Implied price elasticity", .default = ""
  )) %>%
  bind_rows(tribble(
    ~"term", ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)",
    "Square of age", "X", "X", "X", "X", "X",
    "Instrument", "Wage earner x Price",
    "PS x Price", "PS x Price", "", "",
    "Method of PS", "", "Pool", "Separate", "Pool", "Separate"
  ))

attr(addtab, "position") <- c(7:9)

rob_ext_stage2 %>%
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
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::add_header_above(c(" " = 1, "FE-2SLS" = 3, "OLS" = 2))

#' Table \@ref(tab:extensive) shows
#' the estimation results of extensive-margin price elasticity.
#' Similar to Table \@ref(tab:intensive),
#' model (1) uses the intersection of the wage earner dummy and
#' giving price as an instrumental variable.
#' Models (2) and (3) use the intersection of propensity score of application
#' and giving price as an instrumental variable.
#' Models (4) and (5) use OLS to estimate a model that
#' uses the intersection of propensity score of application and
#' giving price as an explanatory variable.
#'
#' As a result, the estimated coefficients are in the range of -0.54 to -0.74.
#' The extensive-margin price elasticity,
#' obtained by dividing this factor by the percentage of donors,
#' ranges from -0.76 to -1.04.
#' In other words,
#' a 1% reduction in relative price due to tax incentives increases
#' the probability of donation by 0.7% to 1%
#' for those who apply for tax relief.
#' This result is robust against
#' the effects of the 2014 tax reform announcement
#' (See Table \@ref(tab:robextensive) in Appendix A,
#' which shows the results of the same exercise
#' using subsamples that exclude 2013 and 2014 data).
#' Therefore,
#' those who apply for tax relief are sensitive to tax incentives
#' when deciding on how much to donate rather than whether or not to donate.
#'
# /*
#+
rmarkdown::render(
  "script/R/2-pooled2sls.r",
  output_dir = "report/view"
)
# */