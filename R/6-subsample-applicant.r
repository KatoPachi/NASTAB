#+
library(here)
source(here("R", "_library.r"))

#+ include = FALSE
use <- readr::read_csv(here("data/shaped2.csv")) %>%
  dplyr::filter(year < 2018) %>%
  dplyr::filter(dependents == 0) %>%
  dplyr::filter(tinc > donate) %>%
  select(
    pid,
    hhid,
    year,
    # sex,
    # age,
    # college,
    # highschool,
    # junior,
    sqage,
    hh_num,
    have_dependents,
    indust,
    area,
    tinc_ln,
    credit_benefit,
    credit_loss,
    price_ln,
    lprice_ln,
    d_relief_donate,
    employee,
    intensive = donate_ln
  ) %>%
  mutate(
    effective = d_relief_donate * lprice_ln,
    applicable = price_ln,
    after = if_else(year >= 2014, 1, 0)
  )

#' //NOTE: Estimate application model
#+
fixest::setFixest_fml(
  ..stage2 = ~ tinc_ln + sqage + hh_num + have_dependents +
    factor(indust) + factor(area) | year + pid
)

femod <- list(
  d_relief_donate ~ credit_benefit:after + credit_loss:after + ..stage2,
  d_relief_donate ~ employee + credit_benefit:after + credit_loss:after +
    employee:credit_benefit + employee:credit_loss +
    credit_benefit:employee:after + credit_loss:employee:after + ..stage2,
  d_relief_donate ~ applicable + ..stage2,
  d_relief_donate ~ employee + applicable + applicable:employee + ..stage2
)

est_femod <- femod %>%
  purrr::map(~ feols(., data = use, cluster = ~hhid))

#' //NOTE: Create regression table of application model
#+
out.file <- file(here("export", "tables", "fe-application.tex"), open = "w")

tab <- est_femod %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = "Fixed Effect Model of Application of Tax Relief\\label{tab:fe-application}",
    coef_map = c(
      "credit_benefit:after" = "Decrease x Credit period",
      "after:credit_loss" = "Increase x Credit period",
      "applicable" = "Log applicable price",
      "effective" = "Log effective last-price",
      "employee" = "Wage earner",
      "employee:credit_benefit" = "Wage earner x Decrease",
      "employee:credit_loss" = "Wage earner x Increase",
      "employee:credit_benefit:after" =
        "Wage earner x Decrease x Credit period",
      "employee:after:credit_loss" =
        "Wage earner x Increase x Credit period",
      "employee:applicable" = "Wage earner x Log applicable price",
      "tinc_ln" = "Log income"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    output = "latex"
  ) %>%
  kableExtra::kable_styling(font_size = 8) %>%
  kableExtra::add_header_above(c(
    " " = 1, "Application of tax relief" = 4
  )) %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. We use standard errors clustered at household level. An outcome variable is a dummy indicating application of tax relief. For estimation, models (1)--(4) use donors (intensive-margin sample), and models (5)--(8) use not only donors but also non-donors (extensive-margin sample).  We control squared age (divided by 100), number of household members, a dummy that indicates having dependents, a set of dummies of industry a set of dummies of residential area, and individual and time fixed effects.",
    threeparttable = TRUE,
    escape = FALSE
  ) %>%
  kableExtra::landscape()

writeLines(tab, out.file)
close(out.file)

#' //NOTE: Calculate individual characteristics mean
#' //RUN: Following Mundlak's spirit,
#' we replace individual fixed effects by within-average of exogenous variables
#+
meandf <- use %>%
  dplyr::select(
    pid,
    year,
    # sex,
    # age,
    # college,
    # highschool,
    # junior,
    sqage,
    hh_num,
    have_dependents,
    indust,
    area,
    tinc_ln,
    price_ln,
    employee
  ) %>%
  fastDummies::dummy_cols(
    select_columns = "indust",
    remove_selected_columns = TRUE
  ) %>%
  fastDummies::dummy_cols(
    select_columns = "area",
    remove_selected_columns = TRUE
  ) %>%
  select(-indust_NA) %>%
  group_by(pid) %>%
  summarize_all(list(mean = ~ sum(., na.rm = TRUE))) %>%
  select(-year_mean) %>%
  mutate_at(vars(-pid), list(~ . / length(unique(use$year))))

fixest::setFixest_fml(
  ..mundlak = as.formula(
    paste("~", paste(names(meandf)[-1], collapse = " + "))
  )
)

#'
#' //NOTE: Construct inverse mills ratio
#' //RUN: estimate probit model with sample split by year
#' //RUN: Using linear predictions, calculate inverse mills ratio
#+
imrdf <- use %>%
  dplyr::left_join(meandf, by = "pid") %>%
  group_by(year) %>%
  nest() %>%
  mutate(est = map(data, ~ feglm(
    d_relief_donate ~ sqage + hh_num + have_dependents + tinc_ln +
      #sex + age + college + highschool + junior +
      price_ln + employee + factor(indust) + factor(area) + ..mundlak,
    data = .x,
    family = binomial(link = "probit")
  ))) %>%
  mutate(
    linear = map2(data, est, ~ modelr::add_predictions(.x, .y, type = "link")),
    linear = map(linear, ~ mutate(.x, imr = dnorm(pred) / pnorm(pred)))
  ) %>%
  select(year, linear) %>%
  unnest(cols = linear) %>%
  ungroup()

#'
#' //NOTE: Estimate Second-Stage FE Model
#' //RUN: Following Semykina and Wooldridge (2010),
#' estimate pooled model or pooled 2SLS
#+
femod <- list(
  intensive ~ applicable + ..stage2,
  intensive ~ ..stage2 | effective ~ applicable,
  intensive ~ applicable + sqage + hh_num + have_dependents + tinc_ln +
    #sex + age + college + highschool + junior +
    employee + factor(indust) + factor(area) + ..mundlak +
    imr:factor(year) | year,
  intensive ~ sqage + hh_num + have_dependents + tinc_ln +
    #sex + age + college + highschool + junior +
    employee + factor(indust) + factor(area) + ..mundlak +
    imr:factor(year) | year | effective ~ applicable
)

est_femod <- femod %>%
  purrr::map(~ feols(
    .,
    data = subset(imrdf, d_relief_donate == 1),
    cluster = ~hhid
  )) %>%
  setNames(paste0("(", seq(length(.)), ")"))

#' //NOTE: Create regression table
#+
stat_tab <- tibble(est = est_femod) %>%
  mutate(
    mod = paste0("mod", seq(length(est))),
    F = map_dbl(est, ~ as_vector(fitstat(.x, "ivf")[[1]])[1]),
    F = if_else(is.na(F), "", sprintf("%1.2f", F))
  ) %>%
  dplyr::select(-est) %>%
  pivot_wider(names_from = mod, values_from = F) %>%
  bind_cols(tibble(name = "F-statistics of instrument"), .)

attr(stat_tab, "position") <- 7

out.file <- file(here("export", "tables", "fe2sls-applicants.tex"), open = "w")

tab <- est_femod %>%
  modelsummary(
    title = "Estimation of Tax-Price Elasticity for Applicants\\label{tab:fe2sls-applicants}",
    coef_map = c(
      "applicable" = "Log applicable price",
      "fit_effective" = "Log effective last-price",
      "tinc_ln" = "Log income"
    ),
    add_rows = stat_tab,
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|R2",
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    output = "latex"
  ) %>%
  kableExtra::kable_styling(font_size = 8) %>%
  kableExtra::add_header_above(c(
    " " = 1, "Fixed-effect model" = 2,
    "Pooled model with sample selection correction" = 2
  )) %>%
  kableExtra::add_header_above(c(
    " ", "Log donation" = 4
  )) %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. Standard errors are clustered at household level. We use only applicants of tax deduction (or tax credit). Fixed effect models (1)--(2) control squared age (divided by 100), number of household members, a dummy that indicates having dependents, a set of dummies of industry, a set of dummies of residential area, and individual and time fixed effects. Models (3)--(4) correct sample selection bias, proposed by \\cite{Semykina2010} which is analogous to the control function approach. These models additionally control the inverse mills ratio (IMR) and interactions of IMR with time dummies, and use within-mean of explanatory variables as individual fixed effects. Model (2) and (4) are 2SLS where log appricable price is an instrument of log effective last-price.",
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(tab, out.file)
close(out.file)

#' //NOTE: Test for sample selection bias
#+
bias_test <- list(
  intensive ~ imr + applicable + ..stage2,
  intensive ~ imr:factor(year) + applicable + ..stage2,
  intensive ~ imr + ..stage2 | effective ~ applicable,
  intensive ~ imr:factor(year) + ..stage2 | effective ~ applicable
)

est_bias_test <- bias_test %>%
  purrr::map(~ feols(
    .,
    data = subset(imrdf, d_relief_donate == 1),
    cluster = ~hhid
  )) %>%
  setNames(paste0("(", seq(length(.)), ")"))

#' //NOTE: Create regression table for sample selection bias test
#+
wald_bias_test <- est_bias_test %>%
  purrr::map(~ tibble(
    wald = wald(., "imr")$p,
    f = as_vector(fitstat(., "ivf")[[1]])[1]
  )) %>%
  reduce(bind_rows) %>%
  mutate(mod = paste0("mod", seq(length(bias_test)))) %>%
  pivot_longer(wald:f) %>%
  pivot_wider(names_from = mod, values_from = value) %>%
  mutate(
    name = recode(
      name, "wald" = "p-value", "f" = "F-statistics of instrument"
    )
  ) %>%
  mutate_at(
    vars(-name),
    list(~ case_when(
      name == "p-value" ~ sprintf("%1.3f", .),
      !is.na(.) ~ sprintf("%1.2f", .),
      TRUE ~ ""
    ))
  )

attr(wald_bias_test, "position") <- c(21, 22)

out.file <- file(
  here("export", "tables", "fe2sls-selection-test.tex"), open = "w"
)

tab <- est_bias_test %>%
  modelsummary(
    title = "Test for Sample Selection Bias of FE model\\label{tab:fe2sls-selection-test}",
    coef_map = c(
      "applicable" = "Log applicable price",
      "effective" = "Log effective last-price",
      "fit_effective" = "Log effective last-price",
      "tinc_ln" = "Log income",
      "imr" = "Inverse mills ratio (IMR)",
      "imr:factor(year)2012" = "IMR x Year = 2012",
      "imr:factor(year)2013" = "IMR x Year = 2013",
      "imr:factor(year)2014" = "IMR x Year = 2014",
      "imr:factor(year)2015" = "IMR x Year = 2015",
      "imr:factor(year)2016" = "IMR x Year = 2016",
      "imr:factor(year)2017" = "IMR x Year = 2017"
    ),
    add_rows = wald_bias_test,
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|R2",
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    output = "latex"
  ) %>%
  kableExtra::kable_styling(font_size = 8) %>%
  kableExtra::add_header_above(c(
    " ", "FE" = 2, "FE-2SLS" = 2
  )) %>%
  kableExtra::add_header_above(c(
    " ", "Log donation" = 4
  )) %>%
  kableExtra::pack_rows(
    "Wald test for joint null of coefficients related to IMR",
    21, 21,
    bold = FALSE, italic = TRUE
  ) %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. Standard errors are clustered at household level. We use only applicants of tax deduction (or tax credit). We control squared age (divided by 100), number of household members, a dummy that indicates having dependents, a set of dummies of industry, a set of dummies of residential area, the computed inverse mills ratio (and interactions with year dummies) and individual and time fixed effects. Model (3) and (4) are 2SLS where log appricable price is an instrument of log effective last-price. Following \\cite{Semykina2010}, we test for sample selection bias by a wald test for join null of coefficients on IMR (and interactions with year dummies).",
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(tab, out.file)
close(out.file)

#+ kdiff-model
fixest::setFixest_fml(
  ..kdiff1 = ~ linc_ln_d1 + sqage_d1,
  ..kdiff2 = ~ linc_ln_d2 + sqage_d2,
  ..kdiff3 = ~ linc_ln_d3 + sqage_d3,
  ..kdifffe = ~ year + area + indust
)

kdiffmod <- list(
  "(1)" = fixest::xpd(
    donate_ln_d1 ~ ..kdiff1 | ..kdifffe | price_ln_d1 ~ log(price_iv1)
  ),
  "(2)" = fixest::xpd(
    donate_ln_d2 ~ ..kdiff2 | ..kdifffe | price_ln_d2 ~ log(price_iv2)
  ),
  "(3)" = fixest::xpd(
    donate_ln_d3 ~ ..kdiff3 | ..kdifffe | price_ln_d3 ~ log(price_iv3)
  )
)

est_kdiffmod <- kdiffmod %>%
  purrr::map(~fixest::feols(
    ., data = subset(estdf, d_relief_donate == 1),
    cluster = ~ hhid
  ))

stage1_kdiffmod <- 1:3 %>%
  purrr::map(function(i) {
    x <- est_kdiffmod[[i]]
    coef <- x$iv_first_stage[[paste("price_ln_d", i, sep = "")]]$coeftable[1, 1]
    ivwald <- fitstat(x, "ivwald")[[1]]$stat

    tibble(coef = coef, wald = ivwald) %>%
      pivot_longer(everything()) %>%
      mutate(value = case_when(
        name == "coef" ~ sprintf("%1.3f", value),
        name == "wald" ~ sprintf("[%1.1f]", value)
      ))
  }) %>%
  reduce(left_join, by = "name") %>%
  setNames(c("term", sprintf("(%1d)", 1:3))) %>%
  mutate(term = recode(
    term,
    "coef" = "First-stage: Instrument", .default = ""
  ))

attr(stage1_kdiffmod, "position") <- c(5, 6)

out.file <- file(here("tables", "kdiff-model.tex"), open = "w")

tab <- est_kdiffmod %>%
  modelsummary(
    title = paste(
      "$k$-th Difference Model Using Those Who Applied for Tax Relief",
      "\\label{tab:kdiff-model}"
    ),
    coef_map = c(
      "fit_price_ln_d1" = "Difference of logged first price",
      "linc_ln_d1" = "Difference of logged income",
      "fit_price_ln_d2" = "Difference of logged first price",
      "linc_ln_d2" = "Difference of logged income",
      "fit_price_ln_d3" = "Difference of logged first price",
      "linc_ln_d3" = "Difference of logged income"
    ),
    gof_omit = "^(?!N)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = stage1_kdiffmod,
    output = "latex"
  ) %>%
  # kableExtra::kable_styling(font_size = 8) %>%
  kableExtra::add_header_above(c(
    " " = 1, "1-year lag" = 1,
    "2-year lag" = 1, "3-year lag" = 1
  )) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at individual level.",
      "Instrument is difference between lagged first price in year $t$",
      "and in year $t - k$ fixing income in year $t - k$."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(tab, out.file)
close(out.file)
