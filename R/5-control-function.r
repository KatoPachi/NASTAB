#+ include = FALSE
library(here)
source(here("R", "_library.r"))

#+ include = FALSE
use <- readr::read_csv(here("data/shaped2.csv")) %>%
  dplyr::filter(2010 <= year & year < 2018) %>%
  dplyr::filter(tinc < 1100 | 1300 < tinc) %>%
  dplyr::filter(tinc < 4500 | 4700 < tinc) %>%
  dplyr::filter(tinc < 8700 | 8900 < tinc) %>%
  dplyr::filter(tinc < 14000 | 16000 < tinc) %>%
  dplyr::filter(tinc < 30000) %>%
  dplyr::filter(dependents == 0) %>%
  dplyr::filter(tinc > donate) %>%
  select(
    pid,
    hhid,
    year,
    tinc_ln,
    sqage,
    hh_num,
    have_dependents,
    indust,
    area,
    applicable = price_ln,
    d_relief_donate,
    employee,
    outcome_intensive = donate_ln,
    outcome_extensive = d_donate
  ) %>%
  mutate(
    flag_extensive = 1,
    flag_intensive = if_else(outcome_extensive == 1, 1, 0)
  )

meandf <- use %>%
  dplyr::select(
    pid,
    year,
    sqage,
    hh_num,
    have_dependents,
    indust,
    area,
    tinc_ln,
    applicable,
    employee
  ) %>%
  # mutate(cross = applicable * employee) %>%
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

mundlak_use <- use %>%
  left_join(meandf, by = "pid")

#' //NOTE: Estimate application models with LPM and Probit
#' //RUN: We use Chamberlain-Mundlak device for individual fixed effect
#+
fixest::setFixest_fml(
  ..mundlak = as.formula(
    paste("~", paste(names(meandf)[-1], collapse = " + "))
  ),
  ..stage2 = ~ tinc_ln + sqage + hh_num + have_dependents +
    factor(indust) + factor(area) + factor(year)
)

#+
psmod <- d_relief_donate ~ applicable + employee + ..mundlak + ..stage2

est_psmod <- lm_robust(
  xpd(psmod),
  data = mundlak_use,
  cluster = hhid,
  se_type = "stata"
)

out.file <- file(here("export", "tables", "application.tex"), open = "w")

list(est_psmod) %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = "Estimation Results of Tax Relief Application Model (Linear Probability Model)\\label{tab:application}",
    coef_map = c(
      "employee" = "Wage earner",
      "applicable" = "Applicable price",
      "tinc_ln" = "Log income",
      "sqage" = "Squared age (divided by 100)",
      "hh_num" = "Number of household members",
      "have_dependents" = "Having dependents"
    ),
    gof_omit = "^(?!R2 Adj.|Num)",
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    output = "latex",
    escape = FALSE
  ) %>%
  kable_styling(font_size = 8) %>%
  group_rows("Excluded instrument", 1, 2, italic = TRUE, bold = FALSE) %>%
  group_rows("Covariates", 3, 12, italic = TRUE, bold = FALSE) %>%
  add_header_above(c(" " = 1, "Dummy of application" = 1)) %>%
  column_spec(1, width = "25em") %>%
  column_spec(2, width = "15em") %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. Standard errors clustered at household level are in parentheses. An outcome variable is a dummy of application of tax relief. For estimation, we use both donors and non-donors (extensive-margin sample). Additionally, we control a set of dummies of industry, a set of dummies of residential area, and time fixed effects. We use a wage earner dummy as an instrument. Instead individual fixed effects, we control a vector of individual-level sample mean of all exogenous variables including instrument (Chamberlain-Mundlak device).",
    threeparttable = TRUE,
    escape = FALSE
  ) %>%
  writeLines(out.file)

close(out.file)

#' //NOTE: Estimate CF models
#+
cf_use <- mundlak_use %>%
  modelr::add_residuals(est_psmod) %>%
  pivot_longer(
    outcome_intensive:flag_intensive,
    names_to = c(".value", "type"),
    names_pattern = "(.*)_(.*)"
  )

cfmod <- list(
  outcome ~ applicable:d_relief_donate + resid +
    ..mundlak + ..stage2,
  outcome ~ applicable:d_relief_donate + resid +
    resid:applicable:d_relief_donate + ..mundlak + ..stage2
)

est_cfmod <- cf_use %>%
  mutate(type = factor(type, levels = c("intensive", "extensive"))) %>%
  group_by(type) %>%
  nest() %>%
  mutate(
    est1 = map(data, ~ lm_robust(
      xpd(cfmod[[1]]),
      data = subset(.x, flag == 1),
      cluster = hhid,
      se_type = "stata"
    )),
    est2 = map(data, ~ lm_robust(
      xpd(cfmod[[2]]),
      data = subset(.x, flag == 1),
      cluster = hhid,
      se_type = "stata"
    ))
  ) %>%
  pivot_longer(est1:est2, values_to = "fit", names_to = "model")

#' //NOTE: Create regression tables of CF models
#+
mu <- with(subset(cf_use, type == "extensive"), mean(outcome))

implied_e <- est_cfmod %>%
  dplyr::filter(model == 'est1') %>%
  mutate(
    tidy = map(fit, tidy),
    tidy = map(tidy, ~subset(., str_detect(term, "applicable"))),
    tidy = map(tidy, ~subset(., !str_detect(term, "resid|mean"))),
    std.err = map_dbl(tidy, ~ .$std.error / mu),
    std.err = sprintf("(\\num{%1.3f})", std.err),
    estimate = map_dbl(tidy, ~ .$estimate / mu),
    p.value = map_dbl(tidy, ~ .$p.value),
    estimate = case_when(
      p.value < 0.01 ~ sprintf("\\num{%1.3f}***", estimate),
      p.value < 0.05 ~ sprintf("\\num{%1.3f}**", estimate),
      p.value < 0.1  ~ sprintf("\\num{%1.3f}*", estimate),
      TRUE           ~ sprintf("\\num{%1.3f}", estimate)
    )
  ) %>% {
    tribble(
      ~term, ~ext_mod1,
      "Estimate", .$estimate[2],
      "", .$std.err[2]
    )
  }

attr(implied_e, "position") <- c(7, 8)

out.file <- file(here("export", "tables", "cf-intensive.tex"), open = "w")

est_cfmod %>%
  dplyr::filter(model == 'est1' & type == "intensive") %>%
  .$fit %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = "Estimation Results of Control Function Model (Intensive-margin Sample)\\label{tab:cf-intensive}",
    coef_map = c(
      "applicable:d_relief_donate" = "Effective price ($\\beta_e$)",
      "tinc_ln" = "Log income",
      "resid" = "Residuals of Application ($\\psi_1$)"
    ),
    gof_omit = "^(?!R2 Adj.|Num)",
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    output = "latex",
    escape = FALSE
  ) %>%
  kable_styling(font_size = 8) %>%
  add_header_above(
    c(" " = 1, "Log donation" = 1)
  ) %>%
  column_spec(1, width = "25em") %>%
  column_spec(2, width = "15em") %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. Standard errors clustered at household level are in parentheses. An outcome variable is logged value of amount of charitable giving. For estimation, we use only donors (intensive-margin sample). We control squared age (divided by 100), number of household members, a dummy that indicates having dependents, a set of dummies of industry, a set of dummies of residential area, and time fixed effects. We use an wage earner dummy as an instrument to obtain residuals of application. Instead individual fixed effects, we control a vector of individual-level sample mean of all exogenous variables including instruments (Chamberlain-Mundlak device).",
    threeparttable = TRUE,
    escape = FALSE
  ) %>%
  writeLines(out.file)

close(out.file)

out.file <- file(here("export", "tables", "cf-extensive.tex"), open = "w")

est_cfmod %>%
  dplyr::filter(model == "est1" & type == "extensive") %>%
  .$fit %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = "Estimation Results of Control Function Model (Extensive-margin Sample)\\label{tab:cf-extensive}",
    coef_map = c(
      "applicable:d_relief_donate" = "Effective price ($\\beta_e$)",
      "tinc_ln" = "Log income",
      "resid" = "Residuals of Application ($\\psi_1$)"
    ),
    gof_omit = "^(?!R2 Adj.|Num)",
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    add_rows = implied_e,
    output = "latex",
    escape = FALSE
  ) %>%
  kable_styling(font_size = 8) %>%
  add_header_above(
    c(" " = 1, "Dummy of donors" = 1)
  ) %>%
  kableExtra::group_rows(
    "Implied price elasticity", 7, 8,
    bold = FALSE, italic = TRUE
  ) %>%
  column_spec(1, width = "25em") %>%
  column_spec(2, width = "15em") %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. Standard errors clustered at household level are in parentheses. An outcome variable is a dummy of donor. For estimation, we use both donors and non-donors (extensive-margin sample). We control squared age (divided by 100), number of household members, a dummy that indicates having dependents, a set of dummies of industry, a set of dummies of residential area, and time fixed effects. We use an wage earner dummy as an instrument to obtain residuals of application. Instead individual fixed effects, we control a vector of individual-level sample mean of all exogenous variables including instruments (Chamberlain-Mundlak device).",
    threeparttable = TRUE,
    escape = FALSE
  ) %>%
  writeLines(out.file)

close(out.file)

#' //NOTE: 2SLS-decomposition theorem and individual elasticity
#+
intensive_personal_e <- est_cfmod %>%
  dplyr::filter(type == 'intensive' & model == 'est2') %>%
  unnest(cols = data) %>%
  select(
    type,
    pid,
    hhid,
    year,
    flag,
    resid,
    fit,
    d_relief_donate,
    applicable,
    tinc_ln,
    sqage,
    hh_num,
    have_dependents,
    employee,
    indust,
    area
  ) %>%
  dplyr::filter(!is.na(resid) & flag == 1) %>%
  mutate(
    avg_e = map_dbl(fit, ~ coef(.)["applicable:d_relief_donate"]),
    dev_e = map_dbl(fit, ~ coef(.)['applicable:d_relief_donate:resid']),
    ind_e = avg_e + dev_e * resid,
    effective = applicable * d_relief_donate
  ) %>%
  dplyr::select(-fit, -flag)

auxiliary <- feols(
  effective ~ tinc_ln + sqage + hh_num + have_dependents + employee |
    year + pid + indust + area,
  data = intensive_personal_e,
  cluster = ~ hhid
)

n1 <- nrow(subset(intensive_personal_e, d_relief_donate == 1))

intensive_personal_e_2 <- intensive_personal_e %>%
  modelr::add_residuals(auxiliary, var = 'resid_effective') %>%
  mutate(w_denom = (effective / n1) * resid_effective)

sum_w_denom <- sum(intensive_personal_e_2$w_denom)

final_intensive_personal <- intensive_personal_e_2 %>%
  mutate(
    w = resid_effective / sum_w_denom,
    w = effective * w / n1
  ) %>%
  select(-resid_effective, -w_denom)

#' //NOTE: CF with random coefficient (intensive)
#+
mean <- with(final_intensive_personal, mean(ind_e))
mean1 <- with(subset(final_intensive_personal, d_relief_donate == 1), mean(ind_e))
mean0 <- with(subset(final_intensive_personal, d_relief_donate == 0), mean(ind_e))
weight_sum <- with(final_intensive_personal, sum(w * ind_e))

agg_e <- tibble::tribble(
  ~term, ~est,
  'Sample average', mean,
  'Sample average among claimants', mean1,
  'Sample average among non-claimants', mean0,
  "Weighted sum (Chaisemartin and D'haultf\\oe uille, 2020)", weight_sum 
)

attr(agg_e, 'position') <- seq(9, length.out = nrow(agg_e))

out.file <- file(here("export", "tables", "random-coefficient.tex"), open = "w")

est_cfmod %>%
  dplyr::filter(type == 'intensive' & model == 'est2') %>%
  .$fit %>%
  modelsummary(
    title = 'Random Coefficient Model for Intensive-Margin Price Elasticities\\label{tab:random-coefficient}',
    coef_map = c(
      "applicable:d_relief_donate" = "Effective price ($\\beta_e$)",
      "tinc_ln" = "Log income",
      "resid" = "Residuals of Application ($\\psi_1$)",
      "applicable:d_relief_donate:resid" =
        'Effective price $\\times$ Residuals of Application ($\\psi_2$)'
    ),
    gof_omit = "^(?!R2 Adj.|Num)",
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    add_rows = agg_e,
    output = "latex",
    escape = FALSE
  ) %>%
  kable_styling(font_size = 8) %>%
  add_header_above(
    c(" " = 1, "Log donation" = 1)
  ) %>%
  group_rows(
    "Aggregated price elasticity", 9, 9 + nrow(agg_e) - 1,
    italic = TRUE, bold = FALSE
  ) %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. Robust standard errors are in parentheses. An outcome variable is logged value of amount of charitable giving in model (1). For estimation, model (1) uses only donors (intensive-margin sample). We control squared age (divided by 100), number of household members, a dummy that indicates having dependents, a set of dummies of industry, a set of dummies of residential area, and time fixed effects. We use an wage earner dummy as an instrument to obtain residuals of application. Instead individual fixed effects, we control a vector of individual-level sample mean of all exogenous variables including instruments (Chamberlain-Mundlak device).",
    threeparttable = TRUE,
    escape = FALSE
  ) %>%
  writeLines(out.file)

close(out.file)

#' //NOTE: Density of elasticity
#+
dist_ind_e <- final_intensive_personal %>%
  mutate(d_relief_donate = factor(
    d_relief_donate,
    labels = c("Non-claimants", "Claimants")
  )) %>%
  ggplot(aes(x = ind_e, fill = d_relief_donate)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("white", "grey50")) +
  labs(x = 'Intensive-margin price elasticities', y = 'Density', fill = '') +
  ggtemp()

ggsave(
  here("export", "figures", "individual-elasticities.pdf"),
  plot = dist_ind_e,
  width = 10,
  height = 6
)
