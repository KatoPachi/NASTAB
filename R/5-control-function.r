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
  mutate(cross = applicable * employee) %>%
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
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2|se_type",
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    output = "latex",
    escape = FALSE
  ) %>%
  kable_styling(font_size = 8) %>%
  group_rows("Excluded instrument", 1, 2, italic = TRUE, bold = FALSE) %>%
  group_rows("Covariates", 3, 12, italic = TRUE, bold = FALSE) %>%
  add_header_above(c(" " = 1, "Dummy of application" = 1)) %>%
  column_spec(2, width = "12em") %>%
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
  # modelr::add_predictions(est_psmod, type = "link") %>%
  # mutate(
  #   imr = dnorm(pred) / pnorm(pred),
  #   inv_imr = dnorm(-pred) / pnorm(-pred),
  #   gr = d_relief_donate * imr - (1 - d_relief_donate) * inv_imr
  # ) %>%
  pivot_longer(
    outcome_intensive:flag_intensive,
    names_to = c(".value", "type"),
    names_pattern = "(.*)_(.*)"
  )

cfmod <- outcome ~ applicable:d_relief_donate + resid + resid:d_relief_donate +
  ..mundlak + ..stage2

est_cfmod <- cf_use %>%
  mutate(type = factor(type, levels = c("intensive", "extensive"))) %>%
  group_by(type) %>%
  do(est = lm_robust(xpd(cfmod), data = subset(., flag == 1)))

#' //NOTE: Create regression tables of CF models
#+
mu <- with(subset(cf_use, type == "extensive"), mean(outcome))

implied_e <- subset(est_cfmod, type == "extensive")$est[[1]] %>% {
  res <- subset(tidy(.), str_detect(term, "applicable:d_relief_donate")) %>%
    mutate(
      estimate = estimate / mu,
      estimate = case_when(
        p.value < 0.01 ~ sprintf("\\num{%1.3f}***", estimate),
        p.value < 0.05 ~ sprintf("\\num{%1.3f}**", estimate),
        p.value < 0.1  ~ sprintf("\\num{%1.3f}*", estimate),
        TRUE           ~ sprintf("\\num{%1.3f}", estimate)
      ),
      std.error = sprintf("(\\num{%1.3f})", std.error / mu)
    )

  tribble(
    ~term, ~mod1, ~mod2,
    "Estimates", "", res$estimate,
    "", "", res$std.error
  )
}

attr(implied_e, "position") <- c(9, 10)

out.file <- file(here("export", "tables", "cf.tex"), open = "w")

est_cfmod$est %>%
  flatten() %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = "Estimation Results of Control Function Model\\label{tab:cf}",
    coef_map = c(
      "applicable:d_relief_donate" = "Effective price",
      "tinc_ln" = "Log income",
      "resid" = "Residuals of Application",
      "d_relief_donate:resid" =
        "Application $\\times$ Residuals of Application"
    ),
    gof_omit = "^(?!R2 Adj.|Num)",
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    add_rows = implied_e,
    output = "latex",
    escape = FALSE
  ) %>%
  kable_styling(font_size = 8) %>%
  add_header_above(
    c(" " = 1, "Log donation" = 1, "A dummy of donor" = 1)
  ) %>%
  group_rows(
    "Implied price elasticity", 9, 10, italic = TRUE, bold = FALSE
  ) %>%
  column_spec(2:3, width = "10em") %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. Standard errors clustered at household level are in parentheses. An outcome variable is logged value of amount of charitable giving in model (1) and a dummy indicating that donor in model (2). For estimation, model (1) uses only donors (intensive-margin sample) and model (2) use both donors and non-donors (extensive-margin sample). We control squared age (divided by 100), number of household members, a dummy that indicates having dependents, a set of dummies of industry, a set of dummies of residential area, and time fixed effects. We use an wage earner dummy as an instrument to obtain residuals of application. Instead individual fixed effects, we control a vector of individual-level sample mean of all exogenous variables including instruments (Chamberlain-Mundlak device).",
    threeparttable = TRUE,
    escape = FALSE
  ) %>%
  writeLines(out.file)

close(out.file)
