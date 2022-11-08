#+ include = FALSE
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
  ..stage2 = ~ tinc_ln + sqage + hh_num + have_dependents + employee +
    factor(indust) + factor(area) + factor(year)
)

#+
psmod <- d_relief_donate ~ applicable + applicable:employee +
  ..mundlak + ..stage2

est_psmod <- list(
  feols(psmod, data = mundlak_use, cluster = ~hhid),
  feglm(
    psmod, data = mundlak_use,
    cluster = ~hhid, family = binomial("probit")
  )
)

out.file <- file(here("export", "tables", "application.tex"), open = "w")

est_psmod %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = "Estimation Results of Tax Relief Application Model\\label{tab:application}",
    coef_map = c(
      "applicable:employee" = "Applicable price $\\times$ Wage earner",
      "applicable" = "Applicable price",
      "employee" = "Wage earner",
      "tinc_ln" = "Log income"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    output = "latex",
    escape = FALSE
  ) %>%
  kable_styling(font_size = 8) %>%
  group_rows("Excluded instruments", 1, 2, italic = TRUE, bold = FALSE) %>%
  group_rows("Covariates", 3, 8, italic = TRUE, bold = FALSE) %>%
  add_header_above(c(" " = 1, "LPM" = 1, "Probit" = 1)) %>%
  add_header_above(c(" " = 1, "Dummy of application" = 2)) %>%
  column_spec(2:3, width = "6em") %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. We use standard errors clustered at household level. An outcome variable is a dummy of application of tax relief. For estimation, we use both donors and non-donors (extensive-margin sample). In addition to logged total income and a wage earner dummy, we control squared age (divided by 100), number of household members, a dummy that indicates having dependents, a set of dummies of industry, a set of dummies of residential area, and time fixed effects. We use $\\\\text{Applicable price}\\\\times\\\\text{Wave earner}$ as an instrument. Instead individual fixed effects, we control a vector of individual-level sample mean of all exogenous variables including instruments (Chamberlain-Mundlak device).",
    threeparttable = TRUE,
    escape = FALSE
  ) %>%
  writeLines(out.file)

close(out.file)

#' //NOTE: Estimate CF models
#+
cf_use <- mundlak_use %>%
  modelr::add_residuals(est_psmod[[1]]) %>%
  modelr::add_predictions(est_psmod[[2]], type = "link") %>%
  mutate(
    imr = dnorm(pred) / pnorm(pred),
    inv_imr = dnorm(-pred) / pnorm(-pred),
    gr = d_relief_donate * imr - (1 - d_relief_donate) * inv_imr
  ) %>%
  pivot_longer(
    outcome_intensive:flag_intensive,
    names_to = c(".value", "type"),
    names_pattern = "(.*)_(.*)"
  )

cfmod <- list(
  outcome ~ applicable:d_relief_donate + resid +
    ..mundlak + ..stage2,
  outcome ~ applicable:d_relief_donate + resid + resid:d_relief_donate +
    ..mundlak + ..stage2
)

est_cfmod <- cf_use %>%
  mutate(type = factor(type, levels = c("intensive", "extensive"))) %>%
  group_by(type) %>%
  do(est = lapply(
    cfmod,
    function(x) feols(x, data = subset(., flag == 1), cluster = ~hhid)
  ))

#' //NOTE: Create regression tables of CF models
#+
mu <- with(subset(cf_use, type == "extensive"), mean(outcome))

implied_e <- subset(est_cfmod, type == "extensive")$est[[1]] %>%
  purrr::map(function(x) {
    res <- subset(tidy(x), str_detect(term, "applicable:d_relief_donate")) %>%
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
      ~term, ~mod,
      "Estimates", res$estimate,
      "Estimates se", res$std.error
    )
  }) %>%
  reduce(left_join, by = "term") %>%
  setNames(c("term", paste0("mod", seq(length(cfmod)))))

add_table <- implied_e %>%
  left_join(
    tibble(
      term = c("Estimates", "Estimates se"),
      imod1 = rep("", 2),
      imod2 = rep("", 2)
    ),
    .,
    by = "term"
  ) %>%
  mutate(term = dplyr::recode(term, "Estimates se" = "", .default = term))

attr(add_table, "position") <- c(9, 10)

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
    add_rows = add_table,
    output = "latex",
    escape = FALSE
  ) %>%
  kable_styling(font_size = 8) %>%
  add_header_above(
    c(" " = 1, "Intensive-margin" = 2, "Extensive-margin" = 2)
  ) %>%
  group_rows(
    "Implied price elasticity", 9, 10, italic = TRUE, bold = FALSE
  ) %>%
  column_spec(2:5, width = "7.5em") %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. We use standard errors clustered at household level. An outcome variable is logged value of amount of charitable giving in models (1) and (2) and a dummy indicating that donor in models (3) and (4). For estimation, we use only donors (intensive-margin sample) in models (1) and (2) and both donors and non-donors (extensive-margin sample) in models (3) and (4). We control squared age (divided by 100), number of household members, a dummy that indicates having dependents, a wage earner dummy, a set of dummies of industry, a set of dummies of residential area, and time fixed effects. We use $\\\\text{Applicable price}\\\\times\\\\text{Wave earner}$ as an instrument to obtaine residuals of application. Instead individual fixed effects, we control a vector of individual-level sample mean of all exogenous variables including instruments (Chamberlain-Mundlak device).",
    threeparttable = TRUE,
    escape = FALSE
  ) %>%
  writeLines(out.file)

close(out.file)
