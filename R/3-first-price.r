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
    price_ln,
    d_relief_donate,
    employee,
    outcome_intensive = donate_ln,
    outcome_extensive = d_donate
  ) %>%
  mutate(
    flag_extensive = 1,
    flag_intensive = if_else(outcome_extensive == 1, 1, 0)
  ) %>%
  pivot_longer(
    outcome_intensive:flag_intensive,
    names_to = c(".value", "type"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  mutate(
    effective_reverse = (1 - d_relief_donate) * price_ln,
    effective = d_relief_donate * price_ln,
    applicable = price_ln
  )

fixest::setFixest_fml(
  ..stage2 = ~ tinc_ln + sqage + hh_num + have_dependents + employee |
    year + pid + indust + area
)

#' //NOTE: Estimate 1st stage model
#+
est_stage1 <- use %>%
  mutate(type = factor(type, levels = c("intensive", "extensive"))) %>%
  group_by(type) %>%
  do(est = feols(
    effective ~ applicable + ..stage2,
    data = subset(., flag == 1), cluster = ~hhid
  ))

out.file <- file(here("export", "tables", "main-stage1.tex"), open = "w")

est_stage1 %>%
  pull(est) %>%
  flatten() %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = "First-Stage Models\\label{tab:main-stage1}",
    coef_map = c(
      "applicable" = "Applicable price",
      "employee" = "Wage earner",
      "tinc_ln" = "Log income"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    output = "latex",
    escape = FALSE
  ) %>%
  kable_styling(font_size = 8) %>%
  add_header_above(c(
    " " = 1,
    "Donors (Intensive-margin)" = 1,
    "Donors and Non-donors (Extensive-margin)" = 1
  )) %>%
  add_header_above(c(" " = 1, "Effective price" = 2)) %>%
  group_rows("Excluded instruments", 1, 2, italic = TRUE, bold = FALSE) %>%
  group_rows("Covariates", 3, 6, italic = TRUE, bold = FALSE) %>%
  column_spec(2:3, width = "18.75em") %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. Standard errors clustered at household level are in parentheses. An outcome variable is logged value of the effective price. For estimation, model (1) use donors only (intensive-margin sample), and model (2) use not only donors but also non-donors (extensive-margin sample). In addition to logged income and wage earner dummy shown in table, covariates consist of squared age (divided by 100), number of household members, a dummy that indicates having dependents, a set of dummies of industry a set of dummies of residential area, and individual and time fixed effects. Excluded instrument is logged value of applicable price.",
    threeparttable = TRUE,
    escape = FALSE
  ) %>%
  writeLines(out.file)

close(out.file)

#' //NOTE: Estimate price elasticity
#+ fe-model include = FALSE
femod <- list(
  outcome ~ applicable + ..stage2,
  outcome ~ effective + ..stage2,
  outcome ~ ..stage2 | effective ~ applicable
)

est_femod <- use %>%
  mutate(type = factor(type, levels = c("intensive", "extensive"))) %>%
  group_by(type) %>%
  do(est = lapply(
    femod,
    function(x) feols(x, data = subset(., flag == 1), cluster = ~hhid)
  ))

#' //NOTE: Create regression table of price elasticity
#+
ivtable <- tibble(
  terms = c(
    "F-statistics of instruments",
    "Wu-Hausman test, p-value"
  ),
  model1 = rep(NA_real_, 2),
  model2 = rep(NA_real_, 2),
  model3 = c(
    fitstat(est_femod[1, ]$est[[1]][[3]], "ivf")[[1]]$stat,
    fitstat(est_femod[1, ]$est[[1]][[3]], "wh")$wh$p
  ),
  model4 = rep(NA_real_, 2),
  model5 = rep(NA_real_, 2),
  model6 = c(
    fitstat(est_femod[2, ]$est[[1]][[3]], "ivf")[[1]]$stat,
    fitstat(est_femod[2, ]$est[[1]][[3]], "wh")$wh$p
  )
)

mu <- with(subset(use, type == "extensive"), mean(outcome))

implied_e <- subset(est_femod, type == "extensive")$est[[1]] %>%
  purrr::map(function(x) {
    res <- subset(tidy(x), str_detect(term, "effective|applicable")) %>%
      mutate(
        estimate = estimate / mu,
        estimate = case_when(
          p.value < 0.01 ~ sprintf("\\num{%1.3f}***", estimate),
          p.value < 0.05 ~ sprintf("\\num{%1.3f}**", estimate),
          p.value < 0.1 ~ sprintf("\\num{%1.3f}*", estimate),
          TRUE ~ sprintf("\\num{%1.3f}", estimate)
        ),
        std.error = sprintf("(\\num{%1.3f})", std.error / mu)
      )

    tribble(
      ~terms, ~mod,
      "Estimates", res$estimate,
      "Estimates se", res$std.error
    )
  }) %>%
  reduce(left_join, by = "terms") %>%
  setNames(c("terms", paste0("model", seq(length(femod)) + 3)))

addtab <- ivtable %>%
  mutate_at(
    vars(-terms), list(~ifelse(is.na(.), "", sprintf("\\num{%1.3f}", .)))
  ) %>%
  mutate_at(
    vars(-terms), list(~ ifelse(. == "\\num{0.000}", "$<$ \\num{0.001}", .))
  ) %>%
  bind_rows(implied_e, .) %>%
  select(terms, model1:model3, model4:model6) %>%
  mutate_at(vars(model1, model2, model3), list(~ifelse(is.na(.), "", .))) %>%
  mutate(terms = dplyr::recode(terms, "Estimates se" = "", .default = terms))

attr(addtab, "position") <- 7:10

out.file <- file(here("export", "tables", "main.tex"), open = "w")

est_femod %>%
  pull(est) %>%
  flatten() %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = "Estimation Results of Price Elasticities\\label{tab:main}",
    coef_map = c(
      "applicable" = "Applicable price",
      "effective" = "Effective price",
      "fit_effective" = "Effective price",
      "tinc_ln" = "Log income"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    add_rows = addtab,
    output = "latex"
  ) %>%
  kable_styling(font_size = 8) %>%
  add_header_above(c(
    " " = 1, "FE" = 2, "FE-2SLS" = 1, "FE" = 2, "FE-2SLS" = 1
  )) %>%
  add_header_above(c(" " = 1, "Log donation" = 3, "Dummy of donor" = 3)) %>%
  group_rows("Implied price elasticity", 7, 8, italic = TRUE, bold = FALSE) %>%
  group_rows(
    "1st stage information (Excluded instrument: Applicable price)",
    9, 10, bold = FALSE, italic = TRUE
  ) %>%
  column_spec(2:7, width = "5em") %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. Standard errors clustered at household level are in parentheses. An outcome variable is logged value of amount of charitable giving for models (1)--(3) and a dummy of donor for models (4)--(6). For estimation, models (1)--(3) use donors only (intensive-margin sample), and models (4)--(6) use not only donors but also non-donors (extensive-margin sample). For outcome equation, we control squared age (divided by 100), number of household members, a dummy that indicates having dependents, employee dummy, a set of dummies of industry a set of dummies of residential area, and individual and time fixed effects. For FE-2SLS, we use logged value of applicable price as an instrument.",
    threeparttable = TRUE,
    escape = FALSE
  ) %>%
  writeLines(out.file)

close(out.file)

#' //NOTE: Estimate price elasticity excluding announcement effect
#+
est_announce <- use %>%
  dplyr::filter(year < 2013 | 2014 < year) %>%
  mutate(type = factor(type, levels = c("intensive", "extensive"))) %>%
  group_by(type) %>%
  do(est = lapply(
    femod,
    function(x) feols(x, data = subset(., flag == 1), cluster = ~hhid)
  ))

#' //NOTE: Create regression table of intensive-margin price elasticity
#+
ivtable <- tibble(
  terms = c(
    "F-statistics of instruments",
    "Wu-Hausman test, p-value"
  ),
  model1 = rep(NA_real_, 2),
  model2 = rep(NA_real_, 2),
  model3 = c(
    fitstat(est_announce[1, ]$est[[1]][[3]], "ivf")[[1]]$stat,
    fitstat(est_announce[1, ]$est[[1]][[3]], "wh")$wh$p
  ),
  model4 = rep(NA_real_, 2),
  model5 = rep(NA_real_, 2),
  model6 = c(
    fitstat(est_announce[2, ]$est[[1]][[3]], "ivf")[[1]]$stat,
    fitstat(est_announce[2, ]$est[[1]][[3]], "wh")$wh$p
  )
)

mu <- with(subset(use, type == "extensive"), mean(outcome))

implied_e <- subset(est_announce, type == "extensive")$est[[1]] %>%
  purrr::map(function(x) {
    res <- subset(tidy(x), str_detect(term, "effective|applicable")) %>%
      mutate(
        estimate = estimate / mu,
        estimate = case_when(
          p.value < 0.01 ~ sprintf("\\num{%1.3f}***", estimate),
          p.value < 0.05 ~ sprintf("\\num{%1.3f}**", estimate),
          p.value < 0.1 ~ sprintf("\\num{%1.3f}*", estimate),
          TRUE ~ sprintf("\\num{%1.3f}", estimate)
        ),
        std.error = sprintf("(\\num{%1.3f})", std.error / mu)
      )

    tribble(
      ~terms, ~mod,
      "Estimates", res$estimate,
      "Estimates se", res$std.error
    )
  }) %>%
  reduce(left_join, by = "terms") %>%
  setNames(c("terms", paste0("model", seq(length(femod)) + 3)))

addtab <- ivtable %>%
  mutate_at(
    vars(-terms), list(~ ifelse(is.na(.), "", sprintf("\\num{%1.3f}", .)))
  ) %>%
  mutate_at(
    vars(-terms), list(~ ifelse(. == "\\num{0.000}", "$<$ \\num{0.001}", .))
  ) %>%
  bind_rows(implied_e, .) %>%
  select(terms, model1:model3, model4:model6) %>%
  mutate_at(vars(model1, model2, model3), list(~ ifelse(is.na(.), "", .))) %>%
  mutate(terms = dplyr::recode(terms, "Estimates se" = "", .default = terms))

attr(addtab, "position") <- 7:10

out.file <- file(here("export", "tables", "announcement.tex"), open = "w")

est_announce %>%
  pull(est) %>%
  flatten() %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = "Estimation of Price Elasticities Excluding Announcement Effect\\label{tab:announcement}",
    coef_map = c(
      "applicable" = "Applicable price",
      "effective" = "Effective price",
      "fit_effective" = "Effective price",
      "tinc_ln" = "Log income"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    output = "latex",
    add_rows = addtab
  ) %>%
  kable_styling(font_size = 8) %>%
  add_header_above(c(
    " " = 1, "FE" = 2, "FE-2SLS" = 1, "FE" = 2, "FE-2SLS" = 1
  )) %>%
  add_header_above(c(" " = 1, "Log donation" = 3, "Dummy of donor" = 3)) %>%
  group_rows("Implied price elasticity", 7, 8, italic = TRUE, bold = FALSE) %>%
  group_rows(
    "1st stage information (Excluded instrument: Applicable price)",
    9, 10, bold = FALSE, italic = TRUE
  ) %>%
  column_spec(2:7, width = "5em") %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. Standard errors clustered at household level are in parentheses. An outcome variable is logged value of amount of charitable giving for models (1)--(3) and a dummy of donor for models (4)--(6). For estimation, models (1)--(3) use donors only (intensive-margin sample), and models (4)--(6) use not only donors but also non-donors (extensive-margin sample). To exclude announcement effect, we exclude samples from 2013 and 2014. For outcome equation, we control squared age (divided by 100), number of household members, a dummy that indicates having dependents, employee dummy, a set of dummies of industry a set of dummies of residential area, and individual and time fixed effects. For FE-2SLS, we use logged value of applicable price as an instrument.",
    threeparttable = TRUE,
    escape = FALSE
  ) %>%
  writeLines(out.file)

close(out.file)