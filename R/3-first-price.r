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

#' //NOTE: Estimate compound error
#+
est_error <- use %>%
  group_by(type) %>%
  nest() %>%
  mutate(
    data = map(data, ~ subset(., flag == 1)),
    fit = map(
      data,
      ~ feols(effective_reverse ~ applicable + ..stage2, data = .)
    )
  )

est_error$fit %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = "Fixed Effect Model of Reverse Effective Price",
    coef_map = c("applicable" = "Applicable price"),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1)
  ) %>%
  kable_styling() %>%
  add_header_above(c("Sample:", "Donors", "Donors and Non-donors"))

#' //NOTE: Estimate 1st stage model
#+
stage1 <- list(
  effective ~ applicable + ..stage2,
  effective ~ applicable + applicable:employee + ..stage2
)

est_stage1 <- use %>%
  mutate(type = factor(type, levels = c("intensive", "extensive"))) %>%
  group_by(type) %>%
  do(est = lapply(
    stage1,
    function(x) feols(x, data = subset(., flag == 1), cluster = ~hhid)
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
      "applicable:employee" = "Applicable price $\\times$ Wage earner",
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
    "Donors (Intensive-margin)" = 2,
    "Donors and Non-donors (Extensive-margin)" = 2
  )) %>%
  add_header_above(c(" " = 1, "Effective price" = 4)) %>%
  group_rows("Excluded instruments", 1, 4, italic = TRUE, bold = FALSE) %>%
  group_rows("Covariates", 5, 8, italic = TRUE, bold = FALSE) %>%
  column_spec(2:5, width = "7.5em") %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. We use standard errors clustered at household level. An outcome variable is logged value of the effective price. For estimation, models (1) and (2) use donors only (intensive-margin sample), and models (3) and (4) use not only donors but also non-donors (extensive-margin sample). In addition to logged income and wage earner dummy shown in table, covariates consist of squared age (divided by 100), number of household members, a dummy that indicates having dependents, a set of dummies of industry a set of dummies of residential area, and individual and time fixed effects. Excluded instruments are $\\\\text{Applicable price}$ and $\\\\text{Applicable price}\\\\times\\\\text{Wage earner}$.",
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
  outcome ~ ..stage2 | effective ~ applicable,
  outcome ~ ..stage2 | effective ~ applicable + applicable:employee
)

est_femod <- use %>%
  mutate(type = factor(type, levels = c("intensive", "extensive"))) %>%
  group_by(type) %>%
  do(est = lapply(
    femod,
    function(x) feols(x, data = subset(., flag == 1), cluster =~hhid)
  ))

#' //NOTE: Create regression table of intensive-margin price elasticity
#+
ivtable1 <- tibble(
  terms = c(
    "F-statistics of instruments",
    "Wu-Hausman test, p-value",
    "Sargan Test, p-value"
  ),
  model1 = rep(NA_real_, 3),
  model2 = rep(NA_real_, 3)
)

ivtable <- subset(est_femod, type == "intensive")$est[[1]][3:4] %>%
  purrr::map(function(x) {
    sargan <- fitstat(x, "sargan")$sargan
    data.frame(stat = c(
      fitstat(x, "ivf")[[1]]$stat,
      fitstat(x, "wh")$wh$p,
      ifelse(sum(is.na(sargan)) == 0, sargan$p, NA_real_)
    ))
  }) %>%
  reduce(bind_cols) %>%
  bind_cols(ivtable1, .) %>%
  mutate_at(
    vars(-terms), list(~ifelse(is.na(.), "", sprintf("\\num{%1.3f}", .)))
  ) %>%
  mutate_at(
    vars(-terms), list(~ ifelse(. == "\\num{0.000}", "$<$ \\num{0.001}", .))
  ) %>%
  bind_rows(c(
    "terms" = "Instruments",
    "model1" = "",
    "model2" = "",
    "stat...4" = "Applicable",
    "stat...5" = "Applicable * Wage earner"
  ), .)

attr(ivtable, "position") <- 7:10

out.file <- file(here("export", "tables", "main-int.tex"), open = "w")

subset(est_femod, type == "intensive")$est[[1]] %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = "Estimation Results of Intensive-Margin Price Elasticities\\label{tab:main-int}",
    coef_map = c(
      "applicable" = "Applicable price",
      "effective" = "Effective price",
      "fit_effective" = "Effective price",
      "tinc_ln" = "Log income"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    add_rows = ivtable,
    output = "latex"
  ) %>%
  kable_styling(font_size = 8) %>%
  add_header_above(c(" " = 1, "FE" = 2, "FE-2SLS" = 2)) %>%
  add_header_above(c(" " = 1, "Log donation" = 4)) %>%
  group_rows("1st stage information", 7, 10, bold = FALSE, italic = TRUE) %>%
  column_spec(2:5, width = "7.5em") %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. We use standard errors clustered at household level. An outcome variable is logged value of amount of charitable giving. For estimation, we use donors only (intensive-margin sample). For outcome equation, we control squared age (divided by 100), number of household members, a dummy that indicates having dependents, employee dummy, a set of dummies of industry a set of dummies of residential area, and individual and time fixed effects. For FE-2SLS, we use following instrumental variables: $\\\\text{Applicable price}$ in model (3); $\\\\text{Applicable price} + \\\\text{Applicable price} \\\\times \\\\text{Wage earner}$ in model (4).",
    threeparttable = TRUE,
    escape = FALSE
  ) %>%
  writeLines(out.file)

close(out.file)

#' //NOTE: Create regression table of extensive-margin price elasticity
#+
mu <- with(subset(use, type == "extensive"), mean(outcome))

implied_e <- subset(est_femod, type == "extensive")$est[[1]] %>%
  purrr::map(function(x) {
    res <- subset(tidy(x), str_detect(term, "effective|applicable")) %>%
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
  setNames(c("term", paste0("mod", seq(length(femod)))))

ivtable <- subset(est_femod, type == "extensive")$est[[1]][3:4] %>%
  purrr::map(function(x) {
    sargan <- fitstat(x, "sargan")$sargan
    data.frame(stat = c(
      fitstat(x, "ivf")[[1]]$stat,
      fitstat(x, "wh")$wh$p,
      ifelse(sum(is.na(sargan)) == 0, sargan$p, NA_real_)
    ))
  }) %>%
  reduce(bind_cols) %>%
  bind_cols(ivtable1, .) %>%
  mutate_at(
    vars(-terms), list(~ifelse(is.na(.), "", sprintf("\\num{%1.3f}", .)))
  ) %>%
  mutate_at(
    vars(-terms), list(~ ifelse(. == "\\num{0.000}", "$<$ \\num{0.001}", .))
  ) %>%
  bind_rows(c(
    "terms" = "Instruments",
    "model1" = "",
    "model2" = "",
    "stat...4" = "Applicable",
    "stat...5" = "Applicable $\\times$ Wage earner"
  ), .) %>%
  setNames(c("term", paste0("mod", seq(length(femod)))))

add_table <- bind_rows(implied_e, ivtable) %>%
  mutate(term = dplyr::recode(term, "Estimates se" = "", .default = term))

attr(add_table, "position") <- 7:12

out.file <- file(here("export", "tables", "main-ext.tex"), open = "w")

subset(est_femod, type == "extensive")$est[[1]] %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = "Estimation Results of Extensive-Margin Price Elasticities\\label{tab:main-ext}",
    coef_map = c(
      "applicable" = "Applicable price",
      "effective" = "Effective price",
      "fit_effective" = "Effective price",
      "tinc_ln" = "Log income"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    add_rows = add_table,
    output = "latex"
  ) %>%
  kable_styling(font_size = 8) %>%
  add_header_above(c(" " = 1, "FE" = 2, "FE-2SLS" = 2)) %>%
  add_header_above(c(" " = 1, "A dummy of donor" = 4)) %>%
  group_rows("Implied price elasticity", 7, 8, italic = TRUE, bold = FALSE) %>%
  group_rows("1st stage information", 9, 12, italic = TRUE, bold = FALSE) %>%
  column_spec(2:5, width = "7.5em") %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. We use standard errors clustered at household level. An outcome variable is a dummy indicating that donor. For estimation, we use not only donors but also non-donors (extensive-margin sample). For outcome equation, we control squared age (divided by 100), number of household members, a dummy that indicates having dependents, a employee dummy, a set of dummies of industry, a set of dummies of residential area, and individual and time fixed effects. For FE-2SLS, we use following instrumental variables: $\\\\text{Applicable price}$ in model (3); $\\\\text{Applicable price} + \\\\text{Applicable price} \\\\times \\\\text{Wage earner}$ in model (4). We calculate implied price elasticities by dividing estimates by proportion of donors in our sample.",
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
ivtable1 <- tibble(
  terms = c(
    "F-statistics of instruments",
    "Wu-Hausman test, p-value",
    "Sargan Test, p-value"
  ),
  model1 = rep(NA_real_, 3),
  model2 = rep(NA_real_, 3)
)

ivtable <- subset(est_announce, type == "intensive")$est[[1]][3:4] %>%
  purrr::map(function(x) {
    sargan <- fitstat(x, "sargan")$sargan
    data.frame(stat = c(
      fitstat(x, "ivf")[[1]]$stat,
      fitstat(x, "wh")$wh$p,
      ifelse(sum(is.na(sargan)) == 0, sargan$p, NA_real_)
    ))
  }) %>%
  reduce(bind_cols) %>%
  bind_cols(ivtable1, .) %>%
  mutate_at(
    vars(-terms), list(~ ifelse(is.na(.), "", sprintf("\\num{%1.3f}", .)))
  ) %>%
  mutate_at(
    vars(-terms), list(~ ifelse(. == "\\num{0.000}", "$<$ \\num{0.001}", .))
  ) %>%
  bind_rows(c(
    "terms" = "Instruments",
    "model1" = "",
    "model2" = "",
    "stat...4" = "Applicable",
    "stat...5" = "Applicable * Wage earner"
  ), .)

attr(ivtable, "position") <- 7:10

out.file <- file(here("export", "tables", "announcement-int.tex"), open = "w")

subset(est_announce, type == "intensive")$est[[1]] %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = "Estimation of Intensive-Margin Price Elasticities Excluding Announcement Effect\\label{tab:announcement-int}",
    coef_map = c(
      "applicable" = "Applicable price",
      "effective" = "Effective price",
      "fit_effective" = "Effective price",
      "tinc_ln" = "Log income"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    add_rows = ivtable,
    output = "latex"
  ) %>%
  kable_styling(font_size = 8) %>%
  add_header_above(c(" " = 1, "FE" = 2, "FE-2SLS" = 2)) %>%
  add_header_above(c(" " = 1, "Log donation" = 4)) %>%
  group_rows("1st stage information", 7, 10, bold = FALSE, italic = TRUE) %>%
  column_spec(2:5, width = "7.5em") %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. We use standard errors clustered at household level. An outcome variable is logged value of amount of charitable giving. For estimation, we use donors only (intensive-margin sample), but exclude samples from 2013 and 2014. For outcome equation, we control squared age (divided by 100), number of household members, a dummy that indicates having dependents, employee dummy, a set of dummies of industry a set of dummies of residential area, and individual and time fixed effects. For FE-2SLS, we use following instrumental variables: $\\\\text{Applicable price}$ in model (3); $\\\\text{Applicable price} + \\\\text{Applicable price} \\\\times \\\\text{Wage earner}$ in model (4).",
    threeparttable = TRUE,
    escape = FALSE
  ) %>%
  writeLines(out.file)

close(out.file)

#' //NOTE: Create regression table of extensive-margin price elasticity
#+
mu <- use %>%
  dplyr::filter(type == "extensive") %>%
  dplyr::filter(year < 2013 | 2014 < year) %>%
  with(mean(outcome))

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
      ~term, ~mod,
      "Estimates", res$estimate,
      "Estimates se", res$std.error
    )
  }) %>%
  reduce(left_join, by = "term") %>%
  setNames(c("term", paste0("mod", seq(length(femod)))))

ivtable <- subset(est_announce, type == "extensive")$est[[1]][3:4] %>%
  purrr::map(function(x) {
    sargan <- fitstat(x, "sargan")$sargan
    data.frame(stat = c(
      fitstat(x, "ivf")[[1]]$stat,
      fitstat(x, "wh")$wh$p,
      ifelse(sum(is.na(sargan)) == 0, sargan$p, NA_real_)
    ))
  }) %>%
  reduce(bind_cols) %>%
  bind_cols(ivtable1, .) %>%
  mutate_at(
    vars(-terms), list(~ ifelse(is.na(.), "", sprintf("\\num{%1.3f}", .)))
  ) %>%
  mutate_at(
    vars(-terms), list(~ ifelse(. == "\\num{0.000}", "$<$ \\num{0.001}", .))
  ) %>%
  bind_rows(c(
    "terms" = "Instruments",
    "model1" = "",
    "model2" = "",
    "stat...4" = "Applicable",
    "stat...5" = "Applicable $\\times$ Wage earner"
  ), .) %>%
  setNames(c("term", paste0("mod", seq(length(femod)))))

add_table <- bind_rows(implied_e, ivtable) %>%
  mutate(term = dplyr::recode(term, "Estimates se" = "", .default = term))

attr(add_table, "position") <- 7:12

out.file <- file(here("export", "tables", "announcement-ext.tex"), open = "w")

subset(est_announce, type == "extensive")$est[[1]] %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = "Estimation of Extensive-Margin Price Elasticities Excluding Announcement Effect\\label{tab:announcement-ext}",
    coef_map = c(
      "applicable" = "Applicable price",
      "effective" = "Effective price",
      "fit_effective" = "Effective price",
      "tinc_ln" = "Log income"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    add_rows = add_table,
    output = "latex"
  ) %>%
  kable_styling(font_size = 8) %>%
  add_header_above(c(" " = 1, "FE" = 2, "FE-2SLS" = 2)) %>%
  add_header_above(c(" " = 1, "A dummy of donor" = 4)) %>%
  group_rows("Implied price elasticity", 7, 8, italic = TRUE, bold = FALSE) %>%
  group_rows("1st stage information", 9, 12, italic = TRUE, bold = FALSE) %>%
  column_spec(2:5, width = "7.5em") %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. We use standard errors clustered at household level. An outcome variable is a dummy indicating that donor. For estimation, we use not only donors but also non-donors (extensive-margin sample), but exclude samples from 2013 and 2014. For outcome equation, we control squared age (divided by 100), number of household members, a dummy that indicates having dependents, a employee dummy, a set of dummies of industry, a set of dummies of residential area, and individual and time fixed effects. For FE-2SLS, we use following instrumental variables: $\\\\text{Applicable price}$ in model (3); $\\\\text{Applicable price} + \\\\text{Applicable price} \\\\times \\\\text{Wage earner}$ in model (4). We calculate implied price elasticities by dividing estimates by proportion of donors in our sample.",
    threeparttable = TRUE,
    escape = FALSE
  ) %>%
  writeLines(out.file)

close(out.file)