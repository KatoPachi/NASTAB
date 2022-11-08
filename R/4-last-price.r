#+
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
    tinc,
    tinc_ln,
    sqage,
    hh_num,
    have_dependents,
    indust,
    area,
    price_ln,
    lprice_ln,
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
    effective = d_relief_donate * price_ln,
    applicable = price_ln,
    effective_last = d_relief_donate * lprice_ln,
    applicable_last = lprice_ln
  )

#' //NOTE: Estimate last-price elasticity
#+ reg-stage1, eval = FALSE
fixest::setFixest_fml(
  ..stage2 = ~ tinc_ln + sqage + hh_num + have_dependents + employee |
    year + pid + indust + area
)

femod <- list(
  outcome ~ applicable_last + ..stage2,
  outcome ~ effective_last + ..stage2,
  outcome ~ ..stage2 | applicable_last ~ applicable,
  outcome ~ ..stage2 | effective_last ~ applicable
)

est_femod <- use %>%
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
    "Wu-Hausman test, p-value"
  ),
  model1 = rep(NA_real_, 2),
  model2 = rep(NA_real_, 2)
)

ivtable <- subset(est_femod, type == "intensive")$est[[1]][3:4] %>%
  purrr::map(function(x) {
    sargan <- fitstat(x, "sargan")$sargan
    data.frame(stat = c(
      fitstat(x, "ivf")[[1]]$stat,
      fitstat(x, "wh")$wh$p
    ))
  }) %>%
  reduce(bind_cols) %>%
  bind_cols(ivtable1, .) %>%
  mutate_at(
    vars(-terms), list(~ ifelse(is.na(.), "", sprintf("\\num{%1.3f}", .)))
  ) %>%
  mutate_at(
    vars(-terms), list(~ ifelse(. == "\\num{0.000}", "$<$ \\num{0.001}", .))
  )

attr(ivtable, "position") <- 7:10

out.file <- file(here("export", "tables", "last-int.tex"), open = "w")

subset(est_femod, type == "intensive")$est[[1]] %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = "Estimation Results of Intensive-Margin Last-Price Elasticities\\label{tab:last-int}",
    coef_map = c(
      "applicable_last" = "Applicable last-price",
      "fit_applicable_last" = "Applicable last-price",
      "effective_last" = "Effective last-price",
      "fit_effective_last" = "Effective last-price",
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
  group_rows(
    "1st stage information (Excluded instrument: Applicable price)",
    7, 8, bold = FALSE, italic = TRUE
  ) %>%
  column_spec(2:5, width = "6.25em") %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. Standard errors clustered at household level are in parenthesis. An outcome variable is logged value of amount of charitable giving. For estimation, we use donors only (intensive-margin sample). For outcome equation, we control squared age (divided by 100), number of household members, a dummy that indicates having dependents, employee dummy, a set of dummies of industry a set of dummies of residential area, and individual and time fixed effects. For FE-2SLS, we use a logged applicable price as an instrument.",
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

ivtable <- subset(est_femod, type == "extensive")$est[[1]][3:4] %>%
  purrr::map(function(x) {
    sargan <- fitstat(x, "sargan")$sargan
    data.frame(stat = c(
      fitstat(x, "ivf")[[1]]$stat,
      fitstat(x, "wh")$wh$p
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
  setNames(c("term", paste0("mod", seq(length(femod)))))

add_table <- bind_rows(implied_e, ivtable) %>%
  mutate(term = dplyr::recode(term, "Estimates se" = "", .default = term))

attr(add_table, "position") <- 7:10

out.file <- file(here("export", "tables", "last-ext.tex"), open = "w")

subset(est_femod, type == "extensive")$est[[1]] %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = "Estimation Results of Extensive-Margin Last-Price Elasticities\\label{tab:last-ext}",
    coef_map = c(
      "applicable_last" = "Applicable last-price",
      "fit_applicable_last" = "Applicable last-price",
      "effective_last" = "Effective last-price",
      "fit_effective_last" = "Effective last-price",
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
  group_rows(
    "1st stage information (Excluded instrument: Applicable price)",
    9, 10, italic = TRUE, bold = FALSE
  ) %>%
  column_spec(2:5, width = "6.25em") %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. Standard errors clustered at household level are in parentheses. An outcome variable is a dummy indicating that donor. For estimation, we use not only donors but also non-donors (extensive-margin sample). For outcome equation, we control squared age (divided by 100), number of household members, a dummy that indicates having dependents, a employee dummy, a set of dummies of industry, a set of dummies of residential area, and individual and time fixed effects. For FE-2SLS, we use a logged applicable price as an instrument. We calculate implied price elasticities by dividing estimated coeffcient on price by sample proportion of donors.",
    threeparttable = TRUE,
    escape = FALSE
  ) %>%
  writeLines(out.file)

close(out.file)
