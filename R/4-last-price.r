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

#' //NOTE: Relationship b/w Applicable and Effective
#+ plot-stage1, fig.cap = "Relationship between Applicable First Price and Last Price by Employment Status. Note: The bubble size indicates sample size. Due to the small sample size, the leftmost bubbles for salaried and self-employed workers are less informative ($N=6$ for wage earner and $N=2$ for self-employed).", out.extra = ""
plot_stage1 <- use %>%
  dplyr::filter(
    !is.na(d_relief_donate) & !is.na(lprice_ln) & !is.na(employee)
  ) %>%
  mutate(
    employee = factor(employee, label = c("Self-employed", "Wage earner"))
  ) %>%
  group_by(applicable, employee) %>%
  summarize(
    n = n(),
    d_relief_donate = mean(d_relief_donate),
    effective = mean(effective),
    applicable.last = mean(lprice_ln)
  ) %>%
  pivot_longer(effective:applicable.last, names_to = "type") %>%
  mutate(type = factor(
    type,
    labels = c("Applicable last price", "Effective last price")
  )) %>%
  ggplot(aes(x = applicable, y = value)) +
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  geom_point(aes(size = n, color = employee), alpha = 0.8) +
  scale_color_grey() +
  scale_size(range = c(5, 20)) +
  facet_wrap(~ type) +
  labs(
    x = "log(first price)",
    y = "Sample average",
    color = ""
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 5)),
    size = "none"
  ) +
  ggtemp()

plot_stage1

ggsave(
  here("export", "figures", "plot-stage1.pdf"),
  width = 10,
  height = 5
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
  outcome ~ ..stage2 | effective_last ~ applicable,
  outcome ~ ..stage2 | effective_last ~ applicable + applicable:employee
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
    "Wu-Hausman test, p-value",
    "Sargan Test, p-value"
  ),
  model1 = rep(NA_real_, 3),
  model2 = rep(NA_real_, 3)
)

ivtable <- subset(est_femod, type == "intensive")$est[[1]][3:5] %>%
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
    "stat...5" = "Applicable",
    "stat...6" = "Applicable * Wage earner"
  ), .)

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
  add_header_above(c(" " = 1, "FE" = 2, "FE-2SLS" = 3)) %>%
  add_header_above(c(" " = 1, "Log donation" = 5)) %>%
  group_rows("1st stage information", 7, 10, bold = FALSE, italic = TRUE) %>%
  column_spec(2:6, width = "6em") %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. We use standard errors clustered at household level. An outcome variable is logged value of amount of charitable giving. For estimation, we use donors only (intensive-margin sample). For outcome equation, we control squared age (divided by 100), number of household members, a dummy that indicates having dependents, employee dummy, a set of dummies of industry a set of dummies of residential area, and individual and time fixed effects. For FE-2SLS, we use following instrumental variables: $\\\\text{Applicable first-price}$ in model (3) and (4); $\\\\text{Applicable first-price} + \\\\text{Applicable first-price} \\\\times \\\\text{Wage earner}$ in model (5).",
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

ivtable <- subset(est_femod, type == "extensive")$est[[1]][3:5] %>%
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
    "stat...5" = "Applicable",
    "stat...6" = "Applicable $\\times$ Wage earner"
  ), .) %>%
  setNames(c("term", paste0("mod", seq(length(femod)))))

add_table <- bind_rows(implied_e, ivtable) %>%
  mutate(term = dplyr::recode(term, "Estimates se" = "", .default = term))

attr(add_table, "position") <- 7:12

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
  add_header_above(c(" " = 1, "FE" = 2, "FE-2SLS" = 3)) %>%
  add_header_above(c(" " = 1, "A dummy of donor" = 5)) %>%
  group_rows("Implied price elasticity", 7, 8, italic = TRUE, bold = FALSE) %>%
  group_rows("1st stage information", 9, 12, italic = TRUE, bold = FALSE) %>%
  column_spec(2:6, width = "6em") %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. We use standard errors clustered at household level. An outcome variable is a dummy indicating that donor. For estimation, we use not only donors but also non-donors (extensive-margin sample). For outcome equation, we control squared age (divided by 100), number of household members, a dummy that indicates having dependents, a employee dummy, a set of dummies of industry, a set of dummies of residential area, and individual and time fixed effects. For FE-2SLS, we use following instrumental variables: $\\\\text{Applicable first-price}$ in model (3) and (4); $\\\\text{Applicable first-price} + \\\\text{Applicable first-price} \\\\times \\\\text{Wage earner}$ in model (5). We calculate implied price elasticities by dividing estimates by proportion of donors in our sample.",
    threeparttable = TRUE,
    escape = FALSE
  ) %>%
  writeLines(out.file)

close(out.file)
