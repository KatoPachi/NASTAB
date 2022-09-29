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
    credit_benefit,
    credit_loss,
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
    effective = d_relief_donate * lprice_ln,
    applicable = price_ln,
    after = if_else(year >= 2014, 1, 0)
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

#' //NOTE: Estimate first-stage models
#+ reg-stage1, eval = FALSE
fixest::setFixest_fml(
  ..stage2 = ~ tinc_ln + sqage + hh_num + have_dependents |
    year + pid + indust + area
)

stage1 <- list(
  effective ~ credit_benefit:after + credit_loss:after + ..stage2,
  effective ~ employee + credit_benefit:after + credit_loss:after +
    employee:credit_benefit + employee:credit_loss +
    credit_benefit:employee:after + credit_loss:employee:after + ..stage2,
  effective ~ applicable + ..stage2,
  effective ~ employee + applicable + applicable:employee + ..stage2
)

est_stage1 <- use %>%
  mutate(type = factor(type, levels = c("intensive", "extensive"))) %>%
  group_by(type) %>%
  do(est = lapply(
    stage1,
    function(x) feols(x, data = subset(., flag == 1), cluster = ~hhid)
  ))

#' //NOTE: Create regression table of first-stage model
#+
out.file <- file(here("export", "tables", "fe2sls-stage1.tex"), open = "w")

tab <- est_stage1 %>%
  pull(est) %>%
  flatten() %>%
  setNames(paste0("(", seq(length(stage1) * 2), ")")) %>%
  modelsummary(
    title = "Regression Results of First-Stage Model\\label{tab:fe2sls-stage1}",
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
    "Sample:" = 1,
    "Intensive-margin" = 4, "Extensive-margin" = 4
  )) %>%
  kableExtra::add_header_above(c(
    " " = 1, "Log effective last-price" = 8
  )) %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. We use standard errors clustered at household level. An outcome variable is logged value of the effective last price. For estimation, models (1)--(4) use donors only (intensive-margin sample), and models (5)--(8) use not only donors but also non-donors (extensive-margin sample).  We control squared age (divided by 100), number of household members, a dummy that indicates having dependents, a set of dummies of industry a set of dummies of residential area, and individual and time fixed effects.",
    threeparttable = TRUE,
    escape = FALSE
  ) %>%
  kableExtra::landscape()

writeLines(tab, out.file)
close(out.file)

#'
#' //NOTE: Estimate second-stage models
#+ fe2sls
fe2sls <- list(
  outcome ~ employee + ..stage2 |
    effective ~ credit_benefit:after + credit_loss:after,
  outcome ~ employee + ..stage2 |
    effective ~ credit_benefit:after +
    credit_loss:after + employee:credit_benefit + employee:credit_loss +
    credit_benefit:employee:after + credit_loss:employee:after,
  outcome ~ employee + ..stage2 |
    effective ~ applicable,
  outcome ~ employee + ..stage2 |
    effective ~ applicable + applicable:employee
)

est_models <- use %>%
  mutate(type = factor(type, levels = c("intensive", "extensive"))) %>%
  group_by(type) %>%
  do(est = lapply(
    fe2sls,
    function(x) feols(x, data = subset(., flag == 1), cluster = ~hhid)
  ))

stats_stage1 <- est_models %>%
  group_by(type) %>%
  do(tab = data.frame(
    models = paste0(.$type, c(1, 2, 3, 4)),
    f = lapply(.$est[[1]], function(x)
      fitstat(x, "ivf")[[1]]$stat
    ) %>% as_vector,
    wh = lapply(.$est[[1]], function(x)
      fitstat(x, "wh")$wh$p
    ) %>% as_vector,
    sargan = lapply(.$est[[1]], function(x) {
      test <- fitstat(x, "sargan")$sargan
      if (sum(is.na(test)) == 0) test$p else NA_real_ 
    }) %>% as_vector()
  )) %>%
  { bind_rows(.$tab) } %>%
  mutate(
    f = sprintf("%1.2f", f),
    wh = sprintf("%1.3f", wh),
    sargan = if_else(is.na(sargan), "", sprintf("%1.3f", sargan))
  ) %>%
  pivot_longer(f:sargan, names_to = "terms") %>%
  pivot_wider(names_from = models, values_from = value) %>%
  mutate(
    terms = recode(
      terms,
      "f" = "F-statistics of instruments",
      "wh" = "Wu-Hausman test, p-value",
      "sargan" = "Sargan Test, p-value"
    )
  ) %>%
  bind_rows(c(
    terms = "First-Stage Model",
    intensive1 = "DID",
    intensive2 = "DDD",
    intensive3 = "Price",
    intensive4 = "Hetero-Price",
    extensive1 = "DID",
    extensive2 = "DDD",
    extensive3 = "Price",
    extensive4 = "Hetero-Price"
  ), .)

attr(stats_stage1, "position") <- 5:8

#' //NOTE: Create regression table of second-stage (intensive)
#+
out.file <- file(here("export", "tables", "fe2sls-int.tex"), open = "w")

tab <- est_models %>%
  dplyr::filter(type == "intensive") %>%
  pull(est) %>%
  flatten() %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = "FE-2SLS of Tax-Price Elasticity (Intensive-Margin)\\label{tab:fe2sls-int}",
    coef_map = c(
      "fit_effective" = "Log effective last-price",
      "tinc_ln" = "Log income"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = stats_stage1 %>%
      dplyr::select(- starts_with("extensive")),
    output = "latex"
  ) %>%
  kableExtra::kable_styling(font_size = 8) %>%
  kableExtra::add_header_above(c(
    " ", "Log donation" = 4
  )) %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. Standard errors are clustered at household level. List of abbreviations: DID=standard DID model using the 2014 tax reform (model (1) in Table \\\\ref{tab:fe2sls-stage1}), DDD=DID model plus heterogeneity by wage earner (model (2) in Table \\\\ref{tab:fe2sls-stage1}), Price=applicable first-price (logged value) as an instrument (model (3) in Table \\\\ref{tab:fe2sls-stage1}), and the Hetero-Price=Price model plus heterogeneity by wage earner (model (4) in Table \\\\ref{tab:fe2sls-stage1}). We add the wage earner dummy to a set of covariates of the second stage model. An outcome variable is logged value of amount of charitable giving. For estimation, we use donors only (intensive-margin sample). We control squared age (divided by 100), number of household members, a dummy that indicates having dependents, a set of dummies of industry, a set of dummies of residential area, and individual and time fixed effects.",
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(tab, out.file)
close(out.file)

#' //NOTE: Create regression table of second-stage model (extensive)
#+
mu <- with(subset(use, type == "extensive"), mean(outcome))

implied_e <- est_models %>%
  dplyr::filter(type == "extensive") %>%
  pull(est) %>%
  flatten() %>%
  lapply(function(x) {
    res <- subset(tidy(x), str_detect(term, "effective")) %>%
      mutate(
        estimate = estimate / mu,
        estimate = case_when(
          p.value < 0.01 ~ sprintf("%1.3f***", estimate),
          p.value < 0.05 ~ sprintf("%1.3f**", estimate),
          p.value < 0.1 ~ sprintf("%1.3f*", estimate),
          TRUE ~ sprintf("%1.3f", estimate)
        ),
        std.error = sprintf("(%1.3f)", std.error / mu)
      )

    tribble(
      ~terms, ~extensive,
      "Implied price elasticity", res$estimate,
      "", res$std.error
    )
  }) %>%
  reduce(full_join, by = "terms", suffix = c("1", "2")) %>%
  rename(extensive3 = extensive11, extensive4 = extensive22)

add_rows <- bind_rows(
  implied_e,
  dplyr::select(stats_stage1, -starts_with("intensive"))
)

attr(add_rows, "position") <- 5:10

out.file <- file(here("export", "tables", "fe2sls-ext.tex"), open = "w")

tab <- est_models %>%
  dplyr::filter(type == "extensive") %>%
  pull(est) %>%
  flatten() %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = "FE-2SLS of Tax-Price Elasticity (Extensive-Margin)\\label{tab:fe2sls-ext}",
    coef_map = c(
      "fit_effective" = "Log effective last-price",
      "tinc_ln" = "Log income"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = add_rows,
    output = "latex"
  ) %>%
  kableExtra::kable_styling(font_size = 8) %>%
  kableExtra::add_header_above(c(" ", "A dummy of donor" = 4)) %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. Standard errors are clustered at household level. List of abbreviations: DID=standard DID model using the 2014 tax reform (model (5) in Table \\\\ref{tab:fe2sls-stage1}), DDD=DID model plus heterogeneity by wage earner (model (6) in Table \\\\ref{tab:fe2sls-stage1}), Price=applicable first-price (logged value) as an instrument (model (7) in Table \\\\ref{tab:fe2sls-stage1}), and the Hetero-Price=Price model plus heterogeneity by wage earner (model (8) in Table \\\\ref{tab:fe2sls-stage1}). We add the wage earner dummy to a set of covariates of the second stage model. An outcome variable is a dummy indicating donor. For estimation, we use not only donors but also non-donors (extensive-margin sample). We control squared age (divided by 100), number of household members, a dummy that indicates having dependents, a set of dummies of industry, a set of dummies of residential area, and individual and time fixed effects.",
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(tab, out.file)
close(out.file)
