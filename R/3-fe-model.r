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

#' //NOTE: Estimate price elasticity
#+ fe-model include = FALSE
fixest::setFixest_fml(
  ..stage2 = ~ tinc_ln + sqage + hh_num + have_dependents |
    year + pid + indust + area
)

femod <- list(
  outcome ~ credit_benefit:after + credit_loss:after + ..stage2,
  outcome ~ employee + credit_benefit:after + credit_loss:after +
    employee:credit_benefit + employee:credit_loss +
    credit_benefit:employee:after + credit_loss:employee:after + ..stage2,
  outcome ~ applicable + ..stage2,
  outcome ~ employee + applicable + applicable:employee + ..stage2,
  outcome ~ effective + ..stage2
)

est_femod <- use %>%
  mutate(type = factor(type, levels = c("intensive", "extensive"))) %>%
  group_by(type) %>%
  do(est = lapply(
    femod,
    function(x) feols(x, data = subset(., flag == 1), cluster =~hhid)
  ))

#' //NOTE: Calculate wage-earner elasticity (intensive)
#+ elasticity-intensive
e_employee <- subset(est_femod, type == "intensive")$est[[1]][[4]] %>%
  tidy() %>%
  subset(str_detect(term, "employee|applicable")) %>%
  summarize(
    estimate = sum(estimate),
    std.error = sqrt(sum(std.error^2)),
    p.value = 2 * pt(
      abs(estimate / std.error),
      df = attr(vcov(
        subset(est_femod, type == "intensive")$est[[1]][[4]],
        attr = TRUE
      ), "df.t"),
      lower.tail = FALSE
    )
  ) %>%
  mutate(
    estimate = estimate,
    estimate = case_when(
      p.value < 0.01 ~ sprintf("%1.3f***", estimate),
      p.value < 0.05 ~ sprintf("%1.3f**", estimate),
      p.value < 0.1 ~ sprintf("%1.3f*", estimate),
      TRUE ~ sprintf("%1.3f", estimate)
    ),
    std.error = sprintf("(%1.3f)", std.error)
  ) %>% {
    tribble(
      ~term, ~mod,
      "Wage earner", .$estimate,
      "Wage earner se", .$std.error
    )
  }

e_tab <- e_employee %>%
  dplyr::left_join(
    tribble(
      ~term, ~bk1, ~bk2, ~bk3,
      "Wage earner", "", "", "",
      "Wage earner se", "", "", ""
    ),
    .,
    by = "term"
  ) %>%
  bind_cols(bk4 = c("", "")) %>%
  mutate(term = if_else(str_detect(term, "se"), "", term))

attr(e_tab, "position") <- c(23, 24)

#' //NOTE: Create regression table of FE model (intensive)
#+ fe-model-int
out.file <- file(here("export", "tables", "fe-model-int.tex"), open = "w")

tab <- subset(est_femod, type == "intensive")$est[[1]] %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = "Regression Results of Two-Way Fixed Effect Model (Intensive-Margin Sample)\\label{tab:fe-model-int}",
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
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    add_rows = e_tab,
    output = "latex"
  ) %>%
  kableExtra::kable_styling(font_size = 8) %>%
  kableExtra::add_header_above(c(
    " " = 1, "Log donation" = 5
  )) %>%
  kableExtra::pack_rows(
    "F-test", 23, 24,
    bold = FALSE, italic = TRUE
  ) %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. We use standard errors clustered at household level. An outcome variable is logged value of amount of charitable giving. For estimation, we use donors only (intensive-margin sample). We control squared age (divided by 100), number of household members, a dummy that indicates having dependents, a set of dummies of industry a set of dummies of residential area, and individual and time fixed effects.",
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(tab, out.file)
close(out.file)

#' //NOTE: Compute implied elasticity (extensive)
#+ implied-elasticity-extensive
mu <- with(subset(use, type == "extensive"), mean(outcome))

impe_overall <- subset(est_femod, type == "extensive")$est[[1]][c(3, 5)] %>%
  purrr::map(function(x) {
    res <- subset(tidy(x), str_detect(term, "effective|applicable")) %>%
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
      ~term, ~mod,
      "Overall", res$estimate,
      "Overall se", res$std.error
    )
  })

impe_non_employee <- subset(est_femod, type == "extensive")$est[[1]][[4]] %>%
  tidy() %>%
  subset(str_detect(term, "applicable")) %>%
  subset(!str_detect(term, "employee")) %>%
  mutate(
    estimate = estimate / mu,
    estimate = case_when(
      p.value < 0.01 ~ sprintf("%1.3f***", estimate),
      p.value < 0.05 ~ sprintf("%1.3f**", estimate),
      p.value < 0.1 ~ sprintf("%1.3f*", estimate),
      TRUE ~ sprintf("%1.3f", estimate)
    ),
    std.error = sprintf("(%1.3f)", std.error / mu)
  ) %>% {
    tribble(
      ~term, ~mod,
      "Non wage earner", .$estimate,
      "Non wage earner se", .$std.error
    )
  }

impe_employee <- subset(est_femod, type == "extensive")$est[[1]][[4]] %>%
  tidy() %>%
  subset(str_detect(term, "employee|applicable")) %>%
  summarize(
    estimate = sum(estimate),
    std.error = sqrt(sum(std.error^2)),
    p.value = 2 * (pt(
      abs(estimate/std.error),
      df = attr(vcov(
        subset(est_femod, type == "extensive")$est[[1]][[4]],
        attr = TRUE
      ), "df.t"),
      lower.tail = FALSE
    ))
  ) %>%
  mutate(
    estimate = estimate / mu,
    estimate = case_when(
      p.value < 0.01 ~ sprintf("%1.3f***", estimate),
      p.value < 0.05 ~ sprintf("%1.3f**", estimate),
      p.value < 0.1 ~ sprintf("%1.3f*", estimate),
      TRUE ~ sprintf("%1.3f", estimate)
    ),
    std.error = sprintf("(%1.3f)", std.error / mu)
  ) %>% {
    tribble(
      ~term, ~mod,
      "Wage earner", .$estimate,
      "Wage earner se", .$std.error
    )
  }

impe_tab <- impe_overall[[1]] %>%
  full_join(bind_rows(impe_employee, impe_non_employee), by = "term") %>%
  left_join(impe_overall[[2]], by = "term") %>%
  right_join(
    tribble(
      ~term, ~bk1, ~bk2,
      "Overall", "", "",
      "Overall se", "", "",
      "Wage earner", "", "",
      "Wage earner se", "", "",
      "Non wage earner", "", "",
      "Non wage earner se", "", ""
    ),
    .,
    by = "term"
  ) %>%
  mutate(term = if_else(str_detect(term, "se"), "", term)) %>%
  mutate_at(vars(starts_with("mod")), list(~ifelse(is.na(.), "", .)))

attr(impe_tab, "position") <- 23:28

#' //NOTE: Create regression table (extensive)
#+ fe-mod-ext
out.file <- file(here("export", "tables", "fe-model-ext.tex"), open = "w")

tab <- subset(est_femod, type == "extensive")$est[[1]] %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = paste0(
      "Regression Results of Two-Way Fixed Effect Model (Extensive-Margin Sample)",
      "\\label{tab:fe-model-ext}"
    ),
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
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    add_rows = impe_tab,
    output = "latex"
  ) %>%
  kableExtra::kable_styling(font_size = 8) %>%
  kableExtra::add_header_above(c(
    " " = 1, "A dummy of donation" = 5
  )) %>%
  kableExtra::group_rows(
    "Implied price elasticity",
    23, 28,
    italic = TRUE, bold = FALSE
  ) %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. We use standard errors clustered at household level. An outcome variable is a dummy indicating that donor. For estimation, we use not only donors but also non-donors (extensive-margin sample). We control squared age (divided by 100), number of household members, a dummy that indicates having dependents, a set of dummies of industry, a set of dummies of residential area, and individual and time fixed effects. We calculate implied price elasticities by dividing estimates by proportion of donors in our sample.",
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(tab, out.file)
close(out.file)

#' //NOTE: Execute event-study
#+ event-study
event <- use %>%
  mutate(
    year12 = if_else(year == 2013, 1, 0),
    year14 = if_else(year == 2014, 1, 0),
    year15 = if_else(year == 2015, 1, 0),
    year16 = if_else(year == 2016, 1, 0),
    year17 = if_else(year == 2017, 1, 0)
  ) %>%
  group_by(type) %>%
  nest() %>%
  mutate(fit = map(data, ~ feols(
    outcome ~ credit_benefit:year12 + credit_loss:year12 +
      credit_benefit:year14 + credit_loss:year14 +
      credit_benefit:year15 + credit_loss:year15 +
      credit_benefit:year16 + credit_loss:year16 +
      credit_benefit:year17 + credit_loss:year17 + ..stage2,
    data = subset(.x, flag = 1),
    cluster = ~hhid
  ))) %>%
  mutate(
    tidy = map(fit, tidy),
    t = map(fit, ~ qt(
      0.025,
      df = attr(vcov(.x, attr = TRUE), "df.t"),
      lower.tail = FALSE
    ))
  ) %>%
  select(-data, -fit) %>%
  unnest(c(tidy, t)) %>%
  dplyr::filter(str_detect(term, "year"))

plot_event <- event %>%
  bind_rows(tribble(
    ~type, ~term, ~ estimate, ~std.error, ~statistic, ~p.value, ~t,
    "intensive", "credit_benefit:year13", 0, 0, 0, 0, 0,
    "extensive", "credit_benefit:year13", 0, 0, 0, 0, 0,
    "intensive", "credit_loss:year13", 0, 0, 0, 0, 0,
    "extensive", "credit_loss:year13", 0, 0, 0, 0, 0
  )) %>%
  mutate(
    year = case_when(
      str_detect(term, "year12") ~ 2012,
      str_detect(term, "year13") ~ 2013,
      str_detect(term, "year14") ~ 2014,
      str_detect(term, "year15") ~ 2015,
      str_detect(term, "year16") ~ 2016,
      str_detect(term, "year17") ~ 2017
    ),
    treat = case_when(
      str_detect(term, "benefit") ~ "Decrease",
      str_detect(term, "loss") ~ "Increase"
    ),
    treat = factor(
      treat,
      levels = c("Decrease", "Increase"),
      labels = c("Decreasing price group", "Increasing price group")
    ),
    type = factor(
      type,
      levels = c("intensive", "extensive"),
      labels = c("Intensive-margin", "Extensive-margin")
    ),
    ci.low = estimate - std.error * t,
    ci.high = estimate + std.error * t
  ) %>%
  ungroup()

plot_event %>%
  ggplot(aes(x = year, y = estimate)) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = ci.low, ymax = ci.high), alpha = 0.1) +
  geom_point(size = 4) +
  facet_wrap(~ type * treat, scale = "free_y") +
  labs(x = "Year", y = "Estimates (95%CI)") +
  ggtemp()

ggsave(
  here("export", "figures", "event-study.pdf"),
  width = 10, height = 8
)

#' //NOTE: Estimate application model
#+
femod <- list(
  d_relief_donate ~ credit_benefit:after + credit_loss:after + ..stage2,
  d_relief_donate ~ employee + credit_benefit:after + credit_loss:after +
    employee:credit_benefit + employee:credit_loss +
    credit_benefit:employee:after + credit_loss:employee:after + ..stage2,
  d_relief_donate ~ applicable + ..stage2,
  d_relief_donate ~ employee + applicable + applicable:employee + ..stage2
)

est_femod <- use %>%
  mutate(type = factor(type, levels = c("intensive", "extensive"))) %>%
  group_by(type) %>%
  do(est = lapply(
    femod,
    function(x) feols(x, data = subset(., flag == 1), cluster = ~hhid)
  ))

#' //NOTE: Create regression table of application model
#+
out.file <- file(here("export", "tables", "fe-application.tex"), open = "w")

tab <- est_femod %>%
  pull(est) %>%
  flatten() %>%
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
    "Sample:" = 1,
    "Intensive-margin" = 4, "Extensive-margin" = 4
  )) %>%
  kableExtra::add_header_above(c(
    " " = 1, "Application of tax relief" = 8
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
