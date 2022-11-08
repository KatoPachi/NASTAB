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
    after = if_else(year >= 2014, 1, 0)
  )

fixest::setFixest_fml(
  ..stage2 = ~ tinc_ln + sqage + hh_num + have_dependents + employee |
    year + pid + indust + area
)

#' //NOTE: Conduct event-study
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
