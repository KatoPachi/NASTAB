library(here)
source(here("R", "_library.r"))

use <- readr::read_csv(here("data/shaped2.csv")) %>%
  dplyr::filter(2010 <= year & year < 2018) %>%
  dplyr::filter(tinc < 1100 | 1300 < tinc) %>%
  dplyr::filter(tinc < 4500 | 4700 < tinc) %>%
  dplyr::filter(tinc < 8700 | 8900 < tinc) %>%
  dplyr::filter(tinc < 14000 | 16000 < tinc) %>%
  dplyr::filter(tinc < 30000) %>%
  dplyr::filter(dependents == 0) %>%
  dplyr::filter(tinc > donate) %>%
  dplyr::filter(d_relief_donate == 0 | donate <= religious_ub) %>%
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