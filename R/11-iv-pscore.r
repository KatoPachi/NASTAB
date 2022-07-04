#' ---
#' title: |
#'   Estimating Effect of Tax Incentives on Donations
#'   Considering Self-Selection of Tax Incentives in South Korea
#' subtitle: |
#'   Results of FE-2SLS
#' author:
#'   - Hiroki Kato
#'   - Tsuyoshi Goto
#'   - Yongrok Kim
#' output:
#'   bookdown::html_document2:
#'     toc: true
#'     toc_float: true
#'     number_sections: false
#' params:
#'   preview: true
#' ---
#'
#+ include = FALSE, eval = params$preview
library(here)
source(here("R", "_library.r"))

#+ include = FALSE
source(here("R", "_html_header.r"))

#+ include = FALSE
rawdt <- readr::read_csv(
  here("data/shaped2_propensity.csv"),
  guess_max = 30000
)

flag <- rawdt %>%
  mutate(
    flag_extensive = 1,
    flag_intensive = if_else(d_donate == 1, 1, 0)
  ) %>%
  select(
    pid,
    year,
    flag_extensive,
    flag_intensive
  ) %>%
  pivot_longer(
    flag_extensive:flag_intensive,
    names_to = "outcome",
    values_to = "flag",
    names_prefix = "flag_"
  )

main <- rawdt %>%
  select(
    pid,
    hhid,
    year,
    linc_ln,
    sqage,
    hh_num,
    have_dependents,
    indust,
    area,
    price_ln,
    lprice_ln,
    d_relief_donate,
    employee,
    intensive = donate_ln,
    extensive = d_donate
  ) %>%
  pivot_longer(
    intensive:extensive,
    names_to = "outcome",
    values_to = "y"
  )

use <- flag %>%
  dplyr::left_join(main, by = c("pid", "year", "outcome")) %>%
  mutate(effective = d_relief_donate * lprice_ln)