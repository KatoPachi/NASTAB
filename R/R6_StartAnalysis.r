library(here)
library(R6)
library(tidyverse)
source(here("R/R6_SummaryData.r"))
source(here("R/R6_FirstPrice.r"))
source(here("R/R6_LastPrice.r"))
source(here("R/R6_PolicyEffect.r"))

StartAnalysis <- R6::R6Class("StartAnalysis", list(
  data = NULL,
  incentive_limit_summary = NULL,
  initialize = function(path) {
    dta <- read_csv(path) %>%
      dplyr::filter(age >= 24) %>%
      dplyr::filter(d_relief_donate == 0 | (d_relief_donate == 1 & d_donate == 1)) %>%
      dplyr::filter(2010 <= year & year < 2018) %>%
      dplyr::filter(tinc < 1100 | 1300 < tinc) %>%
      dplyr::filter(tinc < 4500 | 4700 < tinc) %>%
      dplyr::filter(tinc < 8700 | 8900 < tinc) %>%
      dplyr::filter(tinc < 14000 | 16000 < tinc) %>%
      dplyr::filter(tinc < 30000) %>%
      dplyr::filter(bracket13 != "(F) & (G) 30000--" | is.na(bracket13)) %>%
      dplyr::filter(dependents == 0)
    
    incentive_limit_summary <- dta %>%
      mutate(over_bound = donate > incentive_limit) %>%
      group_by(year) %>%
      summarize(mean(over_bound), mean(incentive_limit))
    
    dta <- dta %>%
      dplyr::filter(tinc > donate) %>%
      dplyr::filter(d_relief_donate == 0 | incentive_limit >= donate)

    self$data <- dta %>%
      select(
        pid,
        hhid,
        year,
        bracket13,
        tinc,
        tinc_ln,
        linc,
        price,
        price_ln,
        lprice_ln,
        d_relief_donate,
        age,
        sqage,
        hh_num,
        have_dependents,
        sex,
        college,
        highschool,
        employee,
        indust,
        area,
        donate,
        donate_ln,
        d_donate
      )
  },
  summary = function() SummaryData$new(self$data),
  first_price = function() FirstPrice$new(self$data),
  last_price = function() LastPrice$new(self$data),
  policy_effect = function() PolicyEffect$new(self$data),
  remove_bracket_shift = function() {
    take_lag <- use$data %>%
      group_by(pid) %>%
      arrange(year) %>%
      mutate(
        lag1_price = if_else(year - lag(year) > 1, NA_real_, price - lag(price)),
        lag2_price = if_else(year - lag(year) > 2, NA_real_, price - lag(price, 2)),
        lag3_price = if_else(year - lag(year) > 3, NA_real_, price - lag(price, 3))
      )
    
    shifter1 <- subset(take_lag, year %in% 2011:2013 & lag1_price != 0)$pid
    tbl_shifter1 <- table(table(shifter1))

    cat("Summary of Bracket-shift in 2010--2013\n")
    cat("- One-year bracket shift:", length(shifter1), "obs\n")
    cat("  - one time:", tbl_shifter1[1], "people\n")
    cat("  - two times:", tbl_shifter1[2], "people\n")
    cat("  - three times:", tbl_shifter1[3], "people\n")

    shifter2 <- subset(take_lag, year %in% 2011:2013 & lag2_price != 0)$pid
    tbl_shifter2 <- table(table(shifter2))

    cat("- Two-year bracket shift:", length(shifter2), "obs\n")
    cat("  - one time:", tbl_shifter2[1], "people\n")
    cat("  - two times:", tbl_shifter2[2], "people\n")

    shifter3 <- subset(take_lag, year %in% 2011:2013 & lag3_price != 0)$pid
    
    cat("- Three-year bracket shift:", length(shifter3), "obs\n")
    cat("  - one time:", length(shifter3), "people\n")

    shifter <- unique(c(shifter1, shifter2, shifter3))

    cat("In total, there are", length(shifter), "unique shifters.\n")

    self$data <- subset(self$data, !(pid %in% shifter))
    invisible(self$data)
  },
  limit_2_year = function() {
    self$data <- subset(self$data, year == 2012 | year == 2015)
    invisible(self$data)
  }
))
