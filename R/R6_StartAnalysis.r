library(here)
library(R6)
library(tidyverse)
source(here("R/R6_SummaryData.r"))
source(here("R/R6_FirstPrice.r"))
source(here("R/R6_LastPrice.r"))
source(here("R/R6_PolicyEffect.r"))

StartAnalysis <- R6::R6Class("StartAnalysis", list(
  data = NULL,
  initialize = function(path, threshold_cut = 100) {
    dta <- read_csv(path)
    x <- threshold_cut

    dta <- dta %>%
      dplyr::filter(taxable_tinc < 1200 - x | 1200 + x < taxable_tinc) %>%
      dplyr::filter(taxable_tinc < 4600 - x | 4600 + x < taxable_tinc) %>%
      dplyr::filter(taxable_tinc < 8800 - x | 8800 + x < taxable_tinc) %>%
      dplyr::filter(taxable_tinc < 15000 - x | 15000 + x < taxable_tinc)

    self$data <- dta
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
