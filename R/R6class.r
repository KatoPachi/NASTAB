library(R6)
library(tidyverse)

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
        tinc,
        tinc_ln,
        linc,
        price,
        price_ln,
        donate,
        donate_ln,
        d_donate,
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
        area
      )
  },
  summary = function() SummaryData$new(self$data)
))

SummaryData <- R6::R6Class("SummaryData", list(
  data = NULL,
  initialize = function(data) self$data <- data
))