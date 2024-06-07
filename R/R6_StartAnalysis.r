library(here)
library(R6)
library(tidyverse)
source(here("R/R6_SummaryData.r"))
source(here("R/R6_FirstPrice.r"))
source(here("R/R6_LastPrice.r"))
source(here("R/R6_PolicyEffect.r"))

StartAnalysis <- R6::R6Class("StartAnalysis", list(
  data = NULL,
  initialize = function(data) self$data <- data,
  summary = function() SummaryData$new(self$data),
  first_price = function() FirstPrice$new(self$data),
  last_price = function() LastPrice$new(self$data),
  policy_effect = function() PolicyEffect$new(self$data)
))
