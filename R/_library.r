library(tidyverse)
library(rlist)
library(rlang)
library(modelsummary)
library(kableExtra)
library(estimatr)
library(fixest)
library(patchwork)

lapply(Sys.glob(here("R/functions", "*.r")), source)

options(
  modelsummary_stars_note = FALSE
)