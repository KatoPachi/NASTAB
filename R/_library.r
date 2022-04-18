library(tidyverse)
library(rlist)
library(rlang)
library(modelsummary)
library(kableExtra)
library(estimatr)
library(fixest)

lapply(Sys.glob(file.path("R/functions", "*.r")), source)