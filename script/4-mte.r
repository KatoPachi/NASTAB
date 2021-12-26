#' ---
#' title: Marginal Treatment Effect
#' author: Hiroki Kato
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#' params:
#'   preview: true
#' ---
#'
#+ include = FALSE, eval = params$preview
knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  echo = FALSE,
  cache = FALSE,
  include = TRUE,
  fig.width = 10
)

library(here)
knitr::opts_knit$set(
  root.dir = here::here()
)

options(
  knitr.kable.NA = " ",
  knitr.table.format = "html",
  modelsummary_stars_note = FALSE
)


#'
#+ include = FALSE, eval = params$preview
library(xfun)
xfun::pkg_attach2(c(
  "tidyverse", "rlist", "modelsummary", "kableExtra",
  "estimatr", "fixest",
  "ivmte", "splines2", "gurobi"
))

lapply(Sys.glob(file.path("script/functions", "*.r")), source)

#'
#+
df <- readr::read_csv("data/shaped2.csv")

#'
#+ include = FALSE
# check whether "gurobi" works
# create optimization problem
model <- list()
model$obj        <- c(1, 1, 2)
model$modelsense <- "max"
model$rhs        <- c(4, 1)
model$sense      <- c("<", ">")
model$vtype      <- "B"
model$A          <- matrix(c(1, 2, 3, 1, 1, 0), nrow = 2, ncol = 3,
                           byrow = TRUE)

# solve the optimization problem using Gurobi
result <- gurobi(model, list())

#'
#+
intensive_mte <- ivmte(
  data = df,
  target = "atu",
  m0 = ~ u + linc_ln + factor(year),
  m1 = ~ u + price_ln + linc_ln + factor(year),
  ivlike = donate_ln ~ price_ln + linc_ln + factor(year),
  propensity = d_relief_donate ~ employee,
  link = "probit",
  bootstraps = 100
)

#'
# /*
#+
rmarkdown::render(
  "script/4-mte.r",
  output_dir = "report/view"
)
# */