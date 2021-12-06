#' ---
#' title: |
#'   Price Elasticity of Charitable Giving
#' author: Hiroki Kato
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
  "estimatr", "fixest"
))

lapply(Sys.glob(file.path("script/R/functions", "*.r")), source)

#'
#+ include = FALSE, eval = params$preview
df <- readr::read_csv(
  "data/shaped2.csv",
  col_types = cols(
    ext_credit_giving = col_double(),
    krw_credit_giving = col_double(),
    trust_politician = col_double(),
    political_pref = col_double(),
    addtax = col_double(),
    avg_welfare_tax = col_double(),
    opt_welfare_tax = col_double(),
    now_balance = col_double(),
    ideal_balance = col_double()
  )
) %>%
dplyr::filter(
  ext_benefit_tl == 0 | (ext_benefit_tl == 1 & i_ext_giving == 1)
)

#'
#' # Price Elasticity of Charitable Giving
#'
#+
fixest::setFixest_fml(
  ..cov = ~ log_pinc_all + sqage | year + panelid + area + industry
)

basemod <- list(
  "(1)" = list(
    mod = log_total_g ~ log_price:ext_benefit_tl + ..cov,
    data = df
  ),
  "(2)" = list(
    mod = log_total_g ~ ..cov | log_price:ext_benefit_tl ~ log_price,
    data = df
  ),
  "(3)" = list(
    mod = log_total_g ~ log_price:ext_benefit_tl + ..cov,
    data = subset(df, i_ext_giving == 1)
  ),
  "(4)" = list(
    mod = log_total_g ~ ..cov | log_price:ext_benefit_tl ~ log_price,
    data = subset(df, i_ext_giving == 1)
  ),
  "(5)" = list(
    mod = i_ext_giving ~ log_price:ext_benefit_tl + ..cov,
    data = df
  ),
  "(6)" = list(
    mod = i_ext_giving ~ ..cov | log_price:ext_benefit_tl ~ log_price,
    data = df
  )
)

basemod %>%
  purrr::map(~ fixest::feols(.$mod, .$data, cluster = ~panelid)) %>%
  modelsummary(
    title = "First-Price Elasticities",
    coef_map = c(
      "fit_log_price:ext_benefit_tl" = "log(first price)",
      "log_price:ext_benefit_tl" = "log(first price)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = tribble(
      ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
      "Square of age", "X", "X", "X", "X", "X", "X"
    )
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    " " = 1, "FE" = 1, "FE-2SLS" = 1,
    "FE" = 1, "FE-2SLS" = 1, "FE" = 1, "FE-2SLS" = 1
  )) %>%
  kableExtra::add_header_above(c(
    " " = 1, "Overall" = 2, "Intensive" = 2, "Extensive" = 2
  )) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at individual level.",
      "logged value of first price is zero",
      "if respondent did not apply for tax relief.",
      "Instrument of logged value of first price is",
      "logged value of first price if they had applied for tax relief."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

# /*
#+
rmarkdown::render(
  "script/R/2-elasticity.r",
  output_dir = "report/view"
)
# */