#' ---
#' title: |
#'   Control Function Approach
#' author: Hiroki Kato
#' output:
#'   bookdown::html_document2:
#'     toc: true
#'     toc_float: true
#'     number_sections: false
#' bibliography: ../../Rmarkdown/ref_main.bib
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
  "data/shaped2_propensity.csv",
  col_types = cols(
    ext_credit_giving = col_double(),
    krw_credit_giving = col_double(),
    trust_politician = col_double(),
    political_pref = col_double(),
    addtax = col_double(),
    avg_welfare_tax = col_double(),
    opt_welfare_tax = col_double(),
    now_balance = col_double(),
    ideal_balance = col_double(),
    accountant = col_double(),
    consult = col_double()
  )
) %>%
dplyr::filter(
  ext_benefit_tl == 0 | (ext_benefit_tl == 1 & i_ext_giving == 1)
)

#'
#' # Control Function Approach
#'
#'
#+
fixest::setFixest_fml(
  ..stage2 = ~ log_pinc_all + sqage | panelid + year + industry + area
)

stage2r1 <- list(
  "(1)" = log_total_g ~ log_price + ..stage2,
  "(2)" = log_total_g ~ log_price + gr_sep + ..stage2,
  "(3)" = log_total_g ~ log_price + gr_pool + ..stage2
)

est_stage2r1 <- stage2r1 %>%
  purrr::map(~ feols(
    xpd(.),
    data = subset(df, ext_benefit_tl == 1),
    cluster = ~ panelid
  ))

est_stage2r1 %>%
  modelsummary(
    title = "Estimation of Outcome Equation for $R_{it} = 1$",
    coef_map = c(
      "log_price" = "log(first price)",
      "log_pinc_all" = "log(income)",
      "gr_sep" = "Selection correction term (separate)",
      "gr_pool" = "Selection correction term (pool)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = tribble(
      ~term, ~"(1)", ~"(2)", ~"(3)",
      "Square of Age", "X", "X", "X"
    )
  ) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#'
#+
stage2r0 <- estdf %>%
  dplyr::filter(ext_benefit_tl == 0) %>% {
    list(
      "(1)" = list(
        mod = log_total_g ~ ..stage2,
        data = subset(., i_ext_giving == 1)
      ),
      "(2)" = list(
        mod = log_total_g ~ gr_sep + ..stage2,
        data = subset(., i_ext_giving == 1)
      ),
      "(3)" = list(
        mod = log_total_g ~ gr_pool + ..stage2,
        data = subset(., i_ext_giving == 1)
      ),
      "(4)" = list(
        mod = i_ext_giving ~ ..stage2,
        data = .
      ),
      "(5)" = list(
        mod = i_ext_giving ~ gr_sep + ..stage2,
        data = .
      ),
      "(6)" = list(
        mod = i_ext_giving ~ gr_pool + ..stage2,
        data = .
      )
    )
  }

est_stage2r0 <- stage2r0 %>%
  purrr::map(~ feols(xpd(.$mod), data = .$data, cluster = ~ panelid))

est_stage2r0 %>%
  modelsummary(
    title = "Estimation of Outcome Equation for $R_{it} = 0$",
    coef_map = c(
      "log_pinc_all" = "log(income)",
      "gr_sep" = "Selection correction term (separate)",
      "gr_pool" = "Selection correction term (pool)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = tribble(
      ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
      "Square of Age", "X", "X", "X", "X", "X", "X"
    )
  ) %>%
  kableExtra::add_header_above(c(
    " " = 1, "Intensive" = 3, "Extensive" = 3
  )) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#'
#' ## Estimating Effect of Tax Incentive
#'
#+
pred11 <- est_stage2r1[[1]]
pred12 <- est_stage2r1[[2]]
pred13 <- est_stage2r1[[3]]

pred01 <- est_stage2r0[[1]]
pred02 <- est_stage2r0[[2]]
pred03 <- est_stage2r0[[3]]
pred04 <- est_stage2r0[[4]]
pred05 <- est_stage2r0[[5]]
pred06 <- est_stage2r0[[6]]

preddf <- estdf %>%
  modelr::spread_predictions(
    pred11, pred12, pred13,
    pred01, pred02, pred03,
    pred04, pred05, pred06
  )

set_preddf <- list(
  ate = preddf,
  att = subset(preddf, ext_benefit_tl == 1),
  att = subset(preddf, ext_benefit_tl == 0)
)

set_preddf %>%
  purrr::map(function(x) {
    x %>%
      summarize(
        intensive_1 = mean(pred11 - pred01, na.rm = TRUE),
        intensive_2 = mean(pred12 - pred02, na.rm = TRUE),
        intensive_3 = mean(pred13 - pred03, na.rm = TRUE),
        extensive_1 = mean(1 - pred04, na.rm = TRUE),
        extensive_2 = mean(1 - pred05, na.rm = TRUE),
        extensive_3 = mean(1 - pred06, na.rm = TRUE)
      )
  }) %>%
  reduce(bind_rows) %>%
  mutate(estimand = c("ATE", "ATT", "ATU")) %>%
  pivot_longer(
    -estimand,
    names_to = c("outcome", "correct"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  mutate(correct = dplyr::recode(
    correct,
    "1" = "No",
    "2" = "Separate",
    "3" = "Pool"
  )) %>%
  datasummary(
    (`Outcome` = outcome) *
      (`Include correction term?` = correct) ~
      value * (` ` = mean) * estimand,
    data = .,
    fmt = 3
  )


# /*
#+
rmarkdown::render(
  "script/R/4-cf_approach.r",
  output_dir = "report/view"
)
# */