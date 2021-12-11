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
#' Alternative approach to evaluate tax incentive is
#' the control function approach.
#' To clarify the idea of this approach,
#' we reformulate the second-stage equation as follows:
#'
#' \begin{align*}
#'   \ln g_{it}
#'   &= \theta_i + \gamma \ln(1 - R_{it}s_{it})
#'   + \beta X'_{it} + \iota_t + u_{it} \\
#'   &= \theta_i + \gamma (R_{it} \times \ln(1 - s_{it}))
#'   + \beta X'_{it} + \iota_t + u_{it}.
#' \end{align*}
#'
#' Next, we define the selection equation of tax relief as follows:
#'
#' \begin{align*}
#'   R_{it} = 1[\theta_{i1} + \delta_1 Z'_{it} + \delta_2 \ln(1 - s_{it})
#'   + \beta_1 X'_{it} + \iota_{t1} + v_{it} > 0].
#' \end{align*}
#'
#' where a vector $Z_{it}$ is instuments manipulating applying cost $K_{it}$.
#'
#' If we have cross sectional data,
#'
#' ## First-Stage Result: Who Applied for Tax Relief?
#'
#+
fixest::setFixest_fml(
  ..stage1 = ~ employee + tax_accountant_per +
    log_price + log_pinc_all +
    age + sqage + gender + univ + highschool +
    factor(industry) + factor(area)
)

#'
#+ eval = FALSE
meandf <- df %>%
  dplyr::select(all.vars(fixest::xpd(~ ..stage1)), panelid) %>%
  fastDummies::dummy_cols(
    select_columns = "industry",
    remove_selected_columns = TRUE
  ) %>%
  fastDummies::dummy_cols(
    select_columns = "area",
    remove_selected_columns = TRUE
  ) %>%
  select(-"industry_-9", -industry_NA) %>%
  group_by(panelid) %>%
  summarize_all(list(mean = ~ mean(., na.rm = TRUE)))

mundlak <- meandf %>%
  select(-panelid) %>%
  names %>%
  paste(collapse = "+") %>%
  paste("~", ., sep = "") %>%
  formula

fixest::setFixest_fml(..mundlak = mundlak)

#'
#+
est_stage1_pool <- df %>%
  # left_join(meandf, by = "panelid") %>%
  dplyr::filter(year <= 2017) %>%
  fixest::feglm(
    fixest::xpd(ext_benefit_tl ~ ..stage1 | year),
    family = binomial(link = "probit"),
    data = .
  )

est_stage1_sep <- df %>%
  # left_join(meandf, by = "panelid") %>%
  dplyr::filter(year <= 2017) %>%
  group_by(year) %>%
  do(sepmod = fixest::feglm(
    fixest::xpd(ext_benefit_tl ~ ..stage1),
    family = binomial(link = "probit"),
    data = .
  ))

est_stage1_sep %>%
  pull(sepmod, name = year) %>%
  list.merge(list("Pooled" = est_stage1_pool), .) %>%
  modelsummary(
    title = "Probit Estimation of Selection of Applying for Tax Relief",
    coef_omit = "factor",
    # coef_map = c(
    #   "employee" = "1 = Wage earner",
    #   "log_price" = "log(first price)",
    #   "log_pinc_all" = "log(income)",
    #   "age" = "Age",
    #   "sqage" = "Square of age",
    #   "gender" = "1 = female",
    #   "univ" = "1 = University graduate",
    #   "highschool" = "1 = Highschool graduate"
    # ),
    gof_omit = "R2|AIC|BIC",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    # add_rows = tribble(
    #   ~term, ~Pooled, ~"2012", ~"2013", ~"2014", ~"2015", ~"2016", ~"2017",
    #   "Area dummies", "X", "X", "X", "X", "X", "X", "X",
    #   "Industry dummies", "X", "X", "X", "X", "X", "X", "X",
    # )
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    " " = 2, "Separated Probit Model" = 6
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
#+
estdf <- df %>%
  # left_join(meandf, by = "panelid") %>%
  dplyr::filter(year <= 2017 & !is.na(ext_benefit_tl)) %>%
  mutate(poolmod = list(est_stage1_pool)) %>%
  left_join(est_stage1_sep, by = "year") %>%
  group_by(year) %>%
  do(modelr::spread_predictions(
    ., first(.$poolmod), first(.$sepmod),
    type = "link"
  )) %>%
  rename(lpred_pool = "first(.$poolmod)", lpred_sep = "first(.$sepmod)") %>%
  mutate(
    # propensity score
    psc_pool = pnorm(lpred_pool), psc_sep = pnorm(lpred_sep),

    # inverse mills ratio for R = 1
    imr1_pool = dnorm(lpred_pool) / pnorm(lpred_pool),
    imr1_sep = dnorm(lpred_sep) / pnorm(lpred_sep),

    # inverse mills ratio for R = 0
    imr0_pool = dnorm(lpred_pool) / (1 - pnorm(lpred_pool)),
    imr0_sep = dnorm(lpred_sep) / (1 - pnorm(lpred_sep)),

    # generalized error
    gr_pool = if_else(ext_benefit_tl == 1, imr1_pool, imr0_pool),
    gr_sep = if_else(ext_benefit_tl == 1, imr1_sep, imr0_sep)
  ) %>%
  ungroup() %>%
  select(-poolmod, -sepmod)

#'
#' ## Second-Stage Result
#'
#+
fixest::setFixest_fml(
  ..stage2 = ~ log_pinc_all +
    age + sqage + gender + univ + highschool +
    factor(industry) + factor(area)
)

stage2r1 <- estdf %>%
  dplyr::filter(ext_benefit_tl == 1) %>% {
    list(
      "(1)" = list(
        mod = log_total_g ~ log_price + ..stage2 | panelid + year,
        data = .
      ),
      "(2)" = list(
        mod = log_total_g ~ log_price + ..stage2 + gr_sep | panelid + year,
        data = .
      ),
      "(3)" = list(
        mod = log_total_g ~ log_price + ..stage2 + gr_pool | panelid + year,
        data = .
      )
    )
  }

est_stage2r1 <- stage2r1 %>%
  purrr::map(~ feols(xpd(.$mod), data = .$data, cluster = ~ panelid))

est_stage2r1 %>%
  modelsummary(
    title = "Estimation of Outcome Equation for $R_{it} = 1$",
    # coef_omit = "factor",
    # coef_map = c(
    #   "log_price" = "log(first price)",
    #   "log_pinc_all" = "log(income)",
    #   "gr_sep" = "Selection correction term (separate)",
    #   "gr_pool" = "Selection correction term (pool)"
    # ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std",
    stars = c("***" = .01, "**" = .05, "*" = .1)
    # add_rows = tribble(
    #   ~term, ~"(1)", ~"(2)", ~"(3)",
    #   "Area dummies", "X", "X", "X",
    #   "Industry dummies", "X", "X", "X",
    #   "Square of Age", "X", "X", "X"
    # )
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
fixest::setFixest_fml(
  ..stage2 = ~ log_pinc_all +
    age + sqage + gender + univ + highschool +
    factor(industry) + factor(area)
)

stage2r0 <- estdf %>%
  dplyr::filter(ext_benefit_tl == 0) %>% {
    list(
      "(1)" = list(
        mod = log_total_g ~ ..stage2 | panelid + year,
        data = .
      ),
      "(2)" = list(
        mod = log_total_g ~ ..stage2 + gr_sep | panelid + year,
        data = .
      ),
      "(3)" = list(
        mod = log_total_g ~ ..stage2 + gr_pool | panelid + year,
        data = .
      ),
      "(4)" = list(
        mod = log_total_g ~ ..stage2 | panelid + year,
        data = subset(., i_ext_giving == 1)
      ),
      "(5)" = list(
        mod = log_total_g ~ ..stage2 + gr_sep | panelid + year,
        data = subset(., i_ext_giving == 1)
      ),
      "(6)" = list(
        mod = log_total_g ~ ..stage2 + gr_pool | panelid + year,
        data = subset(., i_ext_giving == 1)
      ),
      "(7)" = list(
        mod = i_ext_giving ~ ..stage2 | panelid + year,
        data = .
      ),
      "(8)" = list(
        mod = i_ext_giving ~ ..stage2 + gr_sep | panelid + year,
        data = .
      ),
      "(9)" = list(
        mod = i_ext_giving ~ ..stage2 + gr_pool | panelid + year,
        data = .
      )
    )
  }

est_stage2r0 <- stage2r0 %>%
  purrr::map(~ feols(xpd(.$mod), data = .$data, cluster = ~ panelid))

est_stage2r0 %>%
  modelsummary(
    title = "Estimation of Outcome Equation for $R_{it} = 0$",
    # coef_omit = "factor",
    # coef_map = c(
    #   "log_pinc_all" = "log(income)",
    #   "gr_sep" = "Selection correction term (separate)",
    #   "gr_pool" = "Selection correction term (pool)"
    # ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std",
    stars = c("***" = .01, "**" = .05, "*" = .1)
    # add_rows = tribble(
    #   ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
    #   ~"(7)", ~"(8)", ~"(9)",
    #   "Area dummies", "X", "X", "X", "X", "X", "X", "X", "X", "X",
    #   "Industry dummies", "X", "X", "X", "X", "X", "X", "X", "X", "X",
    #   "Square of Age", "X", "X", "X", "X", "X", "X", "X", "X", "X"
    # )
  ) %>%
  kableExtra::add_header_above(c(
    " " = 1, "Overall" = 3, "Intensive" = 3, "Extensive" = 3
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
pred07 <- est_stage2r0[[7]]
pred08 <- est_stage2r0[[8]]
pred09 <- est_stage2r0[[9]]

preddf <- estdf %>%
  modelr::spread_predictions(
    pred11, pred12, pred13,
    pred01, pred02, pred03,
    pred04, pred05, pred06,
    pred07, pred08, pred09
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
        overall_1 = mean(pred11 - pred01, na.rm = TRUE),
        overall_2 = mean(pred12 - pred02, na.rm = TRUE),
        overall_3 = mean(pred13 - pred03, na.rm = TRUE),
        intensive_1 = mean(pred11 - pred04, na.rm = TRUE),
        intensive_2 = mean(pred12 - pred05, na.rm = TRUE),
        intensive_3 = mean(pred13 - pred06, na.rm = TRUE),
        extensive_1 = mean(1 - pred07, na.rm = TRUE),
        extensive_2 = mean(1 - pred07, na.rm = TRUE),
        extensive_3 = mean(1 - pred09, na.rm = TRUE)
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
  "script/R/3-cf_approach.r",
  output_dir = "report/view"
)
# */