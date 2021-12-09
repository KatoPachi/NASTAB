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
  ..stage1 = ~ employee +
    log_price + log_pinc_all +
    age + sqage + gender + univ + highschool +
    factor(industry) + factor(area)
)

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
  summarize_all(list(mean = ~ sum(., na.rm = TRUE) / 6))

mundlak <- meandf %>%
  select(-panelid) %>%
  names %>%
  paste(collapse = "+") %>%
  paste("~", ., sep = "") %>%
  formula

fixest::setFixest_fml(..mundlak = mundlak)

est_stage1_pool <- df %>%
  left_join(meandf, by = "panelid") %>%
  dplyr::filter(year <= 2017) %>%
  fixest::feglm(
    fixest::xpd(
      ext_benefit_tl ~ ..stage1 + ..mundlak - univ - highschool | year
    ),
    family = binomial(link = "probit"),
    data = .
  )

est_stage1_sep <- df %>%
  left_join(meandf, by = "panelid") %>%
  dplyr::filter(year <= 2017) %>%
  group_by(year) %>%
  do(sepmod = fixest::feglm(
    fixest::xpd(ext_benefit_tl ~ ..stage1 + ..mundlak - univ - highschool),
    family = binomial(link = "probit"),
    data = .
  ))

est_stage1_sep %>%
  pull(sepmod, name = year) %>%
  list.merge(list("Pooled" = est_stage1_pool), .) %>%
  modelsummary(
    title = "Probit Estimation of Selection of Applying for Tax Relief",
    coef_omit = "factor",
    coef_map = c(
      "employee" = "1 = Wage earner",
      "log_price" = "log(first price)",
      "log_pinc_all" = "log(income)",
      "age" = "Age",
      "sqage" = "Square of age",
      "gender" = "1 = female",
      "univ_mean" = "1 = University graduate",
      "highschool_mean" = "1 = Highschool graduate"
    ),
    gof_omit = "R2|AIC|BIC",
    stars = c("***" = .01, "**" = .05, "*" = .1)
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
  left_join(meandf, by = "panelid") %>%
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
#' ## Estimating Conventional Price Elasticity
#'
#+
fixest::setFixest_fml(
  ..stage2 = ~ + log_pinc_all +
    age + sqage + gender + univ + highschool +
    factor(industry) + factor(area)
)

cfmod <- list(
  "(1)" = list(
    mod = log_total_g ~ log_price:ext_benefit_tl +
      ..stage2 + ..mundlak | year,
    data = estdf
  ),
  "(2)" = list(
    mod = log_total_g ~ log_price:ext_benefit_tl +
      ..stage2 + gr_pool + ..mundlak | year,
    data = estdf
  ),
  "(3)" = list(
    mod = log_total_g ~ log_price:ext_benefit_tl +
      ..stage2 + gr_sep + ..mundlak | year,
    data = estdf
  ),
  "(4)" = list(
    mod = log_total_g ~ log_price:ext_benefit_tl +
      ..stage2 + ..mundlak | year,
    data = subset(estdf, i_ext_giving == 1)
  ),
  "(5)" = list(
    mod = log_total_g ~ log_price:ext_benefit_tl +
      ..stage2 + gr_pool + ..mundlak | year,
    data = subset(estdf, i_ext_giving == 1)
  ),
  "(6)" = list(
    mod = log_total_g ~ log_price:ext_benefit_tl +
      ..stage2 + gr_sep + ..mundlak | year,
    data = subset(estdf, i_ext_giving == 1)
  ),
  "(7)" = list(
    mod = i_ext_giving ~ log_price:ext_benefit_tl +
      ..stage2 + ..mundlak | year,
    data = estdf
  ),
  "(8)" = list(
    mod = i_ext_giving ~ log_price:ext_benefit_tl +
      ..stage2 + gr_pool + ..mundlak | year,
    data = estdf
  ),
  "(9)" = list(
    mod = i_ext_giving ~ log_price:ext_benefit_tl +
      ..stage2 + gr_sep + ..mundlak | year,
    data = estdf
  )
)

est_cfmod <- cfmod %>%
  purrr::map(~ fixest::feols(xpd(.$mod), .$data, cluster = ~panelid))

addtab <- est_cfmod[7:9] %>%
  purrr::map(function(x) {
    dbar <- mean(x$fitted.values + x$residuals)

    tidy(x) %>%
      filter(str_detect(term, "log_price:ext_benefit_tl")) %>%
      mutate(
        estimate = case_when(
          p.value <= .01 ~ sprintf("%1.3f***", estimate / dbar),
          p.value <= .05 ~ sprintf("%1.3f**", estimate / dbar),
          p.value <= .1 ~ sprintf("%1.3f*", estimate / dbar),
          TRUE ~ sprintf("%1.3f", estimate / dbar),
        ),
        std.error = sprintf("(%1.3f)", std.error / dbar)
      ) %>%
      select(estimate, std.error) %>%
      pivot_longer(everything())
  }) %>%
  reduce(left_join, by = "name") %>%
  setNames(c("term", sprintf("(%1d)", 7:9))) %>%
  left_join(tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
    "estimate", "", "", "", "", "", "",
    "std.error", "", "", "", "", "", ""
  ), ., by = "term") %>%
  mutate(term = recode(
    term,
    "estimate" = "Implied price elasticity", .default = ""
  ))

attr(addtab, "position") <- c(7:8)

est_cfmod %>%
  modelsummary(
    title = "First-Price Elasticities",
    coef_map = c(
      "log_price:ext_benefit_tl" = "Apply tax relief x log(first price)",
      "gr_pool" = "Selection crrection term (Pool)",
      "gr_sep" = "Selection crrection term (Separate)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    " " = 1, "Overall" = 3, "Intensive" = 3, "Extensive" = 3
  )) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at individual level."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#'
#+ eval = FALSE
fixest::setFixest_fml(
  ..stage2 = ~ + log_pinc_all +
    age + sqage + gender + univ + highschool +
    factor(industry) + factor(area)
)

stage2r1 <- estdf %>%
  dplyr::filter(ext_benefit_tl == 1) %>% {
    list(
      "(1)" = list(
        mod = log_total_g ~ ..stage2 + ..mundlak - univ - highschool,
        data = .
      ),
      "(2)" = list(
        mod = log_total_g ~ ..stage2 + ..mundlak - univ - highschool + gr,
        data = .
      ),
      "(3)" = list(
        mod = log_total_g ~ ..stage2 + ..mundlak - univ - highschool,
        data = subset(., i_ext_giving == 1)
      ),
      "(4)" = list(
        mod = log_total_g ~ ..stage2 + ..mundlak - univ - highschool + gr,
        data = subset(., i_ext_giving == 1)
      )
    )
  }


est_stage2r1 <- stage2r1 %>%
  purrr::map(~ feols(xpd(.$mod), data = .$data, cluster = ~ panelid))

modelsummary(est_stage2r1, stars = TRUE)


# /*
#+
rmarkdown::render(
  "script/R/3-cf_approach.r",
  output_dir = "report/view"
)
# */