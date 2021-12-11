#' ---
#' title: |
#'   Welfare Implication
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
#+ include = FALSE, eval = params$preview
fixest::setFixest_fml(
  ..stage1 = ~ employee +
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
  summarize_all(list(mean = ~ sum(., na.rm = TRUE) / 6))

mundlak <- meandf %>%
  select(-panelid) %>%
  names %>%
  paste(collapse = "+") %>%
  paste("~", ., sep = "") %>%
  formula

fixest::setFixest_fml(..mundlak = mundlak)

#'
#+ eval = params$preview
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
#+ eval = params$preview
fixest::setFixest_fml(
  ..stage2 = ~ log_pinc_all +
    age + sqage + gender + univ + highschool +
    factor(industry) + factor(area)
)

stage2r1 <- estdf %>%
  dplyr::filter(ext_benefit_tl == 1) %>%
  {
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
  purrr::map(~ feols(xpd(.$mod), data = .$data, cluster = ~panelid))

#'
#' # Welfare Implication
#'
#+
apemod <- list(
  "(1)" = fixest::xpd(i_total_giving ~ price + ..stage2),
  "(2)" = fixest::xpd(i_total_giving ~ price + gr_pool + ..stage2),
  "(3)" = fixest::xpd(i_total_giving ~ price + gr_sep + ..stage2)
)

est_apemod0 <- apemod %>%
  purrr::map(~ fixest::feols(
    .,
    data = subset(estdf, ext_benefit_tl == 0 & i_ext_giving == 1),
    cluster = ~panelid, se = "cluster"
  ))

est_apemod0 %>%
  modelsummary(
    title = "Partial Effect of Price (Subsets with $R_{it} = 0$)",
    coef_map = c(
      "price" = "First price",
      "log_pinc_all" = "log(income)",
      "gr_pool" = "Correction term",
      "gr_sep" = "Correction term"
    ),
    gof_omit = "^(?!R2 Adj.|FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = tribble(
      ~term, ~"(1)", ~"(2)", ~"(3)",
      "Square age", "X", "X", "X",
      "Method of IMR", "", "Pooled", "Separate"
    )
  ) %>%
  kableExtra::kable_styling() %>%
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
#+
rawvalue <- function(x) x

elast <- est_stage2r1 %>%
  purrr::map(function(x) {
    tidy(x) %>%
      dplyr::filter(term == "log_price") %>%
      dplyr::select("elast" = estimate)
  }) %>%
  reduce(bind_rows) %>%
  mutate(model = names(est_stage2r1))

partef <- est_apemod0 %>%
  purrr::map(function(x) {
    est <- tidy(x) %>%
      dplyr::filter(term == "price") %>%
      dplyr::select("partef" = estimate)

    n <- glance(x) %>% select(nobs)

    return(bind_cols(est, n))
  }) %>%
  reduce(bind_rows) %>%
  mutate(
    model = names(est_apemod0),
    partef = partef * nobs
  ) %>%
  dplyr::select(-nobs)

elast %>%
  left_join(partef, by = "model") %>%
  mutate(g1 = with(
    subset(estdf, ext_benefit_tl == 1),
    sum(i_total_giving, na.rm = TRUE)
  )) %>%
  mutate(
    devide = partef / g1,
    elast = abs(elast)
  ) %>%
  datasummary(
    model ~ (` ` = rawvalue) * (
      (`Elasticity (R = 1)` = elast) +
        (`Partial Effect (R = 0)` = partef) +
        (`Sum of Giving (R = 1)` = g1) +
        (`Partial Effect (R = 0) / Sum of Giving (R = 1)` = devide)),
    data = .,
    fmt = 3
  ) %>%
  kableExtra::kable_styling() %>%
  footnote(
    general_title = "",
    general = paste(
      "Partial effect for R = 0 is multiplied by observations."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

# /*
#+
rmarkdown::render(
  "script/R/4-welfare.r",
  output_dir = "report/view"
)
# */