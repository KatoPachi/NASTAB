#' ---
#' title: |
#'   First-Stage Result: Who Applied for Tax Relief?
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
book <- readr::read_csv("data/codebook/shaped2_description.csv")
View(book)
df <- readr::read_csv("data/shaped2.csv")

#'
#+
fixest::setFixest_fml(
  ..stage1 = ~ employee + price_ln + linc_ln +
    sqage + factor(indust) + factor(area) #+
#    tax_accountant_per + gender + univ + highschool
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

df <- df %>% left_join(meandf, by = "panelid")

#'
#+ stage1, results = "hide"
est_stage1_pool <- df %>%
  dplyr::filter(year <= 2017) %>%
  fixest::feglm(
    fixest::xpd(d_relief_donate ~ ..stage1 | year),
    family = binomial(link = "probit"),
    data = .
  )

est_stage1_sep <- df %>%
  dplyr::filter(year <= 2017) %>%
  group_by(year) %>%
  do(sepmod = fixest::feglm(
    fixest::xpd(d_relief_donate ~ ..stage1),
    family = binomial(link = "probit"),
    data = .
  ))

est_stage1_sep %>%
  pull(sepmod, name = year) %>%
  list.merge(list("Pooled" = est_stage1_pool), .) %>%
  modelsummary(
    # title = "Probit Estimation of Selection of Applying for Tax Relief",
    coef_map = c(
      "employee" = "Wage earner",
      "tax_accountant_per" = "\\# Tax accountant",
      "employee:tax_accountant_per" = "Wage earner x \\# Tax accountant",
      "price_ln" = "log(first price)",
      "linc_ln" = "log(income)",
      "age" = "Age",
      "sqage" = "Square of age",
      "gender" = "female",
      "univ" = "University graduate",
      "highschool" = "Highschool graduate"
    ),
    gof_omit = "R2|AIC|BIC",
    stars = c("***" = .01, "**" = .05, "*" = .1)
    # add_rows = tribble(
    #   ~term, ~Pooled, ~"2012", ~"2013", ~"2014", ~"2015", ~"2016", ~"2017",
    #   "Area dummies", "X", "X", "X", "X", "X", "X", "X",
    #   "Industry dummies", "X", "X", "X", "X", "X", "X", "X",
    # )
  ) %>%
  kableExtra::kable_styling(font_size = 6) %>%
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
#+ calc_propensity
estdf <- df %>%
  dplyr::filter(year <= 2017 & !is.na(d_relief_donate)) %>%
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
    gr_pool = if_else(d_relief_donate == 1, imr1_pool, imr0_pool),
    gr_sep = if_else(d_relief_donate == 1, imr1_sep, imr0_sep)
  ) %>%
  ungroup() %>%
  select(-poolmod, -sepmod)

readr::write_csv(estdf, "data/shaped2_propensity.csv")

# /*
#+
rmarkdown::render(
  "script/R/2-stage1.r",
  output_dir = "report/view"
)
# */