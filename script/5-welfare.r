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
#+ include = FALSE, eval = params$preview
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
    cluster = ~panelid
  ))

#'
#' ## Partial Effect of Price (Subsets with $R_{it} = 0$)
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
    # title = "Partial Effect of Price (Subsets with $R_{it} = 0$)",
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
  kableExtra::kable_styling(font_size = 7) %>%
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
#' ## Improve Welfare by Increasing Tax Incentive
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
  kableExtra::kable_styling(font_size = 6) %>%
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
  "script/R/5-welfare.r",
  output_dir = "report/view"
)
# */