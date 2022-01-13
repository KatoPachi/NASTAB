#' ---
#' title: |
#'   Estimating Conventional Price Elasticity of Charitable Giving
#' author: Hiroki Kato
#' bibliography: ../../Rmarkdown/reference.bib
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

lapply(Sys.glob(file.path("script/functions", "*.r")), source)

#'
#+ include = FALSE
book <- readr::read_csv("data/codebook/shaped2_description.csv"); View(book)
df <- readr::read_csv("data/shaped2.csv")

#'
#'
#' ## Heterogenous Price Elasticity (1)
#'
#+ CovHeteroElasticity
covdt <- list(
  female = subset(df, sex == 1),
  male = subset(df, sex == 0),
  univ = subset(df, univ == 1),
  highschool = subset(df, highschool == 1),
  junior = subset(df, junior == 1),
  gen_less40 = subset(df, age < 40),
  gen40 = subset(df, 40 <= age & age <= 50),
  gen_over50 = subset(df, 50 < age),
  wage = subset(df, employee == 1),
  nonwage = subset(df, employee == 0)
)

intcov <- covdt %>%
  purrr::map(~ fixest::feols(
    donate_ln ~ linc_ln + sqage | pid + year + indust + area |
      d_relief_donate:lprice_ln ~ price_ln,
    data = subset(., d_donate == 1), cluster = ~ pid
  )) %>%
  modelsummary(
    stars = c("*" = .1, "**" = .05, "***" = .01),
    output = "data.frame"
  ) %>%
  filter(str_detect(term, "fit_d_relief_donate|Num.Obs.")) %>%
  dplyr::select(-term, -statistic) %>%
  dplyr::mutate(part = c("coef", "se", "N")) %>%
  pivot_longer(-part, names_to = "cov", values_to = "intensive") %>%
  pivot_wider(names_from = part, values_from = intensive)

extcov <- covdt %>%
  purrr::map(~ fixest::feols(
    d_donate ~ linc_ln + sqage | pid + year + indust + area |
      d_relief_donate:lprice_ln ~ price_ln,
    data = ., cluster = ~pid
  )) %>%
  purrr::map(function(x) {
    dbar <- mean(x$fitted.values + x$residuals)

    tidy(x) %>%
      filter(str_detect(term, "price")) %>%
      mutate(
        coef = case_when(
          p.value <= .01 ~ sprintf("%1.3f***", estimate / dbar),
          p.value <= .05 ~ sprintf("%1.3f**", estimate / dbar),
          p.value <= .1 ~ sprintf("%1.3f*", estimate / dbar),
          TRUE ~ sprintf("%1.3f", estimate / dbar),
        ),
        se = sprintf("(%1.3f)", std.error / dbar),
        N = sprintf("%1d", glance(x)$nobs)
      ) %>%
      select(coef, se, N) %>%
      pivot_longer(everything())
  }) %>%
  reduce(left_join, by = "name") %>%
  setNames(c("part", names(covdt))) %>%
  pivot_longer(-part, names_to = "cov", values_to = "extensive") %>%
  pivot_wider(names_from = part, values_from = extensive)

intcov %>%
  dplyr::left_join(extcov, by = "cov") %>%
  dplyr::mutate(cov = recode(
    cov,
    "female" = "Female",
    "male" = "Male",
    "univ" = "University graduate",
    "highschool" = "High school graduate",
    "junior" = "Less than junior high school graduate",
    "gen_less40" = "Age < 40",
    "gen40" = "40 $\\le$ Age $\\le$ 50",
    "gen_over50" = "50 < Age",
    "wage" = "Wage earner",
    "nonwage" = "Non wage earner"
  )) %>%
  kable(
    caption = paste(
      "Heterogenous Last-Unit Price Elasticities",
      "in terms of Individual Characteristics"
    ),
    col.names = c(
      "Covariate", "Estimate", "S.E.", "N",
      "Estimate", "S.E.", "N"
    ),
    align = "lcccccc",
    escape = FALSE, booktabs = TRUE, linesep = ""
  ) %>%
  kableExtra::kable_styling(font_size = 8) %>%
  kableExtra::add_header_above(c(
    " " = 1, "Intensive margin" = 3, "Extensive margin" = 3
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
#' ## Heterogenous Price Elasticity (2)
#'
#+ TypeHeteroElasticity
donate_type <- list(
  welfare = "donate_welfare",
  educ = "donate_educ",
  culture = "donate_culture",
  poliparty = "donate_poliparty",
  religious = "donate_religious",
  religious_action = "donate_religious_action",
  other = "donate_others"
)

int_type <- donate_type[c(1, 2, 4, 5, 6, 7)] %>%
  purrr::map(function(x) {
    mod <- paste(
      paste(x, "_ln", sep = ""),
      "~ linc_ln + sqage | pid + year + indust + area |",
      "d_relief_donate:lprice_ln ~ price_ln"
    )

    cond <- paste("d_", x, sep = "")

    fixest::feols(
      formula(mod), data = df[df[, cond] == 1, ], cluster = ~ pid
    )
  }) %>%
  modelsummary(
    stars = c("*" = .1, "**" = .05, "***" = .01),
    output = "data.frame"
  ) %>%
  filter(str_detect(term, "fit_d_relief_donate|Num.Obs.")) %>%
  dplyr::select(-term, -statistic) %>%
  dplyr::mutate(part = c("coef", "se", "N")) %>%
  pivot_longer(-part, names_to = "type", values_to = "intensive") %>%
  pivot_wider(names_from = part, values_from = intensive)

ext_type <- donate_type %>%
  purrr::map(function(x) {
    mod <- paste(
      paste("d_", x, sep = ""),
      "~ linc_ln + sqage | pid + year + indust + area |",
      "d_relief_donate:lprice_ln ~ price_ln"
    )

    fixest::feols(formula(mod), data = df, cluster = ~pid)
  }) %>%
  purrr::map(function(x) {
    dbar <- mean(x$fitted.values + x$residuals)

    tidy(x) %>%
      filter(str_detect(term, "price")) %>%
      mutate(
        coef = case_when(
          p.value <= .01 ~ sprintf("%1.3f***", estimate / dbar),
          p.value <= .05 ~ sprintf("%1.3f**", estimate / dbar),
          p.value <= .1 ~ sprintf("%1.3f*", estimate / dbar),
          TRUE ~ sprintf("%1.3f", estimate / dbar),
        ),
        se = sprintf("(%1.3f)", std.error / dbar),
        N = sprintf("%1d", glance(x)$nobs)
      ) %>%
      select(coef, se, N) %>%
      pivot_longer(everything())
  }) %>%
  reduce(left_join, by = "name") %>%
  setNames(c("part", names(donate_type))) %>%
  pivot_longer(-part, names_to = "type", values_to = "extensive") %>%
  pivot_wider(names_from = part, values_from = extensive)

int_type %>%
  dplyr::right_join(ext_type, by = "type") %>%
  dplyr::mutate(type = recode(
    type,
    "welfare" = "Social welfare",
    "educ" = "Education",
    "culture" = "Culture",
    "poliparty" = "Political party",
    "religious" = "Religious institution",
    "religious_action" = "Relief activities by religious institution",
    "other" = "Others"
  )) %>%
  kable(
    caption = paste(
      "Estimating Last-Unit Price Elasticities for Each Oraganization Type"
    ),
    col.names = c(
      "Type", "Estimate", "S.E.", "N",
      "Estimate", "S.E.", "N"
    ),
    align = "lcccccc",
    booktabs = TRUE, linesep = ""
  ) %>%
  kableExtra::kable_styling(font_size = 8) %>%
  kableExtra::add_header_above(c(
    " " = 1, "Intensive margin" = 3, "Extensive margin" = 3
  )) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at individual level.",
      "We cannot the intensive-margin price elasticity",
      "for donations for culture due to small sample."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#'
#'
# /*
#+
rmarkdown::render(
  "script/elasticity/4-heterogeneity.r",
  output_dir = "report/view/elasticity"
)
# */