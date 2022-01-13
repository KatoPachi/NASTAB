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
#' ## Estimating Price Elasticity Using Compliers
#'
#+ R1Elasticity
fixest::setFixest_fml(
  ..cov = ~ linc_ln + sqage | year + pid + indust + area
)

r1mod <- list(
  "(1)" = fixest::xpd(donate_ln ~ price_ln + ..cov),
  "(2)" = fixest::xpd(
    donate_ln ~ price_ln + d(price_ln, 1) + d(price_ln, -1) +
    d(linc_ln, 1) + d(linc_ln, -1) + ..cov
  ),
  "(3)" = fixest::xpd(donate_ln ~ ..cov | lprice_ln ~ price_ln),
  "(4)" = fixest::xpd(
    donate_ln ~ d(lprice_ln, 1) + d(lprice_ln, -1) +
    d(linc_ln, 1) + d(linc_ln, -1) + ..cov | lprice_ln ~ price_ln
  )
)

est_r1mod <- r1mod %>%
  purrr::map(~ fixest::feols(
    ., data = subset(df, d_relief_donate == 1),
    panel.id = ~ pid + year, cluster = ~ pid
  ))

addtab <- tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~ "(4)",
  "Instrument: log(first price)", "", "",
  sprintf("%1.3f", est_r1mod[[3]]$iv_first_stage$lprice_ln$coeftable[1, 1]),
  sprintf("%1.3f", est_r1mod[[4]]$iv_first_stage$lprice_ln$coeftable[1, 1]),
  "", "", "",
  sprintf("[%1.1f]", fitstat(est_r1mod[[3]], "ivwald")[[1]]$stat),
  sprintf("[%1.1f]", fitstat(est_r1mod[[4]], "ivwald")[[1]]$stat)#,
  # "Square of age", "X", "X", "X", "X"
)

attr(addtab, "position") <- c(13, 14)

est_r1mod %>%
  modelsummary(
    title = paste(
      "Estimating Intensive-Margin Price Elasticities", 
      "for Those Who Applied for Tax Relief"
    ),
    coef_map = c(
      "price_ln" = "log(first price)",
      "fit_lprice_ln" = "log(last price)",
      "linc_ln" = "log(income)",
      "d(price_ln, 1)" = "1-year lag of price",
      "d(price_ln, -1)" = "1-year lead of price",
      "d(lprice_ln, 1)" = "1-year lag of price",
      "d(lprice_ln, -1)" = "1-year lead of price",
      "d(linc_ln, 1)" = "1-year lag of income",
      "d(linc_ln, -1)" = "1-year lead of income"
    ),
    gof_omit = "^(?!N)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling(font_size = 5) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at individual level.",
      "1-year lead of price cannot be estimated because of collinearity."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#'
#' ## $k$-th Difference Model
#'
#+ KdiffElasticity
fixest::setFixest_fml(
  ..kdiff1 = ~ linc_ln_d1 + sqage_d1,
  ..kdiff2 = ~ linc_ln_d2 + sqage_d2,
  ..kdiff3 = ~ linc_ln_d3 + sqage_d3,
  ..kdifffe = ~ year + area + indust
)

kdiffmod <- list(
  "(1)" = fixest::xpd(
    donate_ln_d1 ~ ..kdiff1 | ..kdifffe | price_ln_d1 ~ log(price_iv1)
  ),
  "(2)" = fixest::xpd(
    donate_ln_d2 ~ ..kdiff2 | ..kdifffe | price_ln_d2 ~ log(price_iv2)
  ),
  "(3)" = fixest::xpd(
    donate_ln_d3 ~ ..kdiff3 | ..kdifffe | price_ln_d3 ~ log(price_iv3)
  )
)

est_kdiffmod <- kdiffmod %>%
  purrr::map(~fixest::feols(
    ., data = subset(df, d_relief_donate == 1),
    cluster = ~pid
  ))

stage1_kdiffmod <- 1:3 %>%
  purrr::map(function(i) {
    x <- est_kdiffmod[[i]]
    coef <- x$iv_first_stage[[paste("price_ln_d", i, sep = "")]]$coeftable[1, 1]
    ivwald <- fitstat(x, "ivwald")[[1]]$stat

    tibble(coef = coef, wald = ivwald) %>%
      pivot_longer(everything()) %>%
      mutate(value = case_when(
        name == "coef" ~ sprintf("%1.3f", value),
        name == "wald" ~ sprintf("[%1.1f]", value)
      ))
  }) %>%
  reduce(left_join, by = "name") %>%
  setNames(c("term", sprintf("(%1d)", 1:3))) %>%
  mutate(term = recode(
    term,
    "coef" = "First-stage: Instrument", .default = ""
  ))

addtab <- stage1_kdiffmod #%>%
  # bind_rows(tribble(
  #   ~term, ~"(1)", ~"(2)", ~"(3)",
  #   "Difference of square age", "X", "X", "X"
  # ))

attr(addtab, "position") <- c(3, 4)

est_kdiffmod %>%
  modelsummary(
    title = paste(
      "$k$-th Difference Model Using Those Who Applied for Tax Relief"
    ),
    coef_map = c(
      "fit_price_ln_d1" = "Difference of logged first price",
      # "linc_ln_d1" = "Difference of logged income",
      "fit_price_ln_d2" = "Difference of logged first price",
      # "linc_ln_d2" = "Difference of logged income",
      "fit_price_ln_d3" = "Difference of logged first price"#,
      # "linc_ln_d3" = "Difference of logged income"
    ),
    gof_omit = "^(?!FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling(font_size = 8) %>%
  kableExtra::add_header_above(c(
    " " = 1, "1-year lag" = 1,
    "2-year lag" = 1, "3-year lag" = 1
  )) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at individual level.",
      "Instrument is difference between lagged first price in year $t$",
      "and in year $t - k$ fixing income in year $t - k$."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#'
#'
# /*
#+
rmarkdown::render(
  "script/elasticity/3-subsample_R1.r",
  output_dir = "report/view/elasticity"
)
# */