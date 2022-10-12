#+ include = FALSE
library(here)
source(here("R", "_library.r"))

#+ include = FALSE
use <- readr::read_csv(
  here("data/shaped2_propensity.csv"),
  guess_max = 30000
)

#+ intensive-r1
fixest::setFixest_fml(
  ..stage2 = ~ linc_ln + sqage + hh_num + have_dependents |
    year + pid + indust + area
)

r1mod <- list(
  "(1)" = fixest::xpd(donate_ln ~ lprice_ln + ..stage2),
  "(2)" = fixest::xpd(
    donate_ln ~ lprice_ln + d(lprice_ln, 1) + d(lprice_ln, -1) +
    d(linc_ln, 1) + d(linc_ln, -1) + ..stage2
  ),
  "(3)" = fixest::xpd(donate_ln ~ ..stage2 | lprice_ln ~ price_ln),
  "(4)" = fixest::xpd(
    donate_ln ~ d(lprice_ln, 1) + d(lprice_ln, -1) +
    d(linc_ln, 1) + d(linc_ln, -1) + ..stage2 | lprice_ln ~ price_ln
  )
)

est_r1mod <- r1mod %>%
  purrr::map(~ fixest::feols(
    ., data = subset(estdf, d_relief_donate == 1),
    panel.id = ~ pid + year, cluster = ~ pid
  ))

addtab <- tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~ "(4)",
  "F-statistics of instrument", "", "",
  sprintf("%1.1f", fitstat(est_r1mod[[3]], "ivwald")[[1]]$stat),
  sprintf("%1.1f", fitstat(est_r1mod[[4]], "ivwald")[[1]]$stat)
)

out.file <- file(here("tables", "intensive-r1.tex"), open = "w")

tab <- est_r1mod %>%
  modelsummary(
    title = paste(
      "Estimating Intensive-Margin Price Elasticities", 
      "for Those Who Applied for Tax Relief",
      "\\label{teb:intensive-r1}"
    ),
    coef_map = c(
      "lprice_ln" = "log(last price)",
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
    add_rows = addtab,
    output = "latex"
  ) %>%
  add_header_above(c(
    "Model:" = 1, "FE" = 2, "FE-2SLS" = 2
  )) %>%
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

writeLines(tab, out.file)
close(out.file)

#+ kdiff-model
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
    ., data = subset(estdf, d_relief_donate == 1),
    cluster = ~ hhid
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

attr(stage1_kdiffmod, "position") <- c(5, 6)

out.file <- file(here("tables", "kdiff-model.tex"), open = "w")

tab <- est_kdiffmod %>%
  modelsummary(
    title = paste(
      "$k$-th Difference Model Using Those Who Applied for Tax Relief",
      "\\label{tab:kdiff-model}"
    ),
    coef_map = c(
      "fit_price_ln_d1" = "Difference of logged first price",
      "linc_ln_d1" = "Difference of logged income",
      "fit_price_ln_d2" = "Difference of logged first price",
      "linc_ln_d2" = "Difference of logged income",
      "fit_price_ln_d3" = "Difference of logged first price",
      "linc_ln_d3" = "Difference of logged income"
    ),
    gof_omit = "^(?!N)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = stage1_kdiffmod,
    output = "latex"
  ) %>%
  # kableExtra::kable_styling(font_size = 8) %>%
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

writeLines(tab, out.file)
close(out.file)

#'
#' 次に、寄付控除を申告した人に限定した分析を行った（**フック欲しい**）。
#'
#' - 寄付控除を申告している人は必ず寄付をしているので、
#' intensive-margin price elasticityのみを推定する
#' - その結果、last priceの価格弾力性は-1.3となり、FE-2SLSより非弾力的になった
#' - 価格のダイナミックな効果を捉えるために、寄付価格と所得のリード変数とラグ変数を加えると、
#' 価格弾力性は統計的に非有意となった。ただし、サンプルサイズが少ないので、この結果はあまり意味がない。
#' - また、所得に対する寄付価格の内生性を考慮した$k$階差分モデルを推定した。
#' その結果、弾力性は-1.9から-4の範囲で得られた。
#'