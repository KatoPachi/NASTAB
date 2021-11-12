#' ---
#' title: |
#'   Price Elasticities Using only Those Who Applied Tax Relief
#' subtitle: Not intended for publication
#' author: Hiroki Kato
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
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
)

#'
#+ include = FALSE, eval = params$preview
fixest::setFixest_fml(
  ..stage21 = ~ log_pinc_all | year + panelid,
  ..stage22 = ~ log_pinc_all + sqage | year + panelid,
  ..stage23 = ~ log_pinc_all + sqage | year + panelid + area,
  ..stage24 = ~ log_pinc_all + sqage | year + panelid + area + industry,
  ..stage1 = ~ log_pinc_all + sqage + factor(area) + factor(industry)
)

poolstage1 <- fixest::feglm(
  fixest::xpd(ext_benefit_tl ~ employee + log_price + ..stage1),
  family = binomial(link = "probit"),
  data = subset(df, year <= 2017)
)

sepstage1 <- df %>%
  dplyr::filter(year <= 2017) %>%
  group_by(year) %>%
  do(sepmod = fixest::feglm(
    fixest::xpd(ext_benefit_tl ~ employee + log_price + ..stage1),
    family = binomial(link = "probit"),
    data = .
  ))

estdf <- df %>%
  dplyr::filter(year <= 2017 & !is.na(ext_benefit_tl)) %>%
  mutate(poolmod = list(poolstage1)) %>%
  left_join(sepstage1, by = "year") %>%
  group_by(year) %>%
  do(modelr::spread_predictions(
    ., first(.$poolmod), first(.$sepmod),
    type = "link"
  )) %>%
  rename(lpred_pool = "first(.$poolmod)", lpred_sep = "first(.$sepmod)") %>%
  mutate(
    # propensity score
    psc2_pool = pnorm(lpred_pool), psc2_sep = pnorm(lpred_sep),

    # inverse mills ratio
    imr_pool = dnorm(lpred_pool) / pnorm(lpred_pool),
    imr_sep = dnorm(lpred_sep) / pnorm(lpred_sep),
  ) %>%
  ungroup() %>%
  select(-poolmod, -sepmod)

#'
#' # Results {#results}
#'
#' ## Elasiticities for Those Who Apply to Tax Relief
#'
#+
subdf <- estdf %>%
  dplyr::filter(ext_benefit_tl == 1 & i_ext_giving == 1)

#'
#' はじめに、ベンチマークとして、寄付申告者に限定した寄付価格の弾力性を推定する。
#' 寄付申告者に限定しているので、推定された弾力性はintensive-marginに関する弾力性を示している。
#'
#+
basemod <- list(
  "(1)" = fixest::xpd(log_total_g ~ log_price + ..stage24),
  "(2)" = fixest::xpd(log_total_g ~ log_price + imr_pool + ..stage24),
  "(3)" = fixest::xpd(log_total_g ~ log_price + imr_sep + ..stage24)
)

basemod %>%
  purrr::map(~ fixest::feols(
    .,
    data = subdf,
    cluster = ~ panelid, se = "cluster"
  )) %>%
  modelsummary(
    title = "First Price Elasiticities for Those Who Apply to Tax Relief",
    coef_rename = c(
      "log_price" = "log(first giving price)",
      "log_pinc_all" = "log(annual taxable income)"
    ),
    coef_omit = "^(?!log)",
    gof_omit = "^(?!R2 Adj.|FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = tribble(
      ~term, ~"(1)", ~"(2)", ~"(3)",
      "Square age", "X", "X", "X"
    )
  )

#'
#' 表\@ref(tab:benchmark)は固定効果モデルの推定結果である。
#' 個人固定効果と時間固定効果を考慮したモデル(1)では、寄付の価格弾力性が-1.45である。
#' 言い換えれば、寄付申告者について、税インセンティブによる価格の1%の減少は寄付を1.45%増やす。
#' また、所得の弾力性は1であるが、これは統計的に非有意である。
#' これらの結果は居住地ダミーや産業ダミーをコントロールしても変化しない。
#'
#+ robustbenchmark1
basemod %>%
  purrr::map(~ fixest::feols(
    .,
    data = subset(subdf, year < 2013 | year > 2014),
    cluster = ~ panelid, se = "cluster"
  )) %>%
  modelsummary(
    title = paste(
      "First Price Elasiticities for Those Who Apply to Tax Relief",
      "(Exclude sample observed in 2013 and 2014)"
    ),
    coef_rename = c(
      "log_price" = "log(first giving price)",
      "log_pinc_all" = "log(annual taxable income)"
    ),
    coef_omit = "^(?!log)",
    gof_omit = "^(?!R2 Adj.|FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = tribble(
      ~term, ~"(1)", ~"(2)", ~"(3)",
      "Square age", "X", "X", "X"
    )
  )

#'
#+ robustbenchmark2
lastmod <- list(
  "(1)" = fixest::xpd(log_total_g ~ ..stage24 | log_lprice ~ log_price),
  "(2)" = fixest::xpd(
    log_total_g ~ imr_pool + ..stage24 | log_lprice ~ log_price
  ),
  "(3)" = fixest::xpd(
    log_total_g ~ imr_sep + ..stage24 | log_lprice ~ log_price
  )
)

lastmod %>%
  purrr::map(~ fixest::feols(
    .,
    data = subdf,
    cluster = ~ panelid, se = "cluster"
  )) %>%
  modelsummary(
    title = paste(
      "Last Price Elasiticities for Those Who Apply to Tax Relief"
    ),
    coef_rename = c(
      "fit_log_lprice" = "log(last giving price)",
      "log_pinc_all" = "log(annual taxable income)"
    ),
    coef_omit = "^(?!log|fit)",
    gof_omit = "^(?!R2 Adj.|FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = tribble(
      ~term, ~"(1)", ~"(2)", ~"(3)",
      "Square age", "X", "X", "X"
    )
  )

#'
#+ robustbenchmark3
fixest::setFixest_fml(
  ..kdiff1 = ~ log_diff1I + diff1_sqage,
  ..kdiff2 = ~ log_diff2I + diff2_sqage,
  ..kdiff3 = ~ log_diff3I + diff3_sqage,
  ..kdifffe = ~ year + area + industry
)

kdiffmod <- list(
  "(1)" = fixest::xpd(
    log_diff1g ~ ..kdiff1 | ..kdifffe | log_diff1p ~ log_iv1price
  ),
  "(2)" = fixest::xpd(
    log_diff2g ~ ..kdiff2 | ..kdifffe | log_diff2p ~ log_iv2price
  ),
  "(3)" = fixest::xpd(
    log_diff3g ~ ..kdiff3 | ..kdifffe | log_diff3p ~ log_iv3price
  )
)

kdiffmod %>%
  purrr::map(~ fixest::feols(
    .,
    cluster = ~ panelid, se = "cluster",
    data = subdf, panel.id = ~ panelid + year
  )) %>%
  modelsummary(
    title = "k-th difference model",
    coef_map = c(
      "fit_log_diff1p" = "1-year lagged difference of first price (log)",
      "log_diff1I" = "1-year lagged difference of annual income (log)",
      "fit_log_diff2p" = "2-year lagged difference of first price (log)",
      "log_diff2I" = "2-year lagged difference of annual income (log)",
      "fit_log_diff3p" = "3-year lagged difference of first price (log)",
      "log_diff3I" = "3-year lagged difference of annual income (log)"
    ),
    gof_omit = "^(?!R2 Adj.|FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = tribble(
      ~term, ~"(1)", ~"(2)", ~"(3)",
      "Difference of square age", "X", "X", "X"
    )
  )

#'
#+ robustbenchmark4
fixest::setFixest_fml(
  ..kdiff1 = ~ log_diff1I + diff1_sqage + d(imr_pool, 1),
  ..kdiff2 = ~ log_diff2I + diff2_sqage + d(imr_pool, 2),
  ..kdiff3 = ~ log_diff3I + diff3_sqage + d(imr_pool, 3),
  ..kdifffe = ~ year + area + industry
)

kdiffmod <- list(
  "(1)" = fixest::xpd(
    log_diff1g ~ ..kdiff1 | ..kdifffe | log_diff1p ~ log_iv1price
  ),
  "(2)" = fixest::xpd(
    log_diff2g ~ ..kdiff2 | ..kdifffe | log_diff2p ~ log_iv2price
  ),
  "(3)" = fixest::xpd(
    log_diff3g ~ ..kdiff3 | ..kdifffe | log_diff3p ~ log_iv3price
  )
)

kdiffmod %>%
  purrr::map(~ fixest::feols(
    .,
    cluster = ~panelid, se = "cluster",
    data = subdf, panel.id = ~ panelid + year
  )) %>%
  modelsummary(
    title = "k-th difference model",
    coef_map = c(
      "fit_log_diff1p" = "1-year lagged difference of first price (log)",
      "log_diff1I" = "1-year lagged difference of annual income (log)",
      "fit_log_diff2p" = "2-year lagged difference of first price (log)",
      "log_diff2I" = "2-year lagged difference of annual income (log)",
      "fit_log_diff3p" = "3-year lagged difference of first price (log)",
      "log_diff3I" = "3-year lagged difference of annual income (log)"
    ),
    gof_omit = "^(?!R2 Adj.|FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = tribble(
      ~term, ~"(1)", ~"(2)", ~"(3)",
      "Difference of square age", "X", "X", "X"
    )
  )

#'
#+ robustbenchmark5
fixest::setFixest_fml(
  ..kdiff1 = ~ log_diff1I + diff1_sqage + d(imr_sep, 1),
  ..kdiff2 = ~ log_diff2I + diff2_sqage + d(imr_sep, 2),
  ..kdiff3 = ~ log_diff3I + diff3_sqage + d(imr_sep, 3),
  ..kdifffe = ~ year + area + industry
)

kdiffmod <- list(
  "(1)" = fixest::xpd(
    log_diff1g ~ ..kdiff1 | ..kdifffe | log_diff1p ~ log_iv1price
  ),
  "(2)" = fixest::xpd(
    log_diff2g ~ ..kdiff2 | ..kdifffe | log_diff2p ~ log_iv2price
  ),
  "(3)" = fixest::xpd(
    log_diff3g ~ ..kdiff3 | ..kdifffe | log_diff3p ~ log_iv3price
  )
)

kdiffmod %>%
  purrr::map(~ fixest::feols(
    .,
    cluster = ~panelid, se = "cluster",
    data = subdf, panel.id = ~ panelid + year
  )) %>%
  modelsummary(
    title = "k-th difference model",
    coef_map = c(
      "fit_log_diff1p" = "1-year lagged difference of first price (log)",
      "log_diff1I" = "1-year lagged difference of annual income (log)",
      "fit_log_diff2p" = "2-year lagged difference of first price (log)",
      "log_diff2I" = "2-year lagged difference of annual income (log)",
      "fit_log_diff3p" = "3-year lagged difference of first price (log)",
      "log_diff3I" = "3-year lagged difference of annual income (log)"
    ),
    gof_omit = "^(?!R2 Adj.|FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = tribble(
      ~term, ~"(1)", ~"(2)", ~"(3)",
      "Difference of square age", "X", "X", "X"
    )
  )

#'
#+
fixest::setFixest_fml(
  ..stage1ll = ~ log_pinc_all + sqage +
    d(log_price, 1) + d(log_price, -1) +
    d(log_pinc_all, 1) + d(log_pinc_all, -1) +
    factor(area) + factor(industry)
)

poolstage1_ll <- fixest::feglm(
  fixest::xpd(ext_benefit_tl ~ employee + log_price + ..stage1ll),
  family = binomial(link = "probit"),
  data = subset(df, year <= 2017),
  panel.id = ~ panelid + year
)

subdf2 <- df %>%
  dplyr::filter(year <= 2017 & !is.na(ext_benefit_tl)) %>%
  modelr::add_predictions(poolstage1_ll, type = "link", var = "lpred_pool") %>%
  mutate(
    # propensity score
    psc_pool = pnorm(lpred_pool),
    # inverse mills ratio
    imr_pool = dnorm(lpred_pool) / pnorm(lpred_pool)
  ) %>%
  dplyr::filter(ext_benefit_tl == 1 & i_ext_giving == 1)

leadlagmod <- list(
  "(1)" = fixest::xpd(
    log_total_g ~ log_price + d(log_price, 1) + d(log_price, -1) +
    d(log_pinc_all, 1) + d(log_pinc_all, -1) + ..stage24
  ),
  "(2)" = fixest::xpd(
    log_total_g ~ log_price + imr_pool +
    d(log_price, 1) + d(log_price, -1) +
    d(log_pinc_all, 1) + d(log_pinc_all, -1) + ..stage24
  )
)

leadlagmod %>%
  purrr::map(~ fixest::feols(
    .,
    data = subdf2, panel.id = ~ panelid + year,
    cluster = ~panelid, se = "cluster"
  )) %>%
  modelsummary(
    title = "First Price Elasiticities for Those Who Apply to Tax Relief",
    coef_rename = c(
      "log_price" = "log(first giving price)",
      "log_pinc_all" = "log(annual taxable income)"
    ),
    coef_omit = "^(?!log)",
    gof_omit = "^(?!R2 Adj.|FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = tribble(
      ~term, ~"(1)", ~"(2)",
      "Square age", "X", "X"
    )
  )

#'
#' この結果の頑健性に関する結果を補論に示した
#' （**Not publication**：パンチラインというか、重要な表はbenchmarkなので、
#' これらはすべて補論に持っていてもいいと思います）。
#' 表\@ref(tab:robustbenchmark1)は税制改革のアナウンス効果を排除するために、
#' 2013年と2014年のデータを除いて弾力性を推定した結果である。
#' 事前に2014年の税制改革を知っているならば、
#' その改革によって寄付の相対価格が高く（安く）なる人は改革前に寄付を増やす（減らす）はずである。
#' したがって、税制改革のアナウンス効果によって、寄付の価格弾力性は過小バイアスを伴う。
#' 結果として、寄付の価格弾力性は約-1.7であり、統計的に有意な結果である。
#' この結果は我々の予想した通りである。
#' また、所得弾力性は約1であるが、これは統計的に非有意な結果である。
#'
#' 表\@ref(tab:robustbenchmark2)はlast priceの価格弾力性の推定結果である。
#' 所得控除制度のもとで、個人が実際に直面する寄付の相対価格はfirst priceではなく、last priceである。
#' したがって、last priceの弾力性が現実的である。
#' しかしながら、寄付額に依存するので、last priceは内生変数である。
#' そこで、寄付額に依存しないfirst priceを操作変数として、panel iv推定をした。
#' 結果として、寄付の価格弾力性は約-1.6であり、統計的に有意である。
#' この弾力性は表\@ref(tab:benchmark)で示したfirst priceの弾力性と非常に近い値を取っているので、
#' first priceの弾力性でも十分に個人の行動を捉えている。
#'
#' 表\@ref(tab:robustbenchmark3)は$k$階差分推定の結果である。
#' 所得控除制度のもとで、個人は所得の操作を通じて寄付の相対価格を変えることができる。
#' したがって、所得の操作に依存するので、寄付額に依存しないfirst priceも内生変数である。
#' そこで、$t-k$年の所得のもとで、$t-k$年と$t$年の寄付のfirst priceを計算し、その差分を説明変数として用いた。
#' このとき、被説明変数は$t-k$年と$t$年の寄付の対数値の差分である。
#' 結果として、$k = 1$や$k = 2$のとき、寄付の価格弾力性は約-1であり、
#' これは統計的に非有意である。
#' しかしながら、$k = 3$のとき、寄付の価格弾力性は約-1.6であり、
#' これは統計的に10%水準である。
#'
# /*
#+
rmarkdown::render(
  "script/R/2-elasticity.r",
  output_dir = "report/view"
)
# */