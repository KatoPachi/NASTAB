#+ include = FALSE
library(here)
source(here("R", "_library.r"))

#+ include = FALSE
rawdt <- readr::read_csv(
  here("data/shaped2_propensity.csv"),
  guess_max = 30000
)

use <- rawdt %>%
  select(
    pid,
    hhid,
    year,
    linc_ln,
    sqage,
    hh_num,
    have_dependents,
    indust,
    area,
    price_ln,
    lprice_ln,
    d_relief_donate,
    employee,
    outcome_intensive = donate_ln,
    outcome_extensive = d_donate
  ) %>%
  mutate(
    flag_extensive = 1,
    flag_intensive = if_else(outcome_extensive == 1, 1, 0)
  ) %>%
  pivot_longer(
    outcome_intensive:flag_intensive,
    names_to = c(".value", "type"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  mutate(
    effective = d_relief_donate * lprice_ln,
    applicable = lprice_ln
  )

#+ include = FALSE
fixest::setFixest_fml(
  ..stage2 = ~ linc_ln + sqage + hh_num + have_dependents |
    year + pid + indust + area
)

#'
#+ fe-model
femod <- list(
  outcome ~ applicable + ..stage2,
  outcome ~ effective + ..stage2
)

est_femod <- use %>%
  mutate(type = factor(type, levels = c("intensive", "extensive"))) %>%
  group_by(type) %>%
  do(est = lapply(
    femod,
    function(x) feols(x, data = subset(., flag == 1), cluster = ~ hhid)
  ))

implied_e <- est_femod %>%
  dplyr::filter(type == "extensive") %>%
  pull(est) %>%
  flatten() %>%
  lapply(function(x) tribble(
    ~term, ~a,
    "Implied price elasticity",
    sprintf("%1.3f", coef(x)[1] / mean(rawdt$d_donate, na.rm = TRUE)),
    "",
    sprintf("(%1.3f)", sqrt(vcov(x)[1, 1]) / mean(rawdt$d_donate, na.rm = TRUE))
  )) %>%
  reduce(full_join, by = "term") %>%
  left_join(
    tribble(
      ~term, ~i1, ~i2,
      "Implied price elasticity", NA_real_, NA_real_,
      "", NA_real_, NA_real_,
    ),
    by = "term"
  ) %>%
  select(term, i1, i2, a.x, a.y)

attr(implied_e, "position") <- 7:8

out.file <- file(here("tables", "fe-model.tex"), open = "w")

tab <- est_femod %>%
  pull(est) %>%
  purrr::flatten() %>%
  setNames(paste0("(", 1:4, ")")) %>%
  modelsummary(
    title = "Fixed Effect Model of Price Elasticity \\label{tab:fe-model}",
    coef_map = c(
      "applicable" = "log(applicable last price)",
      "effective" = "log(effective last price)",
      "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = implied_e,
    output = "latex"
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    "Sample:" = 1,
    "Intensive-margin" = 2, "Extensive-margin" = 2
  ))

writeLines(tab, out.file)
close(out.file)

#' ベースラインとして、操作変数を用いない固定効果モデルの結果を表\@ref(tab:fe-model)示す
#'
#' - 説明変数に用いる価格は、控除を申請するときに適用される価格（applicable price）
#' と控除の有無による価格変動を考慮した現実の価格（effective price）の二つを用いる
#'
#' 寄付者に限定した寄付の価格弾力性（intensive-margin price elasticity）について
#'
#' - applicable priceを用いたintensive-marginの価格弾力性はおよそ-0.9である。
#' - effective priceを用いたintensive-marginの価格弾力性はおよそ-0.5である。
#' - 二つの価格弾力性の標準誤差を考慮すると、二つの価格弾力性の差は誤差の範囲であると考える。
#' - 二つの価格弾力性が似たような値を取った原因として、
#' 控除を申請していない人の平均的な寄付額が、価格に関わらず、
#' 控除を申請している人のそれと大きな差がないことにある。
#'
#' 寄付行動の価格弾力性（extensive-margin price elasticity）について
#'
#' - アウトカム変数が二値なので、推定された係数をextensive-marginの価格弾力性として解釈できない。
#'   - 係数を寄付者割合で割ることによって、extensive-marginの価格弾力性を得る
#' - applicable priceを用いるとき、extensive-marginの価格弾力性は
#' およそ-2.1($=-0.497 / 0.24$)である。
#' - effective priceを用いるとき、extensive-marginの価格弾力性は
#' およそ-12.2($=-2.917 / 0.24$)である。
#' - effective priceを用いることによって、extensive-marginの価格弾力性はより弾力的になった。
#'   - effective priceを用いることで、寄付をしていない人が直面する価格がapplicable priceより高くなるので、
#'   負の相関がよりクリアになった
#'