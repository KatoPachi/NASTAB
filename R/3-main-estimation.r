#' ---
#' title: |
#'   Estimating Effect of Tax Incentives on Donations
#'   Considering Self-Selection of Tax Incentives in South Korea
#' subtitle: |
#'   Results of FE-2SLS
#' author:
#'   - Hiroki Kato
#'   - Tsuyoshi Goto
#'   - Yongrok Kim
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
library(here)
source(here("R", "_library.r"))

#+ include = FALSE
source(here("R", "_html_header.r"))

#+ include = FALSE
rawdt <- readr::read_csv(
  here("data/shaped2_propensity.csv"),
  guess_max = 30000
)

flag <- rawdt %>%
  mutate(
    flag_extensive = 1,
    flag_intensive = if_else(d_donate == 1, 1, 0)
  ) %>%
  select(
    pid,
    year,
    flag_extensive,
    flag_intensive
  ) %>%
  pivot_longer(
    flag_extensive:flag_intensive,
    names_to = "outcome",
    values_to = "flag",
    names_prefix = "flag_"
  )

main <- rawdt %>%
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
    intensive = donate_ln,
    extensive = d_donate
  ) %>%
  pivot_longer(
    intensive:extensive,
    names_to = "outcome",
    values_to = "y"
  )

use <- flag %>%
  dplyr::left_join(main, by = c("pid", "year", "outcome")) %>%
  mutate(effective = d_relief_donate * lprice_ln)

#'
#' ベースラインで示した固定効果モデルには2つの内生性が存在する。
#'
#' 1. 寄付価格が個人の寄付行動によって決まる
#' 1. 寄付控除の申告が自己選択であること
#'
#' これに対処するために、二段階固定効果モデル（FE-2SLS）を推定する。
#' 操作変数は(1)applicable first-unit priceと(2)給与所得者ダミーである。
#'
#' - first-unit priceは寄付額ゼロのときの寄付価格であり、寄付額とは独立に決まる。
#' また、寄付価格が低いほど、寄付による節税の便益が大きいので、
#' この変数は申告による便益を捉えている。
#' - 給与所得者は、自営業者と比較して、簡単に寄付控除を申告出来るので、
#' 給与所得者ダミーは寄付控除の申告コストの代理変数となる。
#'
#+ plot-stage1, fig.cap = "Relationship between Applicable First Price and Last Price by Employment Status. Note: The bubble size indicates the percentage of donation deductions claimed. Due to the small sample size, the leftmost bubbles for salaried and self-employed workers are less informative ($N=6$ for wage earner and $N=2$ for self-employed).", out.extra = ""
use %>%
  dplyr::filter(
    !is.na(d_relief_donate) & !is.na(lprice_ln) & !is.na(employee)
  ) %>%
  mutate(
    employee = factor(employee, label = c("Self-employed", "Wage earner"))
  ) %>%
  group_by(price_ln, employee) %>%
  summarize(
    n = n(),
    d_relief_donate = mean(d_relief_donate),
    effective = mean(effective),
    applicable = mean(lprice_ln)
  ) %>%
  pivot_longer(effective:applicable, names_to = "type") %>%
  mutate(type = factor(
    type,
    labels = c("Applicable last price", "Effective last price")
  )) %>%
  ggplot(aes(x = price_ln, y = value)) +
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  geom_point(aes(size = d_relief_donate, color = employee), alpha = 0.8) +
  scale_color_grey() +
  scale_size(range = c(5, 20)) +
  facet_wrap(~ type) +
  labs(
    x = "log(first price)",
    y = "Sample average",
    color = ""
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 5)),
    size = "none"
  ) +
  ggtemp()

#' 操作変数に関する我々の予測が成立していることをデータから確認する。
#'
#' - 図\@ref(fig:plot-stage1)はapplicable first priceとlast priceの関係を
#' 示している。バブルサイズが大きいほど、寄付控除の申告比率が高い。
#' - 左のパネルははapplicable first priceとapplicable last priceの関係を示していて、
#' 二つの価格は寄付控除の申告行動を考慮していない。
#' 寄付行動による寄付価格の操作が強いほど、バブルは45度線の上側に位置するようになる。
#' - 多くのバブルは45度線上にあるので、
#' 寄付行動による寄付価格の操作から生じる内生性は大きな問題ではない。
#' - 右のパネルはapplicable first priceとapplicable last priceの関係を示している。
#' 寄付行動による寄付価格の操作や寄付控除の非申請が大きいほど、
#' effective last priceは45度線の上側に位置するようになる。
#' - 左のパネルの結果を考えれば、バブルが45度線の上側に位置している原因は
#' 寄付控除の行動によるものであると考えられる。
#' 事実、バブルサイズが小さいほど、バブルは45度線の上側に位置している。
#' - applicable first priceが高いほど、バブルサイズが小さくなっている。
#' すなわち、相対的な寄付価格が高いと、申告されにくくなる。
#' - 給与所得者のバブルサイズが自営業主のバブルサイズより大きい。
#' すなわち、給与所得者は、自営業主と比較して、寄付控除を申請していない。
#' その結果として、effective last priceが45度線に近づいている。
#'
#+
fixest::setFixest_fml(
  ..stage2 = ~ linc_ln + sqage + hh_num + have_dependents |
    year + pid + indust + area
)

#+ stage1
stage1 <- list(
  effective ~ price_ln + ..stage2,
  effective ~ price_ln:employee + ..stage2,
  effective ~ price_ln + price_ln:employee + ..stage2
)

est_stage1 <- use %>%
  mutate(outcome = factor(outcome, levels = c("intensive", "extensive"))) %>%
  group_by(outcome) %>%
  do(est = lapply(
    stage1,
    function(x) feols(x, data = subset(., flag == 1), cluster = ~hhid)
  ))

est_stage1 %>%
  pull(est) %>%
  flatten() %>%
  setNames(paste0("(", seq(length(stage1) * 2), ")")) %>%
  modelsummary(
    title = "Results of Regression of Effective Last Price",
    coef_map = c(
      "price_ln" = "log(first price)",
      "price_ln:employee" = "log(first_price)\u00d7wage earner"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1)
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    "Sample:" = 1,
    "Intensive-margin" = 3, "Extensive-margin" = 3
  ))

#'
#' 給与所得者が相対的に控除を申請しやすいのであれば、
#' 給与所得者に限定したeffective last priceとapplicable first priceの相関は
#' 自営業主のそれよりも強くなるはずである。
#'
#' - 表\@ref(tab:stage1)はFE-2SLSの第一段階の推定結果を示している。
#' - 寄付者に限定して推定する（モデル(3)）と、給与所得者以外のeffective last priceと
#' applicable first priceの相関は0.679であり、統計的に有意である。
#' また、給与所得者のeffective last priceとapplicable first priceの相関は
#' 0.707($=0.697 + 0.028$)であり、給与所得者以外のそれよりも若干大きい。
#' これは我々の予想と整合的であるが、給与所得者とそれ以外の相関の差は統計的に非有意である。
#' - 非寄付者も含めて推定する（モデル(6)）と、給与所得者以外のeffective last priceと
#' applicable first priceの相関は0.328であり、統計的に有意である。
#' また、給与所得者のeffective last priceとapplicable first priceの相関は
#' 0.369($=0.328 + 0.0041$)である。
#' 二つの相関の差は我々の予想と整合的であり、統計的に有意である。
#'
#+ fe2sls
fe2sls <- list(
  y ~ ..stage2 | d_relief_donate:lprice_ln ~ price_ln,
  y ~ ..stage2 | d_relief_donate:lprice_ln ~ employee:price_ln,
  y ~ ..stage2 | d_relief_donate:lprice_ln ~ price_ln + employee:price_ln
)

est_models <- use %>%
  mutate(outcome = factor(outcome, levels = c("intensive", "extensive"))) %>%
  group_by(outcome) %>%
  do(est = lapply(
    fe2sls,
    function(x) feols(x, data = subset(., flag == 1), cluster = ~hhid)
  ))

stats_stage1 <- est_models %>%
  group_by(outcome) %>%
  do(tab = data.frame(
    models = paste0(.$outcome, c(1, 2, 3)),
    f = lapply(.$est[[1]], function(x)
      fitstat(x, "ivf")[[1]]$stat
    ) %>% as_vector,
    wh = lapply(.$est[[1]], function(x)
      fitstat(x, "wh")$wh$p
    ) %>% as_vector
  )) %>%
  { bind_rows(.$tab) } %>%
  mutate(
    f = sprintf("%1.2f", f),
    wh = sprintf("%1.3f", wh)
  ) %>%
  pivot_longer(f:wh, names_to = "terms") %>%
  pivot_wider(names_from = models, values_from = value) %>%
  mutate(
    terms = recode(
      terms,
      "f" = "F-statistics of instruments",
      "wh" = "Wu-Hausman test, p-value"
    )
  ) %>%
  bind_rows(c(
    terms = "First-Stage model",
    intensive1 = "(1)",
    intensive2 = "(2)",
    intensive3 = "(3)",
    extensive1 = "(4)",
    extensive2 = "(5)",
    extensive3 = "(6)"
  ), .)

est_models %>%
  pull(est) %>%
  flatten() %>%
  setNames(paste0("(", seq(length(fe2sls)*2), ")")) %>%
  modelsummary(
    title = "Tax-Price Elasticity Estimated by FE-2SLS",
    coef_map = c(
      "d_relief_donate:lprice_ln" =
        "Effective last price",
      "fit_d_relief_donate:lprice_ln" =
        "Effective last price",
      "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = stats_stage1
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    " ",
    "Intensive-margin" = 3, "Extensive-margin" = 3
  )) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at household level.",
      "A square bracket is F statistics of instrument."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#'
#' 表\@ref(tab:fe2sls)はFE-2SLSの第二段階の推定結果を示している。
#'
#' - 寄付者に限定すると、寄付額に対するeffective last priceの価格弾力性は
#' -1.9から-1.7の範囲で得られた。
#' これは通常の固定効果モデルの価格弾力性よりも3倍弾力的であり、
#' Wu-Hausman検定より統計的に有意な差である。
#' また、F値が500以上あるので、この結果の差は操作変数の弱相関による問題ではない。
#' - 寄付行動をアウトカムとするとき、effective last priceの係数は-1.5から-1の範囲で得られた。
#' 価格弾力性に変換するために、この係数を寄付者比率で割ると、
#' 価格弾力性は-6.25から-4.17の範囲となる。
#' これは通常の固定効果モデルの価格弾力性の半分以下とであり、
#' Wu-Hausman検定より統計的に有意な差である。
#' また、F値が1000以上あるので、この結果の差は操作変数の弱相関による問題ではない。
#'
#'
# /*
#+
rmarkdown::render(
  here("R", "3-main-estimation.r"),
  output_dir = here("docs", "html-preview")
)
# */