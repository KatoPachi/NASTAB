#' ---
#' title: |
#'   Estimating Effect of Tax Incentives on Donations
#'   Considering Self-Selection of Tax Incentives in South Korea
#' subtitle: |
#'   Results of Fixed Effect Model
#' author:
#'   - Hiroki Kato
#'   - Tsuyoshi Goto
#'   - Yongrok Kim
#' output:
#'   bookdown::html_document2:
#'     toc: true
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
#' ベースラインとして、操作変数を用いない固定効果モデルの結果を示す
#'
#' - 説明変数に用いる価格は、控除を申請するときに適用される価格（applicable price）
#' と控除の有無による価格変動を考慮した現実の価格（effective price）の二つを用いる
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

est_femod %>%
  pull(est) %>%
  purrr::flatten() %>%
  setNames(paste0("(", 1:4, ")")) %>%
  modelsummary(
    title = "Fixed Effect Model of Price Elasticity",
    coef_map = c(
      "applicable" = "log(applicable last price)",
      "effective" = "log(effective last price)",
      "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = implied_e
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    "Sample:" = 1,
    "Intensive-margin" = 2, "Extensive-margin" = 2
  ))

#+
int_use <- use %>%
  dplyr::filter(d_donate == 1) %>%
  modelr::add_residuals(reg_price_int$applicable, var = "applicable") %>%
  modelr::add_residuals(reg_price_int$effective, var = "effective") %>%
  mutate(
    residual = case_when(
      price_type == "applicable" ~ applicable,
      price_type == "effective" ~ effective
    )
  ) %>%
  select(-applicable, -effective)

reg_anatomy_int <- int_use %>%
  group_by(price_type) %>%
  do(
    est = feols(donate_ln ~ residual, data = ., cluster = ~ pid) %>% tidy()
  ) %>%
  summarize(
    price = price_type,
    label = sprintf(
      "Slope = %1.3f\n(s.e. = %1.3f)",
      est$estimate[2], est$std.error[2]
    )
  )

#+
x_labs <- c(
  applicable = "Residuals of log(last price)",
  effective = "Residuals of log(last price)\u00d7application"
)

title <- c(
  applicable = "A. Applicable Price",
  effective = "B. Effective Price"
)

#+ plot-anatomy-intensive, fig.cap = "Relationship between Giving Amount and Residuals of Giving Price among Donors."
plot_int <- names(x_labs) %>%
  purrr::map(function(x) {
    int_use %>%
      dplyr::filter(price_type == x) %>%
      dplyr::filter(!is.na(residual) & !is.na(donate_ln)) %>%
      group_by(d_relief_donate) %>%
      mutate(group = ntile(residual, 15)) %>%
      group_by(d_relief_donate, group) %>%
      summarize(
        min_residual = min(residual),
        max_residual = max(residual),
        residual = min_residual + (max_residual - min_residual) / 2,
        donate_ln = mean(donate_ln)
      ) %>%
      mutate(d_relief_donate = factor(
        d_relief_donate,
        levels = c(0, 1),
        labels = c("No", "Yes")
      )) %>%
      ggplot(aes(x = residual, y = donate_ln, shape = d_relief_donate)) +
      geom_point(aes(shape = d_relief_donate), size = 5, color = "grey50") +
      geom_smooth(
        method = "lm", data = subset(int_use, price_type == x),
        se = FALSE, color = "black", fullrange = TRUE
      ) +
      annotate(
        geom = "text",
        x = -0.15, y = 3.6,
        label = subset(reg_anatomy_int, price == x)$label,
        size = 5
      ) +
      scale_x_continuous(limits = c(-0.3, 0.2)) +
      scale_y_continuous(limits = c(3.2, 5.1)) +
      labs(
        title = title[x],
        x = x_labs[x],
        y = "log(donate) conditional on givers",
        shape = "Application of tax relief"
      ) +
      ggtemp()
  })

plot_int %>%
  wrap_plots() +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

#'
#' 図\@ref(fig:plot-anatomy-intensive)は寄付者に限定した寄付額と価格の関係を示している。
#'
#' - 縦軸は寄付者の寄付額であり、横軸は価格の残差である。
#'   - 価格の残差は、価格以外の共変量と固定効果で価格を回帰するモデルから得ている。
#' - パネルAとBはそれぞれ、applicable priceとeffective priceを用いている。
#' - どちらのパネルについても、価格の残差がゼロのあたりにサンプルが集中している。
#' - どちらのパネルについても、控除申請の有無によって、寄付額に大きな差がない。
#'
#' Regression anatomy theoremより、
#' 価格の残差に関する寄付額の単直線回帰分析はintensive-marginの価格弾力性となる。
#' 図\@ref(fig:plot-anatomy-intensive)の二つのパネルに示した直線はこの回帰直線である。
#'
#' - パネルAより、applicable priceを用いたintensive-marginの価格弾力性はおよそ-0.9である。
#' - パネルBより、effective priceを用いたintensive-marginの価格弾力性はおよそ-0.5である。
#' - 二つの価格弾力性の標準誤差を考慮すると、二つの価格弾力性の差は誤差の範囲であると考える。
#' - 二つの価格弾力性が似たような値を取った原因として、
#' 控除を申請していない人の平均的な寄付額が、価格に関わらず、
#' 控除を申請している人のそれと大きな差がないことにある。
#'
#+ anatomy-extensive
reg_price_ext <- use %>%
  group_by(price_type) %>%
  do(est = feols(value ~ ..stage2, data = .)) %>%
  pull(est, name = price_type)

ext_use <- use %>%
  modelr::add_residuals(reg_price_ext$applicable, var = "applicable") %>%
  modelr::add_residuals(reg_price_ext$effective, var = "effective") %>%
  mutate(
    residual = case_when(
      price_type == "applicable" ~ applicable,
      price_type == "effective" ~ effective
    )
  ) %>%
  select(-applicable, -effective)

reg_anatomy_ext <- ext_use %>%
  group_by(price_type) %>%
  do(
    est = feols(d_donate ~ residual, data = ., cluster = ~pid) %>% tidy()
  ) %>%
  summarize(
    price = price_type,
    label = sprintf(
      "Slope = %1.3f\n(s.e. = %1.3f)",
      est$estimate[2], est$std.error[2]
    )
  )

#+ plot-anatomy-extensive, fig.cap = "Relationship between Decision to Donate and Residuals of Giving Price."
plot_ext <- names(x_labs) %>%
  purrr::map(function(x) {
    ext_use %>%
      dplyr::filter(price_type == x) %>%
      dplyr::filter(!is.na(residual) & !is.na(d_donate)) %>%
      group_by(d_relief_donate) %>%
      mutate(group = ntile(residual, 15)) %>%
      group_by(d_relief_donate, group) %>%
      summarize(
        min_residual = min(residual),
        max_residual = max(residual),
        residual = min_residual + (max_residual - min_residual) / 2,
        d_donate = mean(d_donate)
      ) %>%
      mutate(d_relief_donate = factor(
        d_relief_donate,
        levels = c(0, 1),
        labels = c("No", "Yes")
      )) %>%
      ggplot(aes(x = residual, y = d_donate, shape = d_relief_donate)) +
      geom_point(aes(shape = d_relief_donate), size = 5, color = "grey50") +
      geom_smooth(
        method = "lm", data = subset(ext_use, price_type == x),
        se = FALSE, color = "black", fullrange = TRUE
      ) +
      annotate(
        geom = "text",
        x = 0.1, y = 0.6,
        label = subset(reg_anatomy_ext, price == x)$label,
        size = 5
      ) +
      scale_x_continuous(limits = c(-0.3, 0.2)) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(
        title = title[x],
        x = x_labs[x],
        y = "Proportion of Donors",
        shape = "Application of tax relief"
      ) +
      ggtemp()
  })

plot_ext %>%
  wrap_plots() +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

#'
#' 図\@ref(fig:plot-anatomy-extensive)は寄付行動と寄付価格の残差の関係を示している。
#'
#' - 縦軸は寄付者の寄付額であり、横軸は価格の残差である。
#'   - 価格の残差は、価格以外の共変量と固定効果で価格を回帰するモデルから得ている。
#' - パネルAとBはそれぞれ、applicable priceとeffective priceを用いている。
#' - どちらのパネルについても、寄付控除を申請した人は全員寄付をしている。
#' その一方で、控除を申請していない人の寄付者の割合は半数に満たない。
#' - Effective priceを用いるとき、控除を申請していない人の価格は常に1となるので、
#' その残差はapplicable priceの残差よりも正の方向に移動している。
#'
#' Regression anatomy theoremより、
#' 価格の残差に関する寄付行動の単直線回帰分析は固定効果モデルの価格の係数となる。
#' 図\@ref(fig:plot-anatomy-intensive)の二つのパネルに示した直線はこの回帰直線である。
#'
#' - アウトカム変数が二値なので、
#' この回帰直線の傾きをextensive-marginの価格弾力性として解釈できない。
#'   - 傾きの係数を寄付者割合で割ることによって、extensive-marginの価格弾力性を得る
#' - パネルAより、applicable priceを用いるとき、extensive-marginの価格弾力性は
#' およそ-2.1($=-0.497 / 0.24$)である。
#' - パネルBより、effective priceを用いるとき、extensive-marginの価格弾力性は
#' およそ-12.2($=-2.917 / 0.24$)である。
#' - effective priceを用いることによって、extensive-marginの価格弾力性はより弾力的になった。
#'
# /*
#+
rmarkdown::render(
  here("R", "9-regression-anatomy.r"),
  output_dir = here("docs", "html-preview")
)
# */