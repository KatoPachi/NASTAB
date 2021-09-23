# /*
# knit
knitr::spin(
  "script/R/7-issue96.r",
  knit = TRUE
)
# */

#+ include = FALSE
knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  echo = FALSE,
  cache = FALSE,
  include = TRUE
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

#+
library(xfun)
xfun::pkg_attach2(c(
  "tidyverse", "rlang", "rlist",
  "lmtest", "sandwich",
  "lfe", "Formula", "fixest",
  "kableExtra", "flextable", "officer", "modelsummary"
))

#+
se <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- na.omit(x)
  }
  sqrt(var(x) / length(x))
}

ggtemp <- function(flip = FALSE,
                   family = NULL,
                   size = list(
                     title = 13,
                     text = 9,
                     caption = 11
                   ),
                   legend_key_size = 1) {
  my_theme <- theme_minimal(base_family = family) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text = element_text(
        color = "black", size = size$text, family = family
      ),
      axis.title = element_text(size = size$title, family = family),
      axis.ticks.length = unit(0.25, "cm"),
      axis.ticks.x = element_line(),
      axis.ticks.y = element_line(),
      axis.line = element_line(),
      legend.text = element_text(size = size$text, family = family),
      legend.key.size = unit(legend_key_size, "cm"),
      legend.title = ggplot2::element_text(size = size$title),
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(size = size$caption)
    )

  if (flip) {
    my_theme <- my_theme +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line()
      )
  }

  return(my_theme)
}

#+
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
#' # Issue96に関するレポート
#'
#' [tracking issue here](https://github.com/KatoPachi/NASTAB/issues/96)
#'
#' ## 異時点間の代替性の検証
#'
#' - 問題の所在：2014年の寄付税制のアナウンスが2013年にあったので、これによる異時点間の代替性が生じているかもしれない
#'   - Anticipation effect
#'   - 所得ラグの推定は所得税の累進性に対応したものなので、この効果に対する明確な対処となっていない
#' - 対応１：所得階層別の寄付額の記述統計の確認する
#'   - アナウンスによる異時点間の代替性が視覚的に生じているかどうか
#'   - 平行トレンドが満たされていれば、anticipation effectは特に問題なし
#' - 以下の図は所得階層を3区分にして、各所得階層の各年の寄付額の対数値の平均をプロット
#'   - income < 1200：2014年改革で寄付の相対価格が下落するグループ
#'   - income b/w 1200 and 4600：2014年改革で寄付の相対価格が変化しないグループ
#'   - income > 4600：2014年改革で寄付の相対価格が増加するグループ
#'   - **視覚的に、異時点間の代替性が生じているとは思えないし、平行トレンドも満たされているようにも見える**
#'
#+ SummaryOutcomebyIncome1
df %>%
  mutate(group = case_when(
    credit_loss == 1 ~ 3,
    credit_neutral == 1 ~ 2,
    credit_benefit == 1 ~ 1
  )) %>%
  dplyr::filter(!is.na(group)) %>%
  group_by(year, group) %>%
  summarize(mu = mean(log_total_g, na.rm = TRUE)) %>%
  mutate(group = factor(
    group, labels = c("< 1200", "[1200, 4600]", "> 4600")
  )) %>%
  ggplot(aes(x = year, y = mu, group = group)) +
    geom_point(aes(color = group)) +
    geom_line(aes(color = group)) +
    geom_vline(aes(xintercept = 2013.5), linetype = 3) +
    scale_x_continuous(breaks = seq(2012, 2018, 1)) +
    labs(
      x = "Year", y = "Mean donations (logged value)",
      color = "Income group (unit:10,000KRW)"
    ) +
    ggtemp()

#'
#' - 2013年の寄付対数値の平均が1となるようにしても、平行トレンドが満たされていて、異時点間の代替性が生じていると思われる
#'
#+ SummaryOutcomebyIncome2
df %>%
  mutate(group = case_when(
    credit_loss == 1 ~ 3,
    credit_neutral == 1 ~ 2,
    credit_benefit == 1 ~ 1
  )) %>%
  dplyr::filter(!is.na(group)) %>%
  group_by(year, group) %>%
  summarize(mu = mean(log_total_g, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = "year", values_from = "mu") %>%
  mutate(base = `2013`) %>%
  dplyr::select(group, base, everything()) %>%
  tidyr::pivot_longer(- (group:base), values_to = "mu", names_to = "year") %>%
  mutate(mu = mu / base, year = as.numeric(year)) %>%
  mutate(group = factor(
    group,
    labels = c("< 1200", "[1200, 4600]", "> 4600")
  )) %>%
  ggplot(aes(x = year, y = mu, group = group)) +
  geom_point(aes(color = group)) +
  geom_line(aes(color = group)) +
  geom_vline(aes(xintercept = 2013.5), linetype = 3) +
  scale_x_continuous(breaks = seq(2012, 2018, 1)) +
  labs(
    x = "Year",
    y = "The ratio of mean donations (logged value)",
    color = "Income group (unit:10,000KRW)",
    caption = paste(
      "The ratio is calculated by",
      "(mean of logged donation in year t) / (mean of logged donation in 2013)."
    )
  ) +
  ggtemp()

#'
#' - 対策２：異時点間の代替性によって影響をうける制度改革の直前と直後のデータを落として、推定する
#'   - 以下の表は2013年と2014年のデータを落として、First priceの弾力性を推定した結果
#'   - 比較のために、フルサンプルの結果も併せて示す
#'   - データを落としても、結果に大きな変化はない
#'   - **異時点間の代替性があったとしても、結果は大きく変化しない**
#+
fixest::setFixest_fml(
  ..ctrl = ~ log_pinc_all + age + sqage + factor(year):factor(educ) +
    factor(year):factor(gender) + factor(year):factor(living_area) | year + pid
)

xlist_tab <- tribble(
  ~term, ~Overall, ~Intensive, ~Extensive,
  "Age (squared age)", "X", "X", "X",
  "Year x Education", "X", "X", "X",
  "Year x Gender", "X", "X", "X",
  "Year x Resident Area", "X", "X", "X"
)

df %>%
  {
    list(
      "Overall" = list(
        eq = fixest::xpd(log_total_g ~ log_price + ..ctrl),
        data = .
      ),
      "Intensive" = list(
        eq = fixest::xpd(log_total_g ~ log_price + ..ctrl),
        data = subset(., i_ext_giving == 1)
      ),
      "Extensive" = list(
        eq = fixest::xpd(i_ext_giving ~ log_price + ..ctrl),
        data = .
      )
    )
  } %>%
  purrr::map(~ fixest::feols(
    .$eq, data = .$data,
    cluster = ~ pid, se = "cluster"
  )) %>%
  modelsummary(
    title = "First price elasticity: Full sample",
    coef_omit = "factor|age",
    coef_rename = c(
      "log_price" = "log(first giving price)",
      "log_pinc_all" = "log(annual taxable income)"
    ),
    stars = c("*" = .1, "**" = .05, "***" = .01),
    gof_omit = "^(?!R2 Adj.|R2 Within|FE|N)",
    add_rows = xlist_tab
  ) %>%
  kableExtra::footnote(
    general_title = "",
    general = paste(
      "Note: * p < 0.1, ** p < 0.05, *** p < 0.01.",
      "Starand errors are culustered at individual level."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )


#'
#+
df %>%
  dplyr::filter(year < 2013 | year > 2014) %>%
  {
    list(
      "Overall" = list(
        eq = fixest::xpd(log_total_g ~ log_price + ..ctrl),
        data = .
      ),
      "Intensive" = list(
        eq = fixest::xpd(log_total_g ~ log_price + ..ctrl),
        data = subset(., i_ext_giving == 1)
      ),
      "Extensive" = list(
        eq = fixest::xpd(i_ext_giving ~ log_price + ..ctrl),
        data = .
      )
    )
  } %>%
  purrr::map(~ fixest::feols(
    .$eq, data = .$data,
    cluster = ~ pid, se = "cluster"
  )) %>%
  modelsummary(
    title = "First price elasticity: Exclude data with year 2013 and 2014",
    coef_omit = "factor|age",
    coef_rename = c(
      "log_price" = "log(first giving price)",
      "log_pinc_all" = "log(annual taxable income)"
    ),
    stars = c("*" = .1, "**" = .05, "***" = .01),
    gof_omit = "^(?!R2 Adj.|R2 Within|FE|N)",
    add_rows = xlist_tab
  ) %>%
  kableExtra::footnote(
    general_title = "",
    general = paste(
      "Note: * p < 0.1, ** p < 0.05, *** p < 0.01.",
      "Starand errors are culustered at individual level."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#'
#' ## 自営業者と給与所得者の異質性
#'
#' - IIPFの宮崎先生のSuggestion
#' - Reduced formで推定してpreliminalyな結果として見せて、tax reliefに起因するものと論じることは可能かも（加藤の意見）
#'
#' 以下に給与所得者（Wage earners）と自営業者（Self-employed）でサブサンプルに分けて推定した結果を示す。
#' ただし、やってみたはいいが、解釈が難しい・・・・。
#'
#+
xlist_tab <- tribble(
  ~term, ~Overall, ~Intensive, ~Extensive, ~Overall, ~Intensive, ~Extensive,
  "Age (squared age)", "X", "X", "X", "X", "X", "X",
  "Year x Education", "X", "X", "X", "X", "X", "X",
  "Year x Gender", "X", "X", "X", "X", "X", "X",
  "Year x Resident Area", "X", "X", "X", "X", "X", "X"
)

df %>%
  dplyr::filter(employee == 1) %>%
  {
    list(
      "Overall" = list(
        eq = fixest::xpd(log_total_g ~ log_price + ..ctrl),
        data = .
      ),
      "Intensive" = list(
        eq = fixest::xpd(log_total_g ~ log_price + ..ctrl),
        data = subset(., i_ext_giving == 1)
      ),
      "Extensive" = list(
        eq = fixest::xpd(i_ext_giving ~ log_price + ..ctrl),
        data = .
      ),
      "Overall" = list(
        eq = fixest::xpd(log_total_g ~ log_price:ext_benefit_tl + ..ctrl),
        data = .
      ),
      "Intensive" = list(
        eq = fixest::xpd(log_total_g ~ log_price:ext_benefit_tl + ..ctrl),
        data = subset(., i_ext_giving == 1)
      ),
      "Extensive" = list(
        eq = fixest::xpd(i_ext_giving ~ log_price:ext_benefit_tl + ..ctrl),
        data = .
      )
    )
  } %>%
  purrr::map(~ fixest::feols(
    .$eq,
    data = .$data,
    cluster = ~pid, se = "cluster"
  )) %>%
  modelsummary(
    title = "First price elasticity: Wage earners",
    coef_omit = "factor|age",
    coef_map = c(
      "log_price" = "log(first giving price)",
      "log_price:ext_benefit_tl" =
        "log(first giving price) X 1 = apply tax relief",
      "log_pinc_all" = "log(annual taxable income)"
    ),
    stars = c("*" = .1, "**" = .05, "***" = .01),
    gof_omit = "^(?!R2 Adj.|R2 Within|FE|N)",
    add_rows = xlist_tab
  )

#'
#+
df %>%
  dplyr::filter(employee == 0) %>%
  {
    list(
      "Overall" = list(
        eq = fixest::xpd(log_total_g ~ log_price + ..ctrl),
        data = .
      ),
      "Intensive" = list(
        eq = fixest::xpd(log_total_g ~ log_price + ..ctrl),
        data = subset(., i_ext_giving == 1)
      ),
      "Extensive" = list(
        eq = fixest::xpd(i_ext_giving ~ log_price + ..ctrl),
        data = .
      ),
      "Overall" = list(
        eq = fixest::xpd(log_total_g ~ log_price:ext_benefit_tl + ..ctrl),
        data = .
      ),
      "Intensive" = list(
        eq = fixest::xpd(log_total_g ~ log_price:ext_benefit_tl + ..ctrl),
        data = subset(., i_ext_giving == 1)
      ),
      "Extensive" = list(
        eq = fixest::xpd(i_ext_giving ~ log_price:ext_benefit_tl + ..ctrl),
        data = .
      )
    )
  } %>%
  purrr::map(~ fixest::feols(
    .$eq,
    data = .$data,
    cluster = ~pid, se = "cluster"
  )) %>%
  modelsummary(
    title = "First price elasticity: Self-employed",
    coef_omit = "factor|age",
    coef_map = c(
      "log_price" = "log(first giving price)",
      "log_price:ext_benefit_tl" =
        "log(first giving price) X 1 = apply tax relief",
      "log_pinc_all" = "log(annual taxable income)"
    ),
    stars = c("*" = .1, "**" = .05, "***" = .01),
    gof_omit = "^(?!R2 Adj.|R2 Within|FE|N)",
    add_rows = xlist_tab
  )
