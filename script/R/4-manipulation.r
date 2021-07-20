#' # Issue #72: RDDアプローチで弾力性を推定したい
#'
#' [リンク](https://github.com/KatoPachi/NASTAB/issues/72#issue-909426778)

#' ## セットアップ：パッケージとggplot2のテンプレート
#'
#' パッケージのロード
#+
library(xfun)
xfun::pkg_attach2(c("tidyverse", "rlist", "patchwork"))
xfun::pkg_attach2(c("plm", "lmtest", "sandwich", "lfe", "Formula"))
xfun::pkg_attach2("kableExtra")

lapply(Sys.glob(file.path("script/R/functions", "*.r")), source)

#' ## データの読み込み
#'
#' 1. `mtrdt.csv`の所得税率(`MTR`)から寄付の相対価格(`price`)を作成
#' 2. 一段低い寄付価格ブラケットの最低所得（`lower_income_10000won`）を`to_next_price`とする
#' 3. ナスタブデータとマージして、一段低い寄付価格ブラケットに移動するために必要な寄付額（`dist_to_next_price`）を作成
#'      - `dist_to_next_price = to_next_price - lincome`
#+
mtrdt <- read_csv("data/origin/mtrdt.csv") %>%
  mutate(price = 1 - MTR) %>%
  arrange(year, lower_income_10000won) %>%
  group_by(year) %>%
  mutate(to_next_price = dplyr::lead(lower_income_10000won))

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

df <- df %>%
  mutate(price = round(price, 2)) %>%
  left_join(mtrdt, by = c("year", "price")) %>%
  mutate(dist_to_next_price = to_next_price - lincome)

#' 以下に2012年と2013年の所得税率と寄付価格に関する情報を示しておく
#+
mtrdt %>% dplyr::filter(year == 2012 | year == 2013)

#' ## 課税前所得と課税後所得の密度分布
#'
#' 所得税ブラケットの境界線で密度が著しく上昇しているかどうかを目視で確認する（Bunching）
#'
#' - 12000Kウォンの境界線の若干右側で密度が高くなるが、スムーズな動きをしているように見える
#' - 46000Kウォンと88000Kウォンでも密度関数が境界線上で著しく跳ね上がるような動きをしていない
#'
#' 所得によるBunchingは見られない。サーベイ調査だからか？
#+
df %>%
  dplyr::filter(year < 2014 & lincome - i_total_giving > 0) %>%
  mutate(segment = round(lincome / 100, 0) * 100) %>%
  dplyr::filter(segment <= 12000) %>%
  ggplot() +
    # geom_histogram(binwidth = 100, color = "black", fill = "grey80") +
    geom_density(
      aes(
        x = (lincome - i_total_giving),
        color = "Annual taxable income - Annual donations"),
      size = 1) +
    geom_density(aes(x = lincome, color = "Annual taxable income"), size = 1) +
    geom_vline(aes(xintercept = 1200), linetype = 2, size = 1) +
    geom_vline(aes(xintercept = 4600), linetype = 2, size = 1) +
    geom_vline(aes(xintercept = 8800), linetype = 2, size = 1) +
    labs(x = "Income (income < 12000)") +
    my_theme

#' ## 所得と寄付の相関図
#'
#' 境界線上で寄付額が非連続に増加（もしくは減少）しているならば、そのRDD効果を推定することで寄付の弾性値を推測できる
#'
#' - 1000ウォン区間を作成し、各区間の寄付の対数値の平均をプロット
#' - 2012年と2013年をまとめてプロット
#'
#' 結果として、ブラケットの境界線上で寄付額の対数値平均がジャンプしているようなことはないと見える
#+
df %>%
  dplyr::filter(year < 2014) %>%
  mutate(segment = round(lincome / 100, 0) * 100) %>%
  group_by(segment) %>%
  summarize(mean = mean(log_total_g, na.rm = TRUE)) %>%
  dplyr::filter(segment <= 12000) %>%
  ggplot(aes(x = segment, y = mean)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_vline(aes(xintercept = 1200), linetype = 2, size = 1) +
    geom_vline(aes(xintercept = 4600), linetype = 2, size = 1) +
    geom_vline(aes(xintercept = 8800), linetype = 2, size = 1) +
    labs(
      x = "Segment of annual taxable income",
      y = "Mean logged donation levels in 2012 and 2013") +
    my_theme

#' ## Next lower blacketと寄付額の相関
#'
#' 1段階安い寄付価格に移動するために必要な寄付額と実際の寄付額の関係を確認する
#'
#' - 寄付額の操作のインセンティブはbracketのボーダーまでの距離で異なるかを確認する
#'
#' 以下の図は2つのパネルで構成されている
#'
#' - パネルAはすべてのブラケットのデータを用いてプロットしたもの
#' - パネルBは各ブラケットにサンプルを分割してプロットしたもの
#' - ボーダーまでの距離を100ウォン単位で分割し、その平均寄付額をプロットしている
#' - 点線の左側の領域は、1段階安い寄付価格ブラケットに移動できるだけの寄付をしている
#'
#' 結果
#'
#' - ボーダーまでの距離が2000Kウォン以内の人以外はブラケットに移動するのに十分な寄付をしていない
#' - パネルAをみると、正の相関をしているように見えるが、これは所得効果によるものと考えられる
#'    - ボーダーまでの距離が100000Kウォンを超える人の所得は最低でも88000Kウォンであるから
#' - 課税前のブラケットの寄付価格が0.76のブラケットは負の相関をしている（ように見える）
#+
full <- df %>%
  dplyr::filter(0.62 < price & year < 2014) %>%
  mutate(segment = round(dist_to_next_price / 100, 0) * 100) %>%
  group_by(segment) %>%
  summarize(mean = mean(i_total_giving, na.rm = TRUE)) %>%
  ggplot(aes(x = segment, y = mean)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
    my_theme
  
sub <- df %>%
  dplyr::filter(0.65 < price & year < 2014) %>%
  mutate(segment = round(dist_to_next_price / 100, 0) * 100) %>%
  group_by(price, segment) %>%
  summarize(mean = mean(i_total_giving, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(price = factor(
    price, labels = sprintf("giving price = %1.2f", unique(price))
  )) %>%
  ggplot(aes(x = segment, y = mean)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
    facet_wrap(~price, ncol = 1) +
    my_theme

(full + sub) &
  labs(
    x = "Segment of distance to next lower giving price",
    y = "Mean donation levels in 2012 and 2013") &
  plot_annotation(tag_levels = "A")

#' ## Next lower blacketによる寄付額の回帰分析
#'
#' 上図の結果を回帰モデルを使って統計的に有意なものであるかを確認する
#' $$ \text{i_total_giving}_{it} = \beta + \text{dist_to_next_price} + \gamma X_{it} + \mu_i + \eta_t + \epsilon_{it} $$
#+
distregs <- list(
  reg1 = i_total_giving ~ dist_to_next_price + log_pinc_all | 0 | 0 | pid,
  reg2 = i_total_giving ~ dist_to_next_price + log_pinc_all | year | 0 | pid,
  reg3 = i_total_giving ~ dist_to_next_price + log_pinc_all |
    year + pid | 0 | pid,
  reg4 = log_total_g ~ log(dist_to_next_price) + log_pinc_all +
    age + sqage + factor(year):factor(educ) + factor(year):factor(gender) +
    factor(year):factor(living_area) | year + pid | 0 | pid
)

covnote <- tribble(
  ~vars, ~stat, ~reg1, ~reg2, ~reg3, ~reg4,
  "Individual FE", "vars", "N", "N", "Y", "Y",
  "Time FE", "vars", "N", "Y", "Y", "Y",
  "Other covariates", "vars", "N", "N", "N", "Y"
)

est_distregs <- distregs %>%
  purrr::map(~felm(., data = df %>% dplyr::filter(year < 2014)))
est_distregs_int <- distregs %>%
  purrr::map(
    ~felm(., data = df %>% dplyr::filter(year < 2014 & i_ext_giving == 1))
  )
est_distregs_ext <- distregs %>%
  purrr::map(~update(as.Formula(.), i_ext_giving ~ .)) %>%
  purrr::map(~felm(., data = df %>% dplyr::filter(year < 2014)))

#' 2012年と2013年のサンプルを用いた結果、Next lower blacketまでの距離と寄付額に相関はない
#+
fullset_tab(est_distregs, keep_coef = "dist_to_next_price")$set %>%
  bind_rows(covnote) %>%
  mutate_at(
    c("reg1", "reg2", "reg3", "reg4"), list(~ifelse(is.na(.), "", .))
  ) %>%
  dplyr::select(-stat) %>%
  kable(
    title = "Full sample",
    col.names = c("", sprintf("(%1d)", 1:4)),
    align = "lcccc",
    format = "html"
  ) %>%
  add_header_above(
   c("", "Giving amount" = 3, "log(Giving amount)" = 1),
   escape = FALSE
  )

#' 2012年と2013年かつ寄付した人のみに限定すると、正の相関が確認できる。
#' しかし、個人固定効果を取ると、その相関がなくなる（やはりあまり関係がないか）
#+
fullset_tab(est_distregs_int, keep_coef = "dist_to_next_price")$set %>%
  bind_rows(covnote) %>%
  mutate_at(
    c("reg1", "reg2", "reg3", "reg4"), list(~ ifelse(is.na(.), "", .))
  ) %>%
  dplyr::select(-stat) %>%
  kable(
    title = "Intentisive margin (Those who give)",
    col.names = c("", sprintf("(%1d)", 1:4)),
    align = "lcccc",
    format = "html"
  ) %>%
  add_header_above(
    c("", "Giving amount" = 3, "log(Giving amount)" = 1),
    escape = FALSE
  )

#' 2012年と2013年にサンプルを限定し、寄付したかどうかを示すダミー変数をアウトカム変数とした
#' その結果、個人固定効果を取ると負の相関が統計的に非有意となる（やはり関係ない？）
#+
fullset_tab(est_distregs_ext, keep_coef = "dist_to_next_price")$set %>%
  bind_rows(covnote) %>%
  mutate_at(
    c("reg1", "reg2", "reg3", "reg4"), list(~ ifelse(is.na(.), "", .))
  ) %>%
  dplyr::select(-stat) %>%
  kable(
    title = "Extensive margin (Whether to give)",
    col.names = c("", sprintf("(%1d)", 1:4)),
    align = "lcccc",
    format = "html"
  ) %>%
  add_header_above(
    c("", "1 = Giving" = 4),
    escape = FALSE
  )

#' ## Last-price elasticity with IV
#' ### Reminder
#'
#' Last priceの弾力性を推定するIVモデルの結果をリマインドしておく
#+
lpregs <- list(
  reg1 = log_total_g ~ log_pinc_all |
    year + pid | (log_lprice ~ log_price) | pid,
  reg2 = log_total_g ~ log_pinc_all + age + sqage |
    year + pid | (log_lprice ~ log_price) | pid,
  reg3 = log_total_g ~ log_pinc_all + age + sqage + factor(year):factor(educ) |
    year + pid | (log_lprice ~ log_price) | pid,
  reg4 = log_total_g ~ log_pinc_all + age + sqage + factor(year):factor(educ) +
    factor(year):factor(gender) |
    year + pid | (log_lprice ~ log_price) | pid,
  reg5 = log_total_g ~ log_pinc_all + age + sqage + factor(year):factor(educ) +
    factor(year):factor(gender) + factor(year):factor(living_area) |
    year + pid | (log_lprice ~ log_price) | pid
)

covnote2 <- tribble(
  ~vars, ~stat, ~reg1, ~reg2, ~reg3, ~reg4, ~reg5,
  "Individual FE", "vars", "Y", "Y", "Y", "Y", "Y",
  "Time FE", "vars", "Y", "Y", "Y", "Y", "Y",
  "Age", "vars", "N", "Y", "Y", "Y", "Y",
  "Year x Education", "vars", "N", "N", "Y", "Y", "Y",
  "Year x Gender", "vars", "N", "N", "N", "Y", "Y",
  "Year x Resident Area", "vars", "N", "N", "N", "N", "Y"
)

#+
est_lpregs <- lpregs %>% purrr::map(~ felm(., data = df))

fullset_tab(est_lpregs, keep_coef = "log_lprice")$set %>%
  bind_rows(covnote2) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    title = "Full sample",
    col.names = c("", sprintf("(%1d)", 1:5)),
    align = "lccccc",
    format = "html"
  ) %>%
  add_header_above(
   c("", "log(Giving amount)" = 5),
   escape = FALSE
  )

#+
est_lpregs_int <- lpregs %>%
  purrr::map(
    ~felm(., data = df %>% dplyr::filter(i_ext_giving == 1))
  )

fullset_tab(est_lpregs_int, keep_coef = "log_lprice")$set %>%
  bind_rows(covnote2) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    title = "Intentive margin (Those who give)",
    col.names = c("", sprintf("(%1d)", 1:5)),
    align = "lccccc",
    format = "html"
  ) %>%
  add_header_above(
   c("", "log(Giving amount)" = 5),
   escape = FALSE
  )

#+
est_lpregs_ext <- lpregs %>%
  purrr::map(~ update(as.Formula(.), i_ext_giving ~ .)) %>%
  purrr::map(~ felm(., data = df))

fullset_tab(est_lpregs_ext, keep_coef = "log_lprice")$set %>%
  bind_rows(covnote2) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    title = "Extensive margin (Whether to give)",
    col.names = c("", sprintf("(%1d)", 1:5)),
    align = "lccccc",
    format = "html"
  ) %>%
  add_header_above(
   c("", "1 = Give" = 5),
   escape = FALSE
  )

#' ### Last price with ivにdistance to next lower blacketをダイレクトにコントロールする
#'
#' next lower blacketまでの距離を共変量としてコントールしてLast priceの弾力性を推定する
#' ただし、2014年以降のtax credit制度ではこの問題は生じないので、
#' `dist_to_next_price = 0`とした
#+
df2 <- df %>%
  mutate(dist_to_next_price = if_else(year >= 2014, 0, dist_to_next_price))

#' Overall elasticityは1\%程度弾力的になる
#+
est_lpregs2 <- lpregs %>%
  purrr::map(~update(as.Formula(.), . ~ . + dist_to_next_price)) %>%
  purrr::map(~felm(., data = df2))

fullset_tab(est_lpregs2, keep_coef = "log_lprice")$set %>%
  bind_rows(covnote2) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    title = "Full sample",
    col.names = c("", sprintf("(%1d)", 1:5)),
    align = "lccccc",
    format = "html"
  ) %>%
  add_header_above(
    c("", "log(Giving amount)" = 5),
    escape = FALSE
  )

#' inventisive-margin elasticityは非弾力的になり、統計的に非有意となる
#+
est_lpregs2_int <- lpregs %>%
  purrr::map(~update(as.Formula(.), . ~ . + dist_to_next_price)) %>%
  purrr::map(
    ~felm(., data = df %>% dplyr::filter(i_ext_giving == 1))
  )

fullset_tab(est_lpregs2_int, keep_coef = "log_lprice")$set %>%
  bind_rows(covnote2) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    title = "Intentisive margin (Those who give)",
    col.names = c("", sprintf("(%1d)", 1:5)),
    align = "lccccc",
    format = "html"
  ) %>%
  add_header_above(
    c("", "log(Giving amount)" = 5),
    escape = FALSE
  )

#' extensive-margin elasticityは弾力的になる
#+
est_lpregs2_ext <- lpregs %>%
  purrr::map(~update(as.Formula(.), i_ext_giving ~ . + dist_to_next_price)) %>%
  purrr::map(~felm(., data = df))

fullset_tab(est_lpregs2_ext, keep_coef = "log_lprice")$set %>%
  bind_rows(covnote2) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    title = "Extensive margin (Whether to give)",
    col.names = c("", sprintf("(%1d)", 1:5)),
    align = "lccccc",
    format = "html"
  ) %>%
  add_header_above(
    c("", "1 = Give" = 5),
    escape = FALSE
  )

#' ### distance to next lower blacketとFirst priceの交差項をIVにした
#'
#' Last priceのIVとして、
#' (1) first price、
#' (2) first priceとdistance to next lower blacketの交差項
#' を用いた
#'
#' 同様に、2014年以降の税額控除ではこの問題が生じないので、
#' `dist_to_next_price = 0`とした
#+
lpregs3 <- list(
  reg1 = log_total_g ~ log_pinc_all |
    year + pid | (log_lprice ~ log_price + log_price:dist_to_next_price) | pid,
  reg2 = log_total_g ~ log_pinc_all + age + sqage |
    year + pid | (log_lprice ~ log_price + log_price:dist_to_next_price) | pid,
  reg3 = log_total_g ~ log_pinc_all + age + sqage + factor(year):factor(educ) |
    year + pid | (log_lprice ~ log_price + log_price:dist_to_next_price) | pid,
  reg4 = log_total_g ~ log_pinc_all + age + sqage + factor(year):factor(educ) +
    factor(year):factor(gender) |
    year + pid | (log_lprice ~ log_price + log_price:dist_to_next_price) | pid,
  reg5 = log_total_g ~ log_pinc_all + age + sqage + factor(year):factor(educ) +
    factor(year):factor(gender) + factor(year):factor(living_area) |
    year + pid | (log_lprice ~ log_price + log_price:dist_to_next_price) | pid
)

#' Overall elasticityは以前の結果とさほど変わらない
#+
est_lpregs3 <- lpregs3 %>% purrr::map(~felm(., data = df2))

fullset_tab(est_lpregs3, keep_coef = "log_lprice")$set %>%
  bind_rows(covnote2) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    title = "Full sample",
    col.names = c("", sprintf("(%1d)", 1:5)),
    align = "lccccc",
    format = "html"
  ) %>%
  add_header_above(
    c("", "log(Giving amount)" = 5),
    escape = FALSE
  )

#' Intesive-margin elasticityはやっぱり変わらない
#+
est_lpregs3_int <- lpregs3 %>%
  purrr::map(~felm(., data = df2 %>% dplyr::filter(i_ext_giving == 1)))

fullset_tab(est_lpregs3_int, keep_coef = "log_lprice")$set %>%
  bind_rows(covnote2) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    title = "Intensive margin (Those who give)",
    col.names = c("", sprintf("(%1d)", 1:5)),
    align = "lccccc",
    format = "html"
  ) %>%
  add_header_above(
    c("", "log(Giving amount)" = 5),
    escape = FALSE
  )

#' Extensive-margin elasticityもやっぱり変わりない
#+
est_lpregs3_ext <- lpregs3 %>%
  purrr::map(~update(Formula(.), i_ext_giving ~ .)) %>%
  purrr::map(~felm(., data = df2))

fullset_tab(est_lpregs3_ext, keep_coef = "log_lprice")$set %>%
  bind_rows(covnote2) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    title = "Extensive margin (Whether to give)",
    col.names = c("", sprintf("(%1d)", 1:5)),
    align = "lccccc",
    format = "html"
  ) %>%
  add_header_above(
    c("", "1 = Give" = 5),
    escape = FALSE
  )
