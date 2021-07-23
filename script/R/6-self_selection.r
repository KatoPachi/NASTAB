#' Issue #76: ITTの問題を克服するために、PSM-DIDのストラテジーを利用した推定
#'
#' パッケージのロード
#+
library(xfun)
xfun::pkg_attach2(c("tidyverse", "rlist"))
xfun::pkg_attach2(c("lmtest", "sandwich", "lfe", "Formula", "fixest"))
xfun::pkg_attach2("kableExtra")

lapply(Sys.glob(file.path("script/R/functions", "*.r")), source)

#' データのロード
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
#' ## 寄付の税申告ファクトの整理
#'
#' 勤労所得もしくは総合所得において寄付の控除申告をしたかどうかをクロス集計で確認する。
#' 勤労所得もしくは控除申告がN/Aの扱いは以下の通り
#'
#' - 勤労所得もしくは総合所得が0ならば、控除申告はなしとみなす
#' - それ以外の場合、欠損値としてみなす（「回答したくない」と「申告していない」を区別できないから）
#'
#' **メッセージ：税制改革以降のtax reportは過半数を下回る。**
#+
df %>%
  with(table(year, ext_benefit_tl, useNA = "always")) %>%
  kable(
    col.names = c("Not tax report", "Tax report", "NA"),
    align = "ccc"
  ) %>%
  kable_styling()

#'
#' 2013年の相対寄付価格と比較して、
#' 2014年の制度改革によって、寄付価格が増加・変化しない・減少するグループに分けて、
#' 税控除申請比率の推移を確認する。
#'
#' **メッセージ：2013年の相対寄付価格が低いほど、申請比率は高い**
#'
#+
df %>%
  group_by(year, credit_neutral, credit_benefit, credit_loss) %>%
  summarize_at(vars(ext_benefit_tl), list(~ mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(group = case_when(
    credit_loss == 1 ~ "loss",
    credit_neutral == 1 ~ "neutral",
    credit_benefit == 1 ~ "benefit"
  )) %>%
  mutate(group = factor(
    group,
    levels = c("loss", "neutral", "benefit"),
    labels = c(
      "2013 giving price < 0.85",
      "2013 giving price = 0.85",
      "2013 giving price > 0.85"
    )
  )) %>%
  filter(!is.na(group)) %>%
  ggplot(aes(x = year, y = ext_benefit_tl, group = group)) +
  geom_point(aes(shape = group), color = "black", size = 3) +
  geom_line(aes(linetype = group), size = 1) +
  geom_vline(aes(xintercept = 2013.5)) +
  scale_x_continuous(breaks = seq(2012, 2018, 1)) +
  labs(y = "share of tax report", linetype = "", shape = "") +
  ggtemp()

#'
#' 以下の就業形態に関する変数を用いる
#'
#' - `p_aa005`：雇用形態（1=常用職2=臨時職日雇い3=自営業4=無給家族従事者）
#' - `paa008`：産業
#'     - 農業
#'     - 林業や漁業
#'     - 鉱業
#'     - 製造業
#'     - 電気、ガスおよび水道事業
#'     - 下水∙、廃棄物処理、原料再生および環境ボクウォンオプ
#'     - 建設業
#'     - 卸売や小売業
#'     - 運輸業
#'     - 宿泊と飲食店業
#'     - 出版、映像、放送通信や情報サービス業
#'     - 金融および保険業
#'     - 不動産業及び賃貸業
#'     - 専門科学および技術サービス業
#'     - 事業施設管理および事業支援サービス業
#'     - 公共行政、国防や、社会保障、行政
#'     - 教育サービス業
#'     - 保険業や社会福祉サービス業
#'     - 芸術、スポーツや余暇関連サービス業
#'     - 協会や団体、修理および個人サービス業
#'     - 世帯内雇用活動と違って分類されていない者が消費、生産活動
#'     - 国際および外国機関
#'
#' `p_aa005`で常用職かどうかのダミー変数を作成する。`paa008`は共変量としてコントロールする。
#' employeeかどうかで控除申請に差があるかどうかを記述統計で確認する。
#'
#' **メッセージ：self-employedと比較して、employeeの方が申請している**
#'
#+
df %>%
  dplyr::filter(!is.na(employee)) %>%
  group_by(year, employee) %>%
  summarize_at(vars(ext_benefit_tl), list(~ mean(., na.rm = TRUE))) %>%
  mutate(employee = factor(employee)) %>%
  ggplot(aes(x = year, y = ext_benefit_tl, group = employee)) +
  geom_point(aes(shape = employee), color = "black", size = 3) +
  geom_line(aes(linetype = employee), size = 1) +
  geom_vline(aes(xintercept = 2013.5)) +
  scale_x_continuous(breaks = seq(2012, 2018, 1)) +
  labs(y = "share of tax report") +
  ggtemp()

#'
#' ## Panel IV: Endog var is tax report x giving price and IV is employed dummy.
#'
#' $\log g_{it}$を寄付の対数値、$\ln p_{it}$は寄付の相対価格（first price）とする。
#' $D_{it}$はtax reportのダミー変数とする。
#' Panel IVの推定において、個人固定効果$\alpha$と時間固定効果$\lambda$はコントロールする。
#' 仮説検定では、個人レベルでクラスターした標準誤差を使用する。
#'
#' Second stage:
#'
#' $$
#' \log g_{it}
#' = \alpha_{2i} + \delta D_{it} \log p_{it} + X_{it} \beta_2
#' + \lambda_{2t} + \epsilon_{it}
#' $$
#'
#' First stage:
#'
#' $$
#' D_{it} \log p_{it}
#' = \alpha_{1i} + \gamma \text{Employed}_{it} + X_{it} \beta_1
#' + \lambda_{1t} + \eta_{it}
#' $$
#'
#' Panel first-stageのメッセージ
#' - 自営業者より労働者はtax reportをして寄付の相対価格を下げている
#' - 個人固定効果をコントロールしても統計的に有意（F-値は最低でも70）
#'
#+
xlist <- list(
  ~ employee,
  ~ employee + log_pinc_all + age + sqage +
    factor(year):factor(educ) + factor(year):factor(gender),
  ~ employee + log_pinc_all + age + sqage +
    factor(year):factor(educ) + factor(year):factor(gender) +
    factor(year):factor(indust)
)

xlist_tab <- tribble(
  ~vars, ~stat, ~reg1, ~reg2, ~reg3,
  "Time FE", "vars", "Y", "Y", "Y",
  "log(income)", "vars", "N", "Y", "Y",
  "Age", "vars", "N", "Y", "Y",
  "Year x Education", "vars", "N", "Y", "Y",
  "Year x Gender", "vars", "N", "Y", "Y",
  "Year x Resident Area", "vars", "N", "Y", "Y",
  "Dummy of industry", "vars", "N", "N", "Y"
)

est_xreport1 <- xlist %>%
  purrr::map(~ fit_fixest(
    y = int_price_benefit ~ ., x = .,
    fixef = ~ year, cluster = ~ pid,
    data = subset(df, year <= 2017)
  ))

est_xreport2 <- xlist %>%
  purrr::map(~ fit_fixest(
    y = int_price_benefit ~ ., x = .,
    fixef = ~year + pid, cluster = ~pid,
    data = subset(df, year <= 2017)
  ))

list(est_xreport1, est_xreport2) %>%
  purrr::map(~ regtab_fixest(., keep_coef = "employee")) %>%
  purrr::map(~ regtab_addline(., list(xlist_tab))) %>%
  reduce(full_join, by = c("vars", "stat")) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist) * 2))),
    align = "lcccccc"
  ) %>%
  add_header_above(c("", "w/o individual FE" = 3, "w/ individual FE" = 3)) %>%
  kable_styling()

#'
#' Panel IVの推定結果のメッセージ
#' - 弾力性は10%。1%の相対価格の上昇が寄付額を10%程度下げる
#' - ITTで考えていたよりもかなり弾力的な結果となる
#+
ivest_xreport1 <- xlist %>%
  purrr::map(~ fit_fixest(
    y = log_total_g ~ .,
    x = update(., ~ . - employee),
    z = int_price_benefit ~ employee,
    fixef = ~ year, cluster = ~ pid,
    data = subset(df, year <= 2017)
  ))

ivest_xreport2 <- xlist %>%
  purrr::map(~ fit_fixest(
    y = log_total_g ~ .,
    x = update(., ~ . - employee),
    z = int_price_benefit ~ employee,
    fixef = ~year + pid, cluster = ~ pid,
    data = subset(df, year <= 2017)
  ))

ivfstat <- list(ivest_xreport1, ivest_xreport2) %>%
  purrr::map_depth(2, ~ fitstat(., ~ ivwald)$ivwald1$stat) %>%
  purrr::map(~ as_vector(.)) %>%
  as_vector() %>%
  matrix(nrow = 1) %>%
  data.frame() %>%
  setNames(paste0("reg", seq_len(length(xlist) * 2)))

ivfstat_tab <- ivfstat %>%
  mutate_all(list(~ sprintf("%1.3f", .))) %>%
  mutate(vars = "Fstat of IV", stat = "vars")

xlist_duptab <- xlist_tab %>%
  full_join(xlist_tab, by = c("vars", "stat")) %>%
  setNames(c("vars", "stat", paste0("reg", seq_len(length(xlist) * 2))))

list(ivest_xreport1, ivest_xreport2) %>%
  flatten() %>%
  regtab_fixest(
    keep_coef = "int_price_benefit",
    label_coef = list("fit_int_price_benefit" = "Report x log(first price)")
  ) %>%
  regtab_addline(list(ivfstat_tab, xlist_duptab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist) * 2))),
    align = "lcccccc"
  ) %>%
  add_header_above(c("", "w/o individual FE" = 3, "w/ individual FE" = 3)) %>%
  kable_styling()

#'
#' extensive-margin elasticityのPanel IVの結果
#'
#' - 価格1%上昇によって、寄付する確率を1%下げる
#' - この結果はITTで考えた推定結果と非常に似ている。
#'
#+
ivest_xreport1_ext <- xlist %>%
  purrr::map(~ fit_fixest(
    y = i_ext_giving ~ .,
    x = update(., ~ . - employee),
    z = int_price_benefit ~ employee,
    fixef = ~year, cluster = ~pid,
    data = subset(df, year <= 2017)
  ))

ivest_xreport2_ext <- xlist %>%
  purrr::map(~ fit_fixest(
    y = i_ext_giving ~ .,
    x = update(., ~ . - employee),
    z = int_price_benefit ~ employee,
    fixef = ~ year + pid, cluster = ~pid,
    data = subset(df, year <= 2017)
  ))

waldtest_ext <- list(ivest_xreport1_ext, ivest_xreport2_ext) %>%
  purrr::map_depth(2, ~ tibble(
    vars = "Implied price elasticity",
    coef = coef(.)["fit_int_price_benefit"],
    se = fixest::se(.)["fit_int_price_benefit"],
    invmu = 1 / mean(model.matrix(., type = "lhs")),
    p = fixest::pvalue(.)["fit_int_price_benefit"]
  )) %>%
  purrr::map_depth(2, function(x)
    x %>%
      mutate(
        coef = case_when(
          p <= .01 ~ sprintf("%1.3f***", coef / invmu),
          p <= .05 ~ sprintf("%1.3f**", coef / invmu),
          p <= .1 ~ sprintf("%1.3f*", coef / invmu),
          TRUE ~ sprintf("%1.3f", coef / invmu)
        ),
        se = sprintf("(%1.3f)", se / invmu)
      ) %>%
      dplyr::select(-p, -invmu) %>%
      pivot_longer(-vars, names_to = "stat", values_to = "val")
  ) %>%
  purrr::map(~ reduce(., full_join, by = c("vars", "stat"))) %>%
  reduce(full_join, by = c("vars", "stat")) %>%
  setNames(c("vars", "stat", paste0("reg", seq_len(length(xlist) * 2)))) %>%
  bind_rows(
    c(vars = "", stat = "", reg1 = "", reg2 = "",
      reg3 = "", reg4 = "", reg5 = "", reg6 = ""),
    .
  )

ivfstat_ext <- list(ivest_xreport1_ext, ivest_xreport2_ext) %>%
  purrr::map_depth(2, ~ fitstat(., ~ ivwald)$ivwald1$stat) %>%
  purrr::map(~ as_vector(.)) %>%
  as_vector() %>%
  matrix(nrow = 1) %>%
  data.frame() %>%
  setNames(paste0("reg", seq_len(length(xlist) * 2)))

ivfstat_ext_tab <- ivfstat_ext %>%
  mutate_all(list(~ sprintf("%1.3f", .))) %>%
  mutate(vars = "Fstat of IV", stat = "vars")

list(ivest_xreport1_ext, ivest_xreport2_ext) %>%
  flatten() %>%
  regtab_fixest(
    keep_coef = "int_price_benefit",
    label_coef = list("fit_int_price_benefit" = "Report x log(first price)")
  ) %>%
  regtab_addline(list(waldtest_ext, ivfstat_tab, xlist_duptab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist) * 2))),
    align = "lcccccc"
  ) %>%
  add_header_above(c("", "w/o individual FE" = 3, "w/ individual FE" = 3)) %>%
  kable_styling()

#'
#' intensive-margin elasticityのPanel IVの結果
#'
#' - 個人固定効果を除くと価格の上昇が寄付の増加を促す（個人固定効果を入れていないのが原因？何らかの解釈が可能？）
#' - 個人固定効果を入れると統計的に非有意となる（弱操作変数の影響とは考えにくい）
#'
#+
ivest_xreport1_int <- xlist %>%
  purrr::map(~ fit_fixest(
    y = log_total_g ~ .,
    x = update(., ~ . - employee),
    z = int_price_benefit ~ employee,
    fixef = ~year, cluster = ~pid,
    data = subset(df, i_ext_giving == 1 & year <= 2017)
  ))

ivest_xreport2_int <- xlist %>%
  purrr::map(~ fit_fixest(
    y = log_total_g ~ .,
    x = update(., ~ . - employee),
    z = int_price_benefit ~ employee,
    fixef = ~ year + pid, cluster = ~pid,
    data = subset(df, i_ext_giving == 1 & year <= 2017)
  ))

ivfstat_int <- list(ivest_xreport1_int, ivest_xreport2_int) %>%
  purrr::map_depth(2, ~ fitstat(., ~ ivwald)$ivwald1$stat) %>%
  purrr::map(~ as_vector(.)) %>%
  as_vector() %>%
  matrix(nrow = 1) %>%
  data.frame() %>%
  setNames(paste0("reg", seq_len(length(xlist) * 2)))

ivfstat_int_tab <- ivfstat_int %>%
  mutate_all(list(~ sprintf("%1.3f", .))) %>%
  mutate(vars = "Fstat of IV", stat = "vars")

list(ivest_xreport1_int, ivest_xreport2_int) %>%
  flatten() %>%
  regtab_fixest(
    keep_coef = "int_price_benefit",
    label_coef = list("fit_int_price_benefit" = "Report x log(first price)")
  ) %>%
  regtab_addline(list(ivfstat_int_tab, xlist_duptab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist) * 2))),
    align = "lcccccc"
  ) %>%
  add_header_above(c("", "w/o individual FE" = 3, "w/ individual FE" = 3)) %>%
  kable_styling()


#'
#' ## Panel IV: Endog var is tax report and IV is employed dummy.
#'
#' $\log g_{it}$を寄付の対数値、$\ln p_{it}$は寄付の相対価格（first price）とする。
#' $D_{it}$はtax reportのダミー変数とし、first-stageの予測値を$\hat{D}_{it}$とする。
#' Panel IVの推定において、個人固定効果$\alpha$と時間固定効果$\lambda$はコントロールする。
#' 仮説検定では、個人レベルでクラスターした標準誤差を使用する。
#'
#' Second stage:
#'
#' $$
#' \log g_{it}
#' = \alpha_{2i} + \delta \hat{D}_{it} \log p_{it} + X_{it} \beta_2
#' + \lambda_{2t} + \epsilon_{it}
#' $$
#'
#' First stage:
#'
#' $$
#' D_{it}
#' = \alpha_{1i} + \gamma \text{Employed}_{it} + X_{it} \beta_1
#' + \lambda_{1t} + \eta_{it}
#' $$
#'
#' Panel first-stageのメッセージ
#' - 自営業者より労働者がtax reportする確率は約50%高い
#' - 個人固定効果をコントロールしても統計的に有意（F-値は最低でも80）
#'
#+
est_report1 <- xlist %>%
  purrr::map(~ fit_fixest(
    y = ext_benefit_tl ~ ., x = .,
    fixef = ~ year, cluster = ~ pid,
    data = subset(df, year <= 2017)
  ))

est_report2 <- xlist %>%
  purrr::map(~ fit_fixest(
    y = ext_benefit_tl ~ ., x = .,
    fixef = ~year + pid, cluster = ~pid,
    data = subset(df, year <= 2017)
  ))

list(est_report1, est_report2) %>%
  purrr::map(~ regtab_fixest(., keep_coef = "employee")) %>%
  purrr::map(~ regtab_addline(., list(xlist_tab))) %>%
  reduce(full_join, by = c("vars", "stat")) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist) * 2))),
    align = "lcccccc"
  ) %>%
  add_header_above(c("", "w/o individual FE" = 3, "w/ individual FE" = 3)) %>%
  kable_styling()

#'
#' 上の表の(6)の予測値を$\hat{D}_{it}$とする。
#+
df$p_ext_benefit_tl <- predict(est_report2[[3]], df)

#'
#' Panel IVの推定結果のメッセージ
#' - ITTとして推定した弾性値（約1%）より若干弾力的である
#' - Tax reportをしていない人の価格は必ず$\log 1$であることを考えれば、納得できる結果
#+
ivest_report1 <- xlist %>%
  purrr::map(~ fit_fixest(
    y = log_total_g ~ .,
    x = update(., ~ . - employee + log_price:p_ext_benefit_tl),
    fixef = ~ year, cluster = ~ pid,
    data = subset(df, year <= 2017)
  ))

ivest_report2 <- xlist %>%
  purrr::map(~ fit_fixest(
    y = log_total_g ~ .,
    x = update(., ~ . - employee + log_price:p_ext_benefit_tl),
    fixef = ~year + pid, cluster = ~ pid,
    data = subset(df, year <= 2017)
  ))

list(ivest_report1, ivest_report2) %>%
  flatten() %>%
  regtab_fixest(
    keep_coef = "log_price",
    label_coef = list(
      "log_price:p_ext_benefit_tl" = "Propensity of Report x log(first price)"
    )
  ) %>%
  regtab_addline(list(xlist_duptab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist) * 2))),
    align = "lcccccc"
  ) %>%
  add_header_above(c("", "w/o individual FE" = 3, "w/ individual FE" = 3)) %>%
  kable_styling()

#'
#' extensive-margin elasticityのPanel IVの結果
#'
#' - ITTとして想定したベースラインの結果（-1.4 ~ -1.2%）より、非弾力的になる（0.1%）
#' - extensive-marginの弾力性はtax reportをしていない人に特に強い影響を与えるのか？
#'
#+
ivest_report1_ext <- xlist %>%
  purrr::map(~ fit_fixest(
    y = i_ext_giving ~ .,
    x = update(., ~ . - employee + log_price:p_ext_benefit_tl),
    fixef = ~year, cluster = ~pid,
    data = subset(df, year <= 2017)
  ))

ivest_report2_ext <- xlist %>%
  purrr::map(~ fit_fixest(
    y = i_ext_giving ~ .,
    x = update(., ~ . - employee + log_price:p_ext_benefit_tl),
    fixef = ~ year + pid, cluster = ~pid,
    data = subset(df, year <= 2017)
  ))

waldtest_ext <- list(ivest_report1_ext, ivest_report2_ext) %>%
  purrr::map_depth(2, ~ tibble(
    vars = "Implied price elasticity",
    coef = coef(.)["log_price:p_ext_benefit_tl"],
    se = fixest::se(.)["log_price:p_ext_benefit_tl"],
    invmu = 1 / mean(model.matrix(., type = "lhs")),
    p = fixest::pvalue(.)["log_price:p_ext_benefit_tl"]
  )) %>%
  purrr::map_depth(2, function(x)
    x %>%
      mutate(
        coef = case_when(
          p <= .01 ~ sprintf("%1.3f***", coef / invmu),
          p <= .05 ~ sprintf("%1.3f**", coef / invmu),
          p <= .1 ~ sprintf("%1.3f*", coef / invmu),
          TRUE ~ sprintf("%1.3f", coef / invmu)
        ),
        se = sprintf("(%1.3f)", se / invmu)
      ) %>%
      dplyr::select(-p, -invmu) %>%
      pivot_longer(-vars, names_to = "stat", values_to = "val")
  ) %>%
  purrr::map(~ reduce(., full_join, by = c("vars", "stat"))) %>%
  reduce(full_join, by = c("vars", "stat")) %>%
  setNames(c("vars", "stat", paste0("reg", seq_len(length(xlist) * 2)))) %>%
  bind_rows(
    c(vars = "", stat = "", reg1 = "", reg2 = "",
      reg3 = "", reg4 = "", reg5 = "", reg6 = ""),
    .
  )

list(ivest_report1_ext, ivest_report2_ext) %>%
  flatten() %>%
  regtab_fixest(
    keep_coef = "log_price",
    label_coef = list(
      "log_price:p_ext_benefit_tl" = "Propensity of Report x log(first price)"
    )
  ) %>%
  regtab_addline(list(waldtest_ext, xlist_duptab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist) * 2))),
    align = "lcccccc"
  ) %>%
  add_header_above(c("", "w/o individual FE" = 3, "w/ individual FE" = 3)) %>%
  kable_styling()

#'
#' intensive-margin elasticityのPanel IVの結果
#'
#' - 個人固定効果を加えると、ITTとして想定した弾性値（-1 ~ -0.5%）と似たような結果を取る
#' - Intenstive-margin elasticityはtax reportをする人に特に影響を与えるのか？
#'
#+
ivest_report1_int <- xlist %>%
  purrr::map(~ fit_fixest(
    y = log_total_g ~ .,
    x = update(., ~ . - employee + log_price:p_ext_benefit_tl),
    fixef = ~year, cluster = ~pid,
    data = subset(df, i_ext_giving == 1 & year <= 2017)
  ))

ivest_report2_int <- xlist %>%
  purrr::map(~ fit_fixest(
    y = log_total_g ~ .,
    x = update(., ~ . - employee + log_price:p_ext_benefit_tl),
    fixef = ~ year + pid, cluster = ~pid,
    data = subset(df, i_ext_giving == 1 & year <= 2017)
  ))

list(ivest_report1_int, ivest_report2_int) %>%
  flatten() %>%
  regtab_fixest(
    keep_coef = "log_price",
    label_coef = list(
      "log_price:p_ext_benefit_tl" = "Propensity of Report x log(first price)"
    )
  ) %>%
  regtab_addline(list(xlist_duptab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist) * 2))),
    align = "lcccccc"
  ) %>%
  add_header_above(c("", "w/o individual FE" = 3, "w/ individual FE" = 3)) %>%
  kable_styling()
