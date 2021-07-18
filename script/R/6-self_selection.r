#' Issue #76: ITTの問題を克服するために、PSM-DIDのストラテジーを利用した推定
#'
#' パッケージのロード
#+
library(xfun)
xfun::pkg_attach2(c("readstata13", "tidyverse", "rlist"))
xfun::pkg_attach2(c("plm", "lmtest", "sandwich", "lfe", "Formula", "censReg"))
xfun::pkg_attach2("kableExtra")

lapply(Sys.glob(file.path("script/R/functions", "*.r")), source)

#' データのロード
#+
df <- read.dta13("data/shaped.dta") %>%
  data.frame() %>%
  mutate(
    log_price = log(price),
    log_lprice = log(lprice),
    log_iv1price = log(iv1price),
    log_iv2price = log(iv2price),
    log_iv3price = log(iv3price),
    log_total_g = log(i_total_giving + 1),
    log_pinc_all = log(lincome + 100000),
    ext_benefit = if_else(year >= 2014, ext_credit_giving, ext_deduct_giving)
  ) %>%
  filter(year >= 2012 & age >= 24)

#'
#' ## 寄付の税申告ファクトの整理
#'
#' - 2012~2013年：所得控除の申請をしたかどうか
#' - 2014年：税額控除の申請をしたかどうか
#'
#' 以下の表はそのクロス集計
#+
df %>%
  with(table(year, ext_benefit, useNA = "always")) %>%
  kable(
    col.names = c("Not receive benefit", "Receive benefit", "NA"),
    align = "ccc"
  ) %>%
  kable_styling()

#'
#' 2013年の相対寄付価格と比較して、
#' 2014年の制度改革によって、寄付価格が増加・変化しない・減少するグループに分けて、
#' 税控除申請比率の推移を確認する。
#'
#' この図のメッセージ
#'
#' - 2013年の相対寄付価格が低いほど、申請比率は高い（グループ間比較）
#' - 2014年の制度改革によって相対寄付価格が減少したグループは控除申請比率が高まった（前後比較）
#'
#+
df %>%
  group_by(year, credit_neutral, credit_benefit, credit_loss) %>%
  summarize_at(vars(ext_benefit), list(~ mean(., na.rm = TRUE))) %>%
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
  ggplot(aes(x = year, y = ext_benefit, group = group)) +
  geom_point(aes(shape = group), color = "black", size = 3) +
  geom_line(aes(linetype = group), size = 1) +
  geom_vline(aes(xintercept = 2013.5)) +
  scale_x_continuous(breaks = seq(2012, 2018, 1)) +
  labs(y = "share of receiving tax benefit", linetype = "", shape = "") +
  ggtemp()

#' ## 雇用形態を操作変数としてIV推定（Panel data）
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
#' `p_aa005`で常用職かどうかのダミー変数を作成する。`paa008`は共変量としてコントロールする
#+
original <- read.dta13("data/merge/merge.dta") %>%
  data.frame() %>%
  dplyr::select(pid, year, p_aa005, paa008) %>%
  mutate(employee = if_else(p_aa005 == 1, 1, 0)) %>%
  rename(indust = paa008) %>%
  dplyr::select(-p_aa005)

df <- df %>%
  left_join(original, by = c("pid", "year"))

#'
#' employeeかどうかで控除申請に差があるかどうかを記述統計で確認する
#'
#' この図のメッセージ：self-employedと比較して、employeeの方が申請している
#'
#+
df %>%
  dplyr::filter(!is.na(employee)) %>%
  group_by(year, employee) %>%
  summarize_at(vars(ext_benefit), list(~ mean(., na.rm = TRUE))) %>%
  mutate(employee = factor(employee)) %>%
  ggplot(aes(x = year, y = ext_benefit, group = employee)) +
  geom_point(aes(shape = employee), color = "black", size = 3) +
  geom_line(aes(linetype = employee), size = 1) +
  geom_vline(aes(xintercept = 2013.5)) +
  scale_x_continuous(breaks = seq(2012, 2018, 1)) +
  labs(y = "share of receiving tax benefit") +
  ggtemp()

#'
#' ### First-stage estimation
#'
#' 2014~2017年に限定してemployeeが申請控除に与える影響を線形確率モデルで推定する
#' はじめに、雇用形態にwithin-variationがある人の割合を見ておく
#'
#+
df %>%
  group_by(pid) %>%
  summarize_at(vars(employee), list(~sd(., na.rm = TRUE))) %>%
  mutate(employee = if_else(employee > 0, 1, 0)) %>%
  ungroup() %>%
  with(table(employee)) %>%
  kable() %>%
  kable_styling()

#'
#' 申請控除の線形確率モデルのメッセージ：
#' employeeは控除申請に正の影響を与えるが、個人固定効果を加えるとその効果がなくなる
#'
#+
xlist <- list(
  ~ employee,
  ~ employee + log_pinc_all + age + sqage +
    factor(year):factor(educ) + factor(year):factor(gender),
  ~ employee + log_pinc_all + age + sqage +
    factor(year):factor(educ) + factor(year):factor(gender) + factor(indust)
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

est_benefit1 <- xlist %>%
  purrr::map(~ est_felm(
    y = ext_benefit ~ ., x = .,
    fixef = ~ year, cluster = ~ pid,
    data = subset(df, 2014 <= year & year <= 2017)
  ))

est_benefit2 <- xlist %>%
  purrr::map(~ est_felm(
    y = ext_benefit ~ ., x = .,
    fixef = ~year + pid, cluster = ~pid,
    data = subset(df, 2014 <= year & year <= 2017)
  ))

list(est_benefit1, est_benefit2) %>%
  purrr::map(~ felm_regtab(., keep_coef = "employee")) %>%
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

#' ### Panel IV Results
#'
#' Panel IVのメッセージ：
#'個人固定効果を除いてもtax receiveの効果は統計的に非有意であった。
#'
#+
estiv_benefit1 <- xlist %>%
  purrr::map(~ est_felm(
    y = log_total_g ~ .,
    x = update(., ~ . - employee),
    z = ext_benefit ~ employee,
    fixef = ~ year, cluster = ~ hhid,
    data = df
  ))

estiv_benefit2 <- xlist %>%
  purrr::map(~ est_felm(
    y = log_total_g ~ .,
    x = update(., ~ . - employee),
    z = ext_benefit ~ employee,
    fixef = ~year + pid, cluster = ~ hhid,
    data = df
  ))

ivfstat <- list(estiv_benefit1, estiv_benefit2) %>%
  purrr::map_depth(2, ~ .$stage1$iv1fstat$ext_benefit["F"]) %>%
  purrr::map(~ as_vector(.)) %>%
  as_vector() %>%
  matrix(nrow = 1) %>%
  data.frame() %>%
  setNames(paste0("reg", seq_len(length(xlist) * 2)))

ivfstat_tab <- ivfstat %>%
  mutate_all(list(~ sprintf("%1.3f", .))) %>%
  mutate(vars = "Fstat of IV", stat = "vars")

waldtest <- list(estiv_benefit1, estiv_benefit2) %>%
  purrr::map_depth(2, ~ felm_wald(
    .,
    hypo = list("Implied elasticity" = "a * `ext_benefit(fit)`"),
    args = list(a = 1 / log(1 - 0.15))
  )) %>%
  purrr::map(~ reduce(., full_join, by = c("vars", "stat"))) %>%
  reduce(full_join, by = c("vars", "stat")) %>%
  setNames(c("vars", "stat", paste0("reg", seq_len(length(xlist) * 2))))

xlist_duptab <- xlist_tab %>%
  full_join(xlist_tab, by = c("vars", "stat")) %>%
  setNames(c("vars", "stat", paste0("reg", seq_len(length(xlist) * 2))))

list(estiv_benefit1, estiv_benefit2) %>%
  flatten() %>%
  felm_regtab(
    keep_coef = "ext_benefit",
    label_coef = list("`ext_benefit(fit)`" = "Receive tax benefit")
  ) %>%
  regtab_addline(list(waldtest, ivfstat_tab, xlist_duptab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist) * 2))),
    align = "lcccccc"
  ) %>%
  add_header_above(c("", "w/o individual FE" = 3, "w/ individual FE" = 3)) %>%
  kable_styling()

#' ## 雇用形態を操作変数としてIV推定（Cross-sectional data）
#'
#' 2012~2018年までのデータを用いた価格弾力性の推定（baseline model）を用いて、
#' Fixed effectを推定する。
#' これを共変量でダイレクトのコントロールしてみる。
#+
overall <- est_felm(
  y = log_total_g ~ .,
  x = ~ log_price + log_pinc_all + age + sqage + factor(year):factor(educ) +
    factor(year):factor(gender) + factor(year):factor(living_area),
  fixef = ~ year + pid, cluster = ~ pid,
  data = df
)

fedt <- getfe(overall) %>%
  tibble() %>%
  dplyr::filter(fe == "pid") %>%
  mutate(pid = as.numeric(as.character(idx))) %>%
  dplyr::select(effect, pid)

df <- df %>% left_join(fedt, by = "pid")

#'
#' Panel IVではなく、2014~2017年の各年でデータを分割してIV推定を行う
#'
#+
yeq <- log_total_g ~ .
xeq <- ~ employee + log_pinc_all + age + sqage + effect +
  factor(educ) + factor(gender) + factor(indust)
zeq <- ext_benefit ~ employee

estiv3_benefit <- df %>%
  dplyr::filter(2014 <= year & year <= 2017) %>%
  group_by(year) %>%
  do(iv = est_felm(y = yeq, x = xeq, z = zeq, data = .))

estiv3_fstat <- estiv3_benefit$iv %>%
  purrr::map(~ .$stage1$iv1fstat$ext_benefit["F"]) %>%
  as_vector() %>%
  matrix(nrow = 1) %>%
  data.frame() %>%
  setNames(paste0("reg", seq_len(nrow(estiv3_benefit))))

iv3fstat_tab <- estiv3_fstat %>%
  mutate_all(list(~ sprintf("%1.3f", .))) %>%
  mutate(vars = "Fstat of IV", stat = "vars")

estiv3_benefit$iv %>%
  felm_regtab(
    .,
    keep_coef = "ext_benefit",
    label_coef = list("`ext_benefit(fit)`" = "Receive tax benefit")
  ) %>%
  regtab_addline(list(iv3fstat_tab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("Year = %4d", 2014:2017)),
    align = "lcccc"
  ) %>%
  kable_styling()
