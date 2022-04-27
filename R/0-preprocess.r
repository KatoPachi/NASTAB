#'
#'
#+ read-raw
library(here)
source(here("R", "_library.r"))

raw <- haven::read_dta(here("data/merge/merge.dta"), encoding = "utf-8") %>%
  as_tibble

#'
#+ merge-key-variables
merge_key <- raw %>%
  dplyr::select(
    hhid, #世帯ID
    pid, #個人ID
    year, #調査年
    wave #調査wave
  )

#'
#+ ses-variables
ses <- raw %>%
  dplyr::select(
    hhid,
    pid,
    wave,
    year,
    sex = p_pgen, #性別
    age = p_page, #年齢
    educ = p_pedu, #学歴
    area = h_b10, #世帯：地域コード
    work = p_aa200, #就業状態（1=就職、2=専業主婦、3=無職、4=学生）
    position = p_aa005, #従事地位（1=常用、2=臨時、3=自営業、4=無給の家族従事者）cond. p_aa200 == 1
    indust = paa008, #産業コード
    family_position = p_prel, #世帯主との関係コード
    tinc = pinc_all, #昨年1年間の総合所得
    linc = inc_bb1 #昨年1年間の労働所得
  ) %>%
  dplyr::mutate(
    sex = sex - 1,
    sqage = age^2 / 100,
    college = case_when(
      educ == 3 ~ 1,
      !is.na(educ) ~ 0,
      TRUE ~ NA_real_
    ),
    highschool = case_when(
      educ == 2 ~ 1,
      !is.na(educ) ~ 0,
      TRUE ~ NA_real_
    ),
    junior = case_when(
      educ == 1 ~ 1,
      !is.na(educ) ~ 0,
      TRUE ~ NA_real_
    ),
    employee = if_else(position == 1, 1, 0),
    family_position = case_when(
      family_position == 1 ~ 1, #世帯主
      family_position == 2 ~ 2, #配偶者
      family_position %in% c(3, 31:37) ~ 3, #世帯主の子供
      family_position %in% c(4, 41:44) ~ 4, #世帯主の子供の配偶者
      family_position == 5 ~ 5, #世帯主の親
      family_position == 6 ~ 6, #配偶者の親
      family_position %in% c(7, 71:75) ~ 7, #孫とその配偶者
      family_position == 8 ~ 8, #ひ孫とその配偶者
      family_position == 9 ~ 9, #世帯主の祖父母
      family_position == 10 ~ 10, #世帯主の兄弟姉妹
      family_position == 11 ~ 11, #世帯主の兄弟姉妹の子供とその配偶者
      family_position == 12 ~ 12, #世帯主の親の兄弟姉妹とその配偶者
      family_position == 13 ~ 13 #その他
    )
  )

ses_label <- list(
  sqage = "[世帯員] 年齢の二乗",
  college = "[世帯員] 1 = (最終教育水準 = 短大卒以上)",
  highschool = "[世帯員] 1 = (最終教育水準 = 高卒)",
  junior = "[世帯員] 1 = (最終教育水準 = 中卒以下)",
  employee = "[世帯員] 1 = (従事上地位 = 常用職)"
)

for (i in names(ses_label)) {
  attr(ses[[i]], "label") <- ses_label[[i]]
}

#'
#+ tax-relief-variables
relief <- raw %>%
  dplyr::select(
    hhid,
    pid,
    year,
    wave,
    tinc = pinc_all, #昨年1年間の総合所得
    linc = inc_bb1, #昨年1年間の労働所得
    d_deduct_linc = pca201, #労働所得控除の有無
    deduct_linc = pca202, #労働所得控除額
    d_deduct_donate_linc = pca225, #労働所得の寄付金所得控除の有無
    deduct_donate_linc = pca226, #寄付金所得控除額
    d_credit_donate_linc = pca227, #労働所得の寄付金税額控除の有無
    credit_donate_linc = pca228, #寄付金税額控除額
    d_deduct_donate_tinc = pda207, #総合所得の寄付金所得控除の有無
    d_credit_donate_tinc = pda209 #総合所得の寄付金税額控除の有無
  ) %>%
  mutate_at(
    vars(
      d_deduct_linc,
      d_deduct_donate_linc, d_credit_donate_linc,
      d_deduct_donate_tinc, d_credit_donate_tinc,
    ),
    list(~ case_when(
      as.numeric(.) == 2 ~ 0,
      as.numeric(.) == -9 ~ NA_real_,
      TRUE ~ as.numeric(.)
    ))
  ) %>%
  mutate(
    deduct_linc = case_when(
      d_deduct_linc == 0 ~ 0,
      deduct_linc == -9 ~ NA_real_,
      TRUE ~ as.numeric(deduct_linc)
    ),
    d_deduct_donate_linc = case_when(
      !is.na(d_deduct_donate_linc) ~ d_deduct_donate_linc,
      linc == 0 ~ 0
    ),
    d_credit_donate_linc = case_when(
      !is.na(d_credit_donate_linc) ~ d_credit_donate_linc,
      linc == 0 ~ 0
    ),
    d_relief_donate_linc = if_else(
      year >= 2014, d_credit_donate_linc, d_deduct_donate_linc
    ),
    d_deduct_donate_tinc = case_when(
      !is.na(d_deduct_donate_tinc) ~ d_deduct_donate_tinc,
      tinc == 0 ~ 0
    ),
    d_credit_donate_tinc = case_when(
      !is.na(d_credit_donate_tinc) ~ d_credit_donate_tinc,
      tinc == 0 ~ 0
    ),
    d_relief_donate_tinc = if_else(
      year >= 2014, d_credit_donate_tinc, d_deduct_donate_tinc
    ),
    d_relief_donate = case_when(
      is.na(d_relief_donate_linc) ~ d_relief_donate_tinc,
      is.na(d_relief_donate_tinc) ~ d_relief_donate_linc,
      d_relief_donate_tinc + d_relief_donate_linc == 0 ~ 0,
      d_relief_donate_tinc + d_relief_donate_linc != 0 ~ 1
    ),
    deduct_donate_linc = case_when(
      d_deduct_donate_linc == 0 ~ 0,
      deduct_donate_linc == -9 ~ NA_real_,
      TRUE ~ as.numeric(deduct_donate_linc)
    ),
    credit_donate_linc = case_when(
      d_credit_donate_linc == 0 ~ 0,
      credit_donate_linc == -9 ~ NA_real_,
      TRUE ~ as.numeric(credit_donate_linc)
    )
  ) %>%
  dplyr::select(-tinc, -linc)

relief_label <- list(
  d_deduct_linc = "[世帯員]労働所得控除の有無",
  deduct_linc = "[世帯員]労働所得控除額",
  d_deduct_donate_linc = "[世帯員]労働所得に対する寄付金所得控除の有無",
  d_credit_donate_linc = "[世帯員]労働所得に対する寄付金税額控除の有無",
  d_relief_donate_linc = "[世帯員]労働所得に対する寄付金控除の有無",
  d_deduct_donate_tinc = "[世帯員]総合所得に対する寄付金所得控除の有無",
  d_credit_donate_tinc = "[世帯員]総合所得に対する寄付金税額控除の有無",
  d_relief_donate_tinc = "[世帯員]総合所得に対する寄付金控除の有無",
  d_relief_donate = "[世帯員]労働・総合所得に対する寄付金控除の有無",
  deduct_donate_linc = "[世帯員]労働所得に対する寄付金所得控除額",
  credit_donate_linc = "[世帯員]労働所得に対する寄付金税額控除額"
)

for (i in names(relief_label)) {
  attr(relief[[i]], "label") <- relief_label[[i]]
}

#'
#' 寄付データの作成
#+
# 寄付先データ
donate_purpose <- raw %>%
  dplyr::select(
    hhid, pid, year, hcr004, hcr007, hcr010,
    hcr013, hcr016, hcr019
  ) %>%
  tidyr::pivot_longer(hcr004:hcr019, names_prefix = "hcr") %>%
  rename(key = name, purpose = value) %>%
  mutate(
    purpose = as.numeric(purpose),
    key = dplyr::recode(
      key, "004" = "005", "007" = "008", "010" = "011",
      "013" = "014", "016" = "017", "019" = "020"
    ),
    purpose = dplyr::recode(
      purpose,
      "1" = "poliparty",
      "2" = "educ",
      "3" = "welfare",
      "4" = "culture",
      "5" = "religious",
      "6" = "religious_action",
      "7" = "others",
      "-9" = "unknown"
    )
  )

# 寄付支出データ（個人単位）
donate_amount <- raw %>%
  dplyr::select(
    hhid, pid, year,
    hcr005, hcr008, hcr011, hcr014, hcr017, hcr020
  ) %>%
  tidyr::pivot_longer(hcr005:hcr020, names_prefix = "hcr") %>%
  rename(key = name, amount = value)

# 寄付先データと支出データのマージ（個人単位）
donate_member <- donate_purpose %>%
  dplyr::left_join(donate_amount, by = c("hhid", "pid", "year", "key")) %>%
  dplyr::select(-key) %>%
  mutate(amount = case_when(
    is.na(amount) ~ 0,
    amount == -9 ~ NA_real_,
    TRUE ~ amount
  )) %>%
  dplyr::filter(!is.na(amount)) %>%
  dplyr::group_by(hhid, pid, year, purpose) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from = purpose, values_from = amount,
    values_fill = 0, names_prefix = "donate_"
  ) %>%
  dplyr::select(-donate_NA) %>%
  mutate(
    donate = donate_religious + donate_welfare + donate_educ + donate_others +
      donate_religious_action + donate_culture + donate_unknown,
    d_donate = if_else(donate > 0, 1, 0)
  ) %>%
  dplyr::select(hhid, pid, year, donate, d_donate)

# 寄付支出データ（世帯単位）
donate_household <- raw %>%
  dplyr::select(hhid, pid, year, h_exp_cr, hcr001) %>%
  rename(h_donate = h_exp_cr, d_h_donate = hcr001) %>%
  mutate(
    d_h_donate = as.numeric(d_h_donate),
    d_h_donate = if_else(d_h_donate == 2, 0, d_h_donate)
  )

# 個人単位データと世帯単位データのマージ
donate <- donate_member %>%
  dplyr::left_join(donate_household, by = c("hhid", "pid", "year"))

#'
#' 寄付データのラベルを設定
#+
donate_label <- list(
  # donate_religious = "[世帯員]宗教団体に対する寄付（単位:10,000KRW）",
  # donate_welfare = "[世帯員]社会福祉団体に対する寄付（単位:10,000KRW）",
  # donate_educ = "[世帯員]教育団体に対する寄付（単位:10,000KRW）",
  # donate_poliparty = "[世帯員]政治団体に対する寄付（単位:10,000KRW）",
  # donate_culture = "[世帯員]文化団体に対する寄付（単位:10,000KRW）",
  # donate_religious_action = "[世帯員]宗教団体の貧困救済活動に対する寄付（単位:10,000KRW）",
  # donate_others = "[世帯員]その他の団体に対する寄付（単位:10,000KRW）",
  # donate_unknown = "[世帯員]支出先不明の寄付（単位:10,000KRW）",
  donate = "[世帯員]年間寄付金支出総額（単位:10,000KRW）",
  d_donate = "[世帯員]寄付金支出の有無",
  h_donate = "[世帯]年間寄付金支出総額（単位:10,000KRW）",
  d_h_donate = "[世帯]寄付金支出の有無"
)

for (i in names(donate_label)) {
  attr(donate[[i]], "label") <- donate_label[[i]]
}

#'
#' 税率に関するデータの作成
#+
mtr <- readr::read_csv("data/origin/mtrdt.csv")

inc <- ses %>%
  dplyr::select(hhid, pid, year, tinc, linc)

# first marginal tax rate(寄付額を差し引く前の税率)
firstdt <- inc %>%
  dplyr::left_join(mtr, by = "year") %>%
  dplyr::filter(!is.na(linc)) %>%
  dplyr::filter(lower_income_10000won <= linc) %>%
  dplyr::group_by(pid, year) %>%
  dplyr::mutate(first_mtr = max(MTR)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-lower_income_10000won, -MTR, -tinc, -linc) %>%
  dplyr::distinct(.keep_all = TRUE)

# last marginal tax rate(寄付額を差し引いた後の税率)
lastdt <- inc %>%
  dplyr::left_join(donate, by = c("pid", "year", "hhid")) %>%
  dplyr::left_join(mtr, by = "year") %>%
  dplyr::filter(!is.na(linc) & !is.na(donate)) %>%
  dplyr::mutate(subtract_linc = linc - donate) %>%
  dplyr::filter(lower_income_10000won <= subtract_linc) %>%
  dplyr::group_by(pid, year) %>%
  dplyr::mutate(last_mtr = max(MTR)) %>%
  dplyr::ungroup() %>%
  dplyr::select(hhid, pid, year, subtract_linc, last_mtr) %>%
  dplyr::distinct(.keep_all = TRUE)

# 所得のラグ変数を作成
lagdt <- inc %>%
  dplyr::group_by(pid) %>%
  dplyr::mutate(
    linc_l1 = dplyr::lag(linc, order_by = year),
    linc_l2 = dplyr::lag(linc, n = 2, order_by = year),
    linc_l3 = dplyr::lag(linc, n = 3, order_by = year)
  ) %>%
  dplyr::ungroup()

# 1期ラグ所得に基づいた税率の計算
lag1dt <- lagdt %>%
  dplyr::select(hhid, pid, year, linc_l1) %>%
  dplyr::left_join(mtr, by = "year") %>%
  dplyr::filter(!is.na(linc_l1)) %>%
  dplyr::filter(lower_income_10000won <= linc_l1) %>%
  dplyr::group_by(pid, year) %>%
  dplyr::mutate(first_mtr_l1 = max(MTR)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-lower_income_10000won, -MTR) %>%
  dplyr::distinct(.keep_all = TRUE)

# 2期ラグ所得に基づいた税率の計算
lag2dt <- lagdt %>%
  dplyr::select(hhid, pid, year, linc_l2) %>%
  dplyr::left_join(mtr, by = "year") %>%
  dplyr::filter(!is.na(linc_l2)) %>%
  dplyr::filter(lower_income_10000won <= linc_l2) %>%
  dplyr::group_by(pid, year) %>%
  dplyr::mutate(first_mtr_l2 = max(MTR)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-lower_income_10000won, -MTR) %>%
  dplyr::distinct(.keep_all = TRUE)

# 3期ラグ所得に基づいた税率の計算
lag3dt <- lagdt %>%
  dplyr::select(hhid, pid, year, linc_l3) %>%
  dplyr::left_join(mtr, by = "year") %>%
  dplyr::filter(!is.na(linc_l3)) %>%
  dplyr::filter(lower_income_10000won <= linc_l3) %>%
  dplyr::group_by(pid, year) %>%
  dplyr::mutate(first_mtr_l3 = max(MTR)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-lower_income_10000won, -MTR) %>%
  dplyr::distinct(.keep_all = TRUE)

# 2014年税制改革によるトリートメントグループの作成
treatdt <- firstdt %>%
  dplyr::filter(year == 2013) %>%
  dplyr::select(hhid, pid, first_mtr) %>%
  dplyr::mutate(
    credit_benefit = if_else(first_mtr < 0.15, 1, 0),
    credit_neutral = if_else(first_mtr == 0.15, 1, 0),
    credit_loss = if_else(first_mtr > 0.15, 1, 0),
  ) %>%
  dplyr::select(-first_mtr)

# ここまでのデータのマージ
tax <- inc %>%
  dplyr::left_join(firstdt, by = c("hhid", "pid", "year")) %>%
  dplyr::left_join(lastdt, by = c("hhid", "pid", "year")) %>%
  dplyr::left_join(lag1dt, by = c("hhid", "pid", "year")) %>%
  dplyr::left_join(lag2dt, by = c("hhid", "pid", "year")) %>%
  dplyr::left_join(lag3dt, by = c("hhid", "pid", "year")) %>%
  dplyr::left_join(treatdt, by = c("hhid", "pid"))

#'
#' 税率に関する変数ラベルの設定
#+
tax_label <- list(
  first_mtr = "[世帯員]年間労働所得ベースの限界所得税率",
  subtract_linc = "[世帯員]年間労働所得（-寄付額）",
  last_mtr = "[世帯員]寄付を差し引いた年間労働所得ベースの限界所得税率",
  linc_l1 = "[世帯員]年間労働所得（lag = 1）",
  linc_l2 = "[世帯員]年間労働所得（lag = 2）",
  linc_l3 = "[世帯員]年間労働所得（lag = 3）",
  first_mtr_l1 = "[世帯員]年間労働所得ベースの限界所得税率(lag = 1)",
  first_mtr_l2 = "[世帯員]年間労働所得ベースの限界所得税率(lag = 2)",
  first_mtr_l3 = "[世帯員]年間労働所得ベースの限界所得税率(lag = 3)",
  credit_neutral = "[世帯員]税制改正の利益（2013年First MTR = 0.15）",
  credit_benefit = "[世帯員]税制改正の利益(2013年First MTR < 0.15)",
  credit_loss = "[世帯員]税制改正の利益(2013年First MTR > 0.15)"
)

for (i in names(tax_label)) {
  attr(tax[[i]], "label") <- tax_label[[i]]
}

#'
#' ここまでのデータのマージと変数の処理
#+
dt <- donate %>%
  dplyr::left_join(tax, by = c("hhid", "pid", "year")) %>%
  dplyr::left_join(ses, by = c("hhid", "pid", "year")) %>%
  dplyr::select(-tinc.y, -linc.y) %>%
  dplyr::rename(tinc = tinc.x, linc = linc.x)

attr(dt$year, "label") <- "年度（調査年度の前年）"

dt <- dt %>%
  dplyr::mutate(
    employee = if_else(position == 1, 1, 0),
    housewife = if_else(working == 2, 1, 0),
    price = case_when(
      year < 2014 ~ 1 - first_mtr,
      year >= 2014 ~ 1 - 0.15
    ),
    lprice = case_when(
      year < 2014 ~ 1 - last_mtr,
      year >= 2014 ~ 1 - 0.15
    ),
    price_l1_deduct = 1 - first_mtr_l1,
    price_l2_deduct = 1 - first_mtr_l2,
    price_l3_deduct = 1 - first_mtr_l3
  )

dt <- dt %>%
  dplyr::group_by(pid) %>%
  dplyr::mutate(
    price_l1 = dplyr::lag(price, order_by = year),
    price_l2 = dplyr::lag(price, n = 2, order_by = year),
    price_l3 = dplyr::lag(price, n = 3, order_by = year)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    price_iv1 = case_when(
      year > 2013 ~ (1 - 0.15) / price_l1,
      year <= 2013 ~ price_l1_deduct / price_l1
    ),
    price_iv2 = case_when(
      year > 2013 ~ (1 - 0.15) / price_l2,
      year <= 2013 ~ price_l2_deduct / price_l2
    ),
    price_iv3 = case_when(
      year > 2013 ~ (1 - 0.15) / price_l3,
      year <= 2013 ~ price_l3_deduct / price_l3
    )
  )

names(dt)

dt <- dt %>%
  dplyr::mutate_at(
    vars(price, lprice, price_iv1, price_iv2, price_iv3),
    list(ln = ~ log(.))
  ) %>%
  dplyr::mutate(
    linc_ln = log(linc + 100000),
    donate_ln = log(donate),
    donate_ln = if_else(is.nan(donate_ln), NA_real_, donate_ln)
  ) %>%
  dplyr::group_by(pid) %>%
  dplyr::mutate_at(
    vars(
      price_ln, donate_ln, linc_ln,
      age, sqage
    ),
    list(
      l1 = ~dplyr::lag(., order_by = year),
      l2 = ~dplyr::lag(., n = 2, order_by = year),
      l3 = ~dplyr::lag(., n = 3, order_by = year)
    )
  ) %>%
  dplyr::ungroup()

dt <- dt %>%
  dplyr::mutate(
    price_ln_d1 = price_ln - price_ln_l1,
    price_ln_d2 = price_ln - price_ln_l2,
    price_ln_d3 = price_ln - price_ln_l3,
    donate_ln_d1 = donate_ln - donate_ln_l1,
    donate_ln_d2 = donate_ln - donate_ln_l2,
    donate_ln_d3 = donate_ln - donate_ln_l3,
    linc_ln_d1 = linc_ln - linc_ln_l1,
    linc_ln_d2 = linc_ln - linc_ln_l2,
    linc_ln_d3 = linc_ln - linc_ln_l3,
    age_d1 = age - age_l1,
    age_d2 = age - age_l2,
    age_d3 = age - age_l3,
    sqage_d1 = sqage - sqage_l1,
    sqage_d2 = sqage - sqage_l2,
    sqage_d3 = sqage - sqage_l3
  ) %>%
  dplyr::select(- (price_ln_l1:sqage_l3))

names(dt)

#'
#' 変数ラベルの設定
#+
cov_label <- list(
  employee = "[世帯員]常用職（給与所得者）ダミー",
  housewife = "[世帯員]専業主婦ダミー",
  univ = "[世帯員]大卒ダミー",
  highschool = "[世帯員]高卒ダミー",
  junior = "[世帯員]中卒ダミー",
  sqage = "[世帯員]年齢の二乗/100",
  credit_treat = "[世帯員]2014年税制改正の利益",
  price = "[世帯員]first giving price",
  lprice = "[世帯員]last giving price",
  price_l1_deduct = "[世帯員]first giving price (lag1 income, tax deduction)",
  price_l2_deduct = "[世帯員]first giving price (lag2 income, tax deduction)",
  price_l3_deduct = "[世帯員]first giving price (lag3 income, tax deduction)",
  price_l1 = "[世帯員]first giving price (lag = 1)",
  price_l2 = "[世帯員]first giving price (lag = 2)",
  price_l3 = "[世帯員]first giving price (lag = 3)",
  price_iv1 = "[世帯員]first price with lag1 inc/lag1 first price",
  price_iv2 = "[世帯員]first price with lag2 inc/lag2 first price",
  price_iv3 = "[世帯員]first price with lag3 inc/lag3 first price",
  price_ln = "priceの対数値",
  lprice_ln = "lpriceの対数値",
  price_iv1_ln = "price_iv1の対数値",
  price_iv2_ln = "price_iv2の対数値",
  price_iv3_ln = "price_iv3の対数値",
  donate_ln = "donateの対数値",
  linc_ln = "lincの対数値",
  price_ln_d1 = "price_ln - price_ln (lag = 1)",
  price_ln_d2 = "price_ln - price_ln (lag = 2)",
  price_ln_d3 = "price_ln - price_ln (lag = 3)",
  donate_ln_d1 = "donate_ln - donate_ln (lag = 1)",
  donate_ln_d2 = "donate_ln - donate_ln (lag = 2)",
  donate_ln_d3 = "donate_ln - donate_ln (lag = 3)",
  linc_ln_d1 = "linc_ln - linc_ln (lag = 1)",
  linc_ln_d2 = "linc_ln - linc_ln (lag = 2)",
  linc_ln_d3 = "linc_ln - linc_ln (lag = 3)",
  age_d1 = "age - age (lag = 1)",
  age_d2 = "age - age (lag = 2)",
  age_d3 = "age - age (lag = 3)",
  sqage_d1 = "sqage - sqage (lag = 1)",
  sqage_d2 = "sqage - sqage (lag = 2)",
  sqage_d3 = "sqage - sqage (lag = 3)"
)

for (i in names(cov_label)) {
  attr(dt[[i]], "label") <- cov_label[[i]]
}

#' 税理士関連データの追加
#+
village <- readr::read_csv("data/origin/village_tax_accountant.csv")

account <- haven::read_dta("data/origin/accountant.dta") %>%
  as_tibble() %>%
  dplyr::select(
    h_b10, year, "人口", "公認会計", "公認会計_従事者", "税理", "税理_従事者"
  ) %>%
  dplyr::rename(
    area = h_b10,
    pops = "人口",
    pub_accountant_firm = "公認会計",
    pub_accountant = "公認会計_従事者",
    tax_accountant_firm = "税理",
    tax_accountant = "税理_従事者"
  ) %>%
  dplyr::mutate(
    pub_accountant_per = pub_accountant / pops,
    tax_accountant_per = tax_accountant / pops
  )

dt <- dt %>%
  dplyr::left_join(village, by = c("year", "area")) %>%
  dplyr::mutate(
    village_accountant = if_else(year <= 2015, 0, accountant),
    village_consult = if_else(year <= 2015, 0, consult)
  ) %>%
  dplyr::left_join(account, by = c("year", "area")) %>%
  dplyr::select(-accountant, -consult)

#' 税理士関連変数のラベル設定
#+
account_label <- list(
  village_accountant = "[地域]村税理士制度に登録している税理士の数",
  village_consult = "[地域]村税理士制度の相談件数",
  pops = "[地域]人口",
  pub_accountant_firm = "[地域]公認会計士事務所の数",
  pub_accountant = "[地域]公認会計士の人数",
  pub_accountant_per = "[地域]公認会計士の人数 / 人口",
  tax_accountant_firm = "[地域]税理士事務所の数",
  tax_accountant = "[地域]税理士の人数",
  tax_accountant_per = "[地域]税理士の人数 / 人口"
)

for (i in names(account_label)) {
  attr(dt[[i]], "label") <- account_label[[i]]
}

#' 1. データの期間と年齢を制限する
#' 2. 控除申請と寄付行動でデータを制限する
#+
dt <- dt %>%
  dplyr::filter(year >= 2012 & age >= 24) %>%
  dplyr::filter(d_relief_donate == 0 | (d_relief_donate == 1 & d_donate == 1))

#' CSVファイルに書き出す
#+
readr::write_csv(dt, file = here("data/shaped2.csv"))

#' 変数の記述に関するcsvデータの作成
#+
book <- data.frame(variable = NULL, descript = NULL)

for (i in names(dt)) {
  book <- bind_rows(book, c(
    variable = i,
    descript = attr(dt[[i]], "label")
  ))
}

readr::write_csv(
  book,
  file = here("data/codebook/shaped2_description.csv")
)
