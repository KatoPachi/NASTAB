#'
#' raw data: data/merge/merge.dta
#+
library(xfun)
xfun::pkg_attach2(c("tidyverse", "rlist"))

raw <- haven::read_dta("data/merge/merge.dta", encoding = "utf-8") %>%
  as_tibble

#'
#' 個人属性に関する変数の処理
#+
ses <- raw %>%
  dplyr::select(
    hhid, pid, year, wave, p_page, p_pedu, p_pgen, h_b10,
    pca201:pca228, pinc_all, inc_bb1,
    pga001, pgb110, pgb120, pgb151:pgb159, pgb051:pgb058, pgb090,
    pgc007, pgc008, pea002, pgb020, pga020
  ) %>%
  dplyr::rename(
    age = p_page, #年齢
    educ = p_pedu, #学歴
    sex = p_pgen, #性別
    area = h_b10, #地域コード
    d_deduct_linc = pca201, #労働所得控除の有無
    deduct_linc = pca202, #労働所得控除額
    d_deduct_donate = pca225, #寄付金所得控除の有無
    deduct_donate = pca226, #寄付金所得控除額
    d_credit_donate = pca227, #寄付金税額控除の有無
    credit_donate = pca228, #寄付金税額控除額
    tinc = pinc_all, #昨年1年間の総合所得
    linc = inc_bb1, #昨年1年間の労働所得

    trust = pga001, #政治家への信頼
    avg_balance = pgb110, #平均的な税負担と福祉水準
    opt_balance = pgb120, #望ましい税負担と福祉水準

    opttax_tinc_1 = pgb151, #所与の総合所得に対する適切な税率
    opttax_tinc_3 = pgb152,
    opttax_tinc_5 = pgb153,
    opttax_tinc_7 = pgb154,
    opttax_tinc_10 = pgb155,
    opttax_tinc_20 = pgb156,
    opttax_tinc_30 = pgb157,
    opttax_tinc_50 = pgb158,
    opttax_tinc_100 = pgb159,

    opttax_linc_15 = pgb051, #所与の勤労所得に対する所与の税額が望ましいかどうか（2016年）
    opttax_linc_35 = pgb052,
    opttax_linc_55 = pgb053,
    opttax_linc_70 = pgb054,
    opttax_linc_90 = pgb055,
    opttax_linc_130 = pgb056,
    opttax_linc_370 = pgb057,
    opttax_linc_1800 = pgb058,

    addtax = pgb090, #福祉拡充のための税の追加負担の意向
    taxact_pg = pgc007, #政府の公共財供給で自身の納税行動が変化する
    taxact_trust = pgc008, #政府の信頼で自身の納税行動が変化する

    polipref = pga020 #政治的な立ち位置（1:極右 <-> 5:極左）
  ) %>%
  mutate_at(
    vars(d_deduct_linc, d_deduct_donate, d_credit_donate),
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
    deduct_donate = case_when(
      d_deduct_donate == 0 ~ 0,
      deduct_donate == -9 ~ NA_real_,
      TRUE ~ as.numeric(deduct_donate)
    ),
    credit_donate = case_when(
      d_credit_donate == 0 ~ 0,
      credit_donate == -9 ~ NA_real_,
      TRUE ~ as.numeric(credit_donate)
    ),

    trust = 6 - trust, #5:very high <-> 1:very low

    welfare = if_else(year < 2015, pea002, pgb020),
    welfare = case_when(
      welfare == -9 ~ NA_real_,
      TRUE ~ 6 - welfare #5:very high <-> 1:very low
    ),

    taxact_pg = 6 - taxact_pg,
    taxact_trust = 6 - taxact_trust,

    avg_balance = if_else(avg_balance == -9, NA_real_, avg_balance)
  ) %>%
  dplyr::select(-pea002, -pgb020)

#'
#' 個人属性の変数ラベルの設定
#+
ses_label <- list(
  tinc = "[世帯員]年間所得総額（単位: 10,000KRW）",
  linc = "[世帯員]年間労働所得（単位: 10,000KRW）",
  area = "[世帯]地域コード",
  d_deduct_linc = "[世帯員]労働所得控除の有無",
  deduct_linc = "[世帯員]労働所得控除額",
  d_deduct_donate = "[世帯員]寄付金所得控除の有無",
  deduct_donate = "[世帯員]寄付金所得控除額",
  d_credit_donate = "[世帯員]寄付金税額控除の有無",
  credit_donate = "[世帯員]寄付金税額控除額",
  trust = "[世帯員]政治家への信頼度",
  welfare = "[世帯員]納税レベルに対する福祉水準",
  avg_balance = "[世帯員]税負担と福祉水準の程度",
  opt_balance = "[世帯員]望ましい税負担と福祉水準の程度",
  addtax = "[世帯員]福祉拡充の追加課税への選好（2018年調査）",
  polipref = "[世帯員]政治的選好（left or right）",
  opttax_tinc_1 = "[世帯員]年間所得10mKRWの適切な税率（2018年調査）",
  opttax_tinc_3 = "[世帯員]年間所得30mKRWの適切な税率（2018年調査）",
  opttax_tinc_5 = "[世帯員]年間所得50mKRWの適切な税率（2018年調査）",
  opttax_tinc_7 = "[世帯員]年間所得70mKRWの適切な税率（2018年調査）",
  opttax_tinc_10 = "[世帯員]年間所得100mKRWの適切な税率（2018年調査）",
  opttax_tinc_20 = "[世帯員]年間所得200mKRWの適切な税率（2018年調査）",
  opttax_tinc_30 = "[世帯員]年間所得300mKRWの適切な税率（2018年調査）",
  opttax_tinc_50 = "[世帯員]年間所得500mKRWの適切な税率（2018年調査）",
  opttax_tinc_100 = "[世帯員]年間所得1000mKRWの適切な税率（2018年調査）",
  taxact_pg = "[世帯員]政府公共財の申告行動への影響（2016年調査）",
  taxact_trust = "[世帯員]政府に対する信頼の申告行動への影響（2016年調査）",
  opttax_linc_15 = "[世帯員]年間労働所得15mKRWの税負担の程度（2016年調査）",
  opttax_linc_35 = "[世帯員]年間労働所得35mKRWの税負担の程度（2016年調査）",
  opttax_linc_55 = "[世帯員]年間労働所得55mKRWの税負担の程度（2016年調査）",
  opttax_linc_70 = "[世帯員]年間労働所得70mKRWの税負担の程度（2016年調査）",
  opttax_linc_90 = "[世帯員]年間労働所得90mKRWの税負担の程度（2016年調査）",
  opttax_linc_130 = "[世帯員]年間労働所得130mKRWの税負担の程度（2016年調査）",
  opttax_linc_370 = "[世帯員]年間労働所得370mKRWの税負担の程度（2016年調査）",
  opttax_linc_1800 = "[世帯員]年間労働所得1800mKRWの税負担の程度（2016年調査）",
  taxact_pg = "[世帯員]政府の公共財供給で自身の納税行動が変化するか（2015-2016年調査）",
  taxact_trust = "[世帯員]政府の信頼で自身の納税行動が変化するか（2015-2016年調査）"
)

for (i in names(ses_label)) {
  attr(ses[[i]], "label") <- ses_label[[i]]
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
  mutate(amount = if_else(amount == -9, NA_real_, amount)) %>%
  dplyr::filter(!is.na(amount)) %>%
  dplyr::group_by(hhid, pid, year, purpose) %>%
  dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    names_from = purpose, values_from = amount,
    values_fill = 0, names_prefix = "donate_"
  ) %>%
  mutate(
    donate = donate_religious + donate_welfare + donate_educ + donate_others +
      donate_religious_action + donate_culture + donate_unknown,
    d_donate = if_else(donate > 0, 1, 0)
  )

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
  donate_religious = "[世帯員]宗教団体に対する寄付（単位:10,000KRW）",
  donate_welfare = "[世帯員]社会福祉団体に対する寄付（単位:10,000KRW）",
  donate_educ = "[世帯員]教育団体に対する寄付（単位:10,000KRW）",
  donate_poliparty = "[世帯員]政治団体に対する寄付（単位:10,000KRW）",
  donate_culture = "[世帯員]文化団体に対する寄付（単位:10,000KRW）",
  donate_religious_action = "[世帯員]宗教団体の貧困救済活動に対する寄付（単位:10,000KRW）",
  donate_others = "[世帯員]その他の団体に対する寄付（単位:10,000KRW）",
  donate_unknown = "[世帯員]支出先不明の寄付（単位:10,000KRW）",
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
