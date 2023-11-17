# //NOTE Read raw data
library(here)
library(tidyverse)
library(haven)

raw <- haven::read_dta(here("data/merge/merge.dta"), encoding = "utf-8")

# //NOTE Socio-economic variables
ses <- raw %>%
  dplyr::select(
    hhid, #世帯ID
    pid, #個人ID
    year, #調査年
    wave, #調査wave
    sex = p_pgen, #性別
    age = p_page, #年齢
    educ = p_pedu, #学歴
    area = h_b10, #世帯：地域コード
    p_aa200, #就業状態（1=就職、2=専業主婦、3=無職、4=学生）
    p_aa005, #従事地位（1=常用、2=臨時、3=自営業、4=無給の家族従事者）cond. p_aa200 == 1
    indust = paa008, #産業コード
    family_position = p_prel, #世帯主との関係コード
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
    work = case_when(
      p_aa005 == 1 ~ 1, #就業（常勤）
      p_aa005 == 2 ~ 2, #就業（臨時）
      p_aa005 == 3 ~ 3, #就業 (自営業)
      p_aa005 == 4 ~ 4, #就業（家族従事者・無給）
      p_aa200 == 2 ~ 5, #専業主婦
      p_aa200 == 3 ~ 6, #無職
      p_aa200 == 4 ~ 7 #学生
    ),
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
    ),
    dependents = if_else(work %in% c(4, 5, 7), 1, 0),
    indust = if_else(indust != -9, indust, NA_real_)
  ) %>%
  select(-p_aa200, -p_aa005)

hh_data <- ses %>%
  dplyr::select(hhid, year, dependents) %>%
  group_by(hhid, year) %>%
  mutate(
    hh_num = n(),
    hh_dependents = sum(dependents),
    have_dependents = if_else(hh_dependents > 0, 1, 0)
  ) %>%
  dplyr::select(-dependents) %>%
  dplyr::distinct(hhid, year, .keep_all = TRUE)

ses <- ses %>%
  dplyr::left_join(hh_data, by = c("hhid", "year"))

# //NOTE Income data
inc <- raw %>%
  dplyr::select(
    hhid,
    pid,
    year,
    certificate = psa13, # 証明書の提示有無：1.勤労所得を提出 2.総合所得を提出 3.両方を提出 4. 未提出
    tinc = pinc_all, #昨年1年間の総合所得
    linc = inc_bb1, #昨年1年間の労働所得
    binc = inc_bb2, #昨年1年間の純事業所得
    rinc = inc_bb3, #昨年1年間の不動産賃貸所得
    dinc = inc_bb4, #昨年1年間の利子・配当・譲渡所得
    oinc = inc_bb5, #昨年1年間のその他所得
  ) %>%
  mutate(
    detect_tinc = linc + binc + rinc + dinc + oinc
  )

# //NOTE Tax benefit data
benefit <- raw %>%
  dplyr::select(
    hhid,
    pid,
    year,
    certificate = psa13, # 証明書の提示有無：1.勤労所得を提出 2.総合所得を提出 3.両方を提出 4. 未提出
    d_deduct_linc = pca201, #労働所得に対する控除（年末調整）の有無
    d_deduct_tinc = pda201, #総合所得に対する控除（確定申告）の有無
    d_deduct_donate_linc = pca225, #寄付金による所得控除の有無（年末調整）
    d_credit_donate_linc = pca227, #寄付金による税額控除の有無（年末調整）
    d_credit_donate_linc = pca227, #寄付金による税額控除の有無（年末調整）
    d_deduct_donate_tinc = pda207, #寄付金による所得控除の有無（確定申告? 総合所得）
    d_credit_donate_tinc = pda209, # 寄付金による税額控除の有無（確定申告? 総合所得）
    deduct_linc = pca202, #労働所得に対する控除額
    deduct_tinc = pda202, #総合所得に対する控除額
    deduct_donate_linc = pca226, #寄付金による所得控除額（年末調整）
    credit_donate_linc = pca228, #寄付金による税額控除額（年末調整）
    deduct_donate_tinc = pda208, #寄付金による所得控除額（確定申告? 総合所得）
    credit_donate_tinc = pda210 # 寄付金による税額控除額（確定申告? 総合所得）
  ) %>%
  mutate_at(
    vars(d_deduct_linc:credit_donate_tinc),
    list(~ ifelse(as.numeric(.) == -9, NA_real_, as.numeric(.)))
  ) %>%
  mutate_at(
    vars(d_deduct_linc:d_credit_donate_tinc),
    list(~ ifelse(. == 2, 0, .))
  ) %>%
  mutate(
    deduct_linc = if_else(d_deduct_linc == 0, 0, deduct_linc),
    deduct_tinc = if_else(d_deduct_tinc == 0, 0, deduct_tinc),
    deduct_donate_linc = if_else(d_deduct_donate_linc == 0, 0, deduct_donate_linc),
    credit_donate_linc = if_else(d_credit_donate_linc == 0, 0, credit_donate_linc),
    deduct_donate_tinc = if_else(d_deduct_donate_tinc == 0, 0, deduct_donate_tinc),
    credit_donate_tinc = if_else(d_credit_donate_tinc == 0, 0, credit_donate_tinc)
  ) %>%
  mutate(
    d_relief_donate_linc = if_else(year >= 2014, d_credit_donate_linc, d_deduct_donate_linc),
    d_relief_donate_tinc = if_else(year >= 2014, d_credit_donate_tinc, d_deduct_donate_tinc),
    d_relief_donate = case_when(
      is.na(d_relief_donate_linc) ~ d_relief_donate_tinc,
      is.na(d_relief_donate_tinc) ~ d_relief_donate_linc,
      d_relief_donate_tinc + d_relief_donate_linc == 0 ~ 0,
      d_relief_donate_tinc + d_relief_donate_linc != 0 ~ 1
    ),
    relief_donate_linc = if_else(year >= 2014, credit_donate_linc, deduct_donate_linc),
    relief_donate_tinc = if_else(year >= 2014, credit_donate_tinc, deduct_donate_tinc)
  ) %>%
  mutate(
    taxable_linc = linc - deduct_linc + relief_donate_linc,
    taxable_tinc = tinc - deduct_tinc + relief_donate_tinc,
    ub = case_when(
      year == 2010 ~ taxable_tinc * 0.15,
      year == 2011 ~ taxable_tinc * 0.20,
      year == 2012 ~ taxable_tinc * 0.30,
      year == 2013 ~ taxable_tinc * 0.30,
      year >= 2014 ~ 3000,
      year >= 2016 ~ 2000
    ),
    religious_ub = case_when(
      year < 2014 ~ taxable_tinc * 0.1,
      year >= 2014 ~ 3000,
      year >= 2016 ~ 2000
    ),
    incentive_limit = religious_ub
  )

#'
#' 寄付データの作成
#+
# 寄付先データ
donate_purpose_data <- raw %>%
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
      "1" = "political",
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
donate_amount_data <- raw %>%
  dplyr::select(
    hhid, pid, year,
    hcr005, hcr008, hcr011, hcr014, hcr017, hcr020
  ) %>%
  tidyr::pivot_longer(hcr005:hcr020, names_prefix = "hcr") %>%
  rename(key = name, amount = value)

# 寄付先データと支出データのマージ（個人単位）
donate_member_data <- donate_purpose_data %>%
  dplyr::left_join(donate_amount_data, by = c("hhid", "pid", "year", "key")) %>%
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
    donate = donate_political +
      donate_welfare + donate_educ + donate_others + donate_culture + donate_unknown,
    religious_donate = donate_religious + donate_religious_action,
    political_donate = donate_political,
    d_donate = if_else(donate > 0, 1, 0)
  ) %>%
  dplyr::select(hhid, pid, year, donate, political_donate, religious_donate, d_donate)

# 寄付支出データ（世帯単位）
donate_household_data <- raw %>%
  dplyr::select(hhid, pid, year, h_exp_cr, hcr001) %>%
  rename(h_donate = h_exp_cr, d_h_donate = hcr001) %>%
  mutate(
    d_h_donate = as.numeric(d_h_donate),
    d_h_donate = if_else(d_h_donate == 2, 0, d_h_donate)
  )

# 個人単位データと世帯単位データのマージ
donate_data <- donate_member_data %>%
  dplyr::left_join(donate_household_data, by = c("hhid", "pid", "year"))

#'
#' 税率に関するデータの作成
#+
mtr <- readr::read_csv("data/origin/mtrdt.csv")

inc_data <- relief_data %>%
  dplyr::select(hhid, pid, year, taxable_linc, taxable_linc)

# first marginal tax rate(寄付額を差し引く前の税率)
first_mtr_data <- inc_data %>%
  dplyr::left_join(mtr, by = "year") %>%
  dplyr::filter(!is.na(taxable_linc)) %>%
  dplyr::filter(lower_income_10000won <= taxable_linc) %>%
  dplyr::group_by(pid, year) %>%
  dplyr::mutate(first_mtr = max(MTR)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-lower_income_10000won, -MTR, -taxable_linc, -taxable_linc) %>%
  dplyr::distinct(.keep_all = TRUE)

# last marginal tax rate(寄付額を差し引いた後の税率)
last_mtr_data <- inc_data %>%
  dplyr::left_join(donate_data, by = c("pid", "year", "hhid")) %>%
  dplyr::left_join(mtr, by = "year") %>%
  dplyr::filter(!is.na(taxable_linc) & !is.na(donate)) %>%
  dplyr::mutate(subtract_tinc = taxable_linc - donate) %>%
  dplyr::filter(lower_income_10000won <= subtract_tinc) %>%
  dplyr::group_by(pid, year) %>%
  dplyr::mutate(last_mtr = max(MTR)) %>%
  dplyr::ungroup() %>%
  dplyr::select(hhid, pid, year, subtract_tinc, last_mtr) %>%
  dplyr::distinct(.keep_all = TRUE)

# 所得のラグ変数を作成
lag_inc_data <- inc_data %>%
  dplyr::group_by(pid) %>%
  dplyr::mutate(
    taxable_linc_l1 = dplyr::lag(taxable_linc, order_by = year),
    taxable_linc_l2 = dplyr::lag(taxable_linc, n = 2, order_by = year),
    taxable_linc_l3 = dplyr::lag(taxable_linc, n = 3, order_by = year)
  ) %>%
  dplyr::ungroup()

# 1期ラグ所得に基づいた税率の計算
lag1_mtr_data <- lag_inc_data %>%
  dplyr::select(hhid, pid, year, taxable_linc_l1) %>%
  dplyr::left_join(mtr, by = "year") %>%
  dplyr::filter(!is.na(taxable_linc_l1)) %>%
  dplyr::filter(lower_income_10000won <= taxable_linc_l1) %>%
  dplyr::group_by(pid, year) %>%
  dplyr::mutate(first_mtr_l1 = max(MTR)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-lower_income_10000won, -MTR) %>%
  dplyr::distinct(.keep_all = TRUE)

# 2期ラグ所得に基づいた税率の計算
lag2_mtr_data <- lag_inc_data %>%
  dplyr::select(hhid, pid, year, taxable_linc_l2) %>%
  dplyr::left_join(mtr, by = "year") %>%
  dplyr::filter(!is.na(taxable_linc_l2)) %>%
  dplyr::filter(lower_income_10000won <= taxable_linc_l2) %>%
  dplyr::group_by(pid, year) %>%
  dplyr::mutate(first_mtr_l2 = max(MTR)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-lower_income_10000won, -MTR) %>%
  dplyr::distinct(.keep_all = TRUE)

# 3期ラグ所得に基づいた税率の計算
lag3_mtr_data <- lag_inc_data %>%
  dplyr::select(hhid, pid, year, taxable_linc_l3) %>%
  dplyr::left_join(mtr, by = "year") %>%
  dplyr::filter(!is.na(taxable_linc_l3)) %>%
  dplyr::filter(lower_income_10000won <= taxable_linc_l3) %>%
  dplyr::group_by(pid, year) %>%
  dplyr::mutate(first_mtr_l3 = max(MTR)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-lower_income_10000won, -MTR) %>%
  dplyr::distinct(.keep_all = TRUE)

# 2014年税制改革によるトリートメントグループの作成
treat_data <- first_mtr_data %>%
  dplyr::filter(year == 2013) %>%
  dplyr::select(hhid, pid, first_mtr) %>%
  dplyr::mutate(
    first_mtr_13 = first_mtr,
    credit_benefit = if_else(first_mtr < 0.15, 1, 0),
    credit_neutral = if_else(first_mtr == 0.15, 1, 0),
    credit_loss = if_else(first_mtr > 0.15, 1, 0),
    credit_treat = case_when(
      credit_benefit == 1 ~ 1,
      credit_neutral == 1 ~ 2,
      credit_loss == 1 ~ 3,
      TRUE ~ NA_real_
    ),
    bracket13 = case_when(
      first_mtr == 0.06 ~ "(A) --1200",
      first_mtr == 0.15 ~ "(B) 1200--4600",
      first_mtr == 0.24 ~ "(C) 4600--8800",
      first_mtr == 0.35 ~ "(D) & (E) 8800--30000",
      first_mtr == 0.38 ~ "(F) & (G) 30000--"
    )
  ) %>%
  dplyr::select(-first_mtr)

# ここまでのデータのマージ
tax_data <- inc_data %>%
  dplyr::left_join(first_mtr_data, by = c("hhid", "pid", "year")) %>%
  dplyr::left_join(last_mtr_data, by = c("hhid", "pid", "year")) %>%
  dplyr::left_join(lag1_mtr_data, by = c("hhid", "pid", "year")) %>%
  dplyr::left_join(lag2_mtr_data, by = c("hhid", "pid", "year")) %>%
  dplyr::left_join(lag3_mtr_data, by = c("hhid", "pid", "year")) %>%
  dplyr::left_join(treat_data, by = c("hhid", "pid"))

#'
#' ここまでのデータのマージと変数の処理
#+
dt <- donate_data %>%
  dplyr::left_join(tax_data, by = c("hhid", "pid", "year")) %>%
  dplyr::left_join(ses_data, by = c("hhid", "pid", "year")) %>%
  dplyr::left_join(relief_data, by = c("hhid", "pid", "year")) %>%
  dplyr::left_join(hh_data, by = c("hhid", "year")) %>%
  dplyr::select(-taxable_linc.y) %>%
  dplyr::rename(taxable_linc = taxable_linc.x)

dt <- dt %>%
  dplyr::mutate(
    employee = if_else(work == 1, 1, 0),
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
    price_l3_deduct = 1 - first_mtr_l3,
    price13 = 1 - first_mtr_13
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
    linc_ln = log(linc + 10000),
    taxable_linc_ln = log(taxable_linc + 10000),
    tinc_ln = log(tinc + 10000),
    taxable_tinc_ln = log(taxable_linc + 10000),
    donate_ln = log(donate + 1)
  ) %>%
  dplyr::group_by(pid) %>%
  dplyr::mutate_at(
    vars(
      price_ln, donate_ln, linc_ln, tinc_ln,
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
    tinc_ln_d1 = tinc_ln - tinc_ln_l1,
    tinc_ln_d2 = tinc_ln - tinc_ln_l2,
    tinc_ln_d3 = tinc_ln - tinc_ln_l3,
    age_d1 = age - age_l1,
    age_d2 = age - age_l2,
    age_d3 = age - age_l3,
    sqage_d1 = sqage - sqage_l1,
    sqage_d2 = sqage - sqage_l2,
    sqage_d3 = sqage - sqage_l3
  ) %>%
  dplyr::select(- (price_ln_l1:sqage_l3))

names(dt)

#' 税理士関連データの追加
#+
# village <- readr::read_csv("data/origin/village_tax_accountant.csv")

# account <- haven::read_dta("data/origin/accountant.dta") %>%
#   as_tibble() %>%
#   dplyr::select(
#     h_b10, year, "人口", "公認会計", "公認会計_従事者", "税理", "税理_従事者"
#   ) %>%
#   dplyr::rename(
#     area = h_b10,
#     pops = "人口",
#     pub_accountant_firm = "公認会計",
#     pub_accountant = "公認会計_従事者",
#     tax_accountant_firm = "税理",
#     tax_accountant = "税理_従事者"
#   ) %>%
#   dplyr::mutate(
#     pub_accountant_per = pub_accountant / pops,
#     tax_accountant_per = tax_accountant / pops
#   )

# dt <- dt %>%
#   dplyr::left_join(village, by = c("year", "area")) %>%
#   dplyr::mutate(
#     village_accountant = if_else(year <= 2015, 0, accountant),
#     village_consult = if_else(year <= 2015, 0, consult)
#   ) %>%
#   dplyr::left_join(account, by = c("year", "area")) %>%
#   dplyr::select(-accountant, -consult)

#' 1. 年齢を制限する
#' 2. 控除申請と寄付行動でデータを制限する
#+
# dt <- dt %>%
#   dplyr::filter(age >= 24) %>%
#   dplyr::filter(d_relief_donate == 0 | (d_relief_donate == 1 & d_donate == 1))

#' CSVファイルに書き出す
#+
readr::write_csv(dt, file = here("data/shaped2.csv"))
