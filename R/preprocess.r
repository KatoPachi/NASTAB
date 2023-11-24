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
    female = p_pgen, #性別
    age = p_page, #年齢
    educ = p_pedu, #学歴(1=中卒以下、2=高卒、3=短大卒以上)
    area = h_b10, #世帯：地域コード
    p_aa200, #就業状態（1=就職、2=専業主婦、3=無職、4=学生）
    p_aa005, #従事地位（1=常用、2=臨時、3=自営業、4=無給の家族従事者）cond. p_aa200 == 1
    indust = paa008, #産業コード
    family_position = p_prel, #世帯主との関係コード
  ) %>%
  dplyr::mutate(
    female = female - 1,
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
    indust = if_else(indust != -9, indust, NA_real_)
  ) %>%
  select(-p_aa200, -p_aa005, -educ)

hh_data <- ses %>%
  dplyr::select(hhid, year) %>%
  group_by(hhid, year) %>%
  mutate(hh_num = n()) %>%
  dplyr::distinct(hhid, year, .keep_all = TRUE)

ses <- ses %>%
  dplyr::left_join(hh_data, by = c("hhid", "year"))

# //NOTE 寄付データの作成
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
      key,
      "004" = "005", "007" = "008", "010" = "011",
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
    donate = donate_welfare + donate_educ + donate_others + donate_culture + donate_unknown,
    d_donate = if_else(donate > 0, 1, 0),
    religious_donate = donate_religious + donate_religious_action,
    political_donate = donate_political,
  ) %>%
  dplyr::select(hhid, pid, year, donate, d_donate, political_donate, religious_donate)

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

# ここまでのデータをマージする
dt <- ses %>%
  dplyr::left_join(donate_data, by = c("hhid", "pid", "year"))

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
    certificate = as.numeric(certificate)
  )

# //NOTE Calculate taxable income
# employment income deduction
employment_income_deduction <- function(linc, year) {
  type1 <- c(2009, 2012, 2013)
  type2 <- c(2010, 2011)
  type3 <- 2014:2017

  case_when(
    year %in% c(type1, type2) & linc < 500 ~ 0.8 * linc,
    year %in% c(type1, type2) & linc < 1500 ~ 400 + 0.5 * (linc - 500),
    year %in% c(type1, type2) & linc < 3000 ~ 900 + 0.15 * (linc - 1500),
    year %in% c(type1, type2) & linc < 4500 ~ 1125 + 0.1 * (linc - 3000),
    year %in% type1 ~ 1275 + 0.05 * (linc - 4500),

    year %in% type2 & linc < 8000 ~ 1275 + 0.05 * (linc - 4500),
    year %in% type2 & linc < 10000 ~ 1450 + 0.03 * (linc - 8000),
    year %in% type2 ~ 1510 + 0.01 * (linc - 10000),

    year %in% type3 & linc < 500 ~ 0.7 * linc,
    year %in% type3 & linc < 1500 ~ 350 + 0.4 * (linc - 500),
    year %in% type3 & linc < 4500 ~ 750 + 0.15 * (linc - 1500),
    year %in% type3 & linc < 10000 ~ 1200 + 0.05 * (linc - 4500),
    year %in% type3 ~ 1475 + 0.02 * (linc - 10000),
    TRUE ~ NA_real_
  )
}

inc <- inc %>%
  mutate(salary_deduct = employment_income_deduction(linc, year))

dt <- dt %>%
  dplyr::left_join(inc, by = c("pid", "hhid", "year"))

test <- dt %>%
  mutate(taxable_tinc = tinc - salary_deduct) %>%
  dplyr::filter(!is.na(taxable_tinc))

# //NOTE Tax benefit data
# //TODO Add additional deduction items
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

# //TODO Calculate taxable income

# //TODO Calculate tax-price
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
  dplyr::left_join(treat_data, by = c("hhid", "pid"))

# //NOTE Make some variables
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
    )
  ) %>%
  dplyr::mutate_at(
    vars(price, lprice),
    list(ln = ~ log(.))
  ) %>%
  dplyr::mutate(
    linc_ln = log(linc + 10000),
    taxable_linc_ln = log(taxable_linc + 10000),
    tinc_ln = log(tinc + 10000),
    taxable_tinc_ln = log(taxable_linc + 10000),
    donate_ln = log(donate + 1)
  )

# //NOTE Write csv file
readr::write_csv(dt, file = here("data/shaped2.csv"))
