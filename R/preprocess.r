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
    employee = if_else(work == 1, 1, 0),
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
    indust = if_else(indust != -9, indust, NA_real_),
    indust = if_else(is.na(indust), 22, indust)
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
    donate_ln = log(donate),
    d_donate = if_else(donate > 0, 1, 0),
    religious_donate = donate_religious + donate_religious_action,
    political_donate = donate_political,
  ) %>%
  dplyr::select(hhid, pid, year, donate, donate_ln, d_donate, political_donate, religious_donate)

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

# //NOTE 所得データの作成
inc <- raw %>%
  dplyr::select(
    hhid,
    pid,
    year,
    certificate = psa13, # 証明書の提示有無：1.勤労所得を提出 2.総合所得を提出 3.両方を提出 4. 未提出
    tinc = pinc_all, # 昨年1年間の総合所得
    linc = inc_bb1, # 昨年1年間の労働所得
    binc = inc_bb2, # 昨年1年間の純事業所得
    rinc = inc_bb3, # 昨年1年間の不動産賃貸所得
    dinc = inc_bb4, # 昨年1年間の利子・配当・譲渡所得
    oinc = inc_bb5, # 昨年1年間のその他所得
  ) %>%
  mutate(
    certificate = as.numeric(certificate)
  )

# //NOTE ここまでのデータをマージ
dt <- ses %>%
  dplyr::left_join(donate_data, by = c("hhid", "pid", "year")) %>%
  dplyr::left_join(inc, by = c("hhid", "pid", "year"))

# //NOTE 課税所得の計算
# FUNCTIONS
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

check_dependent <- function(position, tinc, linc, age) {
  child <- c(3, 4, 7, 8)
  parent <- c(5, 6, 9)

  case_when(
    position == 1 ~ 0,
    position == 2 & tinc == linc & linc <= 500 ~ 1,
    position == 2 & tinc == linc ~ 0,
    position == 2 & tinc <= 100 ~ 1,
    position == 2 ~ 0,
    position %in% child & 20 < age ~ 0,
    position %in% child & tinc == linc & linc <= 500 ~ 1,
    position %in% child & tinc == linc ~ 0,
    position %in% child & tinc <= 100 ~ 1,
    position %in% child ~ 0,
    position %in% parent & age < 60 ~ 0,
    position %in% parent & tinc == linc & linc <= 500 ~ 1,
    position %in% parent & tinc == linc ~ 0,
    position %in% parent & tinc <= 100 ~ 1,
    position %in% parent ~ 0,
    position == 10 & (20 < age | age < 60) ~ 0,
    position == 10 & tinc == linc & linc <= 500 ~ 1,
    position == 10 & tinc == linc ~ 0,
    position == 10 & tinc <= 100 ~ 1,
    position == 10 ~ 0,
    position >= 11 ~ 0
  )
}

# 変数作成とサブセット化
# //DISCUSS condition (1): 24 <= age
# //DISCUSS condition (2): household heads who are self-employed or full-time wage earners
# //DISCUSS condition (3): 2010 <= year < 2018
hh_dependent <- dt %>%
  mutate(
    dependent = check_dependent(family_position, tinc, linc, age),
    over70 = if_else(age >= 70, 1, 0)
  ) %>%
  select(hhid, year, dependent, over70) %>%
  group_by(hhid, year) %>%
  summarize_at(vars(dependent, over70), list(~sum(.))) %>%
  ungroup()

dt2 <- dt %>%
  dplyr::left_join(hh_dependent, by = c("hhid", "year")) %>%
  dplyr::filter(24 <= age) %>%
  dplyr::filter(family_position == 1 & work %in% c(1, 3)) %>%
  dplyr::filter(2010 <= year & year < 2018) %>%
  mutate(
    salary_deduct = employment_income_deduction(linc, year),
    taxable_tinc = tinc - salary_deduct - 150 * (dependent + 1) - 100 * over70,
    linc_ln = log(linc + 10000),
    tinc_ln = log(tinc + 10000),
    taxable_tinc_ln = log(taxable_tinc + 10000)
  )

# //NOTE 限界所得税率と寄付価格の計算
mtr <- function(inc, year) {
  case_when(
    inc < 1200 ~ 0.06,
    inc < 4600 ~ 0.15,
    inc < 8800 ~ 0.24,
    inc < 15000 ~ 0.35,
    inc < 30000 & year < 2014 ~ 0.35,
    inc < 30000 ~ 0.38,
    inc < 50000 & year < 2012 ~ 0.35,
    inc < 50000 ~ 0.38,
    !is.na(inc) & year < 2012 ~ 0.35,
    !is.na(inc) & year < 2017 ~ 0.38,
    !is.na(inc) ~ 0.4,
  )
}

dt3 <- dt2 %>%
  mutate(
    first_mtr = mtr(taxable_tinc, year),
    last_mtr = mtr(taxable_tinc - donate, year),
    bracket = case_when(
      taxable_tinc < 1200 ~ "(A) [0, 1200)",
      taxable_tinc < 4600 ~ "(B) [1200, 4600)",
      taxable_tinc < 8800 ~ "(C) [4600, 8800)",
      taxable_tinc < 30000 ~ "(D) & (E) [8800, 30000)",
      !is.na(taxable_tinc) ~ "(F) & (G) [30000, +infty)"
    ),
    experience_FG = if_else(bracket == "(F) & (G) [30000, +infty)", 1, 0),
    price = case_when(
      year < 2014 ~ 1 - first_mtr,
      year >= 2014 ~ 1 - 0.15
    ),
    lprice = case_when(
      year < 2014 ~ 1 - last_mtr,
      year >= 2014 ~ 1 - 0.15
    ),
    price_ln = log(price),
    lprice_ln = log(lprice),
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
    )
  ) %>%
  group_by(pid) %>%
  mutate(experience_FG = if_else(sum(experience_FG) > 0, 1, 0)) %>%
  ungroup()

# //NOTE 寄付控除・その他所得控除データ
benefit <- raw %>%
  dplyr::select(
    hhid,
    pid,
    year,
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
  )

# //NOTE ここまでのデータをマージ
dt4 <- dt3 %>%
  dplyr::left_join(benefit, by = c("hhid", "pid", "year"))

# //NOTE サブセット条件の追加
# //DISCUSS condition (3): d_relief_donate == 0 | (d_relief_donate == 1 & d_donate == 1)
# //DISCUSS condition (4): no experience bracket (F) & (G)
# //DISCUSS condition (5): amount of donation is lower than incentive upper-bound
dt5 <- dt4 %>%
  dplyr::filter(d_relief_donate == 0 | (d_relief_donate == 1 & d_donate == 1)) %>%
  dplyr::filter(experience_FG == 0) %>%
  dplyr::filter(ub > donate)

# //NOTE Write csv file
readr::write_csv(dt5, file = here("data/shaped2.csv"))