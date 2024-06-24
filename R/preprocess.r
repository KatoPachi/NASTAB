# * Read raw data
library(here)
library(tidyverse)
library(haven)

raw <- haven::read_dta(here("data/merge/merge.dta"), encoding = "utf-8")

# * Socio-economic variables
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
    hhnum = wfnum, #世帯員数
    hhnum_child6 = wcnum_1, #0~6歳未満の世帯員数
    hhnum_child18 = wcnum_2 #6~18歳未満の世帯員数
  ) %>%
  dplyr::mutate(
    female = female - 1,
    hhnum_child = hhnum_child6 + hhnum_child18,
    sqage = age^2 / 10000,
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
  mutate(hhnum_survey = n()) %>%
  dplyr::distinct(hhid, year, .keep_all = TRUE)

ses <- ses %>%
  dplyr::left_join(hh_data, by = c("hhid", "year"))

# * 寄付データの作成
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
    donate = donate_religious + donate_religious_action +
      donate_welfare + donate_educ + donate_others + donate_culture + donate_unknown,
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

# * 所得データの作成
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

# * ここまでのデータをマージ
dt <- ses %>%
  dplyr::left_join(donate_data, by = c("hhid", "pid", "year")) %>%
  dplyr::left_join(inc, by = c("hhid", "pid", "year"))

# * 課税所得の計算
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
    # position %in% child & 20 < age ~ 0,
    # position %in% child & tinc == linc & linc <= 500 ~ 1,
    # position %in% child & tinc == linc ~ 0,
    # position %in% child & tinc <= 100 ~ 1,
    position %in% child ~ 0,
    position %in% parent & age < 60 ~ 0,
    position %in% parent & tinc == linc & linc <= 500 ~ 1,
    position %in% parent & tinc == linc ~ 0,
    position %in% parent & tinc <= 100 ~ 1,
    position %in% parent ~ 0,
    position == 10 & 20 < age & age < 60 ~ 0,
    position == 10 & tinc == linc & linc <= 500 ~ 1,
    position == 10 & tinc == linc ~ 0,
    position == 10 & tinc <= 100 ~ 1,
    position == 10 ~ 0,
    position >= 11 ~ 0
  )
}

pension_deduction <- function(linc, year) {
  # interval <- case_when(
  #   year == 2010 ~ c(220 * 12 / 10, 3600 * 12 / 10),
  #   year == 2011 ~ c(230 * 12 / 10, 3680 * 12 / 10),
  #   year == 2012 ~ c(230 * 12 / 10, 3750 * 12 / 10),
  #   year == 2013 ~ c(240 * 12 / 10, 3890 * 12 / 10),
  #   year >= 2014 ~ c(250 * 12 / 10, 3980 * 12 / 10)
  # )
  case_when(
    year <= 2009 ~ NA_real_,
    year == 2010 & linc < 22 * 12 ~ 0.045 * 22 * 12,
    year == 2010 & linc > 360 * 12 ~ 0.045 * 360 * 12,
    year == 2011 & linc < 23 * 12 ~ 0.045 * 23 * 12,
    year == 2011 & linc > 368 * 12 ~ 0.045 * 368 * 12,
    year == 2012 & linc < 23 * 12 ~ 0.045 * 23 * 12,
    year == 2012 & linc > 375 * 12 ~ 0.045 * 375 * 12,
    year == 2013 & linc < 24 * 12 ~ 0.045 * 24 * 12,
    year == 2013 & linc > 389 * 12 ~ 0.045 * 389 * 12,
    year >= 2014 & linc < 25 * 12 ~ 0.045 * 25 * 12,
    year >= 2014 & linc > 398 * 12 ~ 0.045 * 398 * 12,
    TRUE ~ linc * 0.045
  )
}

# 変数作成とサブセット化
# !condition (1): 24 <= age (N = 118,665)
# !condition (2): taxpayers (N = 113,204)
# !condition (3): 2010 <= year < 2018 (N = 74,688)
# !condition (4): positive taxable income (N = 49,595)
dt2 <- dt %>%
  mutate(
    salary_deduct = employment_income_deduction(linc, year),
    pension_deduct = pension_deduction(linc, year),
    taxable_tinc = tinc - salary_deduct - pension_deduct,
    dependent = check_dependent(family_position, tinc, linc, age),
    payer = 1 - dependent,
    over70 = if_else(age >= 70, 1, 0),
    dependent_over70 = dependent * over70
  )

hh_dependent <- dt2 %>%
  select(hhid, year, dependent, payer, dependent_over70) %>%
  group_by(hhid, year) %>%
  summarize_at(vars(dependent, payer, dependent_over70), list(num =~sum(.))) %>%
  ungroup()

hh_max_inc <- dt2 %>%
  dplyr::filter(payer == 1 & !is.na(taxable_tinc)) %>%
  select(pid, hhid, year, taxable_tinc) %>%
  group_by(hhid, year) %>%
  mutate(hh_max_inc = max(taxable_tinc)) %>%
  ungroup() %>%
  mutate(hh_max_inc = if_else(taxable_tinc == hh_max_inc, 1, 0)) %>%
  select(-taxable_tinc)

dt3 <- dt2 %>%
  dplyr::left_join(hh_dependent, by = c("hhid", "year")) %>%
  dplyr::left_join(hh_max_inc, by = c("pid", "hhid", "year")) %>%
  mutate(
    taxable_tinc = taxable_tinc - 150 - over70 * 100 -
      150 * hh_max_inc * (hhnum_child6 + hhnum_child18 + dependent_num) -
      100 * hh_max_inc * dependent_over70_num
  )

# * Using hh_max_inc to calculate taxable total income
# * No use:
# > with(dt3, summary(taxable_tinc))
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# -1250     126    1025    1803    2550  314185   99811
# * Use:
# > with(dt3, summary(taxable_tinc))
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#    -800     309    1215    1991    2725  314185  103909

dt4 <- dt3 %>%
  dplyr::filter(24 <= age) %>%
  dplyr::filter(payer == 1) %>%
  dplyr::filter(2010 <= year & year < 2018) %>%
  dplyr::filter(0 < taxable_tinc) %>%
  mutate(
    linc_ln = log(linc),
    tinc_ln = log(tinc),
    taxable_tinc_ln = log(taxable_tinc)
  )

# * If we don't use hh_max_inc to calculate taxable total income,
# > with(dt4, summary(taxable_tinc))
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#      0.5    670.0   1578.8   2395.0   3162.3 314185.0
# * If we use hh_max_inc to calculate taxable total income,
# > with(dt4, summary(taxable_tinc))
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#      0.5    643.5   1550.0   2352.6   3130.4 314185.0

# * 限界所得税率と寄付価格の計算
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

dt5 <- dt4 %>%
  mutate(
    first_mtr = mtr(taxable_tinc, year),
    after_tax_tinc = taxable_tinc * (1 - first_mtr),
    after_tax_tinc_ln = log(after_tax_tinc),
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

bracket13 <- dt5 %>%
  dplyr::filter(year == 2013) %>%
  select(pid, hhid, bracket13 = bracket)

dt6 <- dt5 %>%
  dplyr::left_join(bracket13, by = c("hhid", "pid"))

# * 寄付控除・その他所得控除データ
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

# * ここまでのデータをマージ
dt7 <- dt6 %>%
  dplyr::left_join(benefit, by = c("hhid", "pid", "year"))

# * サブセット条件の追加
# !condition (5): d_relief_donate == 0 | (d_relief_donate == 1 & d_donate == 1) (N = 26,918)
# condition (6): no experience bracket (F) & (G) (N = 26,705)
# condition (7): amount of donation is lower than incentive upper-bound (N = 24,923)
dt8 <- dt7 %>%
  dplyr::filter(d_relief_donate == 0 | (d_relief_donate == 1 & d_donate == 1)) %>%
  mutate(
    limit_incentive = taxable_tinc * 0.1,
    over_limit_incentive = if_else(donate >= limit_incentive, 1, 0)
  )

# * Write csv file
readr::write_csv(dt8, file = here("data/shaped2.csv"))