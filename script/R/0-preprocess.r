#' shaped.dtaにラグ変数などを含めたデータを作成する
#'
#' パッケージのロード
#+
library(xfun)
xfun::pkg_attach2(c("haven", "tidyverse", "rlist"))

#' shaped.dtaを読み込む
#+
df <- haven::read_dta("data/shaped.dta") %>% as_tibble()

#' 変数の作成
#+
df <- df %>%
  mutate(
    log_price = log(price),
    log_lprice = log(lprice),
    log_iv1price = log(iv1price),
    log_iv2price = log(iv2price),
    log_iv3price = log(iv3price),
    log_total_g = log(i_total_giving + 1),
    log_pinc_all = log(lincome + 100000)
  ) %>%
  group_by(pid) %>%
  mutate(
    lag1_log_total_g = dplyr::lag(log_total_g, order_by = year),
    lag2_log_total_g = dplyr::lag(log_total_g, order_by = year, n = 2),
    lag3_log_total_g = dplyr::lag(log_total_g, order_by = year, n = 3),
    lag1_log_pinc_all = dplyr::lag(log_pinc_all, order_by = year),
    lag2_log_pinc_all = dplyr::lag(log_pinc_all, order_by = year, n = 2),
    lag3_log_pinc_all = dplyr::lag(log_pinc_all, order_by = year, n = 3),
    lag1_log_price = dplyr::lag(log_price, order_by = year),
    lag2_log_price = dplyr::lag(log_price, order_by = year, n = 2),
    lag3_log_price = dplyr::lag(log_price, order_by = year, n = 3),
    lag1_age = dplyr::lag(age, order_by = year),
    lag2_age = dplyr::lag(age, order_by = year, n = 2),
    lag3_age = dplyr::lag(age, order_by = year, n = 3),
    lag1_sqage = dplyr::lag(sqage, order_by = year),
    lag2_sqage = dplyr::lag(sqage, order_by = year, n = 2),
    lag3_sqage = dplyr::lag(sqage, order_by = year, n = 3)
  ) %>%
  ungroup() %>%
  mutate(
    log_diff1g = log_total_g - lag1_log_total_g,
    log_diff2g = log_total_g - lag2_log_total_g,
    log_diff3g = log_total_g - lag3_log_total_g,
    log_diff1I = log_pinc_all - lag1_log_pinc_all,
    log_diff2I = log_pinc_all - lag2_log_pinc_all,
    log_diff3I = log_pinc_all - lag3_log_pinc_all,
    log_diff1p = log_price - lag1_log_price,
    log_diff2p = log_price - lag2_log_price,
    log_diff3p = log_price - lag3_log_price,
    diff1_age = age - lag1_age,
    diff2_age = age - lag2_age,
    diff3_age = age - lag3_age,
    diff1_sqage = sqage - lag1_sqage,
    diff2_sqage = sqage - lag2_sqage,
    diff3_sqage = sqage - lag3_sqage
  )

#' データの期間と年齢を制限する
#+
df <- df %>% filter(year >= 2012 & age >= 24)

#' 雇用データと総合所得の申請変数を加えるために、オリジナルのデータを処理する
#+
original <- haven::read_dta("data/merge/merge.dta") %>%
  as_tibble() %>%
  dplyr::select(pid, year, p_aa005, paa008, pda207, pda209) %>%
  mutate(employee = if_else(p_aa005 == 1, 1, 0)) %>%
  mutate(
    ext_deduct_giving_tincome = case_when(
      as.numeric(pda207) == 1 ~ 1,
      as.numeric(pda207) == 2 ~ 0
    ),
    ext_credit_giving_tincome = case_when(
      as.numeric(pda209) == 1 ~ 1,
      as.numeric(pda209) == 2 ~ 0
    ),
    ext_benefit_tinc = if_else(
      year >= 2014, ext_credit_giving_tincome, ext_deduct_giving_tincome
    )
  ) %>%
  rename(industry = paa008) %>%
  dplyr::select(-p_aa005, -pda207, -pda209)

#' 既存のデータにマージする
#+
df <- df %>%
  left_join(original, by = c("pid", "year")) %>%
  mutate(
    ext_credit_giving_tincome = case_when(
      !is.na(ext_credit_giving_tincome) ~ ext_credit_giving_tincome,
      tincome == 0 ~ 0
    ),
    ext_deduct_giving_tincome = case_when(
      !is.na(ext_deduct_giving_tincome) ~ ext_deduct_giving_tincome,
      tincome == 0 ~ 0
    ),
    ext_credit_giving = case_when(
      !is.na(ext_credit_giving) ~ ext_credit_giving,
      lincome == 0 ~ 0
    ),
    ext_deduct_giving = case_when(
      !is.na(ext_deduct_giving) ~ ext_deduct_giving,
      lincome == 0 ~ 0
    ),
  ) %>%
  mutate(
    ext_benefit_l = if_else(
      year >= 2014, ext_credit_giving, ext_deduct_giving
    ),
    ext_benefit_t = if_else(
      year >= 2014, ext_credit_giving_tincome, ext_deduct_giving_tincome
    ),
    ext_benefit_tl = case_when(
      is.na(ext_benefit_l) ~ ext_benefit_t,
      is.na(ext_benefit_t) ~ ext_benefit_l,
      ext_benefit_t + ext_benefit_l == 0 ~ 0,
      ext_benefit_t + ext_benefit_l != 0 ~ 1
    ),
    int_price_benefit = log_price * ext_benefit_tl
  )

#' rename関連
#+
df <- df %>%
  rename(panelid = pid, area = living_area)

#' CSVファイルに書き出す
#+
readr::write_csv(df, file = "data/shaped2.csv")
