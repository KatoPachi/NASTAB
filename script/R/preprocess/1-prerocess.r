#'
#' raw dataはdata/merge/merge.dta
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
  )
