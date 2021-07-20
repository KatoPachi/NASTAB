#1elasticity

# /* パッケージのロード
library(xfun)
xfun::pkg_attach2(c("tidyverse", "rlist"))
xfun::pkg_attach2(c("plm", "lmtest", "sandwich", "lfe", "Formula"))
xfun::pkg_attach2("kableExtra")

lapply(Sys.glob(file.path("script/R/functions", "*.r")), source)
# */

# /* データの読み込み
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
# */

#' ## Price and Income Elasticity
#'
#+
xlist <- list(
  reg1 = ~ log_price + log_pinc_all,
  reg2 = ~ log_price + log_pinc_all + age + sqage,
  reg3 = ~ log_price + log_pinc_all + age + sqage + factor(year):factor(educ),
  reg4 = ~ log_price + log_pinc_all + age + sqage + factor(year):factor(educ) +
    factor(year):factor(gender),
  reg5 = ~ log_price + log_pinc_all + age + sqage + factor(year):factor(educ) +
    factor(year):factor(gender) + factor(year):factor(living_area)
)

xlist_tab <- tribble(
  ~vars, ~stat, ~reg1, ~reg2, ~reg3, ~reg4, ~reg5,
  "Individual FE", "vars", "Y", "Y", "Y", "Y", "Y",
  "Time FE", "vars", "Y", "Y", "Y", "Y", "Y",
  "Age", "vars", "N", "Y", "Y", "Y", "Y",
  "Year x Education", "vars", "N", "N", "Y", "Y", "Y",
  "Year x Gender", "vars", "N", "N", "N", "Y", "Y",
  "Year x Resident Area", "vars", "N", "N", "N", "N", "Y"
)

#'
#' We will estimate the elasticity without distinguishing them.
#' We call this elasticity as overall elasticity.
#' The price elasticity is rougly -1,
#' which is statistically significant different from zero.
#'
#+
overall <- xlist %>%
  purrr::map(~ est_felm(
    y = log_total_g ~ ., x = .,
    fixef = ~ year + pid, cluster = ~pid,
    data = df
  ))

overall %>%
  felm_regtab(
    keep_coef = c("log_price", "log_pinc_all"),
    label_coef = list(
      "log_price" = "ln(giving price)",
      "log_pinc_all" = "ln(annual taxable income)"
    )
  ) %>%
  regtab_addline(list(xlist_tab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist)))),
    align = "lccccc"
  ) %>%
  kable_styling()

#'
#' Compared to the overall elasticitiy,
#' the intensive-margin elasticitiy of price and income are less elastic.
#'
#+
intensive <- xlist %>%
  purrr::map(~ est_felm(
    y = log_total_g ~ ., x = .,
    fixef = ~ year + pid, cluster = ~pid,
    data = subset(df, i_ext_giving == 1)
  ))

intensive %>%
  felm_regtab(
    keep_coef = c("log_price", "log_pinc_all"),
    label_coef = list(
      "log_price" = "ln(giving price)",
      "log_pinc_all" = "ln(annual taxable income)"
    )
  ) %>%
  regtab_addline(list(xlist_tab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist)))),
    align = "lccccc"
  ) %>%
  kable_styling()

#'
#' By results of overall elasticities and the intensive-margin elasticities,
#' we expect that
#' the extensive-margin price and income elasticity is more elastic
#' than the overall elasticities.
#'
#' Although the implied price and income elasticity
#' varies with covariates, results are in line with our expectation.
#'
#+
extensive <- xlist %>%
  purrr::map(~ est_felm(
    y = i_ext_giving ~ ., x = .,
    fixef = ~ year + pid, cluster = ~pid,
    data = df
  ))

ext_wald <- extensive %>%
  purrr::map(~ felm_wald(
    .,
    hypo = list(
      "Implied price elasticity" = "imp * log_price",
      "Implied income elasticity" = "imp * log_pinc_all"
    ),
    args = list(imp = 1 / mean(.$response)),
  )) %>%
  reduce(full_join, by = c("vars", "stat")) %>%
  setNames(c("vars", "stat", paste0("reg", seq_len(length(xlist)))))

extensive %>%
  felm_regtab(
    keep_coef = c("log_price", "log_pinc_all"),
    label_coef = list(
      "log_price" = "ln(giving price)",
      "log_pinc_all" = "ln(annual taxable income)"
    )
  ) %>%
  regtab_addline(list(ext_wald, xlist_tab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist)))),
    align = "lccccc"
  ) %>%
  kable_styling()

#' ## Robustness Check
#'
#' The first robustness check estimates the last price elasticity
#' using the first price of giving as an instrument.
#'
#' Compared to the main results, the last price elasticity is more elastic.
#' The aboslute value of estimated coefficient is larger than 2.4,
#' which is statistically significant different from zero.
#'
#+
overall_iv <- xlist %>%
  purrr::map(~ est_felm(
    y = log_total_g ~ .,
    x = update(., ~ . - log_price),
    z = log_lprice ~ log_price,
    fixef = ~ year + pid, cluster = ~ pid,
    data = df
  ))

overall_iv %>%
  felm_regtab(
    keep_coef = c("log_lprice", "log_pinc_all"),
    label_coef = list(
      "`log_lprice(fit)`" = "ln(last giving price)",
      "log_pinc_all" = "ln(annual taxable income)"
    )
  ) %>%
  regtab_addline(list(xlist_tab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist)))),
    align = "lccccc"
  ) %>%
  kable_styling()

#'
#' The intensive-margin elasticity of last price is
#' similar value to the main results.
#'
#+
intensive_iv <- xlist %>%
  purrr::map(~ est_felm(
    y = log_total_g ~ .,
    x = update(., ~ . - log_price),
    z = log_lprice ~ log_price,
    fixef = ~ year + pid, cluster = ~pid,
    data = subset(df, i_ext_giving == 1)
  ))

intensive_iv %>%
  felm_regtab(
    keep_coef = c("log_lprice", "log_pinc_all"),
    label_coef = list(
      "`log_lprice(fit)`" = "ln(last giving price)",
      "log_pinc_all" = "ln(annual taxable income)"
    )
  ) %>%
  regtab_addline(list(xlist_tab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist)))),
    align = "lccccc"
  ) %>%
  kable_styling()

#'
#' The implied last price elasticity
#' evalueated at the sample mean is roughly -3.
#'
#+
extensive_iv <- xlist %>%
  purrr::map(~ est_felm(
    y = i_ext_giving ~ .,
    x = update(., ~ . - log_price),
    z = log_lprice ~ log_price,
    fixef = ~ year + pid, cluster = ~pid,
    data = df
  ))

extiv_wald <- extensive_iv %>%
  purrr::map(~ felm_wald(
    .,
    hypo = list(
      "Implied price elasticity" = "imp * `log_lprice(fit)`",
      "Implied income elasticity" = "imp * log_pinc_all"
    ),
    args = list(imp = 1 / mean(.$response)),
  )) %>%
  reduce(full_join, by = c("vars", "stat")) %>%
  setNames(c("vars", "stat", paste0("reg", seq_len(length(xlist)))))

extensive_iv %>%
  felm_regtab(
    keep_coef = c("log_lprice", "log_pinc_all"),
    label_coef = list(
      "`log_lprice(fit)`" = "ln(last giving price)",
      "log_pinc_all" = "ln(annual taxable income)"
    )
  ) %>%
  regtab_addline(list(extiv_wald, xlist_tab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist)))),
    align = "lccccc"
  ) %>%
  kable_styling()

#'
#' The second robustness check is to control the manupulation of giving price
#' by adjusting income level using two datasets whose
#' ranges are from (i) 2013 to 2018 and (ii) from 2013 to 2014.
#+
xlist2 <- list(
  ~ log_price + log_pinc_all,
  ~ log_price + log_pinc_all + age + sqage + factor(year):factor(educ) +
    factor(year):factor(gender) + factor(year):factor(living_area)
)

xlist2_tab <- tribble(
  ~vars, ~stat, ~reg1, ~reg2,
  "Individual FE", "vars", "Y", "Y",
  "Time FE", "vars", "Y", "Y",
  "Other Controls", "vars", "N", "Y",
)

xlist2_duptab <- xlist2_tab %>%
  full_join(xlist2_tab, by = c("vars", "stat")) %>%
  setNames(c("vars", "stat", paste0("reg", seq_len(4))))

#'
#' When we use data from 2013 to 2018, the estimated
#' price elasticity is similar value to the main results.
#' On the other hand, when we use data from 2013 to 2014,
#' the estimated price elasticity is more elastic than the main results.
#'
#+
overall_s1 <- xlist2 %>%
  purrr::map(~ est_felm(
    y = log_total_g ~ ., x = .,
    fixef = ~ year + pid, cluster = ~ pid,
    data = subset(df, year >= 2013)
  ))

overall_s2 <- xlist2 %>%
  purrr::map(~ est_felm(
    y = log_total_g ~ ., x = .,
    fixef = ~ year + pid, cluster = ~ pid,
    data = subset(df, year == 2013 | year == 2014)
  ))

list(overall_s1, overall_s2) %>%
  flatten() %>%
  felm_regtab(
    keep_coef = c("log_price", "log_pinc_all"),
    label_coef = list(
      "log_price" = "ln(giving price)",
      "log_pinc_all" = "ln(annual taxable income)"
    )
  ) %>%
  regtab_addline(list(xlist2_duptab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist2) * 2))),
    align = "lcccc"
  ) %>%
  kable_styling()

#'
#' When we use data from 2013 to 2018,
#' the intensive-margin price elasiticity is
#' similar to the main results, which is statistically significant from zero.
#' However, when we use data from 2013 to 2014 and
#' include only individual and time fixed effects,
#' the estimated coefficient is statistically insignificant different from zero.
#' By controlling covariates and its interaction with year dummies,
#' the intensive-margin price elasticity is -0.712,
#' which is statistically significant.
#'
#+
intensive_s1 <- xlist2 %>%
  purrr::map(~ est_felm(
    y = log_total_g ~ ., x = .,
    fixef = ~ year + pid, cluster = ~ pid,
    data = subset(df, year >= 2013 & i_ext_giving == 1)
  ))

intensive_s2 <- xlist2 %>%
  purrr::map(~ est_felm(
    y = log_total_g ~ ., x = .,
    fixef = ~ year + pid, cluster = ~ pid,
    data = subset(df, (year == 2013 | year == 2014) & i_ext_giving == 1)
  ))

list(intensive_s1, intensive_s2) %>%
  flatten() %>%
  felm_regtab(
    keep_coef = c("log_price", "log_pinc_all"),
    label_coef = list(
      "log_price" = "ln(giving price)",
      "log_pinc_all" = "ln(annual taxable income)"
    )
  ) %>%
  regtab_addline(list(xlist2_duptab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist2) * 2))),
    align = "lcccc"
  ) %>%
  kable_styling()

#'
#' When we use data from 2013 to 2018,
#' the extensive-margin price elasticity is similar to
#' the main results, which is statistically significant.
#' When we use data from 2013 to 2014,
#' the extensive-margin price elasticity is more elastic than the main results.
#'
#+
extensive_s1 <- xlist2 %>%
  purrr::map(~ est_felm(
    y = i_ext_giving ~ ., x = .,
    fixef = ~ year + pid, cluster = ~pid,
    data = subset(df, year >= 2013)
  ))

extensive_s2 <- xlist2 %>%
  purrr::map(~ est_felm(
    y = i_ext_giving ~ ., x = .,
    fixef = ~ year + pid, cluster = ~pid,
    data = subset(df, year == 2013 | year == 2014)
  ))

exts_wald <- list(extensive_s1, extensive_s2) %>%
  flatten() %>%
  purrr::map(~ felm_wald(
    .,
    hypo = list(
      "Implied price elasticity" = "imp * log_price",
      "Implied income elasticity" = "imp * log_pinc_all"
    ),
    args = list(imp = 1 / mean(.$response)),
  )) %>%
  reduce(full_join, by = c("vars", "stat")) %>%
  setNames(c("vars", "stat", paste0("reg", seq_len(length(xlist2) * 2))))

list(extensive_s1, extensive_s2) %>%
  flatten() %>%
  felm_regtab(
    keep_coef = c("log_price", "log_pinc_all"),
    label_coef = list(
      "log_price" = "ln(giving price)",
      "log_pinc_all" = "ln(annual taxable income)"
    )
  ) %>%
  regtab_addline(list(exts_wald, xlist2_duptab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist2) * 2))),
    align = "lccccc"
  ) %>%
  kable_styling()

#'
#' The third robustness check is
#' to estimate the k-th difference model.
#' The tax reform may have impacts on the giving price in two ways:
#' one is direct giving price change by tax reform and
#' the other is indirect change via wealth effect,
#' which is induced by tax reform.
#' Thus, to isolate the direct effect of the tax reform,
#' we use the information of income level k years before.
#'
#+
kdiff <- list(
  lag1 = list(
    y = log_diff1g ~ .,
    x = ~ log_iv1price + log_diff1I + diff1_age + diff1_sqage
  ),
  lag2 = list(
    y = log_diff2g ~ .,
    x = ~ log_iv2price + log_diff2I + diff2_age + diff2_sqage
  ),
  lag3 = list(
    y = log_diff3g ~ .,
    x = ~ log_iv3price + log_diff3I + diff3_age + diff3_sqage
  )
)

kdiff_tab <- tibble(
  vars = c("Individual FE", "Time FE", "Other controls"),
  stat = rep("vars", 3),
  reg1 = rep("Y", 3), reg2 = rep("Y", 3), reg3 = rep("Y", 3)
)

#'
#' When we take the one year lag (k = 1),
#' the overall price elasticity is roughly -1.9,
#' which is statistically significant.
#' This elasticity slightly varies
#' when we take the two or more year lag (k > 1).
#'
#+
overall_kdiff <- kdiff %>%
  purrr::map(~ est_felm(
    y = .$y,
    x = update(
      .$x,
      ~ . + factor(year):factor(educ) + factor(year):factor(gender) +
        factor(year):factor(living_area)
    ),
    fixef = ~ pid + year, cluster = ~ pid,
    data = df
  ))

overall_kdiff %>%
  felm_regtab(
    keep_coef = c("log_iv", "log_diff"),
    label_coef = list(
      "log_iv1price" = "1-year lagged difference of first price (log)",
      "log_iv2price" = "2-year lagged difference of first price (log)",
      "log_iv3price" = "3-year lagged difference of first price (log)",
      "log_diff1I" = "1-year lagged difference of annual income (log)",
      "log_diff2I" = "2-year lagged difference of annual income (log)",
      "log_diff3I" = "3-year lagged difference of annual income (log)"
    )
  ) %>%
  regtab_addline(list(kdiff_tab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(kdiff)))),
    align = "lccc"
  ) %>%
  kable_styling()

#'
#' When we take the one year lag (k = 1),
#' the intensive-margin price elasticity is roughly -1.8,
#' which is statistically significant.
#' The absolute value of the price elasticity is more than 2
#' when we take two or more year lag (k > 1)
#'
#+
intensive_kdiff <- kdiff %>%
  purrr::map(~ est_felm(
    y = .$y,
    x = update(
      .$x,
      ~ . + factor(year):factor(educ) + factor(year):factor(gender) +
        factor(year):factor(living_area)
    ),
    fixef = ~ pid + year, cluster = ~pid,
    data = subset(df, i_ext_giving == 1)
  ))

intensive_kdiff %>%
  felm_regtab(
    keep_coef = c("log_iv", "log_diff"),
    label_coef = list(
      "log_iv1price" = "1-year lagged difference of first price (log)",
      "log_iv2price" = "2-year lagged difference of first price (log)",
      "log_iv3price" = "3-year lagged difference of first price (log)",
      "log_diff1I" = "1-year lagged difference of annual income (log)",
      "log_diff2I" = "2-year lagged difference of annual income (log)",
      "log_diff3I" = "3-year lagged difference of annual income (log)"
    )
  ) %>%
  regtab_addline(list(kdiff_tab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(kdiff)))),
    align = "lccc"
  ) %>%
  kable_styling()
