#4TaxWelfare


# /* パッケージのロード
library(xfun)
xfun::pkg_attach2(c("readstata13", "tidyverse", "rlist"))
xfun::pkg_attach2(c("plm", "lmtest", "sandwich", "lfe", "Formula"))
xfun::pkg_attach2("kableExtra")

lapply(Sys.glob(file.path("script/R/functions", "*.r")), source)
# */

# /* データの読み込み
df <- read.dta13("data/shaped.dta") %>%
  data.frame() %>%
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
    diff1_age = age - lag1_age,
    diff2_age = age - lag2_age,
    diff3_age = age - lag3_age,
    diff1_sqage = sqage - lag1_sqage,
    diff2_sqage = sqage - lag2_sqage,
    diff3_sqage = sqage - lag3_sqage
  ) %>%
  filter(year >= 2012 & age >= 24)
# */

#' # Government Efficiency and Price Elasticity
#'
#' From the 2015 survey,
#' NaSTaB asks the current and ideal balance
#' between tax burden and welfare level.
#' We use the obtained fixed effect as the efficiency index.
#' We consider that the efficiency index shows
#' the individual’s perception toward government for its efficiency.
#'
#+
balance_dt <- read.dta13("data/shape/balanceid.dta") %>%
  data.frame()

mergedf <- left_join(df, balance_dt, by = "pid") %>%
  mutate(balance3 = ntile(balanceid, 3)) %>%
  mutate(
    log_price_int2 = log_price * (balance3 == 2),
    log_price_int3 = log_price * (balance3 == 3),
  )

#'
#' The efficient index is concentrated around zero.
#' the density of those whose the efficient index is less than zero
#' is larger than of those whose the efficient index is greater than zero.
#'
#+ eval = FALSE
mergedf %>%
  dplyr::select(pid, balanceid) %>%
  distinct(.keep_all = TRUE) %>%
  ggplot(aes(x = balanceid)) +
  geom_histogram(aes(y = ..count..), fill = "grey80", color = "black") +
  labs(x = "Efficient index") +
  ggtemp()

#+
mergedf %>%
  dplyr::select(pid, balanceid, ideal_balanceid) %>%
  distinct(.keep_all = TRUE) %>%
  ggplot(aes(x = balanceid)) +
  stat_density(
    aes(linetype = "Full sample"),
    geom = "line", position = "identity"
  ) +
  stat_density(
    data = ~subset(.x, ideal_balanceid > 0),
    aes(linetype = "Ideal efficient index > 0"),
    geom = "line", position = "identity"
  ) +
  labs(x = "Efficient index", linetype = "") +
  ggtemp()

#'
#' To estimate the heterogenous price elasticity in the efficient index,
#' we include interaction terms between the efficient index and the giving price
#' into the model Eq. (1) and (2) for intensive and extensive margins,
#' respectively.
#' To construct interaction terms,
#' we partition respondents into three groups
#' based on three quantile of efficient index,
#' and create corresponding two dummy variables.
#'
#+
xlist3 <- ~ log_price + log_price_int2 + log_price_int3 + log_pinc_all +
  age + sqage + factor(year):factor(educ) + factor(year):factor(gender) +
  factor(year):factor(living_area)

xlist3_tab <- tibble(
  vars = c("Individual FE", "Time FE", "Other controls"),
  stat = "vars",
  reg1 = "Y"
)

#'
#' The price elasticitiy becomes more elastic
#' as people think the governement’s provision of public goods is inefficient.
#' However, the difference of implied price elasiticity is
#' not statistically significant since two interaction terms
#' between giving price and the quantile group of efficient index
#' are statistically insignificant.
#'
#+
wald_hetero1 <- list(
  "Implied price elasticity (1Q efficient group)" =
    "1*log_price",
  "Implied price elasticity (2Q efficient group)" =
    "1*log_price + 1*log_price_int2",
  "Implied price elasticity (3Q efficient group)" =
    "1*log_price + 1*log_price_int3",
  "Implied income elasticity" = "1*log_pinc_all"
)

overall_hetero <- est_felm(
  y = log_total_g ~ ., x = xlist3,
  fixef = ~ pid + year, cluster = ~ pid,
  data = mergedf
)

overallh_wald <- felm_wald(overall_hetero, wald_hetero1)

intensive_hetero <- est_felm(
  y = log_total_g ~ ., x = xlist3,
  fixef = ~ pid + year, cluster = ~pid,
  data = subset(mergedf, i_ext_giving == 1)
)

intensiveh_wald <- felm_wald(intensive_hetero, wald_hetero1)

wald_hetero2 <- list(
  "Implied price elasticity (1Q efficient group)" =
    "imp*log_price",
  "Implied price elasticity (2Q efficient group)" =
    "imp*log_price + imp*log_price_int2",
  "Implied price elasticity (3Q efficient group)" =
    "imp*log_price + imp*log_price_int3",
  "Implied income elasticity" = "imp*log_pinc_all"
)

extensive_hetero <- est_felm(
  y = i_ext_giving ~ ., x = xlist3,
  fixef = ~ pid + year, cluster = ~ pid,
  data = mergedf
)

extensiveh_wald <- felm_wald(
  extensive_hetero,
  hypo = wald_hetero2,
  args = list(imp = 1 / mean(extensive_hetero$response))
)

waldtab <- list(overallh_wald, extensiveh_wald, intensiveh_wald) %>%
  reduce(full_join, by = c("vars", "stat")) %>%
  setNames(c("vars", "stat", paste0("reg", seq_len(3))))

xlist3_duptab <- list(xlist3_tab, xlist3_tab, xlist3_tab) %>%
  reduce(full_join, by = c("vars", "stat")) %>%
  setNames(c("vars", "stat", paste0("reg", seq_len(3))))

list(overall_hetero, extensive_hetero, intensive_hetero) %>%
  felm_regtab(
    keep_coef = c("log_price", "log_pinc_all"),
    label_coef = list(
      "log_price" = "ln(giving price)",
      "log_price_int2" = "ln(giving price) x 2Q efficient group",
      "log_price_int3" = "ln(giving price) x 3Q efficient group",
      "log_pinc_all" = "ln(annual taxable income)"
    )
  ) %>%
  regtab_addline(list(waldtab, xlist3_duptab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(3))),
    align = "lccc"
  ) %>%
  kable_styling()


#'
#' Previous results show that
#' there is no heterogenous price elasticity in the efficient index.
#' This may be caused by existence of respondents
#' interpreting the survey questions as the questions about the expenditure.
#'
#' the heterogenous price elasticity in the efficient index
#' when we use those whose the ideal efficient index is greater than zero.
#' As a result, we found heterogeneity of the overall and extensive-margin
#' price elasticitiy in governement efficiency (column (1) and (2)).
#'
#+
overall_hetero2 <- est_felm(
  y = log_total_g ~ ., x = xlist3,
  fixef = ~ pid + year, cluster = ~pid,
  data = subset(mergedf, ideal_balanceid > 0)
)

overallh2_wald <- felm_wald(overall_hetero2, wald_hetero1)

intensive_hetero2 <- est_felm(
  y = log_total_g ~ ., x = xlist3,
  fixef = ~ pid + year, cluster = ~pid,
  data = subset(mergedf, i_ext_giving == 1 & ideal_balanceid > 0)
)

intensiveh2_wald <- felm_wald(intensive_hetero2, wald_hetero1)

extensive_hetero2 <- est_felm(
  y = i_ext_giving ~ ., x = xlist3,
  fixef = ~ pid + year, cluster = ~pid,
  data = subset(mergedf, ideal_balanceid > 0)
)

extensiveh2_wald <- felm_wald(
  extensive_hetero2,
  hypo = wald_hetero2,
  args = list(imp = 1 / mean(extensive_hetero2$response))
)

waldtab2 <- list(overallh2_wald, extensiveh2_wald, intensiveh2_wald) %>%
  reduce(full_join, by = c("vars", "stat")) %>%
  setNames(c("vars", "stat", paste0("reg", seq_len(3))))

list(overall_hetero2, extensive_hetero2, intensive_hetero2) %>%
  felm_regtab(
    keep_coef = c("log_price", "log_pinc_all"),
    label_coef = list(
      "log_price" = "ln(giving price)",
      "log_price_int2" = "ln(giving price) x 2Q efficient group",
      "log_price_int3" = "ln(giving price) x 3Q efficient group",
      "log_pinc_all" = "ln(annual taxable income)"
    )
  ) %>%
  regtab_addline(list(waldtab2, xlist3_duptab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(3))),
    align = "lccc"
  ) %>%
  kable_styling()
