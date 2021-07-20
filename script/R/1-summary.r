#0summary

## ---- library
library(xfun)
xfun::pkg_attach2(c("readstata13", "tidyverse", "rlist"))
xfun::pkg_attach2(c("plm", "lmtest", "sandwich"))
xfun::pkg_attach2("kableExtra")

lapply(Sys.glob(file.path("script/R/functions", "*.r")), source)

## ---- ReadData
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

## ---- SummaryOutcome
avgext <- df %>%
  group_by(year) %>%
  summarize_at(vars(i_ext_giving), list(mu = ~mean(., na.rm = TRUE)))

avgint <- df %>%
  filter(i_ext_giving == 1) %>%
  group_by(year) %>%
  summarize_at(vars(i_total_giving), list(mu = ~mean(., na.rm = TRUE)))

ggplot(avgext, aes(x = year, y = mu)) +
  geom_bar(aes(fill = "Extensive margin"), stat = "identity", color = "black") +
  geom_point(
    data = avgint, aes(x = year, y = mu / 1000, color = "Intensive margin"),
    size = 2
  ) +
  geom_line(
    data = avgint, aes(x = year, y = mu / 1000, color = "Intensive margin"),
    size = 1
  ) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 2013.5), color = "red", linetype = 2, size = 1) +
  scale_fill_manual(NULL, values = "grey80") +
  scale_color_manual(NULL, values = "blue") +
  scale_y_continuous(sec.axis = sec_axis(
    ~. * 1000,
    name = "Average Amout of Donations among Donors (Intensive margin)"
  )) +
  scale_x_continuous(breaks = seq(2012, 2018, 1)) +
  labs(x = "Year", y = "Proportion of Donors (Extensive margin)") +
  ggtemp()

## ---- SummaryCovariate
x <- c(
  "lincome", "price", "i_total_giving", "i_ext_giving",
  "now_balance", "ideal_balance",
  "age", "gender", "univ", "highschool", "juniorhigh"
)

df %>%
  sumvars(x, stat = c("N", "mean", "sd", "min", "median", "max")) %>%
  kable(digits = 2) %>%
  kable_styling()

## ---- SummaryPriceChange
df %>%
  filter(year == 2013) %>%
  dplyr::select(lincome, price) %>%
  ggplot(aes(x = lincome)) +
  geom_histogram(
    aes(y = ..count.. / sum(..count..), fill = "Relative frequency"),
    color = "black"
  ) +
  geom_step(
    aes(y = price * 0.5, color = "Giving Price in 2013"),
    size = 1
  ) +
  geom_hline(
    aes(yintercept = (1 - 0.15) * 0.5),
    color = "red", linetype = 2, size = 1
  ) +
  scale_color_manual(NULL, values = "blue") +
  scale_fill_manual(NULL, values = "grey80") +
  scale_y_continuous(
    breaks = seq(0, 0.5, 0.125),
    sec.axis = sec_axis(~ . / 0.5, name = "Giving price")
  ) +
  scale_x_continuous(breaks = c(1200, 4600, 8800, 30000)) +
  labs(x = "Annual taxable income (10,000KRW)", y = "Relative frequency") +
  ggtemp()
