#0summary

## ---- library
library(xfun)
xfun::pkg_attach2(c("tidyverse", "rlist"))
xfun::pkg_attach2(c("plm", "lmtest", "sandwich"))
xfun::pkg_attach2("kableExtra")

lapply(Sys.glob(file.path("script/R/functions", "*.r")), source)

## ---- ReadData
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
