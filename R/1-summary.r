#+ include = FALSE, eval = params$preview
library(here)
source(here("R", "_library.r"))

#'
#+ include = FALSE
use <- readr::read_csv(here("data/shaped2.csv")) %>%
  dplyr::filter(2010 <= year & year < 2018) %>%
  dplyr::filter(tinc < 1100 | 1300 < tinc) %>%
  dplyr::filter(tinc < 4500 | 4700 < tinc) %>%
  dplyr::filter(tinc < 8700 | 8900 < tinc) %>%
  dplyr::filter(tinc < 14000 | 16000 < tinc) %>%
  dplyr::filter(tinc < 30000) %>%
  dplyr::filter(bracket13 != "(F) & (G) 30000--" | is.na(bracket13)) %>%
  dplyr::filter(dependents == 0) %>%
  dplyr::filter(tinc > donate) %>%
  dplyr::filter(d_relief_donate == 0 | donate <= religious_ub)

#' //NOTE: Summary statistics
#+ SummaryCovariate, eval = output_type() != "appx"
out.file <- file(here("export", "tables", "summary-stats.tex"), open = "w")

use %>%
  datasummary(
    (`Annual labor income (unit: 10,000KRW)` = linc) +
    (`Annual total income (unit: 10,000KRW)` = tinc) +
    (`Appricale price` = price) +
    (`Annual chariatable giving (unit: 10,000KRW)` = donate) +
    (`Dummary of donation > 0` = d_donate) +
    (`Dummy of declaration of a tax relief` = d_relief_donate) +
    (`Age` = age) +
    (`Wage earner dummy` = employee) +
    (`Number of household members` = hh_num) +
    (`Dummy of having dependents` = have_dependents) +
    (`Female dummy` = sex) +
    (`Academic history: University` = college) +
    (`Academic history: High school` = highschool) ~
    N +
    (`Mean` = Mean) +
    (`Std.Dev.` = SD), #+
    # (`Min` = Min) +
    # (`Median` = Median) +
    # (`Max` = Max),
    title = "Descriptive Statistics\\label{tab:summary-covariate}",
    data = .,
    align = "lccc",
    output = "latex",
    escape = FALSE
  ) %>%
  kable_styling(font_size = 8) %>%
  pack_rows("Income and giving price", 1, 3, bold = FALSE, italic = TRUE) %>%
  pack_rows("Charitable giving", 4, 6, bold = FALSE, italic = TRUE) %>%
  pack_rows("Demographics", 7, 13, bold = FALSE, italic = TRUE) %>%
  footnote(
    general_title = "",
    general = "Notes: Our data is unbalanced panel data consisting of 8,441 unique individuals and 8 years period (2010--2017)",
    threeparttable = TRUE,
    escape = FALSE
  ) %>%
  writeLines(out.file)

close(out.file)

#' //NOTE: Income distribution and giving price
#+ SummaryPrice, fig.cap = "Income Distribution in 2013 and Relative Giving Price. Notes: The left and right axis measure the relative frequency of respondents (grey bars) and the relative giving price (solid step line and dashed line), respectively. A solid step line and a dashed horizontal line represents the giving price in 2013 and 2014, respectively.", out.extra = "", eval = output_type() != "appx"
plot_price <- use %>%
  filter(year == 2013) %>%
  dplyr::select(tinc, price) %>%
  ggplot(aes(x = tinc)) +
  geom_hline(aes(yintercept = 0)) +
  geom_histogram(
    aes(y = ..count.. / sum(..count..), fill = "Relative frequency"),
    color = "black"
  ) +
  geom_step(
    aes(y = price * 0.5, color = "Giving Price in 2010-2013"),
    size = 1
  ) +
  geom_hline(
    aes(yintercept = (1 - 0.15) * 0.5),
    color = "black", linetype = 2, size = 1
  ) +
  scale_color_manual(NULL, values = "black") +
  scale_fill_manual(NULL, values = "grey80") +
  scale_y_continuous(
    breaks = seq(0, 0.5, 0.125),
    sec.axis = sec_axis(~ . / 0.5, name = "Giving Price")
  ) +
  scale_x_continuous(breaks = c(1200, 4600, 8800, 30000)) +
  labs(
    x = "Annual total income (10,000KRW)",
    y = "Relative frequency"
  ) +
  ggtemp(size = list(title = 15, text = 13, caption = 13))

plot_price

ggsave(
  here("export", "figures", "price-income-dist.pdf"),
  plot = plot_price,
  width = 10,
  height = 6
)

#' //NOTE: Time-trend of giving
#+ SummaryGiving, fig.cap = "Proportion of Donors and Average Donations among Donors. Notes: The left and right axises measure prooortion of donors (grey bars) and the average amount of donations among donors (solid line), respectively.", out.extra = "", eval = output_type() != "appx"
plot_giving <- use %>%
  mutate(
    donate = if_else(d_donate == 1, donate, NA_real_)
  ) %>%
  group_by(year) %>%
  summarize_at(
    vars(d_donate, donate), list(~ mean(., na.rm = TRUE))
  ) %>%
  ggplot(aes(x = year)) +
  geom_bar(
    aes(y = d_donate, fill = "Proportion of Donors"),
    stat = "identity", color = "black"
  ) +
  geom_point(
    aes(
      y = donate / 1000,
      color = "Average Amoount of Donations among Donors"
    ),
    size = 3
  ) +
  geom_line(
    aes(
      y = donate / 1000,
      color = "Average Amoount of Donations among Donors"
    )
  ) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 2013.5), linetype = 2, size = 1) +
  scale_fill_manual(NULL, values = "grey80") +
  scale_color_manual(NULL, values = "black") +
  scale_y_continuous(sec.axis = sec_axis(
    ~ . * 1000,
    name = "Average Amout of Donations among Donors \n (unit: 10,000KRW)"
  )) +
  scale_x_continuous(breaks = seq(2010, 2018, 1)) +
  labs(x = "Year", y = "Proportion of Donors") +
  ggtemp(size = list(title = 15, text = 13))

ggsave(
  here("export", "figures", "giving-time-series.pdf"),
  plot = plot_giving,
  width = 10,
  height = 6
)

#' //NOTE: Giving by Tax Reform
#+ SummaryGivingOverall, fig.cap = "Average Logged Giving by Three Income Groups. Notes: We created three income groups, with the relative price of giving rising (circle), unchanged (triangle), and falling (square) between 2013 and 2014. The group averages are normalized to be zero in 2013.", out.extra = "", eval = output_type() != "appx"
plot_giving2 <- use %>%
  dplyr::filter(!is.na(bracket13)) %>%
  group_by(year, bracket13) %>%
  summarize(mu = mean(donate, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = "year", values_from = "mu") %>%
  mutate(base = `2013`) %>%
  dplyr::select(bracket13, base, everything()) %>%
  tidyr::pivot_longer(
    -(bracket13:base), values_to = "mu", names_to = "year"
  ) %>%
  mutate(mu = mu / base, year = as.numeric(year)) %>%
  ggplot(aes(x = year, y = mu, group = bracket13)) +
  geom_vline(aes(xintercept = 2013.5), linetype = 3) +
  geom_point(aes(shape = bracket13), size = 4) +
  geom_line() +
  scale_shape_manual(values = c(16, 15, 17, 18)) +
  scale_x_continuous(breaks = seq(2010, 2018, 1)) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0, 2.6)) +
  labs(
    title = "Panel A. Amount of Giving",
    x = "Year",
    y = "Normalized average giving",
    shape = "Income bracket (unit:10,000KRW)"
  ) +
  ggtemp(size = list(title = 15, text = 13, caption = 13))

plot_giving3 <- use %>%
  dplyr::filter(!is.na(bracket13)) %>%
  dplyr::filter(d_donate == 1) %>%
  group_by(year, bracket13) %>%
  summarize(mu = mean(donate, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = "year", values_from = "mu") %>%
  mutate(base = `2013`) %>%
  dplyr::select(bracket13, base, everything()) %>%
  tidyr::pivot_longer(
    -(bracket13:base), values_to = "mu", names_to = "year"
  ) %>%
  mutate(mu = mu / base, year = as.numeric(year)) %>%
  ggplot(aes(x = year, y = mu, group = bracket13)) +
  geom_vline(aes(xintercept = 2013.5), linetype = 3) +
  geom_point(aes(shape = bracket13), size = 4) +
  geom_line() +
  scale_shape_manual(values = c(16, 15, 17, 18)) +
  scale_x_continuous(breaks = seq(2010, 2018, 1)) +
  scale_y_continuous(breaks = seq(0, 2, by = 0.5), limits = c(0, 2)) +
  labs(
    title = "Panel A. Amount of Giving Conditional on Donors",
    x = "Year",
    y = "Normalized average giving",
    shape = "Income bracket (unit:10,000KRW)"
  ) +
  ggtemp(size = list(title = 15, text = 13, caption = 13))

plot_giving4 <- use %>%
  dplyr::filter(!is.na(bracket13)) %>%
  group_by(year, bracket13) %>%
  summarize(mu = mean(d_donate, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = "year", values_from = "mu") %>%
  mutate(base = `2013`) %>%
  dplyr::select(bracket13, base, everything()) %>%
  tidyr::pivot_longer(
    -(bracket13:base), values_to = "mu", names_to = "year"
  ) %>%
  mutate(mu = mu / base, year = as.numeric(year)) %>%
  ggplot(aes(x = year, y = mu, group = bracket13)) +
  geom_vline(aes(xintercept = 2013.5), linetype = 3) +
  geom_point(aes(shape = bracket13), size = 4) +
  geom_line() +
  scale_shape_manual(values = c(16, 15, 17, 18)) +
  scale_x_continuous(breaks = seq(2010, 2018, 1)) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0, 2.6)) +
  labs(
    title = "Panel B. Proportion of Donors",
    x = "Year",
    y = "Normalized proportion of donors",
    shape = "Income bracket (unit:10,000KRW)"
  ) +
  ggtemp(size = list(title = 15, text = 13, caption = 13))

plot_merge <- plot_giving2 + plot_giving4 +
  plot_layout(guides = "collect") &
  theme(legend.position = 'bottom') &
  guides(shape = guide_legend(title.position = "top", title.hjust = 0.5))

ggsave(
  here("export", "figures", "intensive-extensive-tax-reform.pdf"),
  plot = plot_merge,
  width = 10,
  height = 6
)

#' //NOTE: Tax relief by tax reform
#+ SummaryReliefbyIncome, fig.cap = "Proportion of Having Applied for Tax Relief by Three Income Groups. Notes: We created three income groups, with the relative price of giving rising (circle), unchanged (triangle), and falling (square) between 2013 and 2014. The group averages are normalized to be zero in 2013.", out.extra = "", eval = FALSE
plot_relief1 <- use %>%
  dplyr::filter(!is.na(bracket13)) %>%
  group_by(year, bracket13) %>%
  summarize(mu = mean(d_relief_donate, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mu, group = bracket13)) +
  geom_vline(aes(xintercept = 2013.5), linetype = 3) +
  geom_point(aes(shape = bracket13), size = 4) +
  geom_line() +
  scale_shape_manual(values = c(16, 15, 17, 18)) +
  scale_x_continuous(breaks = seq(2010, 2018, 1)) +
  labs(
    title = "Panel A. Grouped by Income Bracket",
    x = "Year",
    y = "Proportion of application for tax relief",
    shape = "Income bracket (unit:10,000KRW)"
  ) +
  ggtemp(size = list(title = 15, text = 13, caption = 13)) +
  guides(shape = guide_legend(
    title.position = "top", title.hjust = 0.5, nrow = 2
  ))

plot_relief2 <- use %>%
  dplyr::filter(!is.na(employee)) %>%
  mutate(employee = factor(
    employee,
    levels = c(1, 0), labels = c("Wage earners", "Others")
  )) %>%
  group_by(year, employee) %>%
  summarize_at(vars(d_relief_donate), list(~ mean(., na.rm = TRUE))) %>%
  mutate(employee = factor(employee)) %>%
  ggplot(aes(x = year, y = d_relief_donate, group = employee)) +
  geom_point(aes(shape = employee), color = "black", size = 4) +
  geom_line(aes(linetype = employee)) +
  geom_vline(aes(xintercept = 2013.5), linetype = 3) +
  scale_x_continuous(breaks = seq(2010, 2018, 1)) +
  scale_y_continuous(limits = c(0, 0.4), breaks = seq(0, 1, 0.1)) +
  labs(
    title = "Panel B. Grouped by Wage Earner or Not",
    x = "Year",
    y = "Proportion of application for tax relief",
    shape = "",
    linetype = ""
  ) +
  ggtemp(size = list(title = 15, text = 13))

plot_relief_merge <- plot_relief1 + plot_relief2

ggsave(
  here("export", "figures", "summary-tax-relief.pdf"),
  plot = plot_relief_merge,
  width = 10,
  height = 6
)

#' ```{asis, echo = output_type() == "slide"}
#' ## Distribution of Giving Amount by Application of Tax Relief 
#' ```
#+ SummaryGivingIntensiveDist, fig.cap = "Estimated Distribution of Charitable Giving among Donors in Each Year", out.extra = "", eval = output_type() != "body"
plot_dist <- use %>%
  filter(!is.na(d_relief_donate) & d_donate == 1) %>%
  mutate(d_relief_donate = factor(
    d_relief_donate,
    levels = 1:0,
    labels = c("Applied for tax relief", "Did not apply for tax relief")
  )) %>%
  ggplot(aes(x = donate_ln)) +
  geom_density(aes(linetype = d_relief_donate)) +
  facet_wrap(~ year) +
  labs(
    x = "Charitable Giving (Logged Value)",
    linetype = ""
  ) +
  ggtemp(size = list(title = 15, text = 13, caption = 13))

plot_dist

ggsave(
  here("export", "figures", "giving-dist-by-apply.pdf"),
  plot = plot_dist,
  width = 10,
  height = 6
)