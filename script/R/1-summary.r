#' ---
#' title: "Preview: Data"
#' author: Hiroki Kato
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#' params:
#'   preview: true
#' ---
#'
#+ include = FALSE, eval = params$preview
knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  echo = FALSE,
  cache = FALSE,
  include = TRUE,
  fig.width = 10
)

library(here)
knitr::opts_knit$set(
  root.dir = here::here()
)

options(
  knitr.kable.NA = " ",
  knitr.table.format = "html",
  modelsummary_stars_note = FALSE
)


#'
#+ include = FALSE, eval = params$preview
library(xfun)
xfun::pkg_attach2(c(
  "tidyverse", "rlist", "modelsummary", "kableExtra",
  "estimatr", "fixest"
))

lapply(Sys.glob(file.path("script/R/functions", "*.r")), source)

#'
#+
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
    ideal_balance = col_double(),
    accountant = col_double(),
    consult = col_double()
  )
) %>%
dplyr::filter(
  ext_benefit_tl == 0 | (ext_benefit_tl == 1 & i_ext_giving == 1)
)

#'
#' ## Data
#'
#' We use the Korean annual financial panel survey,
#' called the National Survey of Tax and Benefit (hereafter, NaSTaB).
#'
#' - The subjects of this survey are general households and household members living in 15 cities and provinces nationwide.
#' - This survey is based on a face-to-face interview.
#' - Data is constructed as the subjects represent the population of Korean society.
#' - We exclude the subject of the sample, whose age is under 23, since they are not likely to have income or assets.
#' - We use data from 2013 to 2017.
#'
#' ## Descriptive Statistics
#'
#+ SummaryCovariate
df %>%
  datasummary(
    (`Annual chariatable giving (unit: 10,000KRW)` = i_total_giving) +
    (`Dummary of donation > 0` = i_ext_giving) +
    (`Annual taxable labor income (unit: 10,000KRW)` = lincome) +
    (`First giving relative price` = price) +
    (`Dummy of declaration of a tax relief` = ext_benefit_tl) +
    (`Age` = age) +
    (`Female dummy` = gender) +
    (`University graduate` = univ) +
    (`High school graduate dummy` = highschool) +
    (`Junior high school graduate dummy` = juniorhigh) +
    (`Wage earner dummy` = employee) +
    (`#.Tax accountant / population` = tax_accountant_per) ~
    N +
    (`Mean` = mean) * Arguments(na.rm = TRUE) +
    (`Std.Dev.` = sd) * Arguments(na.rm = TRUE) +
    (`Min` = min) * Arguments(na.rm = TRUE) +
    (`Median` = median) * Arguments(na.rm = TRUE) +
    (`Max` = max) * Arguments(na.rm = TRUE),
    title = "Descriptive Statistics",
    data = .,
    align = "lcccccc"
  ) %>%
  kableExtra::kable_styling(font_size = 7) %>%
  kableExtra::pack_rows("Charitable Donations", 1, 2) %>%
  kableExtra::pack_rows("Income, giving price, and tax report", 3, 5) %>%
  kableExtra::pack_rows("Covariates", 6, 12)

#'
#' ## Summary Statistics: Charitable Giving
#'
#+ SummaryOutcome, fig.cap = "Proportion of Donors and Average Donations among Donors. Notes: The left and right axises measure prooortion of donors and the average amount of donations among donors, respectively. Authors made this graph based on NaSTaB data.", out.extra = "", out.width = "70%"
df %>%
  mutate(
    i_total_giving = if_else(i_ext_giving == 1, i_total_giving, NA_real_)
  ) %>%
  group_by(year) %>%
  summarize_at(
    vars(i_ext_giving, i_total_giving), list(~mean(., na.rm = TRUE))
  ) %>%
  ggplot(aes(x = year)) +
  geom_bar(
    aes(y = i_ext_giving, fill = "Proportion of Donors"),
    stat = "identity", color = "black"
  ) +
  geom_point(
    aes(
      y = i_total_giving / 1000,
      color = "Average Amoount of Donations among Donors"
    ),
    size = 2
  ) +
  geom_line(
    aes(
      y = i_total_giving / 1000,
      color = "Average Amoount of Donations among Donors"
    ),
    size = 1
  ) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 2013.5), color = "red", linetype = 2, size = 1) +
  scale_fill_manual(NULL, values = "grey80") +
  scale_color_manual(NULL, values = "blue") +
  scale_y_continuous(sec.axis = sec_axis(
    ~ . * 1000,
    name = "Average Amout of Donations among Donors"
  )) +
  scale_x_continuous(breaks = seq(2012, 2018, 1)) +
  labs(x = "Year", y = "Proportion of Donors") +
  ggtemp(size = list(title = 15, text = 13))

#'
#' ## Summary: Income and Giving Price
#'
#+ SummaryPrice, fig.cap = "Income Distribution and Relative Giving Price in 2013. Notes: The left and right axis measure the relative frequency of respondents and the relative giving price, respectively. A blue step line and a red dashed horizontal line represents the giving price in 2013 and 2014, respectively. The grey bar shows income distribution in 2013.", out.extra = "", out.width = "70%"
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
    sec.axis = sec_axis(~ . / 0.5, name = "Giving Price")
  ) +
  scale_x_continuous(breaks = c(1200, 4600, 8800, 30000)) +
  labs(
    x = "Annual taxable income (10,000KRW)",
    y = "Relative Frequency",
    caption = "Red dashed line is the giving price after the 2014 tax reform."
  ) +
  ggtemp(size = list(title = 15, text = 13, caption = 13))

#'
#' ## Summary: Charitable Giving by Income Group
#'
#+ SummaryOutcome2, fig.cap = "Average Logged Giving in Three Income Groups. Notes: We created three income groups, with the relative price of giving rising (circle), unchanged (triangle), and falling (square) between 2013 and 2014.", out.extra = "", out.width = "70%"
df %>%
  mutate(group = case_when(
    credit_loss == 1 ~ 3,
    credit_neutral == 1 ~ 2,
    credit_benefit == 1 ~ 1
  )) %>%
  dplyr::filter(year <= 2017) %>%
  dplyr::filter(!is.na(group)) %>%
  group_by(year, group) %>%
  summarize(mu = mean(log_total_g, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = "year", values_from = "mu") %>%
  mutate(base = `2013`) %>%
  dplyr::select(group, base, everything()) %>%
  tidyr::pivot_longer(-(group:base), values_to = "mu", names_to = "year") %>%
  mutate(mu = mu / base, year = as.numeric(year)) %>%
  mutate(group = factor(
    group,
    labels = c("< 1200", "[1200, 4600]", "> 4600")
  )) %>%
  ggplot(aes(x = year, y = mu, group = group)) +
  geom_point(aes(shape = group), size = 4) +
  geom_line() +
  geom_vline(aes(xintercept = 2013.5), linetype = 3) +
  scale_x_continuous(breaks = seq(2012, 2018, 1)) +
  labs(
    x = "Year",
    y = "The ratio of mean donations (logged value)",
    shape = "Income group (unit:10,000KRW)",
    caption = paste(
      "The ratio is calculated by",
      "(mean of logged donation in year t) / (mean of logged donation in 2013)."
    )
  ) +
  ggtemp(size = list(title = 15, text = 13, caption = 13))

#'
#' ## Summary: Share of Tax Relief
#'
#+ SummaryRelief, fig.cap = "Share of Tax Relief. Notes: A solid line is the share of applying for tax relief among wage eaners. A dashed line is the share of applying for tax relief other than wage earners.", out.extra = "", out.width = "70%"
df %>%
  dplyr::filter(year <= 2017) %>%
  dplyr::filter(!is.na(employee) & i_ext_giving == 1) %>%
  mutate(employee = factor(
    employee,
    levels = c(1, 0), labels = c("Yes", "No")
  )) %>%
  group_by(year, employee) %>%
  summarize_at(vars(ext_benefit_tl), list(~ mean(., na.rm = TRUE))) %>%
  mutate(employee = factor(employee)) %>%
  ggplot(aes(x = year, y = ext_benefit_tl, group = employee)) +
  geom_point(aes(shape = employee), color = "black", size = 4) +
  geom_line(aes(linetype = employee)) +
  geom_vline(aes(xintercept = 2013.5)) +
  scale_x_continuous(breaks = seq(2012, 2018, 1)) +
  labs(
    x = "Year",
    y = "share of declaration of a tax relief",
    caption = paste(
      "The share is calculated by",
      "(#. Respondents who applied for tax relief)",
      "/ (#. Respondents who donated)."
    ),
    shape = "Wage earner",
    linetype = "Wage earner"
  ) +
  ggtemp(size = list(title = 15, text = 13))

#'
# /*
#+
rmarkdown::render(
  "script/R/1-summary.r",
  output_dir = "report/view"
)
# */