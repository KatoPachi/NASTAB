#' ---
#' title: "Preview: Data"
#' author: Hiroki Kato
#' output:
#'   bookdown::html_document2:
#'     toc: true
#'     toc_float: true
#'     number_sections: false
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

lapply(Sys.glob(file.path("script/functions", "*.r")), source)

#'
#+
df <- readr::read_csv("data/shaped2.csv")

#'
#' ## National Survey of Tax and Benefit (NaSTaB)
#'
#' - NaSTaB has been implemented by The Korea Institute of Taxation and Finance since 2008
#' - The NaSTaB is an annual panel data on households' tax burden and public benefits
#' - The unit of analysis is 5,634 households throughout the country
#'   - 5,634 family heads and family members with more than 15 years old and with income or economically active
#' - Our analysis uses the NaSTaB data from (i) 2013-2018 and (ii) excluding respondents under the age of 23.
#'   - using the NaSTaB data before 2012 captures the effects of other tax reform than the reform in 2014.
#'   - we exclude respondents whose age is under 23 because they are not likely to have income or assets.
#'
#' ## Descriptive Statistics
#'
#+ SummaryCovariate
df %>%
  dplyr::filter(year <= 2017) %>%
  datasummary(
    (`Annual taxable labor income (unit: 10,000KRW)` = linc) +
    (`First giving relative price` = price) +
    (`Annual chariatable giving (unit: 10,000KRW)` = donate) +
    (`Dummary of donation > 0` = d_donate) +
    (`Dummy of declaration of a tax relief` = d_relief_donate) +
    (`Age` = age) +
    (`Female dummy` = sex) +
    (`University graduate` = univ) +
    (`High school graduate dummy` = highschool) +
    (`Junior high school graduate dummy` = junior) +
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
  kableExtra::kable_styling(font_size = 6) %>%
  kableExtra::pack_rows("Income and giving price", 1, 2) %>%
  kableExtra::pack_rows("Charitable giving", 3, 5) %>%
  kableExtra::pack_rows("Individual Characteristics", 6, 12)

#'
#' ## Right-Skewed Income Distribution and Price Variation for Identification
#'
#+ SummaryPrice, fig.cap = "Income Distribution in 2013 and Relative Giving Price. Notes: The left and right axis measure the relative frequency of respondents (grey bars) and the relative giving price (solid step line and dashed line), respectively. A solid step line and a dashed horizontal line represents the giving price in 2013 and 2014, respectively.", out.extra = ""
df %>%
  filter(year == 2013) %>%
  dplyr::select(linc, price) %>%
  ggplot(aes(x = linc)) +
  geom_histogram(
    aes(y = ..count.. / sum(..count..), fill = "Relative frequency"),
    color = "black"
  ) +
  geom_step(
    aes(y = price * 0.5, color = "Giving Price in 2012-2013"),
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
    x = "Annual taxable income (10,000KRW)",
    y = "Relative frequency",
    caption = "Dashed line is the giving price after the 2014 tax reform."
  ) +
  ggtemp(size = list(title = 15, text = 13, caption = 13))

#'
#' ## Message from Figure \@ref(fig:SummaryPrice)
#'
#' Right-skewed income distribution:
#'
#' - NaSTaB contains the annual taxable labor income last year
#' - Our sample includes subjects with no labor income (e.g. housewives)
#'   - Table \@ref(tab:SummaryCovariate): the average income is 17.54 million KRW
#' - National Tax Statistical Yearbook 2012-2018 (Korean National Tax Service): the average annual taxable income is 32.77 million KRW
#'   - Sample: employees who submitted the tax return
#'
#' Price variation for indetification
#'
#' - Based on changes in tax incentive due to the 2014 tax reform, we can devide into three income groups:
#'     1. less than 120 million KRW: expanded tax incentive (decreased giving price)
#'     1. between 120 million KRW and 460 million KRW: unchanged tax incentive
#'     1. more than 460 million KRW: decreased tax incentive (increaed giving price)
#' - This is main source of identification for effect of tax incentive on giving.
#'
#' ## Donors Decreased Immediately After Tax Reform
#'
#+ SummaryGiving, fig.cap = "Proportion of Donors and Average Donations among Donors. Notes: The left and right axises measure prooortion of donors (grey bars) and the average amount of donations among donors (solid line), respectively.", out.extra = ""
df %>%
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
  scale_x_continuous(breaks = seq(2012, 2018, 1)) +
  labs(x = "Year", y = "Proportion of Donors") +
  ggtemp(size = list(title = 15, text = 13))

#'
#' ## Message from Figure \@ref(fig:SummaryGiving)
#'
#' - Proportion of donors across years: 24% (Table \@ref(tab:SummaryCovariate))
#' - 2014: Proportion of donors is lower than before the tax reform
#'   - After that, proportion of donors has continued to increase, finally surpassing that before the tax reform
#'
#' Notes:
#'
#' - average donation conditional on donors has been stable across year
#'   - 1.5 million KRW (7% of average income)
#'   - Table \@ref(tab:SummaryCovariate): Unconditional average donation is 358,600 KRW (2% of average income)
#'
#' <!--- 加藤コメント：欧米圏の寄付との簡単な比較があると文化差が伝わるかも --->
#'
#' ## Price Effect Can Be Observed
#'
#+ SummaryGivingOverall, fig.cap = "Average Logged Giving by Three Income Groups. Notes: We created three income groups, with the relative price of giving rising (circle), unchanged (triangle), and falling (square) between 2013 and 2014. The group averages are normalized to be zero in 2013.", out.extra = ""
df %>%
  dplyr::filter(year <= 2017) %>%
  dplyr::filter(!is.na(credit_treat)) %>%
  group_by(year, credit_treat) %>%
  summarize(mu = mean(donate_ln, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = "year", values_from = "mu") %>%
  mutate(base = `2013`) %>%
  dplyr::select(credit_treat, base, everything()) %>%
  tidyr::pivot_longer(
    -(credit_treat:base), values_to = "mu", names_to = "year"
  ) %>%
  mutate(mu = mu - base, year = as.numeric(year)) %>%
  mutate(credit_treat = factor(
    credit_treat,
    labels = c("< 1200", "[1200, 4600]", "> 4600")
  )) %>%
  ggplot(aes(x = year, y = mu, group = credit_treat)) +
  geom_vline(aes(xintercept = 2013.5), linetype = 3) +
  geom_hline(aes(yintercept = 0), linetype = 3) +
  geom_point(aes(shape = credit_treat), size = 4) +
  geom_line() +
  scale_x_continuous(breaks = seq(2012, 2018, 1)) +
  labs(
    x = "Year",
    y = "Diff. of average logged giving",
    shape = "Income credit_treat (unit:10,000KRW)",
    caption = paste(
      "The difference is calculated by",
      "(mean of logged donation in year t) - (mean of logged donation in 2013)."
    )
  ) +
  ggtemp(size = list(title = 15, text = 13, caption = 13))

#'
#' ## Message from Figure \@ref(fig:SummaryGivingOverall)
#'
#' Price effect = Tax incentive increases charitable giving
#'
#' - Donations for income groups with unchanged or increased tax incentives have exceeded those in 2013 since 2015
#' - Donations for income groups with reduced tax incentives have been lower than before tax reform since 2015
#'
#' Other findings:
#'
#' 1. Prior to the 2014 tax reform, donations did not change in all groups
#' 1. Donations for all groups were lower than in 2013
#'     - Donations for income groups with reduced tax incentive due to the 2014 tax reform was 40% of that in 2013
#'     - Announcement effect? Learning effect?
#'
#' ## Price Effect Can Be Partially Observed for Intensive Margin
#'
#+ SummaryGivingIntensive, fig.cap = "Average Logged Giving by Three Income Groups Conditional on Donors. Notes: We created three income groups, with the relative price of giving rising (circle), unchanged (triangle), and falling (square) between 2013 and 2014. The group averages are normalized to be zero in 2013.", out.extra = ""
df %>%
  dplyr::filter(year <= 2017) %>%
  dplyr::filter(!is.na(credit_treat) & d_donate == 1) %>%
  group_by(year, credit_treat) %>%
  summarize(mu = mean(donate_ln, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = "year", values_from = "mu") %>%
  mutate(base = `2013`) %>%
  dplyr::select(credit_treat, base, everything()) %>%
  tidyr::pivot_longer(
    -(credit_treat:base), values_to = "mu", names_to = "year"
  ) %>%
  mutate(mu = mu - base, year = as.numeric(year)) %>%
  mutate(credit_treat = factor(
    credit_treat,
    labels = c("< 1200", "[1200, 4600]", "> 4600")
  )) %>%
  ggplot(aes(x = year, y = mu, group = credit_treat)) +
  geom_vline(aes(xintercept = 2013.5), linetype = 3) +
  geom_hline(aes(yintercept = 0), linetype = 3) +
  geom_point(aes(shape = credit_treat), size = 4) +
  geom_line() +
  scale_x_continuous(breaks = seq(2012, 2018, 1)) +
  labs(
    x = "Year",
    y = "Diff. of average logged giving",
    shape = "Income group (unit:10,000KRW)",
    caption = paste(
      "The difference is calculated by",
      "(proportion of donors in year t) - (proportion of donors in 2013)."
    )
  ) +
  ggtemp(size = list(title = 15, text = 13, caption = 13))

#'
#' ## Price Effect Can Be Observed for Extensive Margin
#'
#+ SummaryGivingExtensive, fig.cap = "Proportion of Donors by Three Income Groups. Notes: We created three income groups, with the relative price of giving rising (circle), unchanged (triangle), and falling (square) between 2013 and 2014. The group averages are normalized to be zero in 2013.", out.extra = ""
df %>%
  dplyr::filter(year <= 2017) %>%
  dplyr::filter(!is.na(credit_treat)) %>%
  group_by(year, credit_treat) %>%
  summarize(mu = mean(d_donate, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = "year", values_from = "mu") %>%
  mutate(base = `2013`) %>%
  dplyr::select(credit_treat, base, everything()) %>%
  tidyr::pivot_longer(
    -(credit_treat:base), values_to = "mu", names_to = "year"
  ) %>%
  mutate(mu = mu - base, year = as.numeric(year)) %>%
  mutate(credit_treat = factor(
    credit_treat,
    labels = c("< 1200", "[1200, 4600]", "> 4600")
  )) %>%
  ggplot(aes(x = year, y = mu, group = credit_treat)) +
  geom_vline(aes(xintercept = 2013.5), linetype = 3) +
  geom_hline(aes(yintercept = 0), linetype = 3) +
  geom_point(aes(shape = credit_treat), size = 4) +
  geom_line() +
  scale_x_continuous(breaks = seq(2012, 2018, 1)) +
  labs(
    x = "Year",
    y = "Diff. of proportion of donors",
    shape = "Income group (unit:10,000KRW)",
    caption = paste(
      "The difference is calculated by",
      "(proportion of donors in year t) - (proportion of donors in 2013)."
    )
  ) +
  ggtemp(size = list(title = 15, text = 13, caption = 13))

#'
#' ## Message from Figure \@ref(fig:SummaryGivingIntensive) and \@ref(fig:SummaryGivingExtensive)
#'
#' Intensive margin (How much donors give): Price effect can be partially observed
#'
#' - The income group that increased the donation most was the group whose tax incentive did not change, but the donation of the income group that decreased the tax incentive did not change significantly.
#' - Income groups with unchanged tax incentives has increased donations more than income groups with expanded tax incentives (opposite to the price effect).
#'
#' Extensive margin (Whethre respondents donate): Price effect can be observed
#'
#' - Same trend as Figure \@ref(fig:SummaryGivingOverall)
#'
#' ## Application for Tax Relief Decreased After Tax Reform
#'
#+ SummaryReliefbyIncome, fig.cap = "Proportion of Having Applied for Tax Relief by Three Income Groups. Notes: We created three income groups, with the relative price of giving rising (circle), unchanged (triangle), and falling (square) between 2013 and 2014. The group averages are normalized to be zero in 2013.", out.extra = ""
df %>%
  dplyr::filter(year <= 2017) %>%
  dplyr::filter(!is.na(credit_treat)) %>%
  group_by(year, credit_treat) %>%
  summarize(mu = mean(d_relief_donate, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = "year", values_from = "mu") %>%
  mutate(base = `2013`) %>%
  dplyr::select(credit_treat, base, everything()) %>%
  tidyr::pivot_longer(
    -(credit_treat:base), values_to = "mu", names_to = "year"
  ) %>%
  mutate(mu = mu - base, year = as.numeric(year)) %>%
  mutate(credit_treat = factor(
    credit_treat,
    labels = c("< 1200", "[1200, 4600]", "> 4600")
  )) %>%
  ggplot(aes(x = year, y = mu, group = credit_treat)) +
  geom_vline(aes(xintercept = 2013.5), linetype = 3) +
  geom_hline(aes(yintercept = 0), linetype = 3) +
  geom_point(aes(shape = credit_treat), size = 4) +
  geom_line() +
  scale_x_continuous(breaks = seq(2012, 2018, 1)) +
  labs(
    x = "Year",
    y = "Diff. of proportion of \n application for tax relief",
    shape = "Income group (unit:10,000KRW)",
    caption = paste(
      "The difference is calculated by",
      "(proportion in year t) - (proportion in 2013)."
    )
  ) +
  ggtemp(size = list(title = 15, text = 13, caption = 13))

#'
#+ SummaryGivingIntensiveDist, include = FALSE
df %>%
  filter(year <= 2017) %>%
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
  ggtemp()

#'
#' ## Message from Figure \@ref(fig:SummaryReliefbyIncome)
#'
#' 1. tax incentive negatively correlated with application for tax relief.
#'     - Since the 2014 tax reform, the share of application for tax relief has not increased in all income groups compared to 2013.
#'     - the decrease in the application rate is the largest among income groups whose tax incentives decreased due to the 2014 tax reform.
#' 2. the trend of application for tax relief does not match the trend of share of donors.
#'     - If there is no application cost, all donors should apply for tax relief
#'     - Figure \@ref(fig:SummaryGivingExtensive) and \@ref(fig:SummaryReliefbyIncome) imply that there is cost to apply for tax relief.
#'     - The distribution of donations conditional on donors does not change significantly depending on whether or not they have applied for tax relief, suggesting that the application cost is high (Figure \@ref(fig:SummaryGivingIntensiveDist)).
#'
#' ## Wage Earners Are More Likely to Apply for Tax Relief
#'
#+ SummaryReliefbyEarner, fig.cap = "Share of Tax Relief by Wage Earners. Notes: A solid line is the share of applying for tax relief among wage eaners. A dashed line is the share of applying for tax relief other than wage earners.", out.extra = ""
df %>%
  dplyr::filter(year <= 2017) %>%
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
  scale_x_continuous(breaks = seq(2012, 2018, 1)) +
  labs(
    x = "Year",
    y = "Proportion of application for tax relief",
    shape = "",
    linetype = ""
  ) +
  ggtemp(size = list(title = 15, text = 13))

#'
#+ SummaryReliefbyEarner2, include = FALSE
df %>%
  dplyr::filter(year <= 2017) %>%
  dplyr::filter(!is.na(employee) & d_donate == 1) %>%
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
  scale_x_continuous(breaks = seq(2012, 2018, 1)) +
  labs(
    x = "Year",
    y = "Proportion of application for tax relief",
    shape = "",
    linetype = ""
  ) +
  ggtemp(size = list(title = 15, text = 13))

#'
#' ## Message from Figure \@ref(fig:SummaryReliefbyEarner)
#'
#' - Employment status is one dimension of variation of applied cost.
#'   - self-employed workers have to retain the certificate until they submit tax return.
#'   - wage earners can declare tax relief and submit the certificate through their company at any time.
#' - the proportion of declaring a tax relief among wage earners is higher than the others
#'   - Application cost for wage earners is lower than for other than wage earners
#'   - This trend does not change when we calculate the proportion of application conditional on donors (Figure \@ref(fig:SummaryReliefbyEarner2)).
#'
# /*
#+
rmarkdown::render(
  "script/1-summary.r",
  output_dir = "report/view"
)
# */