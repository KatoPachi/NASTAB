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

lapply(Sys.glob(file.path("script/R/functions", "*.r")), source)

#'
#' # Data
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
  )) %>%
  dplyr::filter(
    ext_benefit_tl == 0 | (ext_benefit_tl == 1 & i_ext_giving == 1)
  ) %>%
  dplyr::filter(year <= 2017)

#'
#' We use the National Survey of Tax and Benefit (hereafter, NaSTaB),
#' which has been implemented by The Korea Institute of Taxation and Finance
#' since 2008.[^nastab_url]
#' The NaSTaB is an annual panel data on
#' households' tax burden and public benefits.
#' The unit of analysis is 5,634 households throughout the country
#' (5,634 family heads and family members with more than 15 years old
#' and with income or economically active).[^interview]
#' The NaSTaB data is constructed
#' as the subjects represent the population of Korean society.
#' Note that subjects are not limited to the taxpayer or
#' income earner reflecting the population.
#'
#' [^nastab_url]: <http://www.welfarestate.re.kr/2063>
#'
#' [^interview]: This survey is based on a face-to-face interview. If it is difficult for investigators to meet subjects, another family member answers on behalf of him.
#'
#+ SummaryCovariate
df %>%
  datasummary(
    (`Annual taxable labor income (unit: 10,000KRW)` = lincome) +
    (`Relative first price of giving` = price) +
    (`Annual chariatable giving (unit: 10,000KRW)` = i_total_giving) +
    (`Dummary of donation > 0` = i_ext_giving) +
    (`Dummy of declaration of a tax relief` = ext_benefit_tl) +
    (`Age` = age) +
    (`Female dummy` = gender) +
    (`University graduate` = univ) +
    (`High school graduate dummy` = highschool) +
    (`Junior high school graduate dummy` = juniorhigh) +
    (`Wage earner dummy` = employee) +
    (`# Tax accountant /population` = tax_accountant_per) ~
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
  kableExtra::kable_styling() %>%
  kableExtra::pack_rows("Income and giving price", 1, 2) %>%
  kableExtra::pack_rows("Charitable giving", 3, 5) %>%
  kableExtra::pack_rows("Individual Characteristics", 6, 12)

#'
#' Our analysis uses the NaSTaB data from (i) 2013-2018
#' and (ii) excluding respondents under the age of 23.
#' As Table \@ref(tab:tabTaxRate) shows,
#' using the NaSTaB data before 2012
#' captures the effects of other tax reform than the reform in 2014.
#' In addition, we exclude respondents whose age is under 23
#' because they are not likely to have income or assets.
#' Table \@ref(tab:SummaryCovariate) shows descriptive statistics of our data.
#'
#' ## Income and Giving Price
#'
#+ SummaryPrice, fig.cap = "Income Distribution and Relative Giving Price in 2013. Notes: The left and right axis measure the relative frequency of respondents (grey bars) and the relative giving price (solid step line and dashed line), respectively. A solid step line and a dashed horizontal line represents the giving price in 2013 and 2014, respectively. The grey bar shows income distribution in 2013.", out.extra = ""
df %>%
  filter(year == 2013) %>%
  dplyr::select(lincome, price) %>%
  ggplot(aes(x = lincome)) +
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
#' The NaSTaB data contains the annual taxable labor income last year.
#' Table \@ref(tab:SummaryCovariate) shows that
#' the average income is 17.54 million KRW (17,45540 USD).
#' According to the National Tax Statistical Yearbook
#' published by Korean National Tax Service,
#' the average annual taxable income is
#' 32.77 million KRW (32,770 USD) from 2012 to 2018
#' for employees who submitted the tax return.[^source]
#' Since our sample includes subjects with no labor income, such as housewives,
#' our sample mean of income is lower than
#' the average income calculated by the public organizations.
#' In Figure \@ref(fig:SummaryPrice),
#' the gray bars show the distribution of annual taxable income in 2013.
#' The income distribution is right-skewed.
#'
#' [^source]: URL
#'
#' Using this variable,
#' we construct the relative price of giving
#' under the tax deduction system (2012 and 2013).[^fprice]
#' After the tax reform (after 2014),
#' the relative price of giving is 0.85 regardless of income,
#' as we explained in Section \@ref(taxreform).
#' In Figure \@ref(fig:SummaryPrice),
#' the solid line shows the giving price in 2012 and 2013,
#' while the dashed line shows the giving price after 2014.
#' Based on changes in tax incentive due to the 2014 tax reform,
#' we can devide into three income groups:
#' (i) less than 120 million KRW (120,000 USD),
#' (ii) between 120 million KRW and 460 million KRW (460,000 USD),
#' (iii) more than 460 million KRW.
#' The 2014 tax reform has expanded tax incentive (decreasing giving price)
#' for the first income group,
#' unchanged tax incentive for the second income group,
#' and decreased tax incentive (increasing giving price)
#' for the third income group.
#' This is main source of identification for effect of tax incentive on giving.
#'
#' [^fprice]: Under the tax deduction system, the giving price shown in Table \@ref(tab:SummaryCovariate) and Figure \@ref(fig:SummaryPrice) is the *first* giving price, which is the giving price wich zero charitable giving. This is because the giving price can be manipulated by an amount of donation under the tax deduction system. Using the first price, we can avoid this endogeneity. We will discuss this issue in the next section.
#'
#' ## Charitable Giving
#'
#' The NaSTaB data contains the amount of donation last year.[^question]
#' This logarithmic value is our first outcome variables.
#' Table \@ref(tab:SummaryCovariate) shows that
#' the average amount of donation is 358,600 KRW (358.6 USD),
#' which is about 2% of average income.
#' Moreover, using this variable,
#' we make a dummy taking 1 if respondent donated last year.
#' This is the second outcome variables to
#' estimate the effect of tax incentive on the decision of donations.
#' Table \@ref(tab:SummaryCovariate) shows that
#' the proportion of donors is 24%.
#'
#' [^question]: Respondents answer the amount of donation for seven specific purposes last year. Seven specific purposes are policitical parties, educational organizations, social welfare organizations, organizations for culutre and art, religious groups, charity activies organaized by religious group, other purposes. We sum up the amount of donations, and consider it as the annual charitable giving.
#'
#' <!--- 加藤コメント：欧米圏の寄付との簡単な比較があると文化差が伝わるかも --->
#'
#+ SummaryGiving, fig.cap = "Proportion of Donors and Average Donations among Donors. Notes: The left and right axises measure prooortion of donors (grey bars) and the average amount of donations among donors (solid line), respectively.", out.extra = ""
df %>%
  mutate(
    i_total_giving = if_else(i_ext_giving == 1, i_total_giving, NA_real_)
  ) %>%
  group_by(year) %>%
  summarize_at(
    vars(i_ext_giving, i_total_giving), list(~ mean(., na.rm = TRUE))
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
    size = 3
  ) +
  geom_line(
    aes(
      y = i_total_giving / 1000,
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
#' Figure \@ref(fig:SummaryGiving) shows
#' the time series of two variables related with charitable giving.
#' The blue line shows the average amount of donation among donors.
#' The average donation amount conditional on donors
#' before and after the 2014 tax reform has not changed significantly.
#' In each year, its value is nearly 1.5 million KRW (1,500 USD),
#' which is 7% of average annual taxable income.
#' On the other hand,
#' the proportion of donors (gray bars) has changed significantly
#' before and after the 2014 tax reform.
#' In 2014, shortly after the tax reform,
#' the proportion of donors is lower than before the tax reform.
#' Since then, the proportion of donors has continued to increase,
#' finally surpassing that before the tax reform.
#'
#+ SummaryGivingOverall, fig.cap = "Average Logged Giving by Three Income Groups. Notes: We created three income groups, with the relative price of giving rising (circle), unchanged (triangle), and falling (square) between 2013 and 2014. The group averages are normalized to be zero in 2013.", out.extra = ""
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
  mutate(mu = mu - base, year = as.numeric(year)) %>%
  mutate(group = factor(
    group,
    labels = c("< 1200", "[1200, 4600]", "> 4600")
  )) %>%
  ggplot(aes(x = year, y = mu, group = group)) +
  geom_vline(aes(xintercept = 2013.5), linetype = 3) +
  geom_hline(aes(yintercept = 0), linetype = 3) +
  geom_point(aes(shape = group), size = 4) +
  geom_line() +
  scale_x_continuous(breaks = seq(2012, 2018, 1)) +
  labs(
    x = "Year",
    y = "Diff. of average logged giving",
    shape = "Income group (unit:10,000KRW)",
    caption = paste(
      "The difference is calculated by",
      "(mean of logged donation in year t) - (mean of logged donation in 2013)."
    )
  ) +
  ggtemp(size = list(title = 15, text = 13, caption = 13))

#'
#' At the descriptive statistics stage,
#' we look at how donations have changed due to the 2014 tax reform.
#' Figure \@ref(fig:SummaryGivingOverall) shows that
#' average logged value of charitable giving by three income groups.
#' For normalization,
#' we subtract the average for each year by the average for 2013.
#' Prior to the 2014 tax reform, donations did not change in all groups.
#' In 2014, shortly after tax reform,
#' donations for all groups were lower than in 2013.
#' In particular,
#' the amount of donations for income groups
#' whose tax incentives were reduced due to the 2014 tax reform
#' (more than 460 million KRW)
#' was about 40% of that in 2013.
#' Since 2015,
#' donations from income groups with unchanged or
#' increased tax incentives have exceeded those in 2013,
#' while donations from income groups with reduced tax incentives
#' have been lower than before tax reform.
#' Therefore, from this figure,
#' we can expect that the tax reform in 2014 had the price effect of donations.
#'
#+ SummaryGivingIntensive, fig.cap = "Average Logged Giving by Three Income Groups Conditional on Donors. Notes: We created three income groups, with the relative price of giving rising (circle), unchanged (triangle), and falling (square) between 2013 and 2014. The group averages are normalized to be zero in 2013.", out.extra = ""
df %>%
  mutate(group = case_when(
    credit_loss == 1 ~ 3,
    credit_neutral == 1 ~ 2,
    credit_benefit == 1 ~ 1
  )) %>%
  dplyr::filter(year <= 2017) %>%
  dplyr::filter(!is.na(group) & i_ext_giving == 1) %>%
  group_by(year, group) %>%
  summarize(mu = mean(log_total_g, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = "year", values_from = "mu") %>%
  mutate(base = `2013`) %>%
  dplyr::select(group, base, everything()) %>%
  tidyr::pivot_longer(-(group:base), values_to = "mu", names_to = "year") %>%
  mutate(mu = mu - base, year = as.numeric(year)) %>%
  mutate(group = factor(
    group,
    labels = c("< 1200", "[1200, 4600]", "> 4600")
  )) %>%
  ggplot(aes(x = year, y = mu, group = group)) +
  geom_vline(aes(xintercept = 2013.5), linetype = 3) +
  geom_hline(aes(yintercept = 0), linetype = 3) +
  geom_point(aes(shape = group), size = 4) +
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
#+ SummaryGivingExtensive, fig.cap = "Proportion of Donors by Three Income Groups. Notes: We created three income groups, with the relative price of giving rising (circle), unchanged (triangle), and falling (square) between 2013 and 2014. The group averages are normalized to be zero in 2013.", out.extra = ""
df %>%
  mutate(group = case_when(
    credit_loss == 1 ~ 3,
    credit_neutral == 1 ~ 2,
    credit_benefit == 1 ~ 1
  )) %>%
  dplyr::filter(year <= 2017) %>%
  dplyr::filter(!is.na(group)) %>%
  group_by(year, group) %>%
  summarize(mu = mean(i_ext_giving, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = "year", values_from = "mu") %>%
  mutate(base = `2013`) %>%
  dplyr::select(group, base, everything()) %>%
  tidyr::pivot_longer(-(group:base), values_to = "mu", names_to = "year") %>%
  mutate(mu = mu - base, year = as.numeric(year)) %>%
  mutate(group = factor(
    group,
    labels = c("< 1200", "[1200, 4600]", "> 4600")
  )) %>%
  ggplot(aes(x = year, y = mu, group = group)) +
  geom_vline(aes(xintercept = 2013.5), linetype = 3) +
  geom_hline(aes(yintercept = 0), linetype = 3) +
  geom_point(aes(shape = group), size = 4) +
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
#' ## Application for Tax Relief
#'
#' The NaSTaB also asks respondents
#' to answer whether they declared a tax relief of giving.
#' This survey data separately asks whether subjects applied for tax
#' relief on giving via tax filing or not,
#' and whether subjects applied for tax reilef
#' on giving via tax withholding.[^total_labor]
#' We make a dummy taking one if subjects applied for either tax relief.
#' Table \@ref(tab:SummaryCovariate) shows
#' the proportion of declaration is about 11%.
#'
#' [^total_labor]: Tax filing is used for *total* income (e.g., business income, dividend income and rental income). Tax withholding is used for *labor* income.
#'
#+ SummaryReliefbyIncome, fig.cap = "Proportion of Having Applied for Tax Relief by Three Income Groups. Notes: We created three income groups, with the relative price of giving rising (circle), unchanged (triangle), and falling (square) between 2013 and 2014.", out.extra = ""
df %>%
  mutate(group = case_when(
    credit_loss == 1 ~ 3,
    credit_neutral == 1 ~ 2,
    credit_benefit == 1 ~ 1
  )) %>%
  dplyr::filter(year <= 2017) %>%
  dplyr::filter(!is.na(group)) %>%
  group_by(year, group) %>%
  summarize(mu = mean(ext_benefit_tl, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = "year", values_from = "mu") %>%
  mutate(base = `2013`) %>%
  dplyr::select(group, base, everything()) %>%
  tidyr::pivot_longer(-(group:base), values_to = "mu", names_to = "year") %>%
  mutate(mu = mu - base, year = as.numeric(year)) %>%
  mutate(group = factor(
    group,
    labels = c("< 1200", "[1200, 4600]", "> 4600")
  )) %>%
  ggplot(aes(x = year, y = mu, group = group)) +
  geom_vline(aes(xintercept = 2013.5), linetype = 3) +
  geom_hline(aes(yintercept = 0), linetype = 3) +
  geom_point(aes(shape = group), size = 4) +
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
#+ SummaryGivingIntensiveDist, fig.cap = "Distribution of Charitable Giving among Those Who Donated", out.extra = ""
df %>%
  filter(year <= 2017) %>%
  filter(!is.na(ext_benefit_tl) & i_ext_giving == 1) %>%
  mutate(ext_benefit_tl = factor(
    ext_benefit_tl,
    levels = 1:0,
    labels = c("Applied for tax relief", "Did not apply for tax relief")
  )) %>%
  ggplot(aes(x = log_total_g)) +
  geom_density(aes(linetype = ext_benefit_tl)) +
  facet_wrap(~ year) +
  labs(
    x = "Charitable Giving (Logged Value)",
    linetype = ""
  ) +
  ggtemp()

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
  summarize_at(vars(ext_benefit_tl), list(~ mean(., na.rm = TRUE))) %>%
  mutate(employee = factor(employee)) %>%
  ggplot(aes(x = year, y = ext_benefit_tl, group = employee)) +
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
#+ SummaryReliefbyEarner2, fig.cap = "Share of Tax Relief by Wage Earners Conditional on Donors. Notes: A solid line is the share of applying for tax relief among wage eaners. A dashed line is the share of applying for tax relief other than wage earners.", out.extra = ""
df %>%
  dplyr::filter(year <= 2017) %>%
  dplyr::filter(!is.na(employee) & i_ext_giving == 1) %>%
  mutate(employee = factor(
    employee,
    levels = c(1, 0), labels = c("Wage earners", "Others")
  )) %>%
  group_by(year, employee) %>%
  summarize_at(vars(ext_benefit_tl), list(~ mean(., na.rm = TRUE))) %>%
  mutate(employee = factor(employee)) %>%
  ggplot(aes(x = year, y = ext_benefit_tl, group = employee)) +
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

# /*
#+
rmarkdown::render(
  "script/R/1-summary.r",
  output_dir = "report/view"
)
# */