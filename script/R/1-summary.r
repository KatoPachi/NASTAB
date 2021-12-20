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
  )
) %>%
dplyr::filter(
  ext_benefit_tl == 0 | (ext_benefit_tl == 1 & i_ext_giving == 1)
)

#'
#' The National Survey of Tax and Benefit (hereafter, NaSTaB) is
#' an annual financial panel survey
#' implemented by The Korea Institute of Taxation and Finance
#' to study the tax burden of households and the benefits
#' that households receive from the government.
#' The subjects of this survey are general households and
#' household members living in 15 cities and provinces nationwide.
#' This survey is based on a face-to-face interview.[^interview]
#' The NaSTaB data is constructed
#' as the subjects represent the population of Korean society.
#' This enables us to derive giving price elasticity of population
#' without re-weighting samples, which is used in the extant research.
#' Moreover, note that subjects are not limited to the taxpayer or
#' income earner reflecting the population.
#'
#' [^interview]: If it is difficult for investigators to meet subjects, another family member answers on behalf of him.
#'
#' In the analysis,
#' we use data from 2013 to 2017 since we focus on the 2014 tax reform.
#' This is because, as Table \@ref(tab:tabTaxRate) shows,
#' the giving price before 2014 was changed frequently
#' and incorporating the data before 2012
#' captures the effects of another tax reform than the reform in 2014.
#' Note that, since tax credit was introduced after 2014 and
#' the credit rate was unchanged since 2014,
#' the giving price does not depend on the income tax rate after 2014.
#' In addition, we exclude the subject of the sample, whose age is under 23,
#' since they are not likely to have income or assets.
#'
#+ SummaryCovariate
df %>%
  dplyr::filter(year <= 2017) %>%
  datasummary(
    (`Annual chariatable giving (unit: 10,000KRW)` = i_total_giving) +
    (`Dummary of donation > 0` = i_ext_giving) +
    (`Annual taxable labor income (unit: 10,000KRW)` = lincome) +
    (`Relative first price of giving` = price) +
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
  kableExtra::pack_rows("Charitable Donations", 1, 2) %>%
  kableExtra::pack_rows("Income, giving price, and tax report", 3, 5) %>%
  kableExtra::pack_rows("Individual Characteristics", 6, 12)

#+ SummaryPrice, fig.cap = "Income Distribution and Relative Giving Price in 2013. Notes: The left and right axis measure the relative frequency of respondents and the relative giving price, respectively. A blue step line and a red dashed horizontal line represents the giving price in 2013 and 2014, respectively. The grey bar shows income distribution in 2013.", out.width = "85%", out.extra = ""
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
#' NaSTaB asks respondents to answer the annual labor income last year.
#' In our sample,
#' the average annual taxable income is 17.54 million KRW (17,540 USD)
#' (See \@ref(tab:SummaryCoveriate)).
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
#' the relative price of giving is 0.85 regardless of labor income,
#' as we explained in Section \@ref(taxreform).
#' In Figure \@ref(fig:SummaryPrice),
#' the blue line shows the giving price in 2012 and 2013,
#' while the red dashed line shows the giving price after 2014.
#' The 2014 tax reform has expanded tax incentives
#' (has decreased the relative price of giving)
#' for those whose annual income is less than 120 million KRW
#' (120,000 USD) in 2013,
#' while reducing tax incentives
#' (has increased the relative price of giving)
#' for those whose annual income is greater than
#' 460 million KRW (460,000 USD) in 2013.
#' The tax incentive has been unchanged for
#' those whose annual income is between 120 million KRW and 460 million KRW
#' due to the 2014 tax reform.
#' This is main source of identification for effect of tax incentive on giving.
#'
#' [^fprice]: The giving price shown in Table \@ref(tab:SummaryCovariate) is the *first* giving price. The giving price can be manipulated by an amount of donation. To avoid this endogeneity, we use the giving price where the amount of donation is zero. We will discuss this issue in the next section.
#'
#+ SummaryGiving, fig.cap = "Proportion of Donors and Average Donations among Donors. Notes: The left and right axises measure prooortion of donors and the average amount of donations among donors, respectively. Authors made this graph based on NaSTaB data.", out.width = "85%", out.extra = "", eval = FALSE
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
  geom_vline(aes(xintercept = 2013.5), linetype = 2, size = 1) +
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
#+ SummaryGivingOverall, fig.cap = "Average Logged Giving by Three Income Groups. Notes: We created three income groups, with the relative price of giving rising (circle), unchanged (triangle), and falling (square) between 2013 and 2014. The group averages are normalized to be zero in 2013.", out.width = "85%", out.extra = ""
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
    y = "Difference of average logged giving",
    shape = "Income group (unit:10,000KRW)",
    caption = paste(
      "The difference is calculated by",
      "(mean of logged donation in year t) - (mean of logged donation in 2013)."
    )
  ) +
  ggtemp(size = list(title = 15, text = 13, caption = 13))

#+ SummaryGivingIntensive, fig.cap = "Average Logged Giving by Three Income Groups Conditional on Donors. Notes: We created three income groups, with the relative price of giving rising (circle), unchanged (triangle), and falling (square) between 2013 and 2014. The group averages are normalized to be zero in 2013.", out.width = "85%", out.extra = ""
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
    y = "Difference of average logged giving",
    shape = "Income group (unit:10,000KRW)",
    caption = paste(
      "The difference is calculated by",
      "(proportion of donors in year t) - (proportion of donors in 2013)."
    )
  ) +
  ggtemp(size = list(title = 15, text = 13, caption = 13))

#'
#+ SummaryGivingExtensive, fig.cap = "Proportion of Donors by Three Income Groups. Notes: We created three income groups, with the relative price of giving rising (circle), unchanged (triangle), and falling (square) between 2013 and 2014. The group averages are normalized to be zero in 2013.", out.width = "85%", out.extra = ""
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
    y = "Difference of proportion of donors",
    shape = "Income group (unit:10,000KRW)",
    caption = paste(
      "The difference is calculated by",
      "(proportion of donors in year t) - (proportion of donors in 2013)."
    )
  ) +
  ggtemp(size = list(title = 15, text = 13, caption = 13))

#'
#+ SummaryOutcome2, fig.cap = "Distribution of Charitable Giving among Those Who Donated", out.width = "85%", out.extra = ""
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
#+ SummaryOutcome3, fig.cap = "Proportion of Donors By Having Applied for Tax Relief", out.width = "85%", out.extra = ""
df %>%
  filter(year <= 2017 & !is.na(ext_benefit_tl)) %>%
  group_by(year, ext_benefit_tl) %>%
  summarize(mu = mean(i_ext_giving, na.rm = TRUE)) %>%
  mutate(ext_benefit_tl = factor(
    ext_benefit_tl,
    levels = 1:0,
    labels = c("Applied for tax relief", "Did not apply for tax relief")
  )) %>%
  ggplot(aes(x = year, y = mu, group = ext_benefit_tl)) +
  geom_point(aes(shape = ext_benefit_tl), size = 4) +
  geom_line() +
  labs(
    x = "Year",
    y = "Proportion of Donors",
    shape = ""
  ) +
  ggtemp()

#'
#' Table \@ref(tab:SummaryCovariate)
#' shows summary statistics of our data.[^Question]
#' The first panel of this table shows variables about charitable giving.
#' The NaSTaB asks respondents to answer the amount of donation last year.
#' This is the first outcome variables.
#' Using this, we make a dummy taking 1 if respondent donated last year.
#' This is the second outcome variables to
#' estimate the price effect on the decision of donations.
#' This table shows that
#' the average amount of donation is almost 300,000 KRW (300 USD),
#' and the proportion of donors is roughly 20\%.
#' Figure \@ref(fig:SummaryOutcome) shows the time-series of two variables.
#' The blue line shows the average amount of donation among donors.
#' In each year, its value is nearly 1.5 million KRW (1,500 USD),
#' which is 7\% of average annual taxable income.
#' The gray bar shows the proportion of donors.
#' After the tax reform, the proportion of donors decreases
#' by 2 percentage points.
#' After that, the proportion of donors is greter than 20\%.
#'
#' [^Question]: Respondents answer the amount of donation for seven specific purposes last year. Seven specific purposes are policitical parties, educational organizations, social welfare organizations, organizations for culutre and art, religious groups, charity activies organaized by religious group, other purposes. We sum up the amount of donations, and consider it as the annual charitable giving.
#'
#+ SummaryRelief, fig.cap = "Share of Tax Relief. Notes: A solid line is the share of applying for tax relief among wage eaners. A dashed line is the share of applying for tax relief other than wage earners.", out.width = "85%", out.extra = ""
df %>%
  dplyr::filter(year <= 2017) %>%
  dplyr::filter(!is.na(employee)) %>%
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
  geom_vline(aes(xintercept = 2013.5), linetype = 3) +
  scale_x_continuous(breaks = seq(2012, 2018, 1)) +
  labs(
    x = "Year",
    y = "share of declaration of a tax relief",
    shape = "Wage earner",
    linetype = "Wage earner"
  ) +
  ggtemp(size = list(title = 15, text = 13))

#' The NaSTaB also asks respondents
#' to answer whether they declared a tax relief of giving.
#' This survey data separately asks whether subjects applied for tax 
#' relief on giving via tax filing or not, 
#' and whether subjects applied for tax reilef on giving via tax withholding.[^total_labor]
#' We make a dummy taking one if subjects applied for either tax relief. 
#' Table \@ref(tab:SummaryCovariate) shows
#' the proportion of declaration is about 11%.
#'
#' [^total_labor]: Tax filing is used for *total* income (e.g., business income, dividend income and rental income). Tax withholding is used for *labor* income.
#'
#+ SummaryRelief2, fig.cap = "Proportion of Having Applied for Tax Relief in Three Income Groups. Notes: We created three income groups, with the relative price of giving rising (circle), unchanged (triangle), and falling (square) between 2013 and 2014.", out.width = "85%", out.extra = ""
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
    y = "Proportion of Having Applied for Tax Relief",
    shape = "Income group (unit:10,000KRW)"
  ) +
  ggtemp(size = list(title = 15, text = 13, caption = 13))

# /*
#+
rmarkdown::render(
  "script/R/1-summary.r",
  output_dir = "report/view"
)
# */