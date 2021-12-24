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
#' # Data
#'
#+
df <- readr::read_csv("data/shaped2.csv")

#'
#' The National Survey of Tax and Benefit (hereafter, NaSTab) is
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
    (`Wage earner dummy` = employee) ~
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
  kableExtra::pack_rows("Individual Characteristics", 6, 11)

#'
#+ SummaryOutcome, fig.cap = "Proportion of Donors and Average Donations among Donors. Notes: The left and right axises measure prooortion of donors and the average amount of donations among donors, respectively. Authors made this graph based on NaSTaB data.", out.width = "85%", out.extra = ""
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
#' The second panel of Table \@ref(tab:SummaryCovariate)
#' shows variables about income, tax report, and the giving price.
#' NaSTaB asks respondents to answer the annual labor income last year.
#' In our sample,
#' the average annual taxable income is 18.76 million KRW (18,760 USD).
#' According to the National Tax Statistical Yearbook
#' published by Korean National Tax Service,
#' the average annual taxable income is
#' 32.77 million KRW (32,770 USD) from 2012 to 2018
#' for employees who submitted the tax return.
#' Since our sample includes subjects with no labor income, such as housewives,
#' our sample mean of income is lower than
#' the average income calculated by the public organizations.
#' In Figure \@ref(fig:SummaryPrice),
#' the gray bars show the distribution of annual taxable income in 2013.
#' The income distribution is right-skewed.
#'
#' Using this variable,
#' we construct the giving price
#' under the tax deduction system (2012 and 2013).[^fprice]
#' After the tax reform (after 2014),
#' the giving price is 0.85 regardless of labor income,
#' as we explained in Section \@ref(taxreform).
#' In Figure \@ref(fig:SummaryPrice),
#' the blue line shows the giving price in 2012 and 2013,
#' while the red dashed line shows the giving price after 2014.
#' From this figure,
#' those whose annual income is less than 120,000,000 KRW
#' (120,000 USD) in 2013 could receive benefit from the 2014 tax reform
#' because the tax reform decreases the giving price.
#' On the other hand,
#' those whose annual income is greater than
#' 460,000,000 KRW (460,000 USD) in 2013 had a loss by the 2014 tax reform
#' since the tax reform increases the giving price.
#'
#' [^fprice]: The giving price shown in Table \@ref(tab:SummaryCovariate) is the *first* giving price. The giving price can be manipulated by an amount of donation. To avoid this endogeneity, we use the giving price where the amount of donation is zero. We will discuss this issue in the next section.
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
  geom_vline(aes(xintercept = 2013.5)) +
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
# /*
#+
rmarkdown::render(
  "script/R/1-summary.r",
  output_dir = "report/view"
)
# */