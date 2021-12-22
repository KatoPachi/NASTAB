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
#' ## Message from Figure \@ref(fig:SummaryPrice): Right-Skewed Distribution
#'
#' - NaSTaB contains the annual taxable labor income last year
#' - Our sample includes subjects with no labor income (e.g. housewives)
#'   - Table \@ref(tab:SummaryCovariate): the average income is 17.54 million KRW
#' - National Tax Statistical Yearbook 2012-2018 (Korean National Tax Service): the average annual taxable income is 32.77 million KRW
#'   - Sample: employees who submitted the tax return
#'
#' ## Message from Figure \@ref(fig:SummaryPrice): Price Variation
#'
#' Based on changes in tax incentive due to the 2014 tax reform,
#' we can devide into three income groups:
#'
#' 1. less than 120 million KRW
#'     - 2014 tax reform has expanded tax incentive (decreased giving price)
#' 1. between 120 million KRW and 460 million KRW
#'     - 2014 tax reform has unchanged tax incentive
#' 1. more than 460 million KRW.
#'     - 2014 tax reform has decreased tax incentive (increaed giving price)
#'
#' This is main source of identification for effect of tax incentive on giving.
#'
#' ## Donors Decreased Immediately After Tax Reform
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
#' ## Message from Figure \@ref(fig:SummaryGivingOverall)
#'
#' - Price effect = Tax incentive increases charitable giving
#'   - Donations for income groups with unchanged or increased tax incentives have exceeded those in 2013 since 2015
#'   - Donations for income groups with reduced tax incentives have been lower than before tax reform since 2015
#'
#' Notes:
#'
#' 1. Prior to the 2014 tax reform, donations did not change in all groups
#' 1. Donations for all groups were lower than in 2013
#'     - Donations for income groups with reduced tax incentive due to the 2014 tax reform was 40% of that in 2013
#'     - Announcement effect? Learning effect (瀧井先生のコメント)?
#'
#' ## Price Effect Can Be Partially Observed for Intensive Margin
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
#' ## Price Effect Can Be Observed for Extensive Margin
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
#' ## Message from Figure \@ref(fig:SummaryGivingIntensive) and \@ref(fig:SummaryGivingExtensive)
#'
#' - Intensive margin (How much donors give): Price effect can be partially observed
#'   - tax incentiveが変化しない所得層が寄付を最も増やしていたが、tax incentiveが減少した所得層はあまり寄付を増やさなかった
#'   - tax incentiveが変化しない所得層の方がtax incentiveが拡充された所得層よりも寄付を増やしていた(price effectではない)
#' - Extensive margin (Whethre respondents donate): Price effect can be observed
#'   - 全体の寄付の増加率のグラフ（Figure \@ref(fig:SummaryGivingOverall)）と同じ傾向
#'
#' ## Application for Tax Relief Decreased After Tax Reform
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
#' ## Message from Figure \@ref(fig:SummaryReliefbyIncome)
#'
#' 1. 2014年税制改革以降、寄付控除を申請した人の割合は減少した
#'     - 全体を通した寄付控除の申請比率：10% (Table \@ref(tab:SummaryCovariate))
#'     - tax incentiveが減少した所得層の寄付控除の申請比率が減少した
#' 2. tax incentiveが増加した所得層の寄付者の割合が増えたにも関わらず、寄付控除の申請比率は伸びていない
#'     - 寄付控除を申告することで得するはずなのに、それをしていない人がいる $\to$ 申請コストが存在する
#'
#' ## Similar Distribution of Giving Regardless of Application
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
#' ## Message from Figure \@ref(fig:SummaryGivingIntensiveDist)
#'
#' - すべての年においても、寄付者に限定した寄付の分布は寄付控除の申請の有無に依存しない
#'   - 寄付控除を申請していない人の寄付額の分布が申請している人よりも左側にあるならば、申請コストの程度は小さいと予想される
#'   - 分布の形状がほとんど一致しているので、申請コストの程度はかなり大きいと予想される
#'
#' ## Wage Earners Are More Likely to Apply (1)
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
#' ## Wage Earners Are More Likely to Apply (2)
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

#'
#' ## Message from Figure \@ref(fig:SummaryReliefbyEarner) and \@ref(fig:SummaryReliefbyEarner2)
#'
#' - 給与所得者はそうでない人よりも寄付控除を申請しやすい
#'   - 自営業者（非給与所得者の主）は寄付の証明書を常に保管しているようなことはない（申請期限前に問い合わせが殺到する）
#'   - 給与所得者の方が申請しやすい環境にあることがデータからもわかる
#'   - 給与所得者かどうかで寄付者の比率が異なる可能性を考慮して、寄付者に限定しても同じ傾向である(Figure \@ref(fig:SummaryReliefbyEarner2))
#' - 2014年税制改革以降、寄付控除の申請比率は減少している（とくに、給与所得者）
#'
# /*
#+
rmarkdown::render(
  "script/R/1-summary.r",
  output_dir = "report/view"
)
# */