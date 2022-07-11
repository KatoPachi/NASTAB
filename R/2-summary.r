#+ include = FALSE, eval = params$preview
library(here)
source(here("R", "_library.r"))

#'
#+ include = FALSE
estdf <- readr::read_csv(here("data/shaped2_propensity.csv"), guess_max = 30000)

#' ```{asis, echo = output_type() %in% c("body", "preview")}
#' 本研究は2008年からKorea Institute of Taxation and Financeが実施した
#' National Survey of Tax and Benefit (NaSTaB)を用いる。
#' これは家計の税負担や公的扶助などに関する年次パネルデータである。
#' この調査は全国から5,634世帯を対象とし、
#' 5,634人の世帯主と15歳以上で経済活動をしている世帯員が調査に回答する。
#' この調査は前年の所得や寄付額に関する情報を含んでおり、
#' それらに加えて、教育年数などの個人属性や税制に対する個人の意識に関する情報を含んでいる。
#' ```
#'
#' ```{asis, echo = output_type() == "slide"}
#' ## About NaSTaB
#'
#' - NaSTaB is annual panel data conducted by
#' Korea Institute of Taxation and Finance
#' - The survey will be administered to 5,634 households
#' from across the country
#'   - 5,634 heads of household and
#'   economically active household members aged 15 and older
#'   complete the survey
#' - Our study uses data from (1) 2013 to 2018 and (2) excluding
#' respondents under the age of 23
#'   - This is because we focus on the 2014 tax reform
#'
#' ## Descriptive Statistics
#' ```
#'
#+ SummaryCovariate, eval = output_type() != "appx"
out.file <- file(here("tables", "summary-covariate.tex"), open = "w")

tab <- estdf %>%
  datasummary(
    (`Annual taxable labor income (unit: 10,000KRW)` = linc) +
    (`First giving relative price` = price) +
    (`Annual chariatable giving (unit: 10,000KRW)` = donate) +
    (`Dummary of donation > 0` = d_donate) +
    (`Dummy of declaration of a tax relief` = d_relief_donate) +
    (`Age` = age) +
    (`Female dummy` = sex) +
    (`University graduate` = college) +
    (`High school graduate dummy` = highschool) +
    (`Junior high school graduate dummy` = junior) +
    (`Wage earner dummy` = employee) ~
    # (`#.Tax accountant / population` = tax_accountant_per) ~
    N +
    (`Mean` = mean) * Arguments(na.rm = TRUE) +
    (`Std.Dev.` = sd) * Arguments(na.rm = TRUE), #+
    # (`Min` = min) * Arguments(na.rm = TRUE) +
    # (`Median` = median) * Arguments(na.rm = TRUE) +
    # (`Max` = max) * Arguments(na.rm = TRUE),
    title = "Descriptive Statistics of NASTAB \\label{tab:summary-covariate}",
    data = .,
    align = "lccc",
    output = "latex"
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::pack_rows("Income and giving price", 1, 2) %>%
  kableExtra::pack_rows("Charitable giving", 3, 5) %>%
  kableExtra::pack_rows("Individual Characteristics", 6, 11)

writeLines(tab, out.file)
close(out.file)

#' ```{asis, echo = output_type() %in% c("body", "preview")}
#' 我々の研究では(1)2013年から2018年かつ、(2)23歳以下の回答者を除いたデータを使用する。
#' データの期間を制限した理由は、2014年の制度改革に注目するためである。
#' 所得控除制度が適用されている期間（2014年の制度改革前）では、所得税率の改正が寄付行動に影響を与える。
#' この制度が適用されている期間において、所得税率の改正は2011年が最後である。
#' したがって、2011年以前の寄付行動を用いると、2014年の制度改革以外の影響を含んでしまう。
#' その可能性を取り除くために、我々は2013年から2018年のデータ（2012年から2017年の寄付行動）を用いる。
#' また、23歳以下の回答者を除いた理由は、所得や資産を十分に持っていない可能性が高いからである。
#' 表\@ref(tab:SummaryCovariate)に記述統計を示した。
#' ```
#'
#' ```{asis, echo = output_type() == "slide"}
#' ## Income Distribution and Giving Price
#' ```
#'
#+ SummaryPrice, fig.cap = "Income Distribution in 2013 and Relative Giving Price. Notes: The left and right axis measure the relative frequency of respondents (grey bars) and the relative giving price (solid step line and dashed line), respectively. A solid step line and a dashed horizontal line represents the giving price in 2013 and 2014, respectively.", out.extra = "", eval = output_type() != "appx"
plot_price <- estdf %>%
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

plot_price

ggsave(
  here("figures", "summary-price.pdf"),
  plot = plot_price,
  width = 10,
  height = 6
)

#' ```{asis, echo = output_type() %in% c("body", "preview")}
#' NaSTabは前年の労働所得を調査している。
#' 表\@ref(tab:SummaryCovariate)は、
#' 我々が用いるサンプルの労働所得の平均額は17.54 million KRWであることを示しており、
#' Korean National Tax Serviceが発行しているNational Tax Statistical Yearbook 2012-2018
#' の平均所得32.77 million KRWより低い。
#' これはNaSTaBが主婦などの労働所得がない人を含んでいるからである。
#' したがって、所得分布は右歪曲な分布になる（図\@ref(fig:SummaryPrice)）。
#' 我々は労働所得に基づいて限界税率を計算し、所得控除における寄付価格を計算した。
#' 図\@ref(fig:SummaryPrice)の黒の実線は2012年から2013年の寄付の相対価格を示している。
#'
#' また、図\@ref(fig:SummaryPrice)は価格弾力性を識別するための価格変動も示している。
#' 先に述べたように、黒の実線は所得控除が適用されている期間（2012年から2013年）の寄付の相対価格を示している。
#' 対して、黒の破線は税額控除が適用されている期間（2014年以降）の寄付の相対価格を示している。
#' 2014年の税制改革による税インセンティブの変化に基づいて、我々は三つの所得グループを作ることができる：
#' (1) 120 million KRWより低い;
#' (2) 120 million KRWから460 million KRWの間;
#' (3) 460 million KRWより高い。
#' 第一のグループに属する人の税インセンティブは税制改革によって拡大した（寄付価格が減少した）。
#' 第二のグループに属する人の税インセンティブは税制改革によって変化しなかった。
#' 第三のグループに属する人の税インセンティブは税制改革によって縮小した（寄付価格が増加した）。
#' このグループによる差分の差分法が我々の第一の識別戦略となる[^pretrend]。
#'
#' [^pretrend]: 2011年以前の寄付行動は所得税率の改正による影響をうけるので、税制改革前の平行トレンドを検証することはできない。
#' ```
#'
#' ```{asis, echo = output_type() == "slide"}
#' ## Identification of Price Elasticity
#'
#' We can create three income groups based on
#' a change of tax incentive due to 2014 tax reform.
#'
#' 1. $<$ 120 million KRW
#'     - expand tax incentive (reduce giving price)
#' 1. $[\text{120 million KRW}, \text{460 million KRW}]$
#'     - tax incentive did not change
#' 1. $>$ 460 million KRW
#'     - shrink tax incentive (increase giving price)
#'
#' ## Summary of Giving Behavior
#' ```
#'
#+ SummaryGiving, fig.cap = "Proportion of Donors and Average Donations among Donors. Notes: The left and right axises measure prooortion of donors (grey bars) and the average amount of donations among donors (solid line), respectively.", out.extra = "", eval = output_type() != "appx"
plot_giving <- estdf %>%
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

plot_giving

ggsave(
  here("figures", "summary-giving.pdf"),
  plot = plot_giving,
  width = 10,
  height = 6
)

#' ```{asis, echo = output_type() %in% c("body", "preview")}
#' 各所得グループの寄付のトレンドを確認する前に、
#' 全体的な寄付行動の傾向を図\@ref(fig:SummaryGiving)に示した。
#' 2012年から2017年にかけて、寄付者の割合は約24%である。
#' 税制改革直後の寄付者の割合は所得控除のもとでの寄付者の割合を下回ったが、
#' 時間を通じて寄付者が増えている（グレーのバー）。
#' また、寄付者に限定した平均寄付額（黒の実線）は約1.5 million KRW（平均所得の約7%）
#' で時間を通じて安定している。
#' 寄付していない人も含めると、平均寄付額は358,600 KRW（平均所得の約2%）である。
#'
#'  <!-- //DISCUSS
#' [^culture]: 欧米圏の寄付との簡単な比較があると文化差が伝わるかも（コメント）
#' -->
#' ```
#'
#' ```{asis, echo = output_type() == "slide"}
#' ## Summary of Giving Amount by Three Income Groups
#' ```
#'
#+ SummaryGivingOverall, fig.cap = "Average Logged Giving by Three Income Groups. Notes: We created three income groups, with the relative price of giving rising (circle), unchanged (triangle), and falling (square) between 2013 and 2014. The group averages are normalized to be zero in 2013.", out.extra = "", eval = output_type() != "appx"
plot_giving2 <- estdf %>%
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
    shape = "Income Group (unit:10,000KRW)",
    caption = paste(
      "The difference is calculated by",
      "(mean of logged donation in year t) - (mean of logged donation in 2013)."
    )
  ) +
  ggtemp(size = list(title = 15, text = 13, caption = 13))

plot_giving2

ggsave(
  here("figures", "summary-giving-overall.pdf"),
  plot = plot_giving2,
  width = 10,
  height = 6
)

#' ```{asis, echo = output_type() == "slide"}
#' ## Summary of Giving Amount by Three Income Groups (Conditional on Donors)
#' ```
#+ SummaryGivingIntensive, fig.cap = "Average Logged Giving by Three Income Groups Conditional on Donors. Notes: We created three income groups, with the relative price of giving rising (circle), unchanged (triangle), and falling (square) between 2013 and 2014. The group averages are normalized to be zero in 2013.", out.extra = "", eval = output_type() != "body"
plot_giving3 <- estdf %>%
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

plot_giving3

ggsave(
  here("figures", "summary-giving-intensive.pdf"),
  plot = plot_giving3,
  width = 10,
  height = 6
)

#' ```{asis, echo = output_type() == "slide"}
#' ## Summary of Proportion of Donors by Three Income Groups
#' ```
#+ SummaryGivingExtensive, fig.cap = "Proportion of Donors by Three Income Groups. Notes: We created three income groups, with the relative price of giving rising (circle), unchanged (triangle), and falling (square) between 2013 and 2014. The group averages are normalized to be zero in 2013.", out.extra = "", eval = output_type() != "body"
plot_giving4 <- estdf %>%
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

plot_giving4

ggsave(
  here("figures", "summary-giving-extensive.pdf"),
  plot = plot_giving4,
  width = 10,
  height = 6
)

#' ```{asis, echo = output_type() %in% c("body", "preview")}
#' 図\@ref(fig:SummaryGivingOverall)は
#' 税インセンティブの変化に基づいた所得グループごとの平均寄付額を示している（非寄付者も含めている）。
#' この図から価格効果を観察できる。言い換えれば、税インセンティブは寄付行動を促進していることが観察される。
#' 2015年以降、税制改革によって税インセンティブが拡大した（もしくは変化しなかった）人は所得控除時よりも増えているが、
#' 税インセンティブが縮小した人は所得控除時よりも減少している。
#' また、寄付者に限定した平均寄付額と寄付者の割合のトレンドを所得グループごとに見ると、
#' 似たような傾向が観察された
#' （補論を参照せよ）。
#' ただし、寄付者に限定した平均寄付のトレンドを見ると、
#' 図\@ref(fig:SummaryGivingOverall)ほどはっきりとした価格効果を観察できない。
#'
#' また、すべての所得グループの2014年の平均寄付額は2013年のそれを下回っている。
#' これはいくつかの可能性が考えられる。
#' 第一に、税制改革のアナウンスメント効果である。
#' 2014年の税制改革は2013年に告知されているので、
#' 税インセンティブが縮小する所得グループにおいては、
#' 2013年の寄付額を増やし、2014年の寄付額を減らすという異時点間の代替効果が予想される。
#' しかしながら、これは税インセンティブが拡大する所得グループの寄付額が減少した事実を説明できない。
#' 第二の可能性は、制度の学習効果が考えられる。
#' 税制改革直後は、税額控除によって自分が寄付によって節税しやすくなったかどうかが分からないので、
#' 税インセンティブが拡大する所得グループでも寄付額は減少した。
#' それ以降、税インセンティブが拡大した納税者は自分が寄付によって節税しやすくなることを学習し、
#' 寄付額を所得控除時よりも増やしたと考えられる。
#' ```
#'
#+ SummaryReliefbyIncome, fig.cap = "Proportion of Having Applied for Tax Relief by Three Income Groups. Notes: We created three income groups, with the relative price of giving rising (circle), unchanged (triangle), and falling (square) between 2013 and 2014. The group averages are normalized to be zero in 2013.", out.extra = "", eval = FALSE
summary_relief <- estdf %>%
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

summary_relief

ggsave(
  here("figures", "summary-relief-by-income.pdf"),
  plot = summary_relief,
  width = 10,
  height = 6
)

#' ```{asis, echo = output_type() == "slide"}
#' ## Distribution of Giving Amount by Application of Tax Relief 
#' ```
#+ SummaryGivingIntensiveDist, fig.cap = "Estimated Distribution of Charitable Giving among Donors in Each Year", out.extra = "", eval = output_type() != "body"
plot_dist <- estdf %>%
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
  here("figures", "summary-giving-intensive-dist.pdf"),
  plot = plot_dist,
  width = 10,
  height = 6
)

#' ```{asis, echo = output_type() %in% c("body", "preview")}
#' 寄付価格の変動は寄付控除の申告の有無でも生じる。
#' 寄付控除を申告した場合の寄付の相対価格は図\@ref(fig:SummaryPrice)に示した通りである一方で、
#' 寄付控除を申告しない場合の寄付の相対価格は1である。
#' よって、控除の有無によって寄付の相対価格は変化する。
#' しかしながら、寄付控除の申告は自己選択なので、内生的である。
#' 後に述べるように、この内生性を解決するために操作変数が必要である。
#'
#' 寄付控除の申告行動において、申告コストは大きな障害となっている可能性が高い。
#' 補論\@ref(addtab)の図\@ref(fig:SummaryGivingIntensiveDist)に示しているように、
#' 寄付控除の申告の有無によって、寄付者に限定した寄付額の分布は大きく変化しない。
#' これは寄付控除の申告の有無は、控除によって得られる便益の差よりも
#' 申告するためのコストの差で説明できることを示唆している。
#' ```
#'
#' ```{asis, echo = output_type() == "slide"}
#' ## Compliance Rate by Wage Earners or Not
#' ```
#'
#+ SummaryReliefbyEarner, fig.cap = "Share of Tax Relief by Wage Earners. Notes: A solid line is the share of applying for tax relief among wage eaners. A dashed line is the share of applying for tax relief other than wage earners.", out.extra = "", eval = output_type() != "appx"
plot_relief2 <- estdf %>%
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

plot_relief2

ggsave(
  here("figures", "summary-relief-by-earner.pdf"),
  plot = plot_relief2,
  width = 10,
  height = 6
)

#' ```{asis, echo = output_type() == "slide"}
#' ## Compliance Rate by Wage Earners or Not (Conditional on Donors)
#' ```
#+ SummaryReliefbyEarner2, fig.cap = "Share of Tax Relief by Wage Earners Conditional on Donors. Notes: A solid line is the share of applying for tax relief among wage eaners. A dashed line is the share of applying for tax relief other than wage earners.", out.extra = "", eval = output_type() != "body"
plot_relief3 <- estdf %>%
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

plot_relief3

ggsave(
  here("figures", "summary-relief-by-earner2.pdf"),
  plot = plot_relief3,
  width = 10,
  height = 6
)

#'
#' ```{asis, echo = output_type() %in% c("body", "preview")}
#' 以上を踏まえて、
#' 我々は申告コストの要素の一つであるレコードキーピングに関する制度背景を第二の識別戦略として用いる。
#' 先に述べたように、自営業者は寄付控除を申請するまで寄付の領収書（証明書）を保持しておく必要がある一方で、
#' 給与所得者は会社を通じてその証明書をいつでも提出でき、
#' その後の申請も会社に手続きを依頼できる。
#' すなわち、給与所得者は自営業者よりも申告コストが低いことが予想される。
#' 事実、図\@ref(fig:SummaryReliefbyEarner)
#' 給与所得者の控除の申請比率は自営業者よりもすべての期間を通じて高いことが分かる[^condfig]。
#' 我々は給与所得者ダミーをレコードキーピングのコストの代理変数として操作変数に用いる。
#'
#' [^condfig]: 寄付者に限定した控除の申告比率についても、給与所得者の方が自営業者よりも高い
#' （補論\@ref(addtab)の図\@ref(fig:SummaryReliefbyEarner2)）。
#' ```