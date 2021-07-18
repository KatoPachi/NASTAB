#' Issue #76: ITTの問題を克服するために、PSM-DIDのストラテジーを利用した推定
#'
#' パッケージのロード
#+
library(xfun)
xfun::pkg_attach2(c("readstata13", "tidyverse", "rlist"))
xfun::pkg_attach2(c("plm", "lmtest", "sandwich", "lfe", "Formula", "censReg"))
xfun::pkg_attach2("kableExtra")

lapply(Sys.glob(file.path("script/R/functions", "*.r")), source)

#' データのロード
#+
df <- read.dta13("data/shaped.dta") %>%
  data.frame() %>%
  mutate(
    log_price = log(price),
    log_lprice = log(lprice),
    log_iv1price = log(iv1price),
    log_iv2price = log(iv2price),
    log_iv3price = log(iv3price),
    log_total_g = log(i_total_giving + 1),
    log_pinc_all = log(lincome + 100000),
    ext_benefit = if_else(year >= 2014, ext_credit_giving, ext_deduct_giving)
  ) %>%
  filter(year >= 2012 & age >= 24)

#'
#' ## 寄付の税申告ファクトの整理
#'
#' - 2012~2013年：所得控除の申請をしたかどうか
#' - 2014年：税額控除の申請をしたかどうか
#'
#' 以下の表はそのクロス集計
#+
df %>%
  with(table(year, ext_benefit, useNA = "always")) %>%
  kable(
    col.names = c("Not receive benefit", "Receive benefit", "NA"),
    align = "ccc"
  ) %>%
  kable_styling()

#'
#' 2013年の相対寄付価格と比較して、
#' 2014年の制度改革によって、寄付価格が増加・変化しない・減少するグループに分けて、
#' 税控除申請比率の推移を確認する。
#'
#' この図のメッセージ
#'
#' - 2013年の相対寄付価格が低いほど、申請比率は高い（グループ間比較）
#' - 2014年の制度改革によって相対寄付価格が減少したグループは控除申請比率が高まった（前後比較）
#'
#+
df %>%
  group_by(year, credit_neutral, credit_benefit, credit_loss) %>%
  summarize_at(vars(ext_benefit), list(~ mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(group = case_when(
    credit_loss == 1 ~ "loss",
    credit_neutral == 1 ~ "neutral",
    credit_benefit == 1 ~ "benefit"
  )) %>%
  mutate(group = factor(
    group,
    levels = c("loss", "neutral", "benefit"),
    labels = c(
      "2013 giving price < 0.85",
      "2013 giving price = 0.85",
      "2013 giving price > 0.85"
    )
  )) %>%
  filter(!is.na(group)) %>%
  ggplot(aes(x = year, y = ext_benefit, group = group)) +
  geom_point(aes(shape = group), color = "black", size = 3) +
  geom_line(aes(linetype = group), size = 1) +
  geom_vline(aes(xintercept = 2013.5)) +
  scale_x_continuous(breaks = seq(2012, 2018, 1)) +
  labs(y = "share of receiving tax benefit", linetype = "", shape = "") +
  ggtemp()
