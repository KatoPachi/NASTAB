#' Issue #71: Intensive marginの推定のときに寄付していない人を考慮できていない問題
#'
#' パッケージのロード。Censored regressionは`censReg`パッケージを使う
#+
library(xfun)
xfun::pkg_attach2(c("readstata13", "tidyverse", "rlist"))
xfun::pkg_attach2(c("plm", "lmtest", "sandwich", "lfe", "Formula", "censReg"))
source("script/R/00-analysis_functions.r")

#' 以下、`ggplot2`のテンプレート
#+
my_theme <- theme_minimal() +
  theme(
    # setting: background
    plot.background = element_rect(
      #fill="#87CEEB50",
      color = "transparent"
    ),

    # setting: plot
    panel.border = element_rect(color = "white", fill = NA),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey80"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),

    # setting: text
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.caption = element_text(size = 11),

    # setting: axis
    axis.text = element_text(color = "black", size = 13),
    axis.title = element_text(size = 13),
    axis.ticks.length = unit(0.25, "cm"),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.line = element_line(),

    # setting: legend
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.5, "cm"),
    #legend.background = element_rect(color = "black"),
    legend.position = "bottom"
  )

#' データの読み込み
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
    ) %>%
    filter(year >= 2012 & age >= 24)

#' ## Intensive-margin elasticityの推定
#'
#' ### Reminder
#'
#' 比較のためにベースラインのモデル結果を示す
#'
#+
xlist <- list(
  quote(log_price + log_pinc_all),
  quote(log_price + log_pinc_all + age + sqage),
  quote(log_price + log_pinc_all + age + sqage + factor(year):factor(educ)),
  quote(
    log_price + log_pinc_all + age + sqage +
    factor(year):factor(educ) + factor(year):factor(gender)
  ),
  quote(
    log_price + log_pinc_all + age + sqage + factor(year):factor(educ) +
    factor(year):factor(gender) + factor(year):factor(living_area)
  )
)
fixef <- list(quote(year + pid))
cluster <- list(quote(pid))

covnote <- tribble(
  ~vars, ~stat, ~reg1, ~reg2, ~reg3, ~reg4, ~reg5,
  "Individual FE", "vars", "Y", "Y", "Y", "Y", "Y",
  "Time FE", "vars", "Y", "Y", "Y", "Y", "Y",
  "Age", "vars", "N", "Y", "Y", "Y", "Y",
  "Year x Education", "vars", "N", "N", "Y", "Y", "Y",
  "Year x Gender", "vars", "N", "N", "N", "Y", "Y",
  "Year x Resident Area", "vars", "N", "N", "N", "N", "Y"
)

i_elast <- est_felm(
  y = list(quote(log_total_g)), x = xlist,
  fixef = fixef, cluster = cluster,
  data = subset(df, i_ext_giving == 1)
)

fullset_tab(
  i_elast$result,
  keep_coef = c("log_price", "log_pinc_all"),
  label_coef = list(
    "log_price" = "ln(giving price)",
    "log_pinc_all" = "ln(annual taxable income)"
  )
)$set %>%
bind_rows(covnote) %>%
mutate(vars = if_else(stat == "se", "", vars)) %>%
dplyr::select(-stat) %>%
kable(
  title = "Itensive margin (Those who give)",
  align = "lccccc"
) %>%
add_header_above(c("", "log(Giving amount)" = 5), escape = FALSE)

#' ### Censored Regression with panel data
#'
#' Function `censReg` automatically estimates
#' a **random effects censored regression model**
#' if argument data is of class "pdata.frame",
#' i.e. created with function pdata.frame of package `plm`
#'
#' Censored regressionはこれ(ML)で出来るが、marginal effectが計算できない。
#' 関数を定義すればよいのだが、まだパネルデータにおける限界効果を計算していない
#' （面倒。やる価値があるほどか？）
#' 
#' よって、意味はないが計算結果だけ示す。
#' 
#+

pdf <- df %>% pdata.frame(df, c("pid", "year"))

censmodel <- list(
  reg1 = log_total_g ~ log_price + log_pinc_all,
  reg2 = log_total_g ~ log_price + log_pinc_all + age + sqage
  # reg3 = log_total_g ~ log_price + log_pinc_all + age + sqage +
  #   factor(year):factor(educ),
  # reg4 = log_total_g ~ log_price + log_pinc_all + age + sqage +
  #   factor(year):factor(educ) + factor(year):factor(gender),
  # reg5 = log_total_g ~ log_price + log_pinc_all + age + sqage +
  #   factor(year):factor(educ) + factor(year):factor(gender) +
  #   factor(year):factor(living_area)
)

est_cens <- censmodel %>%
  purrr::map(~censReg(., data = pdf, method = "BHHH"))

est_cens %>% purrr::map(~summary(.))

#'
#' ### 余談：control function approach
#'
#' もう一つの方法として`control function approach`がある
#' これは傾向スコアでoutcome equationの誤差項のselectionを代用（置き換える）するもの。
#' 詳しくはggr
#'
#' ただし、傾向スコアの計算にexclustion restrictionを満たす操作変数が必要なため、やっぱり難しいか 
#'
#' ## そもそも・・・
#'
#' やる必要ありますかね？
#' 確かに、セレクションによって誤差項がおかしくなることは分かるのだが・・・
#' このデータセットでは難しいです。限界点として指摘しておくか、うまいdiffenceを考えるか。
#'
#' 以下、誤差項がおかしくなるの意味。
#' 
#' $$ Y_i = (X_i \beta + U)D_i $$
#'
#' $$ E(Y_i | D_i = 1, X_i) = X_i \beta + E(U|D_i = 1, X_i) $$