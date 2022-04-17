#' ---
#' title: Marginal Treatment Effect
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
  "estimatr", "fixest",
  "ivmte", "splines2", "gurobi"
))

lapply(Sys.glob(file.path("script/functions", "*.r")), source)

#'
#+
estdf <- readr::read_csv("data/shaped2_propensity.csv", guess_max = 30000)

#'
#+ eval = FALSE
# check whether "gurobi" works
# create optimization problem
model <- list()
model$obj        <- c(1, 1, 2)
model$modelsense <- "max"
model$rhs        <- c(4, 1)
model$sense      <- c("<", ">")
model$vtype      <- "B"
model$A          <- matrix(c(1, 2, 3, 1, 1, 0), nrow = 2, ncol = 3,
                           byrow = TRUE)

# solve the optimization problem using Gurobi
result <- gurobi(model, list())

#'
#' ## 税インセンティブの効果の異質性を考慮する
#'
#' - ここまでは税インセンティブに対する効果が全個人で同質であることを想定していた
#' - 申告の自己選択に観察できない要素があるときに、申告（税インセンティブ）を受けることで寄付をどの程度増やすかを検証する
#'   - Marginal Treatment Effect [@Vytlacil2007]は自己選択によって生じるすべての税インセンティブの異質性を捉えることができる
#' - MTE(x, u) = \mu_1(x) - \mu_0(x) + E[\eta | U = u]
#'   - $\eta$は申告を受けたときの寄付額の観察できない要因と申告を受けないときの寄付額の観察できない要因の差
#'   - $u$は寄付を申告したくない程度を$[0, 1]$の範囲で表したもの
#'   - $E[\eta | U = u]$は$u$について線形として仮定する
#'
#' ## MTEの利点
#'
#' 1. ATE・ATT・ATUを推定することができる
#'     - ATU: 申告コストをゼロにしたとき、これまで申告しなかった人がどれだけ寄付を増やせるか
#' 1. 申告コストを動かす外生的な要因が観察できるので、構造推定に頼ることなく、以上のパラメータを推定できる
#' 1. ATE・ATT・ATU以外の政策パラメータを定義して、推定することができる（Policy revalent treatment effect）
#' 1. MTEの推定から社会厚生への含意が得られる[@Chetty2009; @Tanaka2021]
#'
#' ## 線型のMTE関数
#'
#' $\mu_1(x) - \mu_0(x)$の平均値を切片とし、$E[\eta | U = u]$を示したもの
#'
#+
intargs <- list(
  data = subset(estdf, d_donate == 1),
  target = "att",
  m0 = ~ u + linc_ln + sqage + factor(area) +
    factor(year),
  m1 = ~ u + price_ln + linc_ln + sqage + factor(area) +
    factor(year),
  outcome = ~ donate_ln,
  treat = ~ d_relief_donate,
  propensity = ~ psc_sep,
  solver.options = list(OptimalityTol = 1e-9)
)

intmte <- do.call(ivmte, intargs)

u <- seq(0.05, 0.95, by = .01)
b <- intmte$mtr
x <- intmte$X[, -c(2, 27)]
bx <- x %*% b[-c(2, 27)]

intercept1 <- mean(bx[x[, "[m0](Intercept)"] == 0, ])
intercept0 <- mean(bx[x[, "[m0](Intercept)"] == 1, ])
pred <- intercept1 - intercept0 + u * (b["[m1]u"] - b["[m0]u"])

data.frame(x = u, y = pred) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_hline(aes(yintercept = 0), linetype = 3) +
  labs(
    x = "Unobserved resistance to apply for tax relief (u)",
    y = "MTE(u)"
  ) +
  ggtemp()

#'
#' ## Population-levelパラメータの推定
#'
#+
att <- intmte$point.estimate
muprice_att <- with(
  subset(estdf, d_donate == 1 & d_relief_donate == 1), mean(price_ln)
)
attelast <- att / muprice_att
  

intargs$target <- "ate"
ate <- do.call(ivmte, intargs)$point.estimate
muprice_ate <- with(
  subset(estdf, d_donate == 1), mean(price_ln)
)
ateelast <- ate / muprice_ate

intargs$target <- "atu"
atu <- do.call(ivmte, intargs)$point.estimate
muprice_atu <- with(
  subset(estdf, d_donate == 1 & d_relief_donate == 0), mean(price_ln)
)
atuelast <- atu / muprice_atu

intpara <- data.frame(
  target = c("ATT", "ATE", "ATU"),
  parameter = c(att, ate, atu),
  meanprice = c(muprice_att, muprice_ate, muprice_atu),
  elasticity = c(attelast, ateelast, atuelast)
)

intpara %>%
  kable(
    caption = paste(
      "Estimating ATE, ATU and ATT on logged donations"
    ),
    col.names = c(
      "Target", "Estimated Effect", "Mean of Logged Price",
      "Implied Elasticity"
    ),
    align = "lccc",
    digits = 4, booktabs = TRUE, linesep = ""
  ) %>%
  kableExtra::kable_styling(font_size = 7)

#'
#' - Estimated Effectは申告によって寄付が何%増えるかを示している
#' - Implied elasticityはEstimated effectからMean of Logged Priceを割って、弾力性を計算したものを示している 
#'
# /*
#+
rmarkdown::render(
  "script/4-mte.r",
  output_dir = "report/view"
)
# */