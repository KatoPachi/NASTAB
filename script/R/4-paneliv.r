#' ---
#' title: |
#'   Preview: Recovering Population Elasticities
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
#+ include = FALSE, eval = params$preview
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
    ideal_balance = col_double()
  )
)

fixest::setFixest_fml(
  ..first1 = ~ log_pinc_all | year + panelid,
  ..first2 = ~ log_pinc_all + sqage | year + panelid,
  ..first3 = ~ log_pinc_all + sqage | year + panelid + area,
  ..first4 = ~ log_pinc_all + sqage | year + panelid + area + industry
)


#'
#' ## Recovering Population Elasticities
#'
#'
#+
popmod <- list(
  "(1)" = fixest::xpd(log_total_g ~ log_price:ext_benefit_tl + ..first1),
  "(2)" = fixest::xpd(log_total_g ~ log_price:ext_benefit_tl + ..first2),
  "(3)" = fixest::xpd(log_total_g ~ log_price:ext_benefit_tl + ..first3),
  "(4)" = fixest::xpd(log_total_g ~ log_price:ext_benefit_tl + ..first4)
)

popmod %>%
  purrr::map(~ fixest::feols(
    .,
    data = subset(df, i_ext_giving == 1),
    cluster = ~ panelid, se = "cluster"
  )) %>%
  modelsummary(
    title = "Population First-Price Elasiticities",
    coef_map = c(
      "log_price:ext_benefit_tl" =
        "log(first giving price) x Applying tax relief",
      "log_pinc_all" =
        "log(annual taxable income)"
    ),
    gof_omit = "^(?!R2 Adj.|FE|N|Std.Errors)",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = tribble(
      ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)",
      "Square age", "", "X", "X", "X"
    )
  )

#'
#+
xlist <- list(
  ~ employee,
  ~ employee + log_pinc_all + age + sqage +
    factor(year):factor(educ) + factor(year):factor(gender),
  ~ employee + log_pinc_all + age + sqage +
    factor(year):factor(educ) + factor(year):factor(gender) +
    factor(year):factor(indust)
)

xlist_tab <- tribble(
  ~vars, ~stat, ~reg1, ~reg2, ~reg3,
  "Time FE", "vars", "Y", "Y", "Y",
  "log(income)", "vars", "N", "Y", "Y",
  "Age", "vars", "N", "Y", "Y",
  "Year x Education", "vars", "N", "Y", "Y",
  "Year x Gender", "vars", "N", "Y", "Y",
  "Year x Resident Area", "vars", "N", "Y", "Y",
  "Year x Dummy of industry", "vars", "N", "N", "Y"
)

est_xreport1 <- xlist %>%
  purrr::map(~ fit_fixest(
    y = int_price_benefit ~ ., x = .,
    fixef = ~ year, cluster = ~ pid,
    data = subset(df, year <= 2017)
  ))

est_xreport2 <- xlist %>%
  purrr::map(~ fit_fixest(
    y = int_price_benefit ~ ., x = .,
    fixef = ~year + pid, cluster = ~pid,
    data = subset(df, year <= 2017)
  ))

list(est_xreport1, est_xreport2) %>%
  purrr::map(~ regtab_fixest(., keep_coef = "employee")) %>%
  purrr::map(~ regtab_addline(., list(xlist_tab))) %>%
  reduce(full_join, by = c("vars", "stat")) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist) * 2))),
    align = "lcccccc"
  ) %>%
  add_header_above(c("", "w/o individual FE" = 3, "w/ individual FE" = 3)) %>%
  kable_styling()

#'
#' Panel IVの推定結果のメッセージ
#' - 弾力性は10%。1%の相対価格の上昇が寄付額を10%程度下げる
#' - ITTで考えていたよりもかなり弾力的な結果となる
#+
ivest_xreport1 <- xlist %>%
  purrr::map(~ fit_fixest(
    y = log_total_g ~ .,
    x = update(., ~ . - employee),
    z = int_price_benefit ~ employee,
    fixef = ~ year, cluster = ~ pid,
    data = subset(df, year <= 2017)
  ))

ivest_xreport2 <- xlist %>%
  purrr::map(~ fit_fixest(
    y = log_total_g ~ .,
    x = update(., ~ . - employee),
    z = int_price_benefit ~ employee,
    fixef = ~year + pid, cluster = ~ pid,
    data = subset(df, year <= 2017)
  ))

ivfstat <- list(ivest_xreport1, ivest_xreport2) %>%
  purrr::map_depth(2, ~ fitstat(., ~ ivwald)$ivwald1$stat) %>%
  purrr::map(~ as_vector(.)) %>%
  as_vector() %>%
  matrix(nrow = 1) %>%
  data.frame() %>%
  setNames(paste0("reg", seq_len(length(xlist) * 2)))

ivfstat_tab <- ivfstat %>%
  mutate_all(list(~ sprintf("%1.3f", .))) %>%
  mutate(vars = "Fstat of IV", stat = "vars")

xlist_duptab <- xlist_tab %>%
  full_join(xlist_tab, by = c("vars", "stat")) %>%
  setNames(c("vars", "stat", paste0("reg", seq_len(length(xlist) * 2))))

list(ivest_xreport1, ivest_xreport2) %>%
  flatten() %>%
  regtab_fixest(
    keep_coef = "int_price_benefit",
    label_coef = list("fit_int_price_benefit" = "Report x log(first price)")
  ) %>%
  regtab_addline(list(ivfstat_tab, xlist_duptab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist) * 2))),
    align = "lcccccc"
  ) %>%
  add_header_above(c("", "w/o individual FE" = 3, "w/ individual FE" = 3)) %>%
  kable_styling()

#'
#' extensive-margin elasticityのPanel IVの結果
#'
#' - 価格1%上昇によって、寄付する確率を1%下げる
#' - この結果はITTで考えた推定結果と非常に似ている。
#'
#+
ivest_xreport1_ext <- xlist %>%
  purrr::map(~ fit_fixest(
    y = i_ext_giving ~ .,
    x = update(., ~ . - employee),
    z = int_price_benefit ~ employee,
    fixef = ~year, cluster = ~pid,
    data = subset(df, year <= 2017)
  ))

ivest_xreport2_ext <- xlist %>%
  purrr::map(~ fit_fixest(
    y = i_ext_giving ~ .,
    x = update(., ~ . - employee),
    z = int_price_benefit ~ employee,
    fixef = ~ year + pid, cluster = ~pid,
    data = subset(df, year <= 2017)
  ))

waldtest_ext <- list(ivest_xreport1_ext, ivest_xreport2_ext) %>%
  purrr::map_depth(2, ~ tibble(
    vars = "Implied price elasticity",
    coef = coef(.)["fit_int_price_benefit"],
    se = fixest::se(.)["fit_int_price_benefit"],
    invmu = 1 / mean(model.matrix(., type = "lhs")),
    p = fixest::pvalue(.)["fit_int_price_benefit"]
  )) %>%
  purrr::map_depth(2, function(x)
    x %>%
      mutate(
        coef = case_when(
          p <= .01 ~ sprintf("%1.3f***", coef / invmu),
          p <= .05 ~ sprintf("%1.3f**", coef / invmu),
          p <= .1 ~ sprintf("%1.3f*", coef / invmu),
          TRUE ~ sprintf("%1.3f", coef / invmu)
        ),
        se = sprintf("(%1.3f)", se / invmu)
      ) %>%
      dplyr::select(-p, -invmu) %>%
      pivot_longer(-vars, names_to = "stat", values_to = "val")
  ) %>%
  purrr::map(~ reduce(., full_join, by = c("vars", "stat"))) %>%
  reduce(full_join, by = c("vars", "stat")) %>%
  setNames(c("vars", "stat", paste0("reg", seq_len(length(xlist) * 2)))) %>%
  bind_rows(
    c(vars = "", stat = "", reg1 = "", reg2 = "",
      reg3 = "", reg4 = "", reg5 = "", reg6 = ""),
    .
  )

ivfstat_ext <- list(ivest_xreport1_ext, ivest_xreport2_ext) %>%
  purrr::map_depth(2, ~ fitstat(., ~ ivwald)$ivwald1$stat) %>%
  purrr::map(~ as_vector(.)) %>%
  as_vector() %>%
  matrix(nrow = 1) %>%
  data.frame() %>%
  setNames(paste0("reg", seq_len(length(xlist) * 2)))

ivfstat_ext_tab <- ivfstat_ext %>%
  mutate_all(list(~ sprintf("%1.3f", .))) %>%
  mutate(vars = "Fstat of IV", stat = "vars")

list(ivest_xreport1_ext, ivest_xreport2_ext) %>%
  flatten() %>%
  regtab_fixest(
    keep_coef = "int_price_benefit",
    label_coef = list("fit_int_price_benefit" = "Report x log(first price)")
  ) %>%
  regtab_addline(list(waldtest_ext, ivfstat_tab, xlist_duptab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist) * 2))),
    align = "lcccccc"
  ) %>%
  add_header_above(c("", "w/o individual FE" = 3, "w/ individual FE" = 3)) %>%
  kable_styling()

#'
#' intensive-margin elasticityのPanel IVの結果
#'
#' - 個人固定効果を除くと価格の上昇が寄付の増加を促す（個人固定効果を入れていないのが原因？何らかの解釈が可能？）
#' - 個人固定効果を入れると統計的に非有意となる（弱操作変数の影響とは考えにくい）
#'
#+
ivest_xreport1_int <- xlist %>%
  purrr::map(~ fit_fixest(
    y = log_total_g ~ .,
    x = update(., ~ . - employee),
    z = int_price_benefit ~ employee,
    fixef = ~year, cluster = ~pid,
    data = subset(df, i_ext_giving == 1 & year <= 2017)
  ))

ivest_xreport2_int <- xlist %>%
  purrr::map(~ fit_fixest(
    y = log_total_g ~ .,
    x = update(., ~ . - employee),
    z = int_price_benefit ~ employee,
    fixef = ~ year + pid, cluster = ~pid,
    data = subset(df, i_ext_giving == 1 & year <= 2017)
  ))

ivfstat_int <- list(ivest_xreport1_int, ivest_xreport2_int) %>%
  purrr::map_depth(2, ~ fitstat(., ~ ivwald)$ivwald1$stat) %>%
  purrr::map(~ as_vector(.)) %>%
  as_vector() %>%
  matrix(nrow = 1) %>%
  data.frame() %>%
  setNames(paste0("reg", seq_len(length(xlist) * 2)))

ivfstat_int_tab <- ivfstat_int %>%
  mutate_all(list(~ sprintf("%1.3f", .))) %>%
  mutate(vars = "Fstat of IV", stat = "vars")

list(ivest_xreport1_int, ivest_xreport2_int) %>%
  flatten() %>%
  regtab_fixest(
    keep_coef = "int_price_benefit",
    label_coef = list("fit_int_price_benefit" = "Report x log(first price)")
  ) %>%
  regtab_addline(list(ivfstat_int_tab, xlist_duptab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist) * 2))),
    align = "lcccccc"
  ) %>%
  add_header_above(c("", "w/o individual FE" = 3, "w/ individual FE" = 3)) %>%
  kable_styling()


#'
#' ## Panel IV: Endog var is tax report and IV is employed dummy.
#'
#' $\log g_{it}$を寄付の対数値、$\ln p_{it}$は寄付の相対価格（first price）とする。
#' $D_{it}$はtax reportのダミー変数とし、first-stageの予測値を$\hat{D}_{it}$とする。
#' Panel IVの推定において、個人固定効果$\alpha$と時間固定効果$\lambda$はコントロールする。
#' 仮説検定では、個人レベルでクラスターした標準誤差を使用する。
#'
#' Second stage:
#'
#' $$
#' \log g_{it}
#' = \alpha_{2i} + \delta \hat{D}_{it} \log p_{it} + X_{it} \beta_2
#' + \lambda_{2t} + \epsilon_{it}
#' $$
#'
#' First stage:
#'
#' $$
#' D_{it}
#' = \alpha_{1i} + \gamma \text{Employed}_{it} + X_{it} \beta_1
#' + \lambda_{1t} + \eta_{it}
#' $$
#'
#' Panel first-stageのメッセージ
#' - 自営業者より労働者がtax reportする確率は約50%高い
#' - 個人固定効果をコントロールしても統計的に有意（F-値は最低でも80）
#'
#+
est_report1 <- xlist %>%
  purrr::map(~ fit_fixest(
    y = ext_benefit_tl ~ ., x = .,
    fixef = ~ year, cluster = ~ pid,
    data = subset(df, year <= 2017)
  ))

est_report2 <- xlist %>%
  purrr::map(~ fit_fixest(
    y = ext_benefit_tl ~ ., x = .,
    fixef = ~year + pid, cluster = ~pid,
    data = subset(df, year <= 2017)
  ))

wald_iv <- list(est_report1, est_report2) %>%
  purrr::map_depth(2, ~ summary(.)$coeftable["employee", 3]^2) %>%
  purrr::map( 
    ~ c("F-stat of a dummy of employee", "stats", sprintf("%1.2f", as_vector(.)))
  ) %>%
  purrr::map( 
    ~ setNames(., c("vars", "stat", paste0("reg", seq_len(length(xlist)))))
  )

est_report <- list(est_report1, est_report2) 

1:2 %>%
  purrr::map(
    function(x)
    est_report[[x]] %>%
      regtab_fixest(keep_coef = "employee") %>%
      regtab_addline(., list(xlist_tab, wald_iv[[x]]))
  ) %>%
  reduce(full_join, by = c("vars", "stat")) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist) * 2))),
    align = "lcccccc"
  ) %>%
  add_header_above(c("", "w/o individual FE" = 3, "w/ individual FE" = 3)) %>%
  kable_styling()

#'
#' 上の表の(6)の予測値を$\hat{D}_{it}$とする。
#+
df$p_ext_benefit_tl <- predict(est_report2[[3]], df)

#'
#' Panel IVの推定結果のメッセージ
#' - ITTとして推定した弾性値（約1%）より若干弾力的である
#' - Tax reportをしていない人の価格は必ず$\log 1$であることを考えれば、納得できる結果
#+
ivest_report1 <- xlist %>%
  purrr::map(~ fit_fixest(
    y = log_total_g ~ .,
    x = update(., ~ . - employee + log_price:p_ext_benefit_tl),
    fixef = ~ year, cluster = ~ pid,
    data = subset(df, year <= 2017)
  ))

ivest_report2 <- xlist %>%
  purrr::map(~ fit_fixest(
    y = log_total_g ~ .,
    x = update(., ~ . - employee + log_price:p_ext_benefit_tl),
    fixef = ~year + pid, cluster = ~ pid,
    data = subset(df, year <= 2017)
  ))

list(ivest_report1, ivest_report2) %>%
  flatten() %>%
  regtab_fixest(
    keep_coef = "log_price",
    label_coef = list(
      "log_price:p_ext_benefit_tl" = "Propensity of Report x log(first price)"
    )
  ) %>%
  regtab_addline(list(xlist_duptab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist) * 2))),
    align = "lcccccc"
  ) %>%
  add_header_above(c("", "w/o individual FE" = 3, "w/ individual FE" = 3)) %>%
  kable_styling()

#'
#' extensive-margin elasticityのPanel IVの結果
#'
#' - ITTとして想定したベースラインの結果（-1.4 ~ -1.2%）より、非弾力的になる（0.1%）
#' - extensive-marginの弾力性はtax reportをしていない人に特に強い影響を与えるのか？
#'
#+
ivest_report1_ext <- xlist %>%
  purrr::map(~ fit_fixest(
    y = i_ext_giving ~ .,
    x = update(., ~ . - employee + log_price:p_ext_benefit_tl),
    fixef = ~year, cluster = ~pid,
    data = subset(df, year <= 2017)
  ))

ivest_report2_ext <- xlist %>%
  purrr::map(~ fit_fixest(
    y = i_ext_giving ~ .,
    x = update(., ~ . - employee + log_price:p_ext_benefit_tl),
    fixef = ~ year + pid, cluster = ~pid,
    data = subset(df, year <= 2017)
  ))

waldtest_ext <- list(ivest_report1_ext, ivest_report2_ext) %>%
  purrr::map_depth(2, ~ tibble(
    vars = "Implied price elasticity",
    coef = coef(.)["log_price:p_ext_benefit_tl"],
    se = fixest::se(.)["log_price:p_ext_benefit_tl"],
    invmu = 1 / mean(model.matrix(., type = "lhs")),
    p = fixest::pvalue(.)["log_price:p_ext_benefit_tl"]
  )) %>%
  purrr::map_depth(2, function(x)
    x %>%
      mutate(
        coef = case_when(
          p <= .01 ~ sprintf("%1.3f***", coef / invmu),
          p <= .05 ~ sprintf("%1.3f**", coef / invmu),
          p <= .1 ~ sprintf("%1.3f*", coef / invmu),
          TRUE ~ sprintf("%1.3f", coef / invmu)
        ),
        se = sprintf("(%1.3f)", se / invmu)
      ) %>%
      dplyr::select(-p, -invmu) %>%
      pivot_longer(-vars, names_to = "stat", values_to = "val")
  ) %>%
  purrr::map(~ reduce(., full_join, by = c("vars", "stat"))) %>%
  reduce(full_join, by = c("vars", "stat")) %>%
  setNames(c("vars", "stat", paste0("reg", seq_len(length(xlist) * 2)))) %>%
  bind_rows(
    c(vars = "", stat = "", reg1 = "", reg2 = "",
      reg3 = "", reg4 = "", reg5 = "", reg6 = ""),
    .
  )

list(ivest_report1_ext, ivest_report2_ext) %>%
  flatten() %>%
  regtab_fixest(
    keep_coef = "log_price",
    label_coef = list(
      "log_price:p_ext_benefit_tl" = "Propensity of Report x log(first price)"
    )
  ) %>%
  regtab_addline(list(waldtest_ext, xlist_duptab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist) * 2))),
    align = "lcccccc"
  ) %>%
  add_header_above(c("", "w/o individual FE" = 3, "w/ individual FE" = 3)) %>%
  kable_styling()

#'
#' intensive-margin elasticityのPanel IVの結果
#'
#' - 個人固定効果を加えると、ITTとして想定した弾性値（-1 ~ -0.5%）と似たような結果を取る
#' - Intenstive-margin elasticityはtax reportをする人に特に影響を与えるのか？
#'
#+
ivest_report1_int <- xlist %>%
  purrr::map(~ fit_fixest(
    y = log_total_g ~ .,
    x = update(., ~ . - employee + log_price:p_ext_benefit_tl),
    fixef = ~year, cluster = ~pid,
    data = subset(df, i_ext_giving == 1 & year <= 2017)
  ))

ivest_report2_int <- xlist %>%
  purrr::map(~ fit_fixest(
    y = log_total_g ~ .,
    x = update(., ~ . - employee + log_price:p_ext_benefit_tl),
    fixef = ~ year + pid, cluster = ~pid,
    data = subset(df, i_ext_giving == 1 & year <= 2017)
  ))

list(ivest_report1_int, ivest_report2_int) %>%
  flatten() %>%
  regtab_fixest(
    keep_coef = "log_price",
    label_coef = list(
      "log_price:p_ext_benefit_tl" = "Propensity of Report x log(first price)"
    )
  ) %>%
  regtab_addline(list(xlist_duptab)) %>%
  mutate(vars = if_else(stat == "se", "", vars)) %>%
  dplyr::select(-stat) %>%
  kable(
    col.names = c("", sprintf("(%1d)", seq_len(length(xlist) * 2))),
    align = "lcccccc"
  ) %>%
  add_header_above(c("", "w/o individual FE" = 3, "w/ individual FE" = 3)) %>%
  kable_styling()
