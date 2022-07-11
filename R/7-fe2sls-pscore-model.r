#' ---
#' title: |
#'   Estimating Effect of Tax Incentives on Donations
#'   Considering Self-Selection of Tax Incentives in South Korea
#' subtitle: |
#'   Results of FE-2SLS
#' author:
#'   - Hiroki Kato
#'   - Tsuyoshi Goto
#'   - Yongrok Kim
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
library(here)
source(here("R", "_library.r"))

#+ include = FALSE
source(here("R", "_html_header.r"))

#+ include = FALSE
rawdt <- readr::read_csv(
  here("data/shaped2_propensity.csv"),
  guess_max = 30000
)

use <- rawdt %>%
  select(
    pid,
    hhid,
    year,
    linc_ln,
    sqage,
    hh_num,
    have_dependents,
    indust,
    area,
    price_ln,
    lprice_ln,
    d_relief_donate,
    psc_pool,
    psc_sep,
    outcome_intensive = donate_ln,
    outcome_extensive = d_donate
  ) %>%
  mutate(
    flag_extensive = 1,
    flag_intensive = if_else(outcome_extensive == 1, 1, 0)
  ) %>%
  pivot_longer(
    outcome_intensive:flag_intensive,
    names_to = c(".value", "type"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  mutate(
    effective = d_relief_donate * lprice_ln,
    applicable = lprice_ln
  )


#+
fixest::setFixest_fml(
  ..stage2 = ~ linc_ln + sqage + hh_num + have_dependents |
    year + pid + indust + area
)

sep_ps <- list(
  outcome ~ ..stage2 | effective ~ psc_sep:price_ln,
  outcome ~ ..stage2 | effective ~ price_ln + psc_sep:price_ln
)

est_sep_ps <- use %>%
  mutate(type = factor(type, levels = c("intensive", "extensive"))) %>%
  group_by(type) %>%
  do(est = lapply(
    sep_ps,
    function(x) feols(x, data = subset(., flag == 1), cluster = ~hhid)
  ))

#+ psiv-stage1, eval = FALSE
est_sep_ps %>%
  pull(est) %>%
  flatten() %>%
  lapply(function(x) x$iv_first_stage$effective) %>%
  setNames(paste0("(", seq(length(sep_ps) * 2), ")")) %>%
  modelsummary(
    title = "First-Stage Results of FE-2SLS with Propensity Score",
    coef_map = c(
      "price_ln" = "Applicable first price",
      "psc_sep:price_ln" = "Applicable first price\u00d7Propensity score",
      "price_ln:psc_sep" = "Applicable first price\u00d7Propensity score",
      "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1)
  ) %>%
    kableExtra::kable_styling() %>%
    kableExtra::add_header_above(c(
      "Sample:",
      "Intensive-margin" = 2, "Extensive-margin" = 2
    )) %>%
    footnote(
      general_title = "",
      general = paste(
        "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
        "Standard errors are clustered at household level.",
        "A square bracket is F statistics of instrument."
      ),
      threeparttable = TRUE,
      escape = FALSE
    )

#+ psiv-stage2
stats_stage1 <- est_sep_ps %>%
  group_by(type) %>%
  do(tab = data.frame(
    models = paste0(.$type, 1:2),
    f = lapply(.$est[[1]], function(x) {
      fitstat(x, "ivf")[[1]]$stat
    }) %>% as_vector(),
    wh = lapply(.$est[[1]], function(x) {
      fitstat(x, "wh")$wh$p
    }) %>% as_vector()
  )) %>%
  {
    bind_rows(.$tab)
  } %>%
  mutate(
    f = sprintf("%1.2f", f),
    wh = sprintf("%1.3f", wh)
  ) %>%
  pivot_longer(f:wh, names_to = "terms") %>%
  pivot_wider(names_from = models, values_from = value) %>%
  mutate(
    terms = recode(
      terms,
      "f" = "F-statistics of instruments",
      "wh" = "Wu-Hausman test, p-value"
    )
  ) %>%
  bind_rows(c(
    terms = "First-Stage model",
    intensive1 = "(C)",
    intensive2 = "(A)+(C)",
    extensive1 = "(C)",
    extensive2 = "(A)+(C)"
  ), .)

implied_e <- est_sep_ps %>%
  dplyr::filter(type == "extensive") %>%
  pull(est) %>%
  flatten() %>%
  lapply(function(x) {
    tribble(
      ~terms, ~extensive,
      "Implied price elasticity",
      sprintf("%1.3f", coef(x)[1] / mean(rawdt$d_donate, na.rm = TRUE)),
      "",
      sprintf(
        "(%1.3f)", sqrt(vcov(x)[1, 1]) / mean(rawdt$d_donate, na.rm = TRUE)
      )
    )
  }) %>%
  reduce(full_join, by = "terms", suffix = c("1", "2")) %>%
  left_join(
    tribble(
      ~terms, ~intensive1, ~intensive2,
      "Implied price elasticity", NA_character_, NA_character_,
      "", NA_character_, NA_character_
    ),
    by = "terms"
  ) %>%
  select(
    terms,
    intensive1:intensive2,
    extensive1:extensive2
  )

add_rows <- bind_rows(implied_e, stats_stage1)
attr(add_rows, "position") <- c(5, 6)

est_sep_ps %>%
  pull(est) %>%
  flatten() %>%
  setNames(paste0("(", seq(length(sep_ps) * 2), ")")) %>%
  modelsummary(
    title = "Second-Stage Results of FE-2SLS with Propensity Score",
    coef_map = c(
      "fit_effective" = "Effective last price",
      "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = add_rows
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    " ",
    "Intensive-margin" = 2, "Extensive-margin" = 2
  )) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at household level.",
      "Instruments are (A) logged value of the applicable first price, and",
      "(C) the product of propensity score of application and",
      "logged value of the applicable first price.",
      "We control squared age (divided by 100), number of household members,",
      "a dummy that indicates having dependents, a set of dummies of industry,",
      "a set of dummies of residential area,",
      "and individual and time fixed effects."
    ),
    threeparttable = TRUE,
    escape = FALSE
    )

#'
#' 寄付控除の申請が自己選択であることを制御するために、給与所得者ダミーを操作変数として用いている。
#' この代替的な手法として、寄付控除の申請の傾向スコアを給与所得者ダミーの代わりに用いる（**なぜこれする？**）。
#'
#' - サンプルを年ごとに分割して、
#' 給与所得者ダミー・Applicable first price・FE-2SLSで用いた共変量を説明変数としたプロビットモデルを推定した。
#' - 推定されたモデルから得られる予測値を傾向スコアとする。
#' - 表\@ref(tab:psiv-stage1)は傾向スコアを操作変数として用いたFE-2SLSの第一段階の推定結果である。
#' - これまでの結果と整合的であり、寄付控除を申請しやすい人ほど、
#' applicable first priceとeffective last priceの相関が強くなる。
#' - 表\@ref(tab:psiv-stage2)は傾向スコアを操作変数として用いたFE-2SLSの第二段階の推定結果である。
#' - これまでの結果と整合的であり、弾力性の値も大きく変化しない。
#'
#+
pool_ps <- list(
  outcome ~ ..stage2 | effective ~ psc_pool:price_ln,
  outcome ~ ..stage2 | effective ~ price_ln + psc_pool:price_ln
)

est_pool_ps <- use %>%
  mutate(type = factor(type, levels = c("intensive", "extensive"))) %>%
  group_by(type) %>%
  do(est = lapply(
    pool_ps,
    function(x) feols(x, data = subset(., flag == 1), cluster = ~hhid)
  ))

#+ psiv-pool-stage1, eval = FALSE
est_pool_ps %>%
  pull(est) %>%
  flatten() %>%
  lapply(function(x) x$iv_first_stage$effective) %>%
  setNames(paste0("(", seq(length(sep_ps) * 2), ")")) %>%
  modelsummary(
    title = "First-Stage Results of FE-2SLS with Propensity Score (Pooled)",
    coef_map = c(
      "price_ln" = "Applicable first price",
      "psc_pool:price_ln" = "Applicable first price\u00d7Propensity score",
      "price_ln:psc_pool" = "Applicable first price\u00d7Propensity score",
      "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1)
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    "Sample:",
    "Intensive-margin" = 2, "Extensive-margin" = 2
  )) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at household level.",
      "A square bracket is F statistics of instrument."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#+ psiv-pool-stage2
stats_stage1 <- est_pool_ps %>%
  group_by(type) %>%
  do(tab = data.frame(
    models = paste0(.$type, 1:2),
    f = lapply(.$est[[1]], function(x) {
      fitstat(x, "ivf")[[1]]$stat
    }) %>% as_vector(),
    wh = lapply(.$est[[1]], function(x) {
      fitstat(x, "wh")$wh$p
    }) %>% as_vector()
  )) %>%
  {
    bind_rows(.$tab)
  } %>%
  mutate(
    f = sprintf("%1.2f", f),
    wh = sprintf("%1.3f", wh)
  ) %>%
  pivot_longer(f:wh, names_to = "terms") %>%
  pivot_wider(names_from = models, values_from = value) %>%
  mutate(
    terms = recode(
      terms,
      "f" = "F-statistics of instruments",
      "wh" = "Wu-Hausman test, p-value"
    )
  ) %>%
  bind_rows(c(
    terms = "First-Stage model",
    intensive1 = "(C)",
    intensive2 = "(A)+(C)",
    extensive1 = "(C)",
    extensive2 = "(A)+(C)"
  ), .)

implied_e <- est_pool_ps %>%
  dplyr::filter(type == "extensive") %>%
  pull(est) %>%
  flatten() %>%
  lapply(function(x) {
    tribble(
      ~terms, ~extensive,
      "Implied price elasticity",
      sprintf("%1.3f", coef(x)[1] / mean(rawdt$d_donate, na.rm = TRUE)),
      "",
      sprintf(
        "(%1.3f)", sqrt(vcov(x)[1, 1]) / mean(rawdt$d_donate, na.rm = TRUE)
      )
    )
  }) %>%
  reduce(full_join, by = "terms", suffix = c("1", "2")) %>%
  left_join(
    tribble(
      ~terms, ~intensive1, ~intensive2,
      "Implied price elasticity", NA_character_, NA_character_,
      "", NA_character_, NA_character_
    ),
    by = "terms"
  ) %>%
  select(
    terms,
    intensive1:intensive2,
    extensive1:extensive2
  )

add_rows <- bind_rows(implied_e, stats_stage1)
attr(add_rows, "position") <- c(5, 6)

est_pool_ps %>%
  pull(est) %>%
  flatten() %>%
  setNames(paste0("(", seq(length(pool_ps) * 2), ")")) %>%
  modelsummary(
    title = "Second-Stage Results of FE-2SLS with Propensity Score (Pooled)",
    coef_map = c(
      "fit_effective" = "Effective last price",
      "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = add_rows
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    " ",
    "Intensive-margin" = 2, "Extensive-margin" = 2
  )) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at household level.",
      "Instruments are (A) logged value of the applicable first price, and",
      "(C) the product of propensity score of application and",
      "logged value of the applicable first price.",
      "We control squared age (divided by 100), number of household members,",
      "a dummy that indicates having dependents, a set of dummies of industry,",
      "a set of dummies of residential area,",
      "and individual and time fixed effects."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#'
#' - また、全期間のサンプルで推定したプロビットモデルの傾向スコアを用いても、
#' 同じ結果が得られる
#' （第一段階の結果は表\@ref(tab:psiv-pool-stage1)に示し、
#' 第二段階の結果は表\@ref(tab:psiv-pool-stage2)に示した）
#'
# /*
#+
rmarkdown::render(
  here("R", "11-iv-pscore.r"),
  output_dir = here("docs", "html-preview")
)
# */