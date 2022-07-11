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
    employee,
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
  ) %>%
  dplyr::filter(year < 2013 | 2014 < year)

#+ announcement-effect
fixest::setFixest_fml(
  ..stage2 = ~ linc_ln + sqage + hh_num + have_dependents |
    year + pid + indust + area
)

fe2sls <- list(
  outcome ~ ..stage2 | effective ~ price_ln,
  outcome ~ ..stage2 | effective ~ employee:price_ln,
  outcome ~ ..stage2 | effective ~ price_ln + employee:price_ln
)

est_models <- use %>%
  mutate(type = factor(type, levels = c("intensive", "extensive"))) %>%
  group_by(type) %>%
  do(est = lapply(
    fe2sls,
    function(x) feols(x, data = subset(., flag == 1), cluster = ~hhid)
  ))

stats_stage1 <- est_models %>%
  group_by(type) %>%
  do(tab = data.frame(
    models = paste0(.$type, c(1, 2, 3)),
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
    terms = "Instruments",
    intensive1 = "(A)",
    intensive2 = "(B)",
    intensive3 = "(A)+(B)",
    extensive1 = "(A)",
    extensive2 = "(B)",
    extensive3 = "(A)+(B)"
  ), .)

implied_e <- est_models %>%
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
      ~terms, ~intensive1, ~intensive2, ~intensive3,
      "Implied price elasticity", NA_character_, NA_character_, NA_character_,
      "", NA_character_, NA_character_, NA_character_
    ),
    by = "terms"
  ) %>%
  select(
    terms,
    intensive1:intensive3,
    extensive1:extensive2,
    extensive3 = extensive
  )

add_rows <- bind_rows(implied_e, stats_stage1)
attr(add_rows, "position") <- c(5, 6)

est_models %>%
  pull(est) %>%
  flatten() %>%
  setNames(paste0("(", seq(length(fe2sls) * 2), ")")) %>%
  modelsummary(
    title = "Price Elasticity without Announcement Effect",
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
    "Intensive-margin" = 3, "Extensive-margin" = 3
  )) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at household level.",
      "Instruments are (A) logged value of the applicable first price, and",
      "(B) the product of wage earner dummy and",
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
#' 結果の頑健性について議論する。
#' 始めに、2014年の税制改革のアナウンスメント効果と学習効果に対応する
#'
#' - 2014年の税制改革は2013年に告知されているので、
#' インセンティブの縮小を予測した納税者は2013年の寄付を増やし、2014年の寄付を減らすかもしれない。
#' - また、事前告知によってインセンティブが拡大する所得層の納税者の一部は、
#' 制度をよく理解していないので、2014年の寄付をためらうかもしれない。
#' - 価格弾力性の推定において、これらの要素はconfounderとなるので、
#' 2013年と2014年のデータを排除してFE-2SLSモデルを推定する。
#' - 表\@ref(tab:announcement-effect)より、
#' intensive-margin price elasticityは若干弾力的になった、
#' 一方で、extensive-margin price elasticityは若干非弾力的になった。
#' - ただし、標準誤差を考慮すると、係数の変化は誤差の範囲だと考えられ、
#' FE-2SLSの推定結果は制度のアナウンスメント効果や学習効果に対して頑健である。
#'
# /*
#+
rmarkdown::render(
  here("R", "10-announcement-effect.r"),
  output_dir = here("docs", "html-preview")
)
# */