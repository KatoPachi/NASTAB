#+
library(here)
source(here("R", "_library.r"))

#+ include = FALSE
use <- readr::read_csv(here("data/shaped2.csv")) %>%
  dplyr::filter(year < 2018) %>%
  dplyr::filter(dependents == 0) %>%
  dplyr::filter(tinc > donate) %>%
  select(
    pid,
    hhid,
    year,
    tinc_ln,
    sqage,
    hh_num,
    have_dependents,
    indust,
    area,
    credit_benefit,
    credit_loss,
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
    applicable = price_ln,
    after = if_else(year >= 2014, 1, 0)
  )

#' //NOTE: Relationship b/w Applicable and Effective
#+ plot-stage1, fig.cap = "Relationship between Applicable First Price and Last Price by Employment Status. Note: The bubble size indicates sample size. Due to the small sample size, the leftmost bubbles for salaried and self-employed workers are less informative ($N=6$ for wage earner and $N=2$ for self-employed).", out.extra = ""
plot_stage1 <- use %>%
  dplyr::filter(
    !is.na(d_relief_donate) & !is.na(lprice_ln) & !is.na(employee)
  ) %>%
  mutate(
    employee = factor(employee, label = c("Self-employed", "Wage earner"))
  ) %>%
  group_by(applicable, employee) %>%
  summarize(
    n = n(),
    d_relief_donate = mean(d_relief_donate),
    effective = mean(effective),
    applicable.last = mean(lprice_ln)
  ) %>%
  pivot_longer(effective:applicable.last, names_to = "type") %>%
  mutate(type = factor(
    type,
    labels = c("Applicable last price", "Effective last price")
  )) %>%
  ggplot(aes(x = applicable, y = value)) +
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  geom_point(aes(size = n, color = employee), alpha = 0.8) +
  scale_color_grey() +
  scale_size(range = c(5, 20)) +
  facet_wrap(~ type) +
  labs(
    x = "log(first price)",
    y = "Sample average",
    color = ""
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 5)),
    size = "none"
  ) +
  ggtemp()

plot_stage1

ggsave(
  here("figures", "plot-stage1.pdf"),
  plot = plot_stage1,
  width = 10,
  height = 6
)

#' //NOTE: Estimate first-stage models
#+ reg-stage1, eval = FALSE
fixest::setFixest_fml(
  ..stage2 = ~ tinc_ln + sqage + hh_num + have_dependents |
    year + pid + indust + area
)

stage1 <- list(
  effective ~ credit_benefit:after + credit_loss:after + ..stage2,
  effective ~ employee + credit_benefit:after + credit_loss:after +
    employee:credit_benefit + employee:credit_loss +
    credit_benefit:employee:after + credit_loss:employee:after + ..stage2,
  effective ~ applicable + ..stage2,
  effective ~ employee + applicable + applicable:employee + ..stage2
)

est_stage1 <- use %>%
  mutate(type = factor(type, levels = c("intensive", "extensive"))) %>%
  group_by(type) %>%
  do(est = lapply(
    stage1,
    function(x) feols(x, data = subset(., flag == 1), cluster = ~hhid)
  ))

#' //NOTE: Create regression table of first-stage model
#+
out.file <- file(here("export", "tables", "fe2sls-stage1.tex"), open = "w")

tab <- est_stage1 %>%
  pull(est) %>%
  flatten() %>%
  setNames(paste0("(", seq(length(stage1) * 2), ")")) %>%
  modelsummary(
    title = "Regression Results of First-Stage Model\\label{tab:fe2sls-stage1}",
    coef_map = c(
      "credit_benefit:after" = "Decrease x Credit period",
      "after:credit_loss" = "Increase x Credit period",
      "applicable" = "Log applicable price",
      "effective" = "Log effective last-price",
      "employee" = "Wage earner",
      "employee:credit_benefit" = "Wage earner x Decrease",
      "employee:credit_loss" = "Wage earner x Increase",
      "employee:credit_benefit:after" =
        "Wage earner x Decrease x Credit period",
      "employee:after:credit_loss" =
        "Wage earner x Increase x Credit period",
      "employee:applicable" = "Wage earner x Log applicable price",
      "tinc_ln" = "Log income"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    output = "latex"
  ) %>%
  kableExtra::kable_styling(font_size = 8) %>%
  kableExtra::add_header_above(c(
    "Sample:" = 1,
    "Intensive-margin" = 4, "Extensive-margin" = 4
  )) %>%
  kableExtra::add_header_above(c(
    " " = 1, "Log effective last-price" = 8
  )) %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. We use standard errors clustered at household level. An outcome variable is logged value of the effective last price. For estimation, models (1)--(4) use those whose amount of donation is positive (intensive-margin sample), and models (5)--(8) use not only donors but also non-donors (extensive-margin sample).  We control squared age (divided by 100), number of household members, a dummy that indicates having dependents, a set of dummies of industry a set of dummies of residential area, and individual and time fixed effects.",
    threeparttable = TRUE,
    escape = FALSE
  ) %>%
  kableExtra::landscape()

writeLines(tab, out.file)
close(out.file)

#'
#' <!---
#' 給与所得者が相対的に控除を申請しやすいのであれば、
#' 給与所得者に限定したeffective last priceとapplicable first priceの相関は
#' 自営業主のそれよりも強くなるはずである。
#'
#' - 表\@ref(tab:stage1)はFE-2SLSの第一段階の推定結果を示している。
#' - 寄付者に限定して推定する（モデル(3)）と、給与所得者以外のeffective last priceと
#' applicable first priceの相関は0.679であり、統計的に有意である。
#' また、給与所得者のeffective last priceとapplicable first priceの相関は
#' 0.707($=0.697 + 0.028$)であり、給与所得者以外のそれよりも若干大きい。
#' これは我々の予想と整合的であるが、給与所得者とそれ以外の相関の差は統計的に非有意である。
#' - 非寄付者も含めて推定する（モデル(6)）と、給与所得者以外のeffective last priceと
#' applicable first priceの相関は0.328であり、統計的に有意である。
#' また、給与所得者のeffective last priceとapplicable first priceの相関は
#' 0.369($=0.328 + 0.0041$)である。
#' 二つの相関の差は我々の予想と整合的であり、統計的に有意である。
#' --->
#' 
#'
#+ fe2sls
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
    f = lapply(.$est[[1]], function(x)
      fitstat(x, "ivf")[[1]]$stat
    ) %>% as_vector,
    wh = lapply(.$est[[1]], function(x)
      fitstat(x, "wh")$wh$p
    ) %>% as_vector
  )) %>%
  { bind_rows(.$tab) } %>%
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

out.file <- file(here("tables", "fe2sls.tex"), open = "w")

tab <- est_models %>%
  pull(est) %>%
  flatten() %>%
  setNames(paste0("(", seq(length(fe2sls)*2), ")")) %>%
  modelsummary(
    title = "Tax-Price Elasticity Estimated by FE-2SLS \\label{tab:fe2sls}",
    coef_map = c(
      "fit_effective" = "log(Effective last price)",
      "tinc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = add_rows
    # output = "latex"
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

writeLines(tab, out.file)
close(out.file)

#'
#' 表\@ref(tab:fe2sls)はFE-2SLSの第二段階の推定結果を示している。
#'
#' -  操作変数は(1)first applicable price、(2)first applicable priceと
#' 給与所得者ダミーの積、(3) (1)と(2)の両方を用いる。
#' - 寄付者に限定すると、寄付額に対するeffective last priceの価格弾力性は
#' -1.9から-1.7の範囲で得られた。
#' これは通常の固定効果モデルの価格弾力性よりも3倍弾力的であり、
#' Wu-Hausman検定より統計的に有意な差である。
#' また、F値が500以上あるので、この結果の差は操作変数の弱相関による問題ではない。
#' - 寄付行動をアウトカムとするとき、effective last priceの係数は-1.5から-1の範囲で得られた。
#' 価格弾力性に変換するために、この係数を寄付者比率で割ると、
#' 価格弾力性は-6.25から-4.17の範囲となる。
#' これは通常の固定効果モデルの価格弾力性の半分以下とであり、
#' Wu-Hausman検定より統計的に有意な差である。
#' また、F値が1000以上あるので、この結果の差は操作変数の弱相関による問題ではない。
#'
#+
did <- outcome ~ ..stage2 |
  effective ~ credit_benefit:after +
  employee:credit_benefit:after +
  employee:after

use %>%
  # dplyr::filter(year != 2014 & year != 2013) %>%
  group_by(type) %>%
  nest() %>%
  mutate(est = map(data, ~ feols(
    did, data = subset(., flag == 1), cluster = ~hhid
  ))) %>%
  pull(est)

with(use, table(price))
