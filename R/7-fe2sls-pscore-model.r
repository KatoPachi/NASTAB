#+ include = FALSE
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
    applicable = price_ln,
    d_relief_donate,
    employee,
    outcome_intensive = donate_ln,
    outcome_extensive = d_donate
  ) %>%
  mutate(
    flag_extensive = 1,
    flag_intensive = if_else(outcome_extensive == 1, 1, 0)
  )

meandf <- use %>%
  dplyr::select(
    pid,
    year,
    sqage,
    hh_num,
    have_dependents,
    indust,
    area,
    tinc_ln,
    applicable,
    employee
  ) %>%
  mutate(cross = applicable * employee) %>%
  fastDummies::dummy_cols(
    select_columns = "indust",
    remove_selected_columns = TRUE
  ) %>%
  fastDummies::dummy_cols(
    select_columns = "area",
    remove_selected_columns = TRUE
  ) %>%
  select(-indust_NA) %>%
  group_by(pid) %>%
  summarize_all(list(mean = ~ sum(., na.rm = TRUE))) %>%
  select(-year_mean) %>%
  mutate_at(vars(-pid), list(~ . / length(unique(use$year))))

mundlak_use <- use %>%
  left_join(meandf, by = "pid")

#' //NOTE: Estimate application models with LPM and Probit
#' //RUN: We use Chamberlain-Mundlak device for individual fixed effect
#+
fixest::setFixest_fml(
  ..mundlak = as.formula(
    paste("~", paste(names(meandf)[-1], collapse = " + "))
  ),
  ..stage2 = ~ tinc_ln + sqage + hh_num + have_dependents + employee +
    factor(indust) + factor(area) + factor(year)
)

#+
psmod <- d_relief_donate ~ applicable + applicable:employee +
  ..mundlak + ..stage2

est_psmod <- list(
  feols(psmod, data = mundlak_use, cluster = ~hhid),
  feglm(
    psmod, data = mundlak_use,
    cluster = ~hhid, family = binomial("probit")
  )
)

est_psmod %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    coef_map = c(
      "applicable:employee" = "Applicable price $\\times$ Wage earner",
      "applicable" = "Applicable price",
      "employee" = "Wage earner",
      "tinc_ln" = "Log income"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1)
  ) %>%
  kable_styling() %>%
  group_rows("Excluded instruments", 1, 2, italic = TRUE, bold = FALSE) %>%
  group_rows("Covariates", 3, 8, italic = TRUE, bold = FALSE) %>%
  add_header_above(c(" " = 1, "LPM" = 1, "Probit" = 1)) %>%
  add_header_above(c(" " = 1, "Dummy of application" = 2))

#' //NOTE: Estimate CF models
#+
cf_use <- mundlak_use %>%
  modelr::add_residuals(est_psmod[[1]]) %>%
  modelr::add_predictions(est_psmod[[2]], type = "link") %>%
  mutate(
    imr = dnorm(pred) / pnorm(pred),
    inv_imr = dnorm(-pred) / pnorm(-pred),
    gr = d_relief_donate * imr - (1 - d_relief_donate) * inv_imr
  ) %>%
  pivot_longer(
    outcome_intensive:flag_intensive,
    names_to = c(".value", "type"),
    names_pattern = "(.*)_(.*)"
  )

cfmod <- list(
  outcome ~ applicable:d_relief_donate + resid +
    ..mundlak + ..stage2,
  outcome ~ applicable:d_relief_donate + resid + resid:d_relief_donate +
    ..mundlak + ..stage2
)

est_cfmod <- cf_use %>%
  mutate(type = factor(type, levels = c("intensive", "extensive"))) %>%
  group_by(type) %>%
  do(est = lapply(
    cfmod,
    function(x) feols(x, data = subset(., flag == 1), cluster = ~hhid)
  ))

#' //NOTE: Create regression tables of CF models
#+
mu <- with(subset(cf_use, type == "extensive"), mean(outcome))

implied_e <- subset(est_cfmod, type == "extensive")$est[[1]] %>%
  purrr::map(function(x) {
    res <- subset(tidy(x), str_detect(term, "applicable:d_relief_donate")) %>%
      mutate(
        estimate = estimate / mu,
        estimate = case_when(
          p.value < 0.01 ~ sprintf("\\num{%1.3f}***", estimate),
          p.value < 0.05 ~ sprintf("\\num{%1.3f}**", estimate),
          p.value < 0.1  ~ sprintf("\\num{%1.3f}*", estimate),
          TRUE           ~ sprintf("\\num{%1.3f}", estimate)
        ),
        std.error = sprintf("(\\num{%1.3f})", std.error / mu)
      )

    tribble(
      ~term, ~mod,
      "Estimates", res$estimate,
      "Estimates se", res$std.error
    )
  }) %>%
  reduce(left_join, by = "term") %>%
  setNames(c("term", paste0("mod", seq(length(cfmod)))))

add_table <- implied_e %>%
  left_join(
    tibble(
      term = c("Estimates", "Estimates se"),
      imod1 = rep("", 2),
      imod2 = rep("", 2)
    ),
    .,
    by = "term"
  ) %>%
  mutate(term = dplyr::recode(term, "Estimates se" = "", .default = term))

attr(add_table, "position") <- c(9, 10)

est_cfmod$est %>%
  flatten() %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = "Estimation Results of Control Function Model\\label{tab:cf}",
    coef_map = c(
      "applicable:d_relief_donate" = "Effective price",
      "tinc_ln" = "Log income",
      "resid" = "Residuals of Application",
      "d_relief_donate:resid" =
        "Application $\\times$ Residuals of Application"
    ),
    gof_omit = "^(?!R2 Adj.|Num)",
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    add_rows = add_table
  ) %>%
  kable_styling() %>%
  add_header_above(
    c(" " = 1, "Intensive-margin" = 2, "Extensive-margin" = 2)
  ) %>%
  group_rows(
    "Implied price elasticity", 9, 10, italic = TRUE, bold = FALSE
  ) %>%
  column_spec(2:5, width = "7.5em") %>%
  footnote(
    general_title = "",
    general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. We use standard errors clustered at household level. An outcome variable is logged value of amount of charitable giving in models (1) and (2) and a dummy indicating that donor in models (3) and (4). For estimation, we use only donors (intensive-margin sample) in models (1) and (2) and both donors and non-donors (extensive-margin sample) in models (3) and (4). We control squared age (divided by 100), number of household members, a dummy that indicates having dependents, an employee dummy, a set of dummies of industry, a set of dummies of residential area, and time fixed effects. We use $\\\\{Appricable price}\\\\times\\\\{Wave earner}$ as an instrument to obtaine residuals of application. Instead individual fixed effects, we control a vector of individual-level sample mean of all exogenous variables including instruments (Chamberlain-Mundlak device).",
    threeparttable = TRUE,
    escape = FALSE
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

out.file <- file(here("tables", "psiv-stage2.tex"), open = "w")

tab <- est_sep_ps %>%
  pull(est) %>%
  flatten() %>%
  setNames(paste0("(", seq(length(sep_ps) * 2), ")")) %>%
  modelsummary(
    title = paste(
      "Second-Stage Results of FE-2SLS with Propensity Score",
      "\\label{tab:psiv-stage2}"
    ),
    coef_map = c(
      "fit_effective" = "Effective last price",
      "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = add_rows,
    output = "latex"
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

writeLines(tab, out.file)
close(out.file)

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

out.file <- file(here("tables", "psiv-pool-stage2.tex"), open = "w")

tab <- est_pool_ps %>%
  pull(est) %>%
  flatten() %>%
  setNames(paste0("(", seq(length(pool_ps) * 2), ")")) %>%
  modelsummary(
    title = paste(
      "Second-Stage Results of FE-2SLS with Propensity Score (Pooled)",
      "\\label{tab:psiv-pool-stage2}"
    ),
    coef_map = c(
      "fit_effective" = "Effective last price",
      "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = add_rows,
    output = "latex"
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

writeLines(tab, out.file)
close(out.file)

#'
#' - また、全期間のサンプルで推定したプロビットモデルの傾向スコアを用いても、
#' 同じ結果が得られる
#' （第一段階の結果は表\@ref(tab:psiv-pool-stage1)に示し、
#' 第二段階の結果は表\@ref(tab:psiv-pool-stage2)に示した）
