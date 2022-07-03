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

flag <- rawdt %>%
  mutate(
    flag_extensive = 1,
    flag_intensive = if_else(d_donate == 1, 1, 0)
  ) %>%
  select(
    pid,
    year,
    flag_extensive,
    flag_intensive
  ) %>%
  pivot_longer(
    flag_extensive:flag_intensive,
    names_to = "outcome",
    values_to = "flag",
    names_prefix = "flag_"
  )

main <- rawdt %>%
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
    intensive = donate_ln,
    extensive = d_donate
  ) %>%
  pivot_longer(
    intensive:extensive,
    names_to = "outcome",
    values_to = "y"
  )

use <- flag %>%
  dplyr::left_join(main, by = c("pid", "year", "outcome")) %>%
  mutate(effective = d_relief_donate * lprice_ln) %>%
  dplyr::filter(year < 2013 | 2014 < year)

#+
fixest::setFixest_fml(
  ..stage2 = ~ linc_ln + sqage + hh_num + have_dependents |
    year + pid + indust + area
)

fe2sls <- list(
  y ~ ..stage2 | d_relief_donate:lprice_ln ~ price_ln,
  y ~ ..stage2 | d_relief_donate:lprice_ln ~ employee:price_ln,
  y ~ ..stage2 | d_relief_donate:lprice_ln ~ price_ln + employee:price_ln
)

est_models <- use %>%
  mutate(outcome = factor(outcome, levels = c("intensive", "extensive"))) %>%
  group_by(outcome) %>%
  do(est = lapply(
    fe2sls,
    function(x) feols(x, data = subset(., flag == 1), cluster = ~hhid)
  ))

stats_stage1 <- est_models %>%
  group_by(outcome) %>%
  do(tab = data.frame(
    models = paste0(.$outcome, c(1, 2, 3)),
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
    intensive1 = "(1)",
    intensive2 = "(2)",
    intensive3 = "(3)",
    extensive1 = "(4)",
    extensive2 = "(5)",
    extensive3 = "(6)"
  ), .)

est_models %>%
  pull(est) %>%
  flatten() %>%
  setNames(paste0("(", seq(length(fe2sls) * 2), ")")) %>%
  modelsummary(
    title = "Price Elasticity without Announcement Effect",
    coef_map = c(
      "fit_d_relief_donate:lprice_ln" =
        "Effective last price",
      "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = stats_stage1
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
      "A square bracket is F statistics of instrument."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

# /*
#+
rmarkdown::render(
  here("R", "10-announcement-effect.r"),
  output_dir = here("docs", "html-preview")
)
# */