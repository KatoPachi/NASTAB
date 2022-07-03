#' ---
#' title: |
#'   Estimating Conventional Price Elasticity of Charitable Giving (1)
#' author: Hiroki Kato
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
  dplyr::left_join(main, by = c("pid", "year", "outcome"))

#+
fixest::setFixest_fml(
  ..stage2 = ~ linc_ln + sqage + hh_num + have_dependents |
    year + pid + indust + area
)

#+
fe2sls <- list(
  y ~ ..stage2 | d_relief_donate:lprice_ln ~ price_ln,
  y ~ ..stage2 | d_relief_donate:lprice_ln ~ employee:price_ln
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
    models = paste0(.$outcome, c(1, 2)),
    coef = lapply(.$est[[1]], function(x)
      x$iv_first_stage[["d_relief_donate:lprice_ln"]]$coeftable[1, 1]
    ) %>% as_vector,
    f = lapply(.$est[[1]], function(x)
      fitstat(x, "ivf")[[1]]$stat
    ) %>% as_vector,
    wh = lapply(.$est[[1]], function(x)
      fitstat(x, "wh")$wh$p
    ) %>% as_vector
  )) %>%
  { bind_rows(.$tab) } %>%
  mutate(
    coef = sprintf("%1.3f", coef),
    f = sprintf("[%1.2f]", f),
    wh = sprintf("%1.3f", wh)
  ) %>%
  pivot_longer(coef:wh, names_to = "terms") %>%
  pivot_wider(names_from = models, values_from = value) %>%
  mutate(
    terms = recode(
      terms,
      "coef" = "First-stage: Instrument",
      "f" = "",
      "wh" = "Wu-Hausman test, p-value"
    )
  ) %>%
  bind_rows(c(
    terms = "Instrument",
    intensive1 = "First price",
    intensive2 = "First price x WE",
    extensive1 = "First price",
    extensive2 = "First price x WE"
  ), .)

est_models %>%
  pull(est) %>%
  flatten() %>%
  setNames(paste0("(", seq(length(fe2sls)*2), ")")) %>%
  modelsummary(
    title = "Tax-Price Elasticity Estimated by FE-2SLS",
    coef_map = c(
      "d_relief_donate:lprice_ln" =
        "Effective last price",
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

#'
#' *Last-unit price elasticity*.
#' First-unit priceは寄付していないときに直面する寄付価格である。
#' 意思決定者が直面する価格はfirst-unit priceではなく、last-unit priceである。
#' Last-unit priceは意思決定者の寄付額を用いて計算した寄付価格である。
#' Last-unit priceを用いて、
#' 我々は価格弾力性の推定し、その結果を補論\@ref(addtab)の
#' 表\@ref(tab:LastIntensive)と\@ref(tab:LastExtensive)に示した。
#' Last-unit priceは寄付額に対して内生的であるので、
#' 我々は表\@ref(tab:MainIntensive)と同じ操作変数法でこれに対応した。
#' その結果、first-unit priceを用いた価格弾力性と比べて、
#' intensive-margin price elasticityと
#' extensive-margin price elasticityはより弾力的に推定された。
#' とくに、extensive-margin price elasticityは2倍近く弾力的になった。
#' また、操作変数による寄付控除の自己選択の制御によって、
#' intensive-margin price elasticityがより弾力的に推定されるとともに、
#' extensive-margin price elasticityが非弾力的に推定されることも観察された。
#'
# /*
#+
rmarkdown::render(
  here("R", "3-main-estimation.r"),
  output_dir = here("docs", "html-preview")
)
# */