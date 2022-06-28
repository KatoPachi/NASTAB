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

models <- list(
  y ~ ..stage2 | d_relief_donate:lprice_ln ~ price_ln,
  y ~ ..stage2 | d_relief_donate:lprice_ln ~ employee:price_ln
)

#+
est_models <- use %>%
  mutate(outcome = factor(outcome, levels = c("intensive", "extensive"))) %>%
  group_by(outcome) %>%
  do(est = lapply(
    models,
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
  setNames(paste0("(", seq(length(models)*2), ")")) %>%
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

#' ```{asis, echo = output_type() == "appx"}
#' ## Extensive-Margin Price Elasticity: Last-Unit Price
#' ```
#+ last-extensive, eval = output_type() == "appx"
lastextmod <- list(
  "(1)" = d_donate ~ d_relief_donate:lprice_ln + ..stage2,
  "(2)" = d_donate ~ psc_pool:lprice_ln + ..stage2,
  "(3)" = d_donate ~ psc_sep:lprice_ln + ..stage2,
  "(4)" = d_donate ~ ..stage2 | d_relief_donate:lprice_ln ~ employee:price_ln,
  "(5)" = d_donate ~ ..stage2 | d_relief_donate:lprice_ln ~ psc_pool:price_ln,
  "(6)" = d_donate ~ ..stage2 | d_relief_donate:lprice_ln ~ psc_sep:price_ln
)

est_lastextmod <- lastextmod %>%
  purrr::map(~ fixest::feols(
    xpd(.),
    data = estdf,
    cluster = ~pid
  ))

stage1_lastextmod <- est_lastextmod[4:6] %>%
  purrr::map(function(x) {
    coef <- x$iv_first_stage[["d_relief_donate:lprice_ln"]]$coeftable[1, 1]
    ivwald <- fitstat(x, "ivwald")[[1]]$stat

    tibble(coef = coef, wald = ivwald) %>%
      pivot_longer(everything()) %>%
      mutate(value = case_when(
        name == "coef" ~ sprintf("%1.3f", value),
        name == "wald" ~ sprintf("[%1.1f]", value)
      ))
  }) %>%
  reduce(left_join, by = "name") %>%
  bind_cols(tribble(
    ~value.a, ~value.b, ~value.c,
    "", "", "",
    "", "", ""
  ), .) %>%
  select(name, value.a, value.b, value.c, value.x, value.y, value) %>%
  setNames(c("term", sprintf("(%1d)", 1:6))) %>%
  mutate(term = recode(
    term,
    "coef" = "First-stage: Instrument", .default = ""
  ))

impelast_lastextmod <- est_lastextmod %>%
  purrr::map(function(x) {
    dbar <- mean(x$fitted.values + x$residuals)

    tidy(x) %>%
      filter(str_detect(term, "price")) %>%
      mutate(
        estimate = case_when(
          p.value <= .01 ~ sprintf("%1.3f***", estimate / dbar),
          p.value <= .05 ~ sprintf("%1.3f**", estimate / dbar),
          p.value <= .1 ~ sprintf("%1.3f*", estimate / dbar),
          TRUE ~ sprintf("%1.3f", estimate / dbar),
        ),
        std.error = sprintf("(%1.3f)", std.error / dbar)
      ) %>%
      select(estimate, std.error) %>%
      pivot_longer(everything())
  }) %>%
  reduce(left_join, by = "name") %>%
  setNames(c("term", sprintf("(%1d)", 1:6))) %>%
  mutate(term = recode(
    term,
    "estimate" = "Implied price elasticity", .default = ""
  ))

addtab <- impelast_lastextmod %>%
  bind_rows(stage1_lastextmod) %>%
  bind_rows(tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
    # "Square of age", "X", "X", "X", "X", "X", "X",
    "Instrument", "", "", "", "WE x Price",
    "PS x Price", "PS x Price",
    "Method of PS", "", "Pool", "Separate", "", "Pool", "Separate"
  ))

attr(addtab, "position") <- 7:10

est_lastextmod %>%
  modelsummary(
    # title = "Extensive-Margin Tax-Price Elasticity (Last-Unit Price)",
    coef_map = c(
      "d_relief_donate:lprice_ln" =
        "Applying tax relief x log(last price)",
      "fit_d_relief_donate:lprice_ln" =
        "Applying tax relief x log(last price)",
      "psc_pool:lprice_ln" =
        "PS of applying tax relief x log(last price)",
      "psc_sep:lprice_ln" =
        "PS of applying tax relief x log(last price)",
      "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling(font_size = 8, latex_options = "hold_position") %>%
  kableExtra::column_spec(1, width = "10em") %>%
  kableExtra::add_header_above(c(
    " ",
    "FE" = 3, "FE-2SLS" = 3
  )) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at individual level.",
      "A square bracket is wald statistics of instrument."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#'
#' ```{asis, echo = output_type() %in% c("paper", "preview")}
#' 結果を解釈する前に、ここまでの推定結果が頑健に観察されることを確認する。
#'
#' \noindent
#' *Exclude Annoucement Effect*.
#' 第\@ref(nastab)節の図\@ref(fig:SummaryGivingOverall)で示したように、
#' 2014年の税制改革による税インセンティブの変化に関わらず、寄付額が減少している。
#' この一つの可能性として、税制改革が事前告知されたことによる異時点間の代替性が生じているかもしれない。
#' これを排除して弾力性を推定するために、
#' 我々は事前のアナウンスメントによる異時点間の代替性が2013年と2014年のみで生じていることを仮定して、
#' 2013年と2014年のデータを除いて弾力性を推定した。
#' 推定結果を補論\@ref(addtab)の
#' 表\@ref(tab:WoAnnoucementIntensive)と\@ref(tab:WoAnnouncementExtensive)に示した。
#' その結果、推定値は大きく変化しなかった。
#' また、操作変数による寄付控除の自己選択の制御によって、
#' intensive-margin price elasticityがより弾力的に推定されるとともに、
#' extensive-margin price elasticityが非弾力的に推定されることも観察された。
#'
#' \noindent
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
#' ```
#'
#' ```{asis, echo = output_type() == "slide"}
#' - *Exclude Annoucement Effect*
#'   - intertemporal substitution due to annoucement of 2014 tax reform
#'   - We drop observations in 2013 and 2014
#'   - Estimated elasticity does not change significantly
#' - *Last-unit Price Elasticity*
#'   - The acutual price that decision-makers face is *last-unit*
#'     (especially, intensive-margin decision)
#'   - We use first-unit price as an instrument of last-unit one
#'   - Last-unit price elasticity is more elastic
#'   than first-unit price elasticity
#' ```
#'
# /*
#+
rmarkdown::render(
  here("R", "3-main-estimation.r"),
  output_dir = here("docs", "html-preview")
)
# */