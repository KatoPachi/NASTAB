#' ---
#' title: |
#'   Estimating Conventional Price Elasticity of Charitable Giving (1)
#' author: Hiroki Kato
#' bibliography: ../../Rmarkdown/reference.bib
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

lapply(Sys.glob(file.path("script/functions", "*.r")), source)

#'
#+ include = FALSE
book <- readr::read_csv("data/codebook/shaped2_description.csv"); View(book)
df <- readr::read_csv("data/shaped2.csv")
estdf <- readr::read_csv("data/shaped2_propensity.csv", guess_max = 30000)


#'
#'
#+ MainIntensive
fixest::setFixest_fml(
  ..stage2 = ~ linc_ln + sqage | year + pid + indust + area
)

intmod <- list(
  "(1)" = donate_ln ~ d_relief_donate:price_ln + ..stage2,
  "(2)" = donate_ln ~ psc_pool:price_ln + ..stage2,
  "(3)" = donate_ln ~ psc_sep:price_ln + ..stage2,
  "(4)" = donate_ln ~ ..stage2 | d_relief_donate:price_ln ~ employee:price_ln,
  "(5)" = donate_ln ~ ..stage2 | d_relief_donate:price_ln ~ psc_pool:price_ln,
  "(6)" = donate_ln ~ ..stage2 | d_relief_donate:price_ln ~ psc_sep:price_ln
)

est_intmod <- intmod %>%
  purrr::map(~ fixest::feols(
    xpd(.),
    data = subset(estdf, d_donate == 1),
    cluster = ~ pid
  ))

stage1_intmod <- est_intmod[4:6] %>%
  purrr::map(function(x) {
    coef <- x$iv_first_stage[["d_relief_donate:price_ln"]]$coeftable[1, 1]
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

addtab <- stage1_intmod %>%
  bind_rows(tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~ "(6)",
    # "Square of age", "X", "X", "X", "X", "X", "X",
    "Instrument", "", "", "", "WE x Price",
    "PS x Price", "PS x Price",
    "Method of PS", "", "Pool", "Separate", "", "Pool", "Separate"
  ))

attr(addtab, "position") <- 5:6

est_intmod %>%
  modelsummary(
    title = "Intensive-Margin Tax-Price Elasticity",
    coef_map = c(
      "d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "fit_d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "psc_pool:price_ln" =
        "PS of applying tax relief x log(first price)",
      "psc_sep:price_ln" =
        "PS of applying tax relief x log(first price)"#,
      # "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling(font_size = 7) %>%
  kableExtra::add_header_above(c(
    " ", "FE" = 3, "FE-2SLS" = 3
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
#' ## 結果: Extensive-Margin Tax-Price Elasticity
#'
#+ MainExtensive
extmod <- list(
  "(1)" = d_donate ~ d_relief_donate:price_ln + ..stage2,
  "(2)" = d_donate ~ psc_pool:price_ln + ..stage2,
  "(3)" = d_donate ~ psc_sep:price_ln + ..stage2,
  "(4)" = d_donate ~ ..stage2 | d_relief_donate:price_ln ~ employee:price_ln,
  "(5)" = d_donate ~ ..stage2 | d_relief_donate:price_ln ~ psc_pool:price_ln,
  "(6)" = d_donate ~ ..stage2 | d_relief_donate:price_ln ~ psc_sep:price_ln
)

est_extmod <- extmod %>%
  purrr::map(~ fixest::feols(
    xpd(.),
    data = estdf,
    cluster = ~pid
  ))

stage1_extmod <- est_extmod[4:6] %>%
  purrr::map(function(x) {
    coef <- x$iv_first_stage[["d_relief_donate:price_ln"]]$coeftable[1, 1]
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

impelast_extmod <- est_extmod %>%
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

addtab <- impelast_extmod %>%
  bind_rows(stage1_extmod) %>%
  bind_rows(tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
    # "Square of age", "X", "X", "X", "X", "X", "X",
    "Instrument", "", "", "", "WE x Price",
    "PS x Price", "PS x Price",
    "Method of PS", "", "Pool", "Separate", "", "Pool", "Separate"
  ))

attr(addtab, "position") <- 5:8

est_extmod %>%
  modelsummary(
    title = "Extensive-Margin Tax-Price Elasticity",
    coef_map = c(
      "d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "fit_d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "psc_pool:price_ln" =
        "PS of applying tax relief x log(first price)",
      "psc_sep:price_ln" =
        "PS of applying tax relief x log(first price)"#,
      # "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling(font_size = 7) %>%
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
#+ WoAnnoucementIntensive, include = FALSE
rob1_intmod <- intmod %>%
  purrr::map(~ fixest::feols(
    xpd(.),
    data = subset(estdf, d_donate == 1 & (year < 2013 | 2014 < year)),
    cluster = ~pid
  ))

stage1_rob1_intmod <- rob1_intmod[4:6] %>%
  purrr::map(function(x) {
    coef <- x$iv_first_stage[["d_relief_donate:price_ln"]]$coeftable[1, 1]
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

addtab <- stage1_intmod %>%
  bind_rows(tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
    # "Square of age", "X", "X", "X", "X", "X", "X",
    "Instrument", "", "", "", "WE x Price",
    "PS x Price", "PS x Price",
    "Method of PS", "", "Pool", "Separate", "", "Pool", "Separate"
  ))

attr(addtab, "position") <- 5:6

rob1_intmod %>%
  modelsummary(
    title = paste(
      "Intensive-Margin Tax-Price Elasticity",
      "Excluding 2013 and 2014 data"
    ),
    coef_map = c(
      "d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "fit_d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "psc_pool:price_ln" =
        "PS of applying tax relief x log(first price)",
      "psc_sep:price_ln" =
        "PS of applying tax relief x log(first price)"#,
      # "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling(font_size = 7) %>%
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
#+ WoAnnouncementExtensive, include = FALSE
rob1_extmod <- extmod %>%
  purrr::map(~ fixest::feols(
    xpd(.),
    data = subset(estdf, year < 2013 | 2014 < year),
    cluster = ~pid
  ))

stage1_rob1_extmod <- rob1_extmod[4:6] %>%
  purrr::map(function(x) {
    coef <- x$iv_first_stage[["d_relief_donate:price_ln"]]$coeftable[1, 1]
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

impelast_rob1_extmod <- rob1_extmod %>%
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

addtab <- impelast_rob1_extmod %>%
  bind_rows(stage1_rob1_extmod) %>%
  bind_rows(tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
    # "Square of age", "X", "X", "X", "X", "X", "X",
    "Instrument", "", "", "", "WE x Price",
    "PS x Price", "PS x Price",
    "Method of PS", "", "Pool", "Separate", "", "Pool", "Separate"
  ))

attr(addtab, "position") <- 5:8

rob1_extmod %>%
  modelsummary(
    title = paste(
      "Extensive-Margin Tax-Price Elasticity",
      "Excluding 2013 and 2014 data"
    ),
    coef_map = c(
      "d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "fit_d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "psc_pool:price_ln" =
        "PS of applying tax relief x log(first price)",
      "psc_sep:price_ln" =
        "PS of applying tax relief x log(first price)"#,
      # "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling(font_size = 7) %>%
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
#+ LastIntensive, include = FALSE
lastintmod <- list(
  "(1)" = donate_ln ~ d_relief_donate:lprice_ln + ..stage2,
  "(2)" = donate_ln ~ psc_pool:lprice_ln + ..stage2,
  "(3)" = donate_ln ~ psc_sep:lprice_ln + ..stage2,
  "(4)" = donate_ln ~ ..stage2 | d_relief_donate:lprice_ln ~ employee:price_ln,
  "(5)" = donate_ln ~ ..stage2 | d_relief_donate:lprice_ln ~ psc_pool:price_ln,
  "(6)" = donate_ln ~ ..stage2 | d_relief_donate:lprice_ln ~ psc_sep:price_ln
)

est_lastintmod <- lastintmod %>%
  purrr::map(~ fixest::feols(
    xpd(.),
    data = subset(estdf, d_donate == 1),
    cluster = ~pid
  ))

stage1_lastintmod <- est_lastintmod[4:6] %>%
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

addtab <- stage1_lastintmod %>%
  bind_rows(tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
    # "Square of age", "X", "X", "X", "X", "X", "X",
    "Instrument", "", "", "", "WE x Price",
    "PS x Price", "PS x Price",
    "Method of PS", "", "Pool", "Separate", "", "Pool", "Separate"
  ))

attr(addtab, "position") <- 5:6

est_lastintmod %>%
  modelsummary(
    title = "Intensive-Margin Tax-Price Elasticity (Last-Unit Price)",
    coef_map = c(
      "d_relief_donate:lprice_ln" =
        "Applying tax relief x log(last price)",
      "fit_d_relief_donate:lprice_ln" =
        "Applying tax relief x log(last price)",
      "psc_pool:lprice_ln" =
        "PS of applying tax relief x log(last price)",
      "psc_sep:lprice_ln" =
        "PS of applying tax relief x log(last price)"#,
      # "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling(font_size = 7) %>%
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
#+ LastExtensive, include = FALSE
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

attr(addtab, "position") <- 5:8

est_lastextmod %>%
  modelsummary(
    title = "Extensive-Margin Tax-Price Elasticity (Last-Unit Price)",
    coef_map = c(
      "d_relief_donate:lprice_ln" =
        "Applying tax relief x log(last price)",
      "fit_d_relief_donate:lprice_ln" =
        "Applying tax relief x log(last price)",
      "psc_pool:lprice_ln" =
        "PS of applying tax relief x log(last price)",
      "psc_sep:lprice_ln" =
        "PS of applying tax relief x log(last price)"#,
      # "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling(font_size = 7) %>%
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
#' ## ロバストネスチェック
#'
#' 1. 2013-2014年データを除外 (Table \@ref(tab:WoAnnoucementIntensive) and \@ref(tab:WoAnnouncementExtensive))
#'     - 税制改革のアナウンスメント効果を排除
#' 1. First-unit priceではなく、Last-unit priceで弾力性を推定 (Table \@ref(tab:LastIntensive) and \@ref(tab:LastExtensive))
#' 1. 給与所得者ダミーと寄付価格の交差項ではなく、first-unit priceを操作変数にする (Table \@ref(tab:MainElasticity)-\@ref(tab:WoAnnoucementElasticity))
#' 1. 寄付申告者に限定し、所得控除制度による内生性（e.g. 所得の変動）を考慮した分析を実施 (Table \@ref(tab:R1Elasticity) and \@ref(tab:KdiffElasticity))
#'     - 階差モデルやリードラグ変数の使用 [@Randolph1995; @Saez2002; @Scharf2020]
#'
#' ほとんどの分析で、intensive-margin tax-price elasticityは-1.5から-2の間に入り、
#' extensive-margin tax-price elasticityは-1.7から-5の間に入る
#'
#' ## 韓国での寄付の価格弾力性は先行研究より弾力的
#'
#' - 申告の自己選択を無視すると、Intensive-margin tax-price elasticityは過小推定
#'   - 寄付者の寄付額を決める観察できない要素と申告が正の相関をしている
#'   - そのような要素を寄付額を高めるならば、節税による便益が高くなるので、申告しやすくなる
#' - 申告の自己選択を無視すると、Extensive-margin tax-price elasticityは過大推定
#'   - 寄付価格が申告と寄付するかどうかの意思決定の両方に同じ方向の影響を与え、負の相関をより強くした可能性がある
#'
# /*
#+
rmarkdown::render(
  "script/elasticity/1-main.r",
  output_dir = "report/view/elasticity"
)
# */