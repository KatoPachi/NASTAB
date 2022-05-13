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
estdf <- readr::read_csv(here("data/shaped2_propensity.csv"), guess_max = 30000)

#'
#' ```{asis, echo = output_type() %in% c("body", "preview")}
#' ## Main Results
#' ```
#'
#' ```{asis, echo = output_type() == "slide"}
#' ## Main Results: Intensive-Margin Price Elasticity
#' ```
#'
#+ intensive, eval = output_type() != "appx"
fixest::setFixest_fml(
  ..stage2 = ~ linc_ln + sqage + hh_num + have_dependents |
    year + pid + indust + area
)

intmod <- list(
  donate_ln ~ price_ln + ..stage2,
  donate_ln ~ d_relief_donate:price_ln + ..stage2,
  donate_ln ~ psc_pool:price_ln + ..stage2,
  donate_ln ~ psc_sep:price_ln + ..stage2,
  donate_ln ~ ..stage2 | d_relief_donate:price_ln ~ employee:price_ln,
  donate_ln ~ ..stage2 | d_relief_donate:price_ln ~ psc_pool:price_ln,
  donate_ln ~ ..stage2 | d_relief_donate:price_ln ~ psc_sep:price_ln
)

est_intmod <- intmod %>%
  purrr::map(~ fixest::feols(
    xpd(.),
    data = subset(estdf, d_donate == 1),
    cluster = ~ pid
  ))

names(est_intmod) <- paste0("(", seq_len(length(est_intmod)), ")")

stage1_intmod <- est_intmod[5:7] %>%
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
    ~value.a, ~value.b, ~value.c, ~value.d,
    "", "", "", "",
    "", "", "", ""
  ), .) %>%
  select(name, value.a, value.b, value.c, value.d, value.x, value.y, value) %>%
  setNames(c("term", sprintf("(%1d)", 1:7))) %>%
  mutate(term = recode(
    term,
    "coef" = "First-stage: Instrument", .default = ""
  ))

addtab <- stage1_intmod %>%
  bind_rows(tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~ "(6)", ~ "(7)",
    "Square of age", "X", "X", "X", "X", "X", "X", "X",
    "Number of household members", "X", "X", "X", "X", "X", "X", "X",
    "Have dependents", "X", "X", "X", "X", "X", "X", "X",
    "Instrument", "", "", "", "", "WE x Price",
    "PS x Price", "PS x Price",
    "Method of PS", "", "", "Pool", "Separate", "", "Pool", "Separate"
  ))

attr(addtab, "position") <- 9:10

est_intmod %>%
  modelsummary(
    title = "Intensive-Margin Tax-Price Elasticity",
    coef_map = c(
      "price_ln" = "log(first price)",
      "d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "fit_d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "psc_pool:price_ln" =
        "PS of applying tax relief x log(first price)",
      "psc_sep:price_ln" =
        "PS of applying tax relief x log(first price)",
      "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|R2|FE",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling(font_size = 8) %>%
  kableExtra::column_spec(1, width = "10em") %>%
  kableExtra::add_header_above(c(
    " ", "FE" = 4, "FE-2SLS" = 3
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

#' ```{asis, echo = output_type() %in% c("body", "preview")}
#' 表\@ref(tab:MainIntensive)は寄付者に限定した
#' 寄付の価格弾力性（intensive-margin price elasticity）の推定結果である。
#' モデル(1)は標準的なtwo-way fixed effect modelであり、
#' 操作変数による寄付控除の自己選択を制御していない。
#' このモデルはintensive-margin price elasticityが-0.748であることを示している。
#' 言い換えれば、税インセンティブによる寄付価格が1%上昇することによって、
#' 寄付者に限定した寄付額が約0.75\%減少する。
#' モデル(2)-(6)は操作変数法による寄付控除の自己選択を制御したときの
#' intensive-margin price elasticityを示している。
#' その結果、推定方法に関わらず、intensive-margin price elasticityは約-1.5になった。
#' 言い換えれば、寄付控除の自己選択を制御したとき、
#' 1%の寄付価格の上昇によって、寄付者に限定した寄付額が1.5\%減少する。
#' したがって、操作変数法による寄付控除の自己選択を制御したとき、
#' intensive-margin price elasticityはより弾力的に推定された[^fstage1]。
#'
#' [^fstage1]: モデル(4)-(6)に示した個人固定効果と時間固定効果を含めた二段階最小二乗法（FE-2SLS）を用いて、
#' 我々は操作変数の弱相関の程度を確認できる。その結果、操作変数の種類に関わらず、F値は450以上ある。
#' したがって、操作変数法によってintensive-margin price elasticityがより弾力的になった結果は
#' 操作変数の弱相関によるものではない。
#' ```
#'
#' ```{asis, echo = output_type() == "slide"}
#' ## Main Results: Extensive-Margin Price Elasticity
#' ```
#'
#+ extensive, eval = output_type() != "appx"
extmod <- list(
  d_donate ~ price_ln + ..stage2,
  d_donate ~ d_relief_donate:price_ln + ..stage2,
  d_donate ~ psc_pool:price_ln + ..stage2,
  d_donate ~ psc_sep:price_ln + ..stage2,
  d_donate ~ ..stage2 | d_relief_donate:price_ln ~ employee:price_ln,
  d_donate ~ ..stage2 | d_relief_donate:price_ln ~ psc_pool:price_ln,
  d_donate ~ ..stage2 | d_relief_donate:price_ln ~ psc_sep:price_ln
)

est_extmod <- extmod %>%
  purrr::map(~ fixest::feols(
    xpd(.),
    data = estdf,
    cluster = ~pid
  ))

names(est_extmod) <- paste0("(", seq_len(length(est_extmod)), ")")

stage1_extmod <- est_extmod[5:7] %>%
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
    ~value.a, ~value.b, ~value.c, ~value.d,
    "", "", "", "",
    "", "", "", ""
  ), .) %>%
  select(name, value.a, value.b, value.c, value.d, value.x, value.y, value) %>%
  setNames(c("term", sprintf("(%1d)", 1:7))) %>%
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
  setNames(c("term", sprintf("(%1d)", 1:7))) %>%
  mutate(term = recode(
    term,
    "estimate" = "Implied price elasticity", .default = ""
  ))

addtab <- impelast_extmod %>%
  bind_rows(stage1_extmod) %>%
  bind_rows(tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)", ~"(7)",
    "Square of age", "X", "X", "X", "X", "X", "X", "X",
    "Number of household members", "X", "X", "X", "X", "X", "X", "X",
    "Have dependents", "X", "X", "X", "X", "X", "X", "X",
    "Instrument", "", "", "", "", "WE x Price",
    "PS x Price", "PS x Price",
    "Method of PS", "", "", "Pool", "Separate", "", "Pool", "Separate"
  ))

attr(addtab, "position") <- 9:12

est_extmod %>%
  modelsummary(
    # title = "Extensive-Margin Tax-Price Elasticity",
    coef_map = c(
      "price_ln" = "log(first price)",
      "d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "fit_d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "psc_pool:price_ln" =
        "PS of applying tax relief x log(first price)",
      "psc_sep:price_ln" =
        "PS of applying tax relief x log(first price)",
      "linc_ln" = "log(income)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|R2|FE",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling(font_size = 8) %>%
  kableExtra::column_spec(1, width = "10em") %>%
  kableExtra::add_header_above(c(
    " ",
    "FE" = 4, "FE-2SLS" = 3
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
#' ```{asis, echo = output_type() %in% c("body", "preview")}
#' 表\@ref(tab:MainExtensive)は寄付行動の有無の価格弾力性
#' （extensive-margin price elasticity）を示している。
#' モデル(1)は標準的なtwo-way fixed effect modelである。
#' このモデルの寄付価格の係数は-2.8である。
#' しかしながら、先述の通り、
#' 線型確率モデルなので、この係数を直接弾力性として解釈できない。
#' 弾力性を得るために、この係数を寄付者の比率で割る必要がある。
#' その結果、操作変数による寄付控除の自己選択を制御しない場合、
#' extensive-margin price elasticityは約-10.8となった。
#' 言い換えれば、1%の価格上昇が寄付する確率を約10%減少させる。
#'
#' 表\@ref(tab:MainIntensive)と同様に、
#' 表\@ref(tab:MainExtensive)のモデル(2)-(6)は操作変数法によって寄付控除の自己選択を制御したモデルである。
#' その結果、推定方法に関わらず、寄付価格の係数は-0.738から-0.452の間に入っている。
#' これを価格弾力性に変換すると、-1.7から-2.8のレンジで得られた。
#' 言い換えれば、寄付価格1%の上昇によって、寄付をする確率が約1.7-2.8%減少する。
#' したがって、操作変数によって寄付控除の自己選択を制御した場合、
#' extensive-margin price elasticityはより非弾力的に推定された。
#' ```
#'
#+ int-anatomy
int_anatomy_df <- estdf %>%
  dplyr::filter(d_donate == 1) %>%
  mutate(
    applicable = price_ln,
    effective = d_relief_donate * price_ln
  )

est_int_resid1 <- feols(applicable ~ ..stage2, data = int_anatomy_df)
est_int_resid2 <- feols(effective ~ ..stage2, data = int_anatomy_df)

int_anatomy_df <- int_anatomy_df %>%
  modelr::add_residuals(est_int_resid1, var = "resid1") %>%
  modelr::add_residuals(est_int_resid2, var = "resid2")

int_anatomy_df %>%
  mutate(d_relief_donate = factor(
    d_relief_donate,
    levels = c(0, 1),
    labels = c("Not", "Yes")
  )) %>%
  ggplot(aes(x = resid1, y = donate_ln, color = d_relief_donate)) +
    geom_point(size = 3, alpha = 0.5) +
    geom_smooth(se = FALSE, method = "lm", color = "black") +
    ggtemp()

lm(donate_ln ~ resid2, data = int_anatomy_df)

#'
#+ ext-anatomy
ext_anatomy_df <- estdf %>%
  mutate(
    applicable = price_ln,
    effective = d_relief_donate * price_ln
  )

est_ext_resid1 <- feols(applicable ~ ..stage2, data = ext_anatomy_df)
est_ext_resid2 <- feols(effective ~ ..stage2, data = ext_anatomy_df)

ext_anatomy_df <- ext_anatomy_df %>%
  modelr::add_residuals(est_ext_resid1, var = "resid1") %>%
  modelr::add_residuals(est_ext_resid2, var = "resid2")

ext_anatomy_df %>%
  mutate(d_relief_donate = factor(
    d_relief_donate,
    levels = c(0, 1),
    labels = c("Not", "Yes")
  )) %>%
  ggplot(aes(x = resid2, y = d_donate, color = d_relief_donate)) +
    geom_point(size = 3) +
    geom_smooth(se = FALSE, method = "lm", color = "black") +
    ggtemp()

#'
#' ```{asis, echo = output_type() != "appx"}
#' ## Robustness Check
#' ```
#'
#' ```{asis, echo = output_type() == "appx"}
#' ## Intensive-Margin Price Elasticity: Exclude Annoucement Effect
#' ```
#' 
#+ wo-annoucement-intensive, eval = output_type() == "appx"
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

attr(addtab, "position") <- 7:8

rob1_intmod %>%
  modelsummary(
    # title = paste(
    #   "Intensive-Margin Tax-Price Elasticity",
    #   "Excluding 2013 and 2014 data"
    # ),
    coef_map = c(
      "d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "fit_d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "psc_pool:price_ln" =
        "PS of applying tax relief x log(first price)",
      "psc_sep:price_ln" =
        "PS of applying tax relief x log(first price)",
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

#' ```{asis, echo = output_type() == "appx"}
#' ## Extensive-Margin Price Elasticity: Exclude Announcement Effect
#' ```
#+ wo-annoucement-extensive, eval = output_type() == "appx"
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

attr(addtab, "position") <- 7:10

rob1_extmod %>%
  modelsummary(
    # title = paste(
    #   "Extensive-Margin Tax-Price Elasticity",
    #   "Excluding 2013 and 2014 data"
    # ),
    coef_map = c(
      "d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "fit_d_relief_donate:price_ln" =
        "Applying tax relief x log(first price)",
      "psc_pool:price_ln" =
        "PS of applying tax relief x log(first price)",
      "psc_sep:price_ln" =
        "PS of applying tax relief x log(first price)",
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

#' ```{asis, echo = output_type() == "appx"}
#' ## Intensive-Margin Price Elasticity: Last-Unit Price
#' ```
#+ last-intensive, eval = output_type() == "appx"
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

attr(addtab, "position") <- 7:8

est_lastintmod %>%
  modelsummary(
    # title = "Intensive-Margin Tax-Price Elasticity (Last-Unit Price)",
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