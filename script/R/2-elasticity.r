#' ---
#' title: |
#'   Price Elasticity of Charitable Giving
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
) %>%
dplyr::filter(
  ext_benefit_tl == 0 | (ext_benefit_tl == 1 & i_ext_giving == 1)
)

#'
#' # Conceptual Framework
#' ## Optimization Problem
#'
#' Following @Almunia2020, we formulate the optimaization problem as follows:
#'
#' \begin{align*}
#'   \max_{x_{it}, g_{it}, R_{it}} &U(x_{it}, g_{it}, G_t)
#'   = u_i(x_{it}, g_{it}, G_{it}) - R_{it}K_{it}, \\
#'   \text{s.t.}\:\:
#'   &x_{it} + g_{it} = y_{it} - T_{it}(y_{it}, R_{it} g_{it}), \\
#'   &G_t = g_{it} + G_{-it}, \\
#'   &T_{it} = \begin{cases}
#'     \tau_t(y_{it} - R_{it} g_{it}) (y_{it} - R_{it} g_{it}) &t < 2014 \\
#'     \tau_t(y_{it}) y_{it} - m R_{it} g_{it} &t \ge 2014
#'   \end{cases}
#' \end{align*}
#'
#' We assume that
#'
#' - no saving
#' - decision-making before 2014 is based on $\tau(y_{it})$ (first-price)
#' - $G_{-it}$ is large enough to $\frac{\partial u_i}{\partial G}(x, g, G) \approx 0$
#'
#' By these assumptions,
#' we can reformulate the optimization problem as follows:
#'
#' \begin{align*}
#'   \max_{g_{it}, R_{it}}&
#'   u_i((1 - \tau_t(y_{it}))y_{it} - (1 - R_{it}s_{it})g_{it}, g_{it}, G_{it})
#'   - R_{it}K_{it}, \\
#'   \text{s.t.}\:\:
#'   &s_{it} = \begin{cases}
#'     \tau(y_{it}) &t < 2014 \\
#'     m &t \ge 2014
#'   \end{cases},
#' \end{align*}
#'
#' where $s_{it}$ is tax incentive of monetary donations
#' for individual $i$ in year $t$.
#'
#' Define $g_i(1 - s_{it}, (1 - \tau_t(y_{it}))y_{it})$
#' and $g_i(1, (1 - \tau_t(y_{it}))y_{it})$
#' to be the optimal levels of donations
#' for choices $R_{it} = 1, 0$ respectively.
#' Then, we can write indirect utility as
#'
#' \begin{align*}
#'   &v_i(1 - s_{it}, (1 - \tau_t(y_{it}))y_{it}, G_{-it}) - K_{it},  \\
#'   &v_i(1, (1 - \tau_t(y_{it}))y_{it}, G_{-it}).
#' \end{align*}
#'
#' Thus, individual $i$ applies for tax relief in year $t$,
#' that is, $R_{it} = 1$ iff
#'
#' \begin{align*}
#'   \Delta v_{it} \equiv
#'   v_i(1 - s_{it}, (1 - \tau_t(y_{it}))y_{it}, G_{-it})
#'   - v_i(1, (1 - \tau_t(y_{it}))y_{it}, G_{-it})
#'   \ge K_{it}.
#' \end{align*}
#'
#' # Price Elasticity of Charitable Giving
#'
#' Estimation equations are
#'
#' \begin{align*}
#'   \text{Second-stage: }&
#'   \ln g_{it} = \theta_i + \gamma \ln(1 - R_{it}s_{it}) +
#'   \beta X'_{it} + \iota_t + u_{it} \\
#'   \text{First-stage: }&
#'   \ln (1 - R_{it} s_{it}) = \theta_i + \delta_1 \ln (1 - s_{it}) +
#'   \delta_2 X'_{it} + \iota_t + v_{it}
#' \end{align*}
#'
#' To justify monotonicity assumption,
#' consider a decrease of tax incentive $s_{it}$, that is, $s > s'$
#' ($1 - s < 1 - s'$).
#' Then, we can classify tax-payers into three types:
#'
#' - Type A (Always-complier): $R_{it} = 1$ under both $1 - s$ and $1 - s'$
#' - Type B (Defier): $R_{it} = 1$ if $1 - s$ and $R_{it} = 0$ if $1 - s'$
#' - Type C (Always-noncomplier): $R_{it} = 0$ under both $1 - s$ and $1 - s'$
#'
#' Our model excludes
#' those who $R_{it} = 0$ if $1 - s$ and $R_{it} = 1$ if $1 - s'$
#' because a decrease of tax incentive also reduces benefit from tax relief:
#' $\Delta v_{it}$.[^fix_K]
#' Then, a price change by a decrease of tax incentive
#' can be summarized as follows:
#'
#' - Type A: $1 - s \to 1 - s'$
#' - Type B: $1 - s \to 1$
#' - Type C: $1 \to 1$
#'
#' Thus, instrument $\ln(1 - s_{it})$ changes
#' our main endogenous variable $\ln(1 - R_{it}s_{it})$ in one directly,
#' which implies that monotonicity assumption is justified.
#' Moreover, we expect $1 > \delta_1 > 0$.
#'
#' [^fix_K]: We fix $K_{it} = k$.
#'
#+
fixest::setFixest_fml(
  ..cov = ~ log_pinc_all + sqage | year + panelid + area + industry
)

firstmod <- list(
  "(1)" = list(
    mod = log_total_g ~ log_price:ext_benefit_tl + ..cov,
    data = df
  ),
  "(2)" = list(
    mod = log_total_g ~ ..cov | log_price:ext_benefit_tl ~ log_price,
    data = df
  ),
  "(3)" = list(
    mod = log_total_g ~ log_price:ext_benefit_tl + ..cov,
    data = subset(df, i_ext_giving == 1)
  ),
  "(4)" = list(
    mod = log_total_g ~ ..cov | log_price:ext_benefit_tl ~ log_price,
    data = subset(df, i_ext_giving == 1)
  ),
  "(5)" = list(
    mod = i_ext_giving ~ log_price:ext_benefit_tl + ..cov,
    data = df
  ),
  "(6)" = list(
    mod = i_ext_giving ~ ..cov | log_price:ext_benefit_tl ~ log_price,
    data = df
  )
)

est_firstmod <- firstmod %>%
  purrr::map(~ fixest::feols(.$mod, .$data, cluster = ~panelid))

impelast <- est_firstmod[5:6] %>%
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
  setNames(c("term", sprintf("(%1d)", 5:6))) %>%
  left_join(tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)",
    "estimate", "", "", "", "",
    "std.error", "", "", "", "",
  ), ., by = "term") %>%
  mutate(term = recode(
    term, "estimate" = "Implied price elasticity", .default = ""
  ))

stage1 <- est_firstmod[c(2, 4, 6)] %>%
  purrr::map(function(x) {
    coef <- x$iv_first_stage[["log_price:ext_benefit_tl"]]$coeftable[1, 1]
    ivwald <- fitstat(x, "ivwald")[[1]]$stat

    tibble(coef = coef, wald = ivwald) %>%
      pivot_longer(everything()) %>%
      mutate(value = case_when(
        name == "coef" ~ sprintf("%1.3f", value),
        name == "wald" ~ sprintf("[%1.1f]", value)
      )) %>%
      mutate(value2 = c("", "")) %>%
      select(name, value2, value)
  }) %>%
  reduce(left_join, by = "name") %>%
  setNames(c("term", sprintf("(%1d)", 1:6))) %>%
  mutate(term = recode(
    term, "coef" = "First-stage: Instrument", .default = ""
  ))

addtab <- impelast %>%
  bind_rows(stage1) %>%
  bind_rows(tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
    "Square of age", "X", "X", "X", "X", "X", "X"
  ))

attr(addtab, "position") <- c(3:6)

est_firstmod %>%
  modelsummary(
    title = "First-Price Elasticities",
    coef_map = c(
      "fit_log_price:ext_benefit_tl" = "log(first price)",
      "log_price:ext_benefit_tl" = "log(first price)"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std",
    stars = c("***" = .01, "**" = .05, "*" = .1),
    add_rows = addtab
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    " " = 1, "FE" = 1, "FE-2SLS" = 1,
    "FE" = 1, "FE-2SLS" = 1, "FE" = 1, "FE-2SLS" = 1
  )) %>%
  kableExtra::add_header_above(c(
    " " = 1, "Overall" = 2, "Intensive" = 2, "Extensive" = 2
  )) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: $^{*}$ $p < 0.1$, $^{**}$ $p < 0.05$, $^{***}$ $p < 0.01$.",
      "Standard errors are clustered at individual level.",
      "A square bracket is wald statistics of instrument.",
      "log (first price) is $\\ln(1 - R_{it}s_{it})$.",
      "Instrument is $\\ln(1 - s_{it})$."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

# /*
#+
rmarkdown::render(
  "script/R/2-elasticity.r",
  output_dir = "report/view"
)
# */