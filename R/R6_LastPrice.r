library(here)
library(R6)
library(tidyverse)
library(rlang)
library(patchwork)
library(modelsummary)
library(kableExtra)
source(here("R/misc.r"))

LastPrice <- R6::R6Class("LastPrice",
  public = list(
    result = list(),
    data = NULL,
    initialize = function(data) {
      self$data <- data %>%
        rename(
          outcome_intensive = donate_ln,
          outcome_extensive = d_donate,
          applicable = price_ln,
          applicable_last = lprice_ln
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
          effective = d_relief_donate * applicable,
          effective_last = d_relief_donate * applicable_last
        ) %>%
        dplyr::filter(flag == 1)

      setFixest_fml(
        ..stage2 = ~ tinc_ln + sqage + hh_num + have_dependents + employee +
          factor(indust) + factor(area) | pid + year
      )
    },
    fit = function() {
      self$result <- private$component_regtab(self$data)
      invisible(self)
    },
    intensive = function() {
      est <- self$result

      est$fit %>%
        dplyr::filter(type == "intensive") %>%
        pull(fit) %>%
        setNames(paste0("(", seq(length(.)), ")")) %>%
        modelsummary(
          title = "Estimation Results of Intensive-Margin Last-Price Elasticities\\label{tab:last-int}",
          coef_map = c(
            "applicable_last" = "Applicable last-price",
            "fit_applicable_last" = "Applicable last-price",
            "effective_last" = "Effective last-price",
            "fit_effective_last" = "Effective last-price",
            "tinc_ln" = "Log income"
          ),
          gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2|RMSE",
          stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
          add_rows = est$stats %>% select(name, starts_with("intensive")),
          output = "latex",
          escape = FALSE
        ) %>%
        kable_styling(font_size = 8) %>%
        add_header_above(c(" " = 1, "FE" = 2, "FE-2SLS" = 2)) %>%
        add_header_above(c(" " = 1, "Log donation" = 4)) %>%
        group_rows(
          "1st stage information (Excluded instrument: Applicable price)",
          7, 8,
          bold = FALSE, italic = TRUE
        ) %>%
        column_spec(2:5, width = "6.25em") %>%
        footnote(
          general_title = "",
          general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. Standard errors clustered at household level are in parenthesis. An outcome variable is logged value of amount of charitable giving. For estimation, we use donors only (intensive-margin sample). For outcome equation, we control squared age (divided by 100), number of household members, a dummy that indicates having dependents, employee dummy, a set of dummies of industry a set of dummies of residential area, and individual and time fixed effects. For FE-2SLS, we use a logged applicable price as an instrument.",
          threeparttable = TRUE,
          escape = FALSE
        )
    },
    extensive = function() {
      est <- self$result

      est$fit %>%
        dplyr::filter(type == "extensive") %>%
        pull(fit) %>%
        setNames(paste0("(", seq(length(.)), ")")) %>%
        modelsummary(
          title = "Estimation Results of Extensive-Margin Last-Price Elasticities\\label{tab:last-ext}",
          coef_map = c(
            "applicable_last" = "Applicable last-price",
            "fit_applicable_last" = "Applicable last-price",
            "effective_last" = "Effective last-price",
            "fit_effective_last" = "Effective last-price",
            "tinc_ln" = "Log income"
          ),
          gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2|RMSE",
          stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
          add_rows = est$stats %>% select(name, starts_with("extensive")),
          output = "latex",
          escape = FALSE
        ) %>%
        kable_styling(font_size = 8) %>%
        add_header_above(c(" " = 1, "FE" = 2, "FE-2SLS" = 2)) %>%
        add_header_above(c(" " = 1, "A dummy of donor" = 4)) %>%
        group_rows("Implied price elasticity", 7, 8, italic = TRUE, bold = FALSE) %>%
        group_rows(
          "1st stage information (Excluded instrument: Applicable price)",
          9, 10,
          italic = TRUE, bold = FALSE
        ) %>%
        column_spec(2:5, width = "6.25em") %>%
        footnote(
          general_title = "",
          general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. Standard errors clustered at household level are in parentheses. An outcome variable is a dummy indicating that donor. For estimation, we use not only donors but also non-donors (extensive-margin sample). For outcome equation, we control squared age (divided by 100), number of household members, a dummy that indicates having dependents, a employee dummy, a set of dummies of industry, a set of dummies of residential area, and individual and time fixed effects. For FE-2SLS, we use a logged applicable price as an instrument. We calculate implied price elasticities by dividing estimated coeffcient on price by sample proportion of donors.",
          threeparttable = TRUE,
          escape = FALSE
        )
    }
  ),
  private = list(
    fe2sls_mod = list(
      outcome ~ applicable_last + ..stage2,
      outcome ~ effective_last + ..stage2,
      outcome ~ ..stage2 | applicable_last ~ applicable,
      outcome ~ ..stage2 | effective_last ~ applicable
    ),
    component_regtab = function(data) {
      fit <- data %>%
        group_by(type) %>%
        nest() %>%
        mutate(
          mu = map_dbl(data, ~ with(., mean(outcome, na.rm = TRUE))),
          fit_mod1 = map(data, ~ feols(private$fe2sls_mod[[1]], data = ., vcov = ~hhid)),
          fit_mod2 = map(data, ~ feols(private$fe2sls_mod[[2]], data = ., vcov = ~hhid)),
          fit_mod3 = map(data, ~ feols(private$fe2sls_mod[[3]], data = ., vcov = ~hhid)),
          fit_mod4 = map(data, ~ feols(private$fe2sls_mod[[4]], data = ., vcov = ~hhid))
        ) %>%
        select(-data) %>%
        arrange(desc(type)) %>%
        pivot_longer(
          fit_mod1:fit_mod4,
          names_to = c(".value", "model"),
          names_pattern = "(.*)_(.*)"
        )

      addtab <- fit %>%
        mutate(
          imp = map2(fit, mu, ~ implied_e(.x, .y)),
          ivf = map_dbl(fit, ~ get_fitstat(., "ivf", "stat")),
          wh = map_dbl(fit, ~ get_fitstat(., "wh", "p"))
        ) %>%
        select(-mu, -fit) %>%
        unnest(imp) %>%
        mutate_at(vars(estimate, estimate_se), list(~ ifelse(type == "extensive", ., ""))) %>%
        mutate_at(vars(ivf, wh), list(~ ifelse(is.na(.), "", sprintf("\\num{%1.3f}", .)))) %>%
        mutate_at(vars(ivf, wh), list(~ ifelse(. == "\\num{0.000}", "$<$ \\num{0.001}", .))) %>%
        mutate(id = paste0(type, "_", model)) %>%
        ungroup() %>%
        select(-type, -model) %>%
        pivot_longer(-id) %>%
        pivot_wider(names_from = id, values_from = value) %>%
        mutate(name = recode(
          name,
          "estimate" = "Estimate",
          "estimate_se" = "",
          "ivf" = "F-statistics of instrument",
          "wh" = "Wu-Hausman test, p-value"
        ))

      attr(addtab, "position") <- 7:10

      list(
        fit = fit,
        stats = addtab
      )
    }
  )
)