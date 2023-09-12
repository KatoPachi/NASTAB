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
        relocate(donate_ln, d_donate, .after = last_col()) %>%
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
    intensive = function(title = "", label = "", notes = "", font_size = 8) {
      est <- self$result

      if (label != "") label <- paste0("\\label{tab:", label, "}")

      est$fit %>%
        dplyr::filter(type == "intensive") %>%
        pull(fit) %>%
        setNames(paste0("(", seq(length(.)), ")")) %>%
        modelsummary(
          title = paste0(title, label),
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
          escape = FALSE
        ) %>%
        kable_styling(font_size = font_size) %>%
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
          general = notes,
          threeparttable = TRUE,
          escape = FALSE
        )
    },
    extensive = function(title = "", label = "", notes = "", font_size = 8) {
      est <- self$result

      if (label != "") label <- paste0("\\label{tab:", label, "}")

      est$fit %>%
        dplyr::filter(type == "extensive") %>%
        pull(fit) %>%
        setNames(paste0("(", seq(length(.)), ")")) %>%
        modelsummary(
          title = paste0(title, label),
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
          escape = FALSE
        ) %>%
        kable_styling(font_size = font_size) %>%
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
          general = notes,
          threeparttable = TRUE,
          escape = FALSE
        )
    },
    claimant_only = function(title = "", label = "", notes = "", font_size = 8) {
      dta <- subset(self$data, d_relief_donate == 1 & type == "intensive")
      
      fit <- private$fe2sls_mod[c(1, 3)] %>%
        map(~feols(., data = dta, vcov = ~ hhid))
      
      stat_stage1 <- c(
        get_fitstat(fit[[2]], "ivf", "stat"),
        get_fitstat(fit[[2]], "wh", "p")
      )
      stat_stage1 <- sprintf("\\num{%1.3f}", stat_stage1)
      stat_stage1 <- ifelse(stat_stage1 == "\\num{0.000}", "$<$ \\num{0.001}", stat_stage1)

      addtab <- tibble(
        term = c("F-statistics of instrument", "Wu-Hausman test, p-value"),
        mod1 = c("", ""),
        mod2 = stat_stage1
      )

      attr(addtab, "position") <- 5:6

      if (label != "") label <- paste0("\\label{tab:", label, "}")

      fit %>%
        modelsummary(
          title = paste0(title, label),
          coef_map = c(
            "applicable_last" = "Applicable last-price",
            "fit_applicable_last" = "Applicable last-price",
            "tinc_ln" = "Log income"
          ),
          gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2|RMSE",
          stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
          add_rows = addtab,
          escape = FALSE
        ) %>%
        kable_styling(font_size = font_size) %>%
        add_header_above(c(" " = 1, "FE" = 1, "FE-2SLS" = 1)) %>%
        add_header_above(c(" " = 1, "Log donation" = 2)) %>%
        group_rows(
          "1st stage information (Excluded instrument: Applicable price)",
          5, 6,
          bold = FALSE, italic = TRUE
        ) %>%
        column_spec(2:3, width = "6.25em") %>%
        footnote(
          general_title = "",
          general = notes,
          threeparttable = TRUE,
          escape = FALSE
        )
    },
    claim_elasticity = function(title = "", label = "", notes = "", font_size = 8) {
      dta <- subset(self$data, type == "extensive")
      mu <- with(dta, mean(d_relief_donate, na.rm = TRUE))
      
      fit <- private$fe2sls_mod[c(1, 3)] %>%
        map(~feols(., data = dta, vcov = ~ hhid))
      
      imp_e_tab <- fit %>%
        map(function(x) implied_e(x, mu) %>% pivot_longer(everything())) %>%
        reduce(left_join, by = "name") %>%
        mutate(name = dplyr::recode(name, "estimate" = "Estimate", "estimate_se" = ""))
      
      stat_stage1 <- c(get_fitstat(fit[[2]], "ivf", "stat"), get_fitstat(fit[[2]], "wh", "p"))
      stat_stage1 <- sprintf("\\num{%1.3f}", stat_stage1)
      stat_stage1 <- ifelse(stat_stage1 == "\\num{0.000}", "$<$ \\num{0.001}", stat_stage1)

      stat_stage1_tab <- tibble(
        name = c("F-statistics of instrument", "Wu-Hausman test, p-value"),
        value.x = c("", ""),
        value.y = stat_stage1
      )

      addtab <- bind_rows(imp_e_tab, stat_stage1_tab)
      attr(addtab, "position") <- 5:8

      if (label != "") label <- paste0("\\label{tab:", label, "}")

      fit %>%
        modelsummary(
          title = paste0(title, label),
          coef_map = c(
            "applicable_last" = "Applicable price",
            "fit_applicable_last" = "Applicable price",
            "tinc_ln" = "Log income"
          ),
          gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2|RMSE",
          stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
          add_rows = addtab,
          escape = FALSE
        ) %>%
        kable_styling(font_size = font_size) %>%
        add_header_above(c(" " = 1, "FE" = 1, "FE-2SLS" = 1)) %>%
        add_header_above(c(" " = 1, "1 = Declaration" = 2)) %>%
        group_rows("Implied price elasticity", 5, 6, italic = TRUE, bold = FALSE) %>%
        group_rows(
          "1st stage information (Excluded instrument: Applicable first-price)",
          7, 8,
          italic = TRUE, bold = FALSE
        ) %>%
        footnote(
          general_title = "",
          general = notes,
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