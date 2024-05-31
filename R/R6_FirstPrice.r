library(here)
library(R6)
library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)
source(here("R/misc.r"))

FirstPrice <- R6::R6Class("FirstPrice",
  public = list(
    data = NULL,
    initialize = function(data) {
      self$data <- data %>%
        relocate(donate_ln, d_donate, .after = last_col()) %>%
        rename(
          outcome_intensive = donate_ln,
          outcome_extensive = d_donate,
          applicable = price_ln
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
          effective = d_relief_donate * applicable
        ) %>%
        dplyr::filter(flag == 1)

      setFixest_fml(
        ..stage2 = ~ after_tax_tinc_ln + sqage + hhnum + hhnum_child + dependent_num +
          hh_max_inc + I(family_position == 1) + employee +
          factor(indust) + factor(area) | pid + year
      )
    },
    stage1 = function(title = "", label = "", notes = "", font_size = 8) {
      est <- self$data %>%
        group_by(type) %>%
        nest() %>%
        mutate(fit = map(data, ~ feols(private$stage1_mod, data = ., cluster = ~hhid))) %>%
        arrange(desc(type))
      
      if (label != "") label <- paste0("\\label{tab:", label, "}")

      est %>%
        pull(fit) %>%
        setNames(paste0("(", seq(length(.)), ")")) %>%
        modelsummary(
          title = paste0(title, label),
          coef_map = c(
            "applicable" = "Applicable price",
            "after_tax_tinc_ln" = "Log after-tax income"
          ),
          gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
          stars = c("***" = .01, "**" = .05, "*" = .1),
          escape = FALSE
        ) %>%
        kable_styling(font_size = font_size) %>%
        add_header_above(c(
          " " = 1,
          "Donors (Intensive-margin)" = 1,
          "Donors and Non-donors (Extensive-margin)" = 1
        )) %>%
        add_header_above(c(" " = 1, "Effective price" = 2)) %>%
        group_rows("Excluded instruments", 1, 2, italic = TRUE, bold = FALSE) %>%
        group_rows("Covariates", 3, 4, italic = TRUE, bold = FALSE) %>%
        column_spec(2:3, width = "18.75em") %>%
        footnote(
          general_title = "",
          general = notes,
          threeparttable = TRUE,
          escape = FALSE
        )
    },
    stage2 = function(title = "", label = "", notes = "", font_size = 8) {
      private$main_reg(self$data, title, label, notes, font_size)
    },
    exclude_announcement = function(title = "", label = "", notes = "", font_size = 8) {
      dta <- subset(self$data, year < 2013 | 2014 < year)
      private$main_reg(dta, title, label, notes, font_size)
    },
    claimant_only = function(title = "", label = "", notes = "", font_size = 8) {
      dta <- subset(self$data, d_relief_donate == 1 & type == "intensive")
      fit <- feols(private$fe2sls_mod[[1]], data = dta, vcov = ~ hhid)

      if (label != "") label <- paste0("\\label{tab:", label, "}")

      list("(1)" = fit) %>%
        modelsummary(
          title = paste0(title, label),
          coef_map = c(
            "applicable" = "Applicable price ($\\beta_a$)",
            "after_tax_tinc_ln" = "Log after-tax income"
          ),
          gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2|RMSE",
          stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
          escape = FALSE
        ) %>%
        kable_styling(font_size = font_size) %>%
        add_header_above(c(" " = 1, "FE" = 1)) %>%
        add_header_above(c(" " = 1, "Log donation" = 1)) %>%
        column_spec(1, width = "25em") %>%
        column_spec(2, width = "15em") %>%
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
      fit <- feols(d_relief_donate ~ applicable + ..stage2, data = dta, vcov = ~ hhid)

      addtab <- implied_e(fit, mu) %>%
        pivot_longer(everything()) %>%
        mutate(name = dplyr::recode(name, "estimate" = "Estimate", "estimate_se" = ""))
      
      attr(addtab, "position") <- 5:6

      if (label != "") label <- paste0("\\label{tab:", label, "}")

      list("(1)" = fit) %>%
        modelsummary(
          title = paste0(title, label),
          coef_map = c(
            "applicable" = "Applicable price",
            "after_tax_tinc_ln" = "Log after-tax income"
          ),
          gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2|RMSE",
          stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
          add_rows = addtab,
          escape = FALSE
        ) %>%
        kable_styling(font_size = font_size) %>%
        add_header_above(c(" " = 1, "FE" = 1)) %>%
        add_header_above(c(" " = 1, "1 = Declaration" = 1)) %>%
        group_rows("Implied price elasticity", 5, 6, italic = TRUE, bold = FALSE) %>%
        column_spec(1, width = "25em") %>%
        column_spec(2, width = "15em") %>%
        footnote(
          general_title = "",
          general = notes,
          threeparttable = TRUE,
          escape = FALSE
        )
    }
  ),
  private = list(
    stage1_mod = effective ~ applicable + ..stage2,
    fe2sls_mod = list(
      outcome ~ applicable + ..stage2,
      outcome ~ effective + ..stage2,
      outcome ~ ..stage2 | effective ~ applicable
    ),
    main_reg = function(data, title = "", label = "", notes = "", font_size = 8) {
      fit <- data %>%
        group_by(type) %>%
        nest() %>%
        mutate(
          mu = map_dbl(data, ~ with(., mean(outcome, na.rm = TRUE))),
          fit_mod1 = map(data, ~ feols(private$fe2sls_mod[[1]], data = ., vcov = ~hhid)),
          fit_mod2 = map(data, ~ feols(private$fe2sls_mod[[2]], data = ., vcov = ~hhid)),
          fit_mod3 = map(data, ~ feols(private$fe2sls_mod[[3]], data = ., vcov = ~hhid))
        ) %>%
        select(-data) %>%
        arrange(desc(type)) %>%
        pivot_longer(
          fit_mod1:fit_mod3,
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

      attr(addtab, "position") <- 9:12

      if (label != "") label <- paste0("\\label{tab:", label, "}")

      fit %>%
        pull(fit) %>%
        setNames(paste0("(", seq(length(.)), ")")) %>%
        modelsummary(
          title = paste0(title, label),
          coef_map = c(
            "applicable" = "Applicable price ($\\beta_a$)",
            "effective" = "Effective price ($\\beta^{FE}_e$)",
            "fit_effective" = "Effective price ($\\beta^{IV}_e$)",
            "after_tax_tinc_ln" = "Log after-tax income"
          ),
          gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2|RMSE",
          stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
          add_rows = addtab,
          escape = FALSE
        ) %>%
        kable_styling(font_size = font_size) %>%
        add_header_above(c(
          " " = 1, "FE" = 2, "FE-2SLS" = 1, "FE" = 2, "FE-2SLS" = 1
        )) %>%
        add_header_above(c(" " = 1, "Log donation" = 3, "Dummy of donor" = 3)) %>%
        group_rows("Implied price elasticity", 9, 10, italic = TRUE, bold = FALSE) %>%
        group_rows(
          "1st stage information (Excluded instrument: Applicable price)",
          11, 12,
          bold = FALSE, italic = TRUE
        ) %>%
        column_spec(2:7, width = "5em") %>%
        footnote(
          general_title = "",
          general = notes,
          threeparttable = TRUE,
          escape = FALSE
        )
    }
  )
)
