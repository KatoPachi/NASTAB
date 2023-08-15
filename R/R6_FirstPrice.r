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
        ..stage2 = ~ tinc_ln + sqage + hh_num + have_dependents + employee +
          factor(indust) + factor(area) | pid + year
      )
    },
    stage1 = function() {
      est <- self$data %>%
        group_by(type) %>%
        nest() %>%
        mutate(fit = map(data, ~ feols(private$stage1_mod, data = ., cluster = ~hhid))) %>%
        arrange(desc(type))

      est %>%
        pull(fit) %>%
        setNames(paste0("(", seq(length(.)), ")")) %>%
        modelsummary(
          title = "First-Stage Models\\label{tab:main-stage1}",
          coef_map = c(
            "applicable" = "Applicable price",
            "tinc_ln" = "Log income"
          ),
          gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
          stars = c("***" = .01, "**" = .05, "*" = .1),
          output = "latex",
          escape = FALSE
        ) %>%
        kable_styling(font_size = 6) %>%
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
          general = "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. Standard errors clustered at household level are in parentheses. An outcome variable is logged value of the effective price. For estimation, model (1) use donors only (intensive-margin sample), and model (2) use not only donors but also non-donors (extensive-margin sample). In addition to logged income and wage earner dummy shown in table, covariates consist of squared age (divided by 100), number of household members, a dummy that indicates having dependents, a set of dummies of industry a set of dummies of residential area, and individual and time fixed effects. Excluded instrument is a logged applicable price.",
          threeparttable = TRUE,
          escape = FALSE
        )
    },
    stage2 = function() {
      private$main_reg(
        self$data,
        "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. Standard errors clustered at household level are in parentheses. An outcome variable is logged value of the effective price. For estimation, model (1) use donors only (intensive-margin sample), and model (2) use not only donors but also non-donors (extensive-margin sample). In addition to logged income and wage earner dummy shown in table, covariates consist of squared age (divided by 100), number of household members, a dummy that indicates having dependents, a set of dummies of industry a set of dummies of residential area, and individual and time fixed effects. Excluded instrument is a logged applicable price."
      )
    },
    exclude_announcement = function() {
      dta <- subset(self$data, year < 2013 | 2014 < year)
      private$main_reg(
        dta,
        "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01. Standard errors clustered at household level are in parentheses. An outcome variable is logged value of amount of charitable giving for models (1)--(3) and a dummy of donor for models (4)--(6). For estimation, models (1)--(3) use donors only (intensive-margin sample), and models (4)--(6) use not only donors but also non-donors (extensive-margin sample). To exclude announcement effect, we exclude samples from 2013 and 2014. For outcome equation, we control squared age (divided by 100), number of household members, a dummy that indicates having dependents, employee dummy, a set of dummies of industry a set of dummies of residential area, and individual and time fixed effects. For FE-2SLS, we use a logged applicable price as an instrument. To obtain the extensive-margin price elasticities in models (4)--(6), we calculate implied price elasticities by divding estimated coeffcient on price by sample proportion of donors."
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
    main_reg = function(data, note) {
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

      fit %>%
        pull(fit) %>%
        setNames(paste0("(", seq(length(.)), ")")) %>%
        modelsummary(
          title = "Estimation Results of Price Elasticities\\label{tab:main}",
          coef_map = c(
            "applicable" = "Applicable price ($\\beta_a$)",
            "effective" = "Effective price ($\\beta^{FE}_e$)",
            "fit_effective" = "Effective price ($\\beta^{IV}_e$)",
            "tinc_ln" = "Log income"
          ),
          gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2|RMSE",
          stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
          add_rows = addtab,
          output = "latex",
          escape = FALSE
        ) %>%
        kable_styling(font_size = 6) %>%
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
          general = note,
          threeparttable = TRUE,
          escape = FALSE
        )
    }
  )
)
