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
      ymin <- with(subset(data, donate > 0), min(donate))
      ymin_claim <- with(
        subset(data, d_relief_donate * donate > 0),
        min(d_relief_donate * donate)
      )

      self$data <- data %>%
        mutate(
          norm_donate = donate / ymin,
          donate_ln = if_else(d_donate == 0, -1, log(norm_donate)),
          claim_donate = d_relief_donate * donate,
          d_claim_donate = if_else(claim_donate > 0, 1, 0),
          norm_claim_donate = claim_donate / ymin_claim,
          claim_donate_ln = if_else(d_claim_donate == 0, -1, log(norm_claim_donate)),
          applicable = price_ln,
          effective = d_relief_donate * applicable
        )

      setFixest_fml(
        ..stage2 = ~ after_tax_tinc_ln + sqage +
          hhnum + hhnum_child + dependent_num +
          hh_max_inc + I(family_position == 1) +
          factor(indust) + factor(area) | pid + year
      )
    },
    stage1 = function(title = "", label = "", notes = "", font_size = 8) {
      est <- feols(private$stage1_mod, data = self$data, cluster = ~hhid)

      if (label != "") label <- paste0("\\label{tab:", label, "}")

      list("(1)" = est) %>%
        modelsummary(
          title = paste0(title, label),
          coef_map = c(
            "applicable" = "Applicable price",
            "after_tax_tinc_ln" = "Log after-tax income"
          ),
          gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2|RMSE",
          stars = c("***" = .01, "**" = .05, "*" = .1),
          escape = FALSE
        ) %>%
        kable_styling(font_size = font_size) %>%
        add_header_above(c(" " = 1, "Effective price" = 1)) %>%
        group_rows("Excluded instruments", 1, 2, italic = TRUE, bold = FALSE) %>%
        group_rows("Covariates", 3, 4, italic = TRUE, bold = FALSE) %>%
        column_spec(2, width = "18.75em") %>%
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
      fit <- feols(
        claim_donate_ln ~ applicable + ..stage2,
        data = self$data, vcov = ~ hhid
      )

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
    main_reg = function(data, title = "", label = "", notes = "", font_size = 8) {
      mods <- list(
        donate_ln ~ applicable + ..stage2,
        donate_ln ~ effective + ..stage2,
        donate_ln ~ ..stage2 | effective ~ applicable,
        d_donate ~ applicable + ..stage2,
        d_donate ~ effective + ..stage2,
        d_donate ~ ..stage2 | effective ~ applicable
      )

      fit_mods <- mods %>%
        purrr::map(~ feols(., data = data, vcov = ~ hhid))

      dmu <- mean(data$d_donate, na.rm = TRUE)

      imp <- fit_mods %>%
        purrr::map(~ implied_e(., dmu)) %>%
        purrr::map(~ pivot_longer(., everything())[, 2]) %>%
        reduce(cbind)
      imp[, 1:3] <- ""

      ivf <- fit_mods %>%
        sapply(function(x) get_fitstat(x, "ivf", "stat")) %>%
        sapply(function(x) ifelse(is.na(x), "", sprintf("\\num{%1.3f}", x)))

      addtab <- data.frame(rbind(imp, ivf))
      addtab <- cbind(
        term = c("Estimate", "", "F-statistics of instrument"),
        addtab
      )

      attr(addtab, "position") <- 9:11

      if (label != "") label <- paste0("\\label{tab:", label, "}")

      fit_mods %>%
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
          11, 11,
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
