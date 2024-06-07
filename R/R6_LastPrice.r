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
          applicable_last = lprice_ln,
          effective = d_relief_donate * applicable,
          effective_last = d_relief_donate * applicable_last
        )

      setFixest_fml(
        ..stage2 = ~ after_tax_tinc_ln + sqage + hhnum + hhnum_child + dependent_num +
          hh_max_inc + I(family_position == 1) +
          factor(indust) + factor(area) | pid + year
      )
    },
    overall = function(title = "", label = "", notes = "", font_size = 8) {
      mods <- list(
        donate_ln ~ applicable_last + ..stage2,
        donate_ln ~ effective_last + ..stage2,
        donate_ln ~ ..stage2 | applicable_last ~ applicable,
        donate_ln ~ ..stage2 | effective_last ~ applicable
      )

      fit_mods <- mods %>%
        map(~ feols(., data = self$data, vcov = ~ hhid))

      ivf <- fit_mods %>%
        lapply(function(x) get_fitstat(x, "ivf", "stat")) %>%
        lapply(function(x) ifelse(is.na(x), "", sprintf("\\num{%1.3f}", x))) %>%
        reduce(cbind)

      addtab <- data.frame(cbind(term = c("F-statistics of instrument"), ivf))
      attr(addtab, "position") <- 7

      if (label != "") label <- paste0("\\label{tab:", label, "}")

      fit_mods %>%
        setNames(paste0("(", seq(length(.)), ")")) %>%
        modelsummary(
          title = paste0(title, label),
          coef_map = c(
            "applicable_last" = "Applicable last-price",
            "fit_applicable_last" = "Applicable last-price",
            "effective_last" = "Effective last-price",
            "fit_effective_last" = "Effective last-price",
            "after_tax_tinc_ln" = "Log after-tax income"
          ),
          gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2|RMSE",
          stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
          add_rows = addtab,
          escape = FALSE
        ) %>%
        kable_styling(font_size = font_size) %>%
        add_header_above(c(" " = 1, "FE" = 2, "FE-2SLS" = 2)) %>%
        add_header_above(c(" " = 1, "Log donation" = 4)) %>%
        group_rows(
          "1st stage information (Excluded instrument: Applicable price)",
          7, 7,
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
      mods <- list(
        d_donate ~ applicable_last + ..stage2,
        d_donate ~ effective_last + ..stage2,
        d_donate ~ ..stage2 | applicable_last ~ applicable,
        d_donate ~ ..stage2 | effective_last ~ applicable
      )

      fit_mods <- mods %>%
        map(~ feols(., data = self$data, vcov = ~hhid))

      ivf <- fit_mods %>%
        sapply(function(x) get_fitstat(x, "ivf", "stat")) %>%
        sapply(function(x) ifelse(is.na(x), "", sprintf("\\num{%1.3f}", x)))

      mu <- mean(self$data$d_donate, na.rm = TRUE)

      imp <- fit_mods %>%
        purrr::map(~ implied_e(., mu)) %>%
        purrr::map(~ pivot_longer(., everything())[, 2]) %>%
        reduce(cbind)

      stats <- data.frame(rbind(imp, ivf))
      addtab <- cbind(term = c("Estimate", "", "F-statistics of instrument"), stats)
      attr(addtab, "position") <- 7:9

      if (label != "") label <- paste0("\\label{tab:", label, "}")

      fit_mods %>%
        setNames(paste0("(", seq(length(.)), ")")) %>%
        modelsummary(
          title = paste0(title, label),
          coef_map = c(
            "applicable_last" = "Applicable last-price",
            "fit_applicable_last" = "Applicable last-price",
            "effective_last" = "Effective last-price",
            "fit_effective_last" = "Effective last-price",
            "after_tax_tinc_ln" = "Log after-tax income"
          ),
          gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2|RMSE",
          stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
          add_rows = addtab,
          escape = FALSE
        ) %>%
        kable_styling(font_size = font_size) %>%
        add_header_above(c(" " = 1, "FE" = 2, "FE-2SLS" = 2)) %>%
        add_header_above(c(" " = 1, "A dummy of donor" = 4)) %>%
        group_rows("Implied price elasticity", 7, 8, italic = TRUE, bold = FALSE) %>%
        group_rows(
          "1st stage information (Excluded instrument: Applicable price)",
          9, 9,
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
      mods <- list(
        claim_donate_ln ~ applicable_last + ..stage2,
        claim_donate_ln ~ ..stage2 | applicable_last ~ applicable
      )

      fit_mods <- mods %>%
        map(~feols(., data = self$data, vcov = ~ hhid))

      stats <- get_fitstat(fit_mods[[2]], "ivf", "stat")
      stats <- sprintf("\\num{%1.3f}", stats)

      addtab <- tibble(
        term = "F-statistics of instrument",
        mod1 = "",
        mod2 = stats
      )

      attr(addtab, "position") <- 5

      if (label != "") label <- paste0("\\label{tab:", label, "}")

      fit_mods %>%
        modelsummary(
          title = paste0(title, label),
          coef_map = c(
            "applicable_last" = "Applicable last-price",
            "fit_applicable_last" = "Applicable last-price",
            "after_tax_tinc_ln" = "Log after-tax income"
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
      mu <- with(self$data, mean(d_relief_donate, na.rm = TRUE))

      mods <- list(
        d_relief_donate ~ applicable_last + ..stage2,
        d_relief_donate ~ ..stage2 | applicable_last ~ applicable
      )

      fit_mods <- mods %>%
        map(~feols(., data = self$data, vcov = ~ hhid))

      imp_e_tab <- fit_mods %>%
        map(function(x) implied_e(x, mu) %>% pivot_longer(everything())) %>%
        reduce(left_join, by = "name") %>%
        mutate(name = dplyr::recode(name, "estimate" = "Estimate", "estimate_se" = ""))

      stats <- get_fitstat(fit_mods[[2]], "ivf", "stat")
      stats <- sprintf("\\num{%1.3f}", stats)

      stat_stage1_tab <- tibble(
        name = "F-statistics of instrument",
        value.x = "",
        value.y = stats
      )

      addtab <- bind_rows(imp_e_tab, stat_stage1_tab)
      attr(addtab, "position") <- 5:7

      if (label != "") label <- paste0("\\label{tab:", label, "}")

      fit_mods %>%
        modelsummary(
          title = paste0(title, label),
          coef_map = c(
            "applicable_last" = "Applicable price",
            "fit_applicable_last" = "Applicable price",
            "after_tax_tinc_ln" = "Log after-tax income"
          ),
          gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2|RMSE",
          stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
          add_rows = addtab,
          escape = FALSE
        ) %>%
        kable_styling(font_size = font_size) %>%
        add_header_above(c(" " = 1, "FE" = 1, "FE-2SLS" = 1)) %>%
        add_header_above(c(" " = 1, "1 = Report" = 2)) %>%
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
    )
  )
)