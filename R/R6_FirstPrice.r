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
      est <- feols(
        effective ~ applicable + ..stage2,
        data = self$data, cluster = ~hhid
      )

      if (label != "") label <- paste0("\\label{tab:", label, "}")

      list("(1)" = est) %>%
        modelsummary(
          title = paste0(title, label),
          coef_map = c(
            "applicable" = "Simulated price",
            "after_tax_tinc_ln" = "Log after-tax income"
          ),
          gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2|RMSE",
          stars = c("***" = .01, "**" = .05, "*" = .1),
          escape = FALSE
        ) %>%
        kable_styling(font_size = font_size) %>%
        add_header_above(c(" " = 1, "Actual price" = 1)) %>%
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
      mods <- list(
        donate_ln ~ applicable + ..stage2,
        donate_ln ~ effective + ..stage2,
        donate_ln ~ ..stage2 | effective ~ applicable,
        d_donate ~ applicable + ..stage2,
        d_donate ~ effective + ..stage2,
        d_donate ~ ..stage2 | effective ~ applicable
      )

      fit_mods <- mods %>%
        purrr::map(~ feols(., data = self$data, vcov = ~hhid))

      dmu <- mean(self$data$d_donate, na.rm = TRUE)

      b <- fit_mods[c(4, 6)] %>%
        map(broom::tidy) %>%
        map_dbl(~ subset(., str_detect(term, "effective|applicable"))$estimate)

      private$ext_elasticities <- b / dmu

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
            "applicable" = "Simulated price ($\\beta_a$)",
            "effective" = "Actual price ($\\beta^{FE}_e$)",
            "fit_effective" = "Actual price ($\\beta^{IV}_e$)",
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
          "1st stage information (Excluded instrument: Simulated price)",
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
    },
    sufficient_stats = function(title = "",
                                label = "",
                                notes = "",
                                font_size = 8) {
      if (is.null(private$ext_elasticities)) stop("Before using this method, you must run the 'stage2' method!")
      ext_values <- private$ext_elasticities

      dt <- self$data %>%
        mutate(
          donate_ln_itt = if_else(d_donate == 0, ext_values[1], log(norm_donate)),
          donate_ln_iv = if_else(d_donate == 0, ext_values[2], log(norm_donate))
        )

      mods <- list(
        donate_ln_itt ~ applicable + ..stage2,
        donate_ln_iv ~ ..stage2 | effective ~ applicable
      )

      fit_mods <- mods %>%
        map(~ feols(., data = dt, vcov = ~ hhid))

      addtab <- tibble(
        term = "Extensive-margin values ($x$)",
        itt = sprintf("\\num{%1.3f}", ext_values[1]),
        iv = sprintf("\\num{%1.3f}", ext_values[2])
      )

      attr(addtab, "position") <- 5

      if (label != "") label <- paste0("\\label{tab:", label, "}")

      fit_mods %>%
        setNames(paste0("(", seq(length(.)), ")")) %>%
        modelsummary(
          title = paste0(title, label),
          coef_map = c(
            "applicable" = "Simulated price ($\\beta_a$)",
            "fit_effective" = "Actual price ($\\beta^{IV}_e$)"
          ),
          gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2|RMSE",
          stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
          add_rows = addtab,
          escape = FALSE
        ) %>%
        kable_styling(font_size = font_size) %>%
        add_header_above(c(" " = 1, "FE" = 1, "FE-2SLS" = 1)) %>%
        add_header_above(c(" " = 1, "Log donation" = 2)) %>%
        column_spec(1, width = "20em") %>%
        column_spec(2:3, width = "10em") %>%
        footnote(
          general_title = "",
          general = notes,
          threeparttable = TRUE,
          escape = FALSE
        )
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
            "applicable" = "Simulated price ($\\beta_a$)",
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
      mu <- with(self$data, mean(d_relief_donate, na.rm = TRUE))
      fit <- feols(
        d_relief_donate ~ applicable + ..stage2,
        data = self$data, vcov = ~ hhid
      )

      addtab <- implied_e(fit, mu) %>%
        pivot_longer(everything()) %>%
        mutate(name = dplyr::recode(name, "estimate" = "Estimate", "estimate_se" = ""))

      attr(addtab, "position") <- 5:6

      if (label != "") label <- paste0("\\label{tab:", label, "}")

      list("(1)" = fit) %>%
        modelsummary(
          title = paste0(title, label),
          coef_map = c(
            "applicable" = "Simulated price",
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
    },
    ext_margin_value = function(value, title = "", label = "", notes = "", font_size = 8) {
      mods <- list(
        donate_ln ~ applicable + ..stage2,
        donate_ln ~ ..stage2 | effective ~ applicable
      )

      fit_mods <- vector("list", length(value))
      for (i in 1:length(value)) {
        x <- value[i]
        dt <- self$data %>%
          mutate(
            donate_ln = if_else(d_donate == 0, -x, log(norm_donate))
          )

        fit_mods[[i]] <- mods %>%
          map(~ feols(., data = dt, vcov = ~ hhid))
      }
      fit_mods <- purrr::flatten(fit_mods)

      ivf <- lapply(fit_mods, function(x) get_fitstat(x, "ivf", "stat")) %>%
        lapply(function(x) ifelse(is.na(x), "", sprintf("\\num{%1.3f}", x))) %>%
        reduce(cbind)

      addtab <- data.frame(cbind(term = c("F-statistics of instrument"), ivf))
      attr(addtab, "position") <- 7

      if (label != "") label <- paste0("\\label{tab:", label, "}")
      header <- c(1, rep(2, length(value)))
      names(header) <- c("Extensive-margin value ($x$)", paste0("$x = ", value, "$"))

      fit_mods %>%
        modelsummary(
          title = paste0(title, label),
          coef_map = c(
            "applicable" = "Simulated price ($\\beta_a$)",
            "fit_effective" = "Actual price ($\\beta^{IV}_e$)",
            "after_tax_tinc_ln" = "Log after-tax income"
          ),
          gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2|RMSE",
          stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
          add_rows = addtab,
          escape = FALSE
        ) %>%
        kable_styling(font_size = font_size) %>%
        add_header_above(header, escape = FALSE) %>%
        group_rows(
          "1st stage information (Excluded instrument: Simulated price)",
          7, 7,
          bold = FALSE, italic = TRUE
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
    ext_elasticities = NULL
  )
)
