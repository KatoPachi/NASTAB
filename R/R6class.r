library(here)
library(R6)
library(tidyverse)
library(patchwork)
library(rlang)
library(fixest)
library(modelsummary)
library(kableExtra)
source(here("R/misc.r"))

StartAnalysis <- R6::R6Class("StartAnalysis", list(
  data = NULL,
  incentive_limit_summary = NULL,
  initialize = function(path) {
    dta <- read_csv(path) %>%
      dplyr::filter(age >= 24) %>%
      dplyr::filter(d_relief_donate == 0 | (d_relief_donate == 1 & d_donate == 1)) %>%
      dplyr::filter(2010 <= year & year < 2018) %>%
      dplyr::filter(tinc < 1100 | 1300 < tinc) %>%
      dplyr::filter(tinc < 4500 | 4700 < tinc) %>%
      dplyr::filter(tinc < 8700 | 8900 < tinc) %>%
      dplyr::filter(tinc < 14000 | 16000 < tinc) %>%
      dplyr::filter(tinc < 30000) %>%
      dplyr::filter(bracket13 != "(F) & (G) 30000--" | is.na(bracket13)) %>%
      dplyr::filter(dependents == 0)
    
    incentive_limit_summary <- dta %>%
      mutate(over_bound = donate > incentive_limit) %>%
      group_by(year) %>%
      summarize(mean(over_bound), mean(incentive_limit))
    
    dta <- dta %>%
      dplyr::filter(tinc > donate) %>%
      dplyr::filter(d_relief_donate == 0 | incentive_limit >= donate)

    self$data <- dta %>%
      select(
        pid,
        hhid,
        year,
        bracket13,
        tinc,
        tinc_ln,
        linc,
        price,
        price_ln,
        d_relief_donate,
        age,
        sqage,
        hh_num,
        have_dependents,
        sex,
        college,
        highschool,
        employee,
        indust,
        area,
        donate,
        donate_ln,
        d_donate
      )
  },
  summary = function() SummaryData$new(self$data),
  first_price = function() FirstPrice$new(self$data)
))

SummaryData <- R6::R6Class("SummaryData", list(
  data = NULL,
  initialize = function(data) self$data <- data,
  stats = function() {
    use %>%
      datasummary(
        (`Annual labor income (unit: 10,000KRW)` = linc) +
          (`Annual total income (unit: 10,000KRW)` = tinc) +
          (`Appricale price` = price) +
          (`Annual chariatable giving (unit: 10,000KRW)` = donate) +
          (`Dummary of donation > 0` = d_donate) +
          (`Dummy of declaration of a tax relief` = d_relief_donate) +
          (`Age` = age) +
          (`Wage earner dummy` = employee) +
          (`Number of household members` = hh_num) +
          (`Dummy of having dependents` = have_dependents) +
          (`Female dummy` = sex) +
          (`Academic history: University` = college) +
          (`Academic history: High school` = highschool) ~
          N +
          (`Mean` = Mean) +
          (`Std.Dev.` = SD),
        title = "Descriptive Statistics\\label{tab:summary-covariate}",
        data = .,
        align = "lccc",
        output = "latex",
        escape = FALSE
      ) %>%
      kable_styling(font_size = 8) %>%
      pack_rows("Income and giving price", 1, 3, bold = FALSE, italic = TRUE) %>%
      pack_rows("Charitable giving", 4, 6, bold = FALSE, italic = TRUE) %>%
      pack_rows("Demographics", 7, 13, bold = FALSE, italic = TRUE) %>%
      footnote(
        general_title = "",
        general = "Notes: Our data is unbalanced panel data consisting of 8,441 unique individuals and 8 years period (2010--2017)",
        threeparttable = TRUE,
        escape = FALSE
      )
  },
  income_dist = function() {
    self$data %>%
      filter(year == 2013) %>%
      dplyr::select(tinc, price) %>%
      ggplot(aes(x = tinc)) +
      geom_hline(aes(yintercept = 0)) +
      geom_histogram(
        aes(y = after_stat(count) / sum(after_stat(count)), fill = "Relative frequency"),
        color = "black"
      ) +
      geom_step(
        aes(y = price * 0.5, color = "Giving Price in 2010-2013"),
        linewidth = 1
      ) +
      geom_hline(
        aes(yintercept = (1 - 0.15) * 0.5),
        color = "black", linetype = 2, linewidth = 1
      ) +
      scale_color_manual(NULL, values = "black") +
      scale_fill_manual(NULL, values = "grey80") +
      scale_y_continuous(
        breaks = seq(0, 0.5, 0.125),
        sec.axis = sec_axis(~ . / 0.5, name = "Giving Price")
      ) +
      scale_x_continuous(breaks = c(1200, 4600, 8800, 30000)) +
      labs(
        x = "Annual total income (10,000KRW)",
        y = "Relative frequency"
      ) +
      ggtemp(size = list(axis_title = 15, axis_text = 13, caption = 13))
  },
  ts_giving = function() {
    plot_data <- test$data %>%
      dplyr::filter(!is.na(bracket13)) %>%
      group_by(year, bracket13) %>%
      summarize(
        amount = mean(donate, na.rm = TRUE),
        donor = mean(d_donate, na.rm = TRUE)
      ) %>%
      pivot_longer(amount:donor, names_to = "vars", values_to = "mu") %>%
      pivot_wider(names_from = "year", values_from = "mu") %>%
      mutate(base = `2013`) %>%
      select(vars, bracket13, base, everything()) %>%
      pivot_longer(-(vars:base), names_to = "year", values_to = "mu") %>%
      mutate(
        mu = mu / base,
        year = as.numeric(year)
      )
    
    plot1 <- plot_data %>%
      dplyr::filter(vars == "amount") %>%
      ggplot(aes(x = year, y = mu, group = bracket13)) +
      geom_vline(aes(xintercept = 2013.5), linetype = 3) +
      geom_point(aes(shape = bracket13), size = 4) +
      geom_line() +
      scale_shape_manual(values = c(16, 15, 17, 18)) +
      scale_x_continuous(breaks = seq(2010, 2018, 1)) +
      scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0, 2.6)) +
      labs(
        title = "Panel A. Amount of Giving",
        x = "Year",
        y = "Normalized average giving",
        shape = "Income bracket (unit:10,000KRW)"
      ) +
      ggtemp(size = list(axis_title = 15, axis_text = 13, title = 13))
    
    plot2 <- plot_data %>%
      dplyr::filter(vars == "donor") %>%
      ggplot(aes(x = year, y = mu, group = bracket13)) +
      geom_vline(aes(xintercept = 2013.5), linetype = 3) +
      geom_point(aes(shape = bracket13), size = 4) +
      geom_line() +
      scale_shape_manual(values = c(16, 15, 17, 18)) +
      scale_x_continuous(breaks = seq(2010, 2018, 1)) +
      scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0, 1.6)) +
      labs(
        title = "Panel A. Amount of Giving",
        x = "Year",
        y = "Normalized average giving",
        shape = "Income bracket (unit:10,000KRW)"
      ) +
      ggtemp(size = list(axis_title = 15, axis_text = 13, title = 13))
    
    plot1 + plot2 +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom") &
      guides(shape = guide_legend(title.position = "top", title.hjust = 0.5))
  },
  ts_claim = function(subset = TRUE) {
    expr_subset <- enquo(subset)
    eval_tf <- eval_tidy(expr_subset, self$data)
    dta <- self$data[eval_tf, , drop = FALSE]

    plot1 <- dta %>%
      dplyr::filter(!is.na(bracket13)) %>%
      group_by(year, bracket13) %>%
      summarize(mu = mean(d_relief_donate, na.rm = TRUE)) %>%
      ggplot(aes(x = year, y = mu, group = bracket13)) +
      geom_vline(aes(xintercept = 2013.5), linetype = 3) +
      geom_point(aes(shape = bracket13), size = 4) +
      geom_line() +
      scale_shape_manual(values = c(16, 15, 17, 18)) +
      scale_x_continuous(breaks = seq(2010, 2018, 1)) +
      labs(
        title = "Panel A. Grouped by Income Bracket",
        x = "Year",
        y = "Proportion of application for tax relief",
        shape = "Income bracket (unit:10,000KRW)"
      ) +
      ggtemp(size = list(axis_title = 15, axis_text = 13, title = 13)) +
      guides(shape = guide_legend(
        title.position = "top", title.hjust = 0.5, nrow = 2
      ))
    
    plot2 <- dta %>%
      dplyr::filter(!is.na(employee)) %>%
      mutate(employee = factor(
        employee,
        levels = c(1, 0), labels = c("Wage earners", "Others")
      )) %>%
      group_by(year, employee) %>%
      summarize_at(vars(d_relief_donate), list(~ mean(., na.rm = TRUE))) %>%
      mutate(employee = factor(employee)) %>%
      ggplot(aes(x = year, y = d_relief_donate, group = employee)) +
      geom_point(aes(shape = employee), color = "black", size = 4) +
      geom_line(aes(linetype = employee)) +
      geom_vline(aes(xintercept = 2013.5), linetype = 3) +
      scale_x_continuous(breaks = seq(2010, 2018, 1)) +
      labs(
        title = "Panel B. Grouped by Wage Earner or Not",
        x = "Year",
        y = "Proportion of application for tax relief",
        shape = "",
        linetype = ""
      ) +
      ggtemp(size = list(axis_title = 15, axis_text = 13, title = 13))
    
    plot1 + plot2
  }
))

FirstPrice <- R6::R6Class("FirstPrice",
  public = list(
    stage1_mod = effective ~ applicable + ..stage2,
    fe2sls_mod = list(
      outcome ~ applicable + ..stage2,
      outcome ~ effective + ..stage2,
      outcome ~ ..stage2 | effective ~ applicable
    ),
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
        mutate(fit = map(data, ~ feols(self$stage1_mod, data = ., cluster = ~hhid))) %>%
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
        kable_styling(font_size = 8) %>%
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
    main_reg = function(data, note) {
      fit <- data %>%
        group_by(type) %>%
        nest() %>%
        mutate(
          mu = map_dbl(data, ~ with(., mean(outcome, na.rm = TRUE))),
          fit_mod1 = map(data, ~ feols(fp$fe2sls_mod[[1]], data = ., vcov = ~hhid)),
          fit_mod2 = map(data, ~ feols(fp$fe2sls_mod[[2]], data = ., vcov = ~hhid)),
          fit_mod3 = map(data, ~ feols(fp$fe2sls_mod[[3]], data = ., vcov = ~hhid))
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
          gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
          stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
          add_rows = addtab,
          output = "latex",
          escape = FALSE
        ) %>%
        kable_styling(font_size = 8) %>%
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
