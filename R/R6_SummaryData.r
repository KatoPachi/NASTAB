library(here)
library(R6)
library(tidyverse)
library(rlang)
library(patchwork)
library(modelsummary)
library(kableExtra)
source(here("R/misc.r"))

SummaryData <- R6::R6Class("SummaryData", list(
  data = NULL,
  initialize = function(data) self$data <- data,
  stats = function(title = "", label = "", notes = "", font_size = 8) {
    if (label != "") label <- paste0("\\label{tab:", label, "}")

    self$data %>%
      datasummary(
        (`Annual after-tax income (unit: 10,000KRW)` = after_tax_tinc) +
          (`Appricale price` = price) +
          (`Annual chariatable giving (unit: 10,000KRW)` = donate) +
          (`Dummary of donation $>$ 0` = d_donate) +
          (`Dummy of declaration of giving` = d_relief_donate) +
          (`Age` = age) +
          (`Wage earner dummy` = employee) +
          (`Number of household members` = hhnum) +
          (`Number of children` = hhnum_child) +
          (`Number of dependents in household` = dependent_num) +
          (`Number of taxpayers in household` = payer_num) +
          (`Female dummy` = female) +
          (`Academic history: University` = college) +
          (`Academic history: High school` = highschool) ~
          N +
          Mean +
          (`Std.Dev.` = SD) +
          Min +
          Median +
          Max,
        title = paste0(title, label),
        data = .,
        align = "lcccccc",
        escape = FALSE
      ) %>%
      kable_styling(font_size = font_size) %>%
      pack_rows("Income and giving price", 1, 2, bold = FALSE, italic = TRUE) %>%
      pack_rows("Charitable giving", 3, 5, bold = FALSE, italic = TRUE) %>%
      pack_rows("Demographics", 6, 14, bold = FALSE, italic = TRUE) %>%
      footnote(
        general_title = "",
        general = notes,
        threeparttable = TRUE,
        escape = FALSE
      )
  },
  income_dist = function() {
    self$data %>%
      filter(year == 2013) %>%
      ggplot(aes(x = taxable_tinc)) +
      geom_hline(aes(yintercept = 0)) +
      geom_histogram(
        aes(y = after_stat(count) / sum(after_stat(count)), fill = "Relative frequency of observations"),
        color = "black"
      ) +
      geom_step(
        aes(y = price * 0.5, color = "Prices of Giving in 2010-2013"),
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
        sec.axis = sec_axis(~ . / 0.5, name = "Prices of Giving")
      ) +
      scale_x_continuous(breaks = c(1200, 4600, 8800, 30000)) +
      labs(
        x = "Annual taxable income (10,000KRW)",
        y = "Relative frequency of observations"
      ) +
      ggtemp(size = list(axis_title = 15, axis_text = 13, caption = 13))
  },
  ts_giving = function() {
    plot_data <- self$data %>%
      dplyr::filter(!is.na(bracket13)) %>%
      mutate(bracket13 = if_else(
        str_detect(bracket13, "C|D|E"),
        "(C), (D) and (E) [4600, 30000)",
        bracket13
      )) %>%
      group_by(year, bracket13) %>%
      summarize(
        amount = mean(donate, na.rm = TRUE),
        donor = mean(d_donate * 100, na.rm = TRUE)
      ) %>%
      pivot_longer(amount:donor, names_to = "vars", values_to = "mu") %>%
      pivot_wider(names_from = "year", values_from = "mu") %>%
      mutate(base = `2013`) %>%
      select(vars, bracket13, base, everything()) %>%
      pivot_longer(-(vars:base), names_to = "year", values_to = "mu") %>%
      mutate(
        mu = mu - base,
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
      scale_y_continuous(breaks = seq(-30, 20, by = 10), limits = c(-30, 20)) +
      labs(
        title = "Panel A. Amount of Giving",
        x = "Year",
        y = "Normalized average giving\n(unit: 10,000KRW)",
        shape = "Income bracket (unit: 10,000KRW)"
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
      scale_y_continuous(
        breaks = seq(-30, 10, by = 10),
        labels = c(-30, -20, -10, 0, 10),
        limits = c(-30, 10)
      ) +
      labs(
        title = "Panel B. Proportion of Donors",
        x = "Year",
        y = "Normalized proportion of donors\n(unit: percentage point)",
        shape = "Income bracket (unit: 10,000KRW)"
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

    dta %>%
      dplyr::filter(!is.na(bracket13)) %>%
      mutate(bracket13 = if_else(
        str_detect(bracket13, "C|D|E"),
        "(C), (D) and (E) [4600, 30000)",
        bracket13
      )) %>%
      group_by(year, bracket13) %>%
      summarize(mu = mean(d_relief_donate, na.rm = TRUE)) %>%
      ggplot(aes(x = year, y = mu, group = bracket13)) +
      geom_vline(aes(xintercept = 2013.5), linetype = 3) +
      geom_point(aes(shape = bracket13), size = 4) +
      geom_line() +
      scale_shape_manual(values = c(16, 15, 17, 18)) +
      scale_x_continuous(breaks = seq(2010, 2018, 1)) +
      labs(
        x = "Year",
        y = "Proportion of claimants",
        shape = "Income bracket (unit:10,000KRW)"
      ) +
      ggtemp(size = list(axis_title = 15, axis_text = 13, title = 13)) +
      guides(shape = guide_legend(title.position = "top", title.hjust = 0.5))
  },
  event_study = function(include_pid = TRUE) {
    setFixest_fml(
      ..event_study = ~ after_tax_tinc_ln + sqage +
        hhnum + hhnum_child + dependent_num +
        hh_max_inc + I(family_position == 1) +
        factor(indust) + factor(area)
    )

    dt <- self$data %>%
      mutate(
        low_bracket = if_else(str_detect(bracket13, "A"), 1, 0),
        high_bracket = if_else(str_detect(bracket13, "C|D|E|F|G"), 1, 0),
        year = factor(year, levels = c(2013, 2010:2012, 2014:2017))
      )

    if (sum(dt$experience_FG) == 0) {
      bracket_label <- "(C), (D) and (E)"
    } else {
      bracket_label <- "(C), (D), (E), (F) and (G)"
    }

    if (include_pid) {
      mods <- list(
        donate ~ low_bracket * year + high_bracket * year + ..event_study | pid,
        d_donate ~ low_bracket * year + high_bracket * year + ..event_study | pid
      )
    } else {
      mods <- list(
        donate ~ low_bracket * year + high_bracket * year + ..event_study,
        d_donate ~ low_bracket * year + high_bracket * year + ..event_study
      )
    }

    est_mods <- mods %>%
      map(~ feols(., data = dt, vcov = ~hhid)) %>%
      map(broom::tidy) %>%
      map(~ subset(., str_detect(term, "bracket"))) %>%
      map(~ mutate(., high = str_detect(term, "high"))) %>%
      map(~ mutate(., year = str_extract(term, "(?<=year)[:digit:]+"))) %>%
      map(~ mutate(., year = as.numeric(year))) %>%
      map(~ bind_rows(., tibble(
        estimate = rep(0, 2),
        std.error = rep(0, 2),
        high = c(TRUE, FALSE),
        year = rep(2013, 2)
      ))) %>%
      map(~ mutate(., high = factor(high, labels = c("(A)", bracket_label)))) %>%
      map(~ mutate(., ci_lb = estimate - 1.96 * std.error)) %>%
      map(~ mutate(., ci_ub = estimate + 1.96 * std.error))

    panel <- 1:2 %>%
      map(function(i) {
        est_mods[[i]] %>%
          ggplot(aes(
            x = year,
            y = estimate,
            color = high,
            shape = high,
            fill = high,
            linetype = high
          )) +
          geom_point(size = 4) +
          geom_line() +
          geom_ribbon(aes(ymin = ci_lb, ymax = ci_ub), alpha = 0.2) +
          scale_fill_manual(values = c("grey20", "grey50")) +
          scale_color_manual(values = c("grey20", "grey50")) +
          scale_x_continuous(breaks = seq(2010, 2017, 1)) +
          labs(
            title = ifelse(
              i == 1,
              "Panel A. Amount of Giving",
              "Panel B. Proportion of Donors"
            ),
            x = "Year",
            y = "Estimate (95%CI)",
            color = "Income bracket",
            shape = "Income bracket",
            fill = "Income bracket",
            linetype = "Income bracket"
          ) +
          ggtemp(size = list(axis_title = 15, axis_text = 13, caption = 13))
      })

    wrap_plots(panel) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom")
  }
))