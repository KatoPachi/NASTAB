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
  stats = function() {
    self$data %>%
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
    plot_data <- self$data %>%
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
        title = "Panel B. Proportion of Donors",
        x = "Year",
        y = "Normalized proportion of donors",
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

    dta %>%
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
        x = "Year",
        y = "Proportion of application for tax relief",
        shape = "Income bracket (unit:10,000KRW)"
      ) +
      ggtemp(size = list(axis_title = 15, axis_text = 13, title = 13)) +
      guides(shape = guide_legend(title.position = "top", title.hjust = 0.5))
  }
))