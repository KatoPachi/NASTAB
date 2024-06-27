# * list of packages used in this source file
library(tidyverse)
library(fixest)

# * Subsample: donut-hole sample (drop samples near threshold)
donut_hole <- function(data, cut = 100) {
  dt <- data
  x <- cut

  dt <- dt %>%
    dplyr::filter(taxable_tinc < 1200 - x | 1200 + x < taxable_tinc) %>%
    dplyr::filter(taxable_tinc < 4600 - x | 4600 + x < taxable_tinc) %>%
    dplyr::filter(taxable_tinc < 8800 - x | 8800 + x < taxable_tinc) %>%
    dplyr::filter(taxable_tinc < 15000 - x | 15000 + x < taxable_tinc) %>%
    dplyr::filter(taxable_tinc < 30000 - x | 30000 + x < taxable_tinc) %>%
    dplyr::filter(taxable_tinc < 50000 - x | 50000 + x < taxable_tinc)

  return(dt)
}

# * Subsample: wage-earner samples
employee <- function(data) {
  dt <- subset(data, employee == 1)
  return(dt)
}

# * Subsample: remove highest bracket (bracket F & G)
remove_highest_bracket <- function(data) {
  dt <- data %>%
  group_by(pid) %>%
  mutate(sum_experience_FG = sum(experience_FG)) %>%
  ungroup() %>%
  dplyr::filter(sum_experience_FG == 0)

  return(dt)
}

# * Subsample: remove bracket shifter
remove_bracket_shift <- function(data) {
  dt <- data %>%
    group_by(pid) %>%
    arrange(year) %>%
    mutate(bracket_shift = bracket != lag(bracket)) %>%
    ungroup()

  ident_shift <- dt %>%
    dplyr::filter(year %in% 2010:2014) %>%
    group_by(pid) %>%
    mutate(sum_bracket_shift = sum(bracket_shift, na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::filter(sum_bracket_shift > 0)

  id_removed <- unique(ident_shift$pid)

  cat("In total, there are", length(id_removed), "unique shifters.\n")

  dt2 <- subset(dt, !(pid %in% id_removed))

  return(dt2)
}

# * Subsample: Set upper bound of donation
set_donate_bound <- function(data) {
  dt <- subset(data, over_limit_incentive == 0)
  return(dt)
}

# * Subsample: Limit data to two-years data
two_years_data <- function(data) {
  dt <- subset(data, year == 2012 | year == 2015)
  return(dt)
}

# * Subsample: Drop one year around tax reform
remove_around_reform <- function(data) {
  dt <- subset(data, year < 2013 | 2014 < year)
  return(dt)
}

# * Subsample: Drop those who require year-end settlement
remove_exempt <- function(data) {
  dt <- subset(data, !(no_withholding == 1 & tinc == linc))
  return(dt)
}

# * Variable: Logged income
log_inc <- function(data, add_base_inc = 0) {
  y <- add_base_inc

  dt <- data %>%
    mutate(
      linc_ln = log(linc + y),
      tinc_ln = log(tinc + y),
      taxable_tinc_ln = log(taxable_tinc + y),
      after_tax_tinc_ln = log(after_tax_tinc + y)
    )

  return(dt)
}

# * Calculate implied price elasticity and create output format (extensive-margin)
implied_e <- function(fit, mu, digits = 3) {
  tbl <- broom::tidy(fit)
  elasticity <- subset(tbl, str_detect(term, "effective|applicable"))
  b <- elasticity$estimate / mu
  se <- elasticity$std.error / mu
  p <- elasticity$p.value

  num_format <- paste0("\\num{%1.", digits, "f}")

  b_show <- case_when(
    p < 0.01 ~ sprintf(paste0(num_format, "***"), b),
    p < 0.05 ~ sprintf(paste0(num_format, "**"), b),
    p < 0.1 ~ sprintf(paste0(num_format, "*"), b),
    TRUE ~ sprintf(paste0(num_format, ""), b)
  )

  se_show <- sprintf(paste0("(", num_format, ")"), se)

  tibble(estimate = b_show, estimate_se = se_show)
}

# * Get test statistics from fixest
get_fitstat <- function(model, type, stat = NULL) {
  test <- unlist(fitstat(model, type, simplify = TRUE))
  if (is.null(stat)) {
    test
  } else {
    test[stat]
  }
}

# * template of ggplot2
ggtemp <- function( flip = FALSE,
                    family = NULL,
                    size = list(
                      axis_title = 13,
                      axis_text = 9,
                      title = 20,
                      caption = 11
                    ),
                    legend_key_size = 1)
{
  my_theme <- theme_minimal(base_family = family) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text = element_text(
        color = "black", size = size$axis_text, family = family
      ),
      axis.title = element_text(size = size$axis_title, family = family),
      axis.ticks.length = unit(0.25, "cm"),
      axis.ticks.x = element_line(),
      axis.ticks.y = element_line(),
      axis.line = element_line(),
      legend.text = element_text(size = size$text, family = family),
      legend.key.size = unit(legend_key_size, "cm"),
      legend.title = ggplot2::element_text(size = size$title),
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(size = size$caption),
      plot.title = element_text(size = size$title)
    )

  if (flip) {
    my_theme <- my_theme +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line()
      )
  }

  return(my_theme)
}
