library(here)
library(R6)
library(kableExtra)

PolicyEffect <- R6::R6Class("PolicyEffect",
  public = list(
    data = NULL,
    initialize = function(data) {
      self$data <- data %>%
        group_by(pid) %>%
        arrange(year) %>%
        mutate(lead_claim = if_else(year - lead(year) > 1, NA_real_, lead(d_relief_donate))) %>%
        ungroup() %>%
        dplyr::filter(year == 2013)
    },
    applicable = function(e,
                          title = "",
                          label = "",
                          notes = "",
                          font_size = 8)
    {
      stats <- private$calc_applicable(e)

      stats %>%
        knitr::kable(
          caption = title,
          label = label,
          col.names = c("", paste0("(", seq_len(ncol(stats)-1), ")")),
          digits = 2,
          booktabs = TRUE,
          linesep = "",
          align = "lccccc"
        ) %>%
        kable_styling(font_size = font_size) %>%
        column_spec(1, width = "10em") %>%
        column_spec(3:6, width = "4em") %>%
        add_header_above(c(
          "2013 Income bracket" = 1,
          "N" = 1,
          "2013 average" = 1,
          "Change (%)" = 1,
          "2013 average" = 1,
          "Change (%)" = 1
        )) %>%
        add_header_above(c(
          " " = 2,
          "Simulated price" = 2,
          "Donations (unit: 10,000KRW)" = 2
        )) %>%
        footnote(
          general_title = "",
          general = notes,
          threeparttable = TRUE,
          escape = FALSE
        )
    },
    effective = function( e,
                          title = "",
                          label = "",
                          notes = "",
                          font_size = 8) {
      stats <- private$calc_effective(e)

      stats %>%
        knitr::kable(
          caption = title,
          label = label,
          col.names = c("", paste0("(", seq_len(ncol(stats)-1), ")")),
          digits = 3,
          booktabs = TRUE,
          linesep = "",
          align = "lcccccccc"
        ) %>%
        kable_styling(font_size = font_size) %>%
        column_spec(1, width = "10em") %>%
        add_header_above(c(
          "2013 Income bracket" = 1,
          "N" = 1,
          "2013" = 1,
          "2014" = 1,
          "2013" = 1,
          "2014" = 1,
          "Change (%)" = 1,
          "2013 average" = 1,
          "Change (%)" = 1
        )) %>%
        add_header_above(c(
          " " = 2,
          "Declaration (%)" = 2,
          "Actual price" = 3,
          "Donations (unit: 10,000KRW)" = 2
        )) %>%
        footnote(
          general_title = "",
          general = notes,
          threeparttable = TRUE,
          escape = FALSE
        )
    }
  ),
  private = list(
    calc_applicable = function(e) {
      stats <- self$data %>%
        group_by(bracket13) %>%
        nest() %>%
        mutate(
          N = map_dbl(data, ~ nrow(.)),
          donation = map_dbl(data, ~ mean(.$donate, na.rm = TRUE)),
          price = map_dbl(data, ~ mean(.$price, na.rm = TRUE))
        ) %>%
        select(-data) %>%
        mutate(
          change_price = 100 * (0.85 - price) / price,
          change_donation = e * change_price
        ) %>%
        ungroup() %>%
        arrange(bracket13) %>%
        select(bracket13, N, price, change_price, donation, change_donation)

      wtg_avg <- stats %>%
        {
          weight <- .$N / sum(.$N)
          tibble(
            bracket13 = "Weighted average",
            change_price = sum(weight * .$change_price),
            change_donation = sum(weight * .$change_donation)
          )
        }

      bind_rows(stats, wtg_avg)
    },
    calc_effective = function(e) {
      stats <- self$data %>%
        dplyr::filter(year == 2013) %>%
        dplyr::filter(!is.na(d_relief_donate) & !is.na(lead_claim)) %>%
        group_by(bracket13, d_relief_donate, lead_claim) %>%
        nest() %>%
        mutate(
          N = map_dbl(data, ~ nrow(.)),
          donation = map_dbl(data, ~ mean(.$donate, na.rm = TRUE)),
          price = map_dbl(data, ~ mean(.$price, na.rm = TRUE)),
          price = if_else(d_relief_donate == 1, price, 1),
          post_price = if_else(lead_claim == 1, 0.85, 1),
          change_price = 100 * (post_price - price) / price,
          change_donation = e * change_price
        ) %>%
        select(-data) %>%
        ungroup() %>%
        group_by(bracket13) %>%
        mutate(weight = N / sum(N)) %>%
        arrange(bracket13, desc(weight))

      bracket_summary <- stats %>%
        summarize(
          N = sum(N),
          claim = 100 * weighted.mean(d_relief_donate, weight),
          post_claim = 100 * weighted.mean(lead_claim, weight),
          price = weighted.mean(price, weight),
          post_price = weighted.mean(post_price, weight),
          change_price = weighted.mean(change_price, weight),
          donation = weighted.mean(donation, weight),
          change_donation = weighted.mean(change_donation, weight)
        ) %>%
        ungroup()

      wtg_avg <- bracket_summary %>%
        {
          weight <- .$N / sum(.$N)
          tibble(
            bracket13 = "Weighted average",
            change_price = sum(weight * .$change_price),
            change_donation = sum(weight * .$change_donation)
          )
        }

      bind_rows(bracket_summary, wtg_avg)
    }
  )
)