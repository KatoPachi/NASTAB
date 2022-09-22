#+ include = FALSE
library(here)
source(here("R", "_library.r"))

#+ include = FALSE
use <- readr::read_csv(here("data/shaped2.csv")) %>%
  dplyr::filter(year < 2018) %>%
  dplyr::filter(dependents == 0) %>%
  dplyr::filter(tinc > donate) %>%
  select(
    pid,
    hhid,
    year,
    tinc_ln,
    sqage,
    hh_num,
    have_dependents,
    indust,
    area,
    credit_benefit,
    credit_loss,
    price_ln,
    lprice_ln,
    d_relief_donate,
    employee,
    outcome_intensive = donate_ln,
    outcome_extensive = d_donate
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
    effective = d_relief_donate * lprice_ln,
    applicable = price_ln,
    after = if_else(year >= 2014, 1, 0)
  )

#+ fe-model include = FALSE
fixest::setFixest_fml(
  ..stage2 = ~ tinc_ln + sqage + hh_num + have_dependents |
    year + pid + indust + area
)

femod <- list(
  outcome ~ credit_benefit:after + credit_loss:after + ..stage2,
  outcome ~ employee + credit_benefit:after + credit_loss:after +
    employee:credit_benefit + employee:credit_loss +
    credit_benefit:employee:after + credit_loss:employee:after + ..stage2,
  outcome ~ applicable + ..stage2,
  outcome ~ employee + applicable + applicable:employee + ..stage2,
  outcome ~ effective + ..stage2
)

est_femod <- use %>%
  mutate(type = factor(type, levels = c("intensive", "extensive"))) %>%
  group_by(type) %>%
  do(est = lapply(
    femod,
    function(x) feols(x, data = subset(., flag == 1), cluster =~hhid)
  ))

#+ fe-model-int
out.file <- file(here("export", "tables", "fe-model-int.tex"), open = "w")

tab <- subset(est_femod, type == "intensive")$est[[1]] %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = paste0(
      "Fixed Effect Model of Price Elasticity (Intensive-Margin)",
      "\\label{tab:fe-model-int}"
    ),
    coef_map = c(
      "credit_benefit:after" = "Decrease x Credit period",
      "after:credit_loss" = "Increase x Credit period",
      "applicable" = "Log applicable price",
      "effective" = "Log effective last-price",
      "employee" = "Wage earner",
      "employee:credit_benefit" = "Wage earner x Decrease",
      "employee:credit_loss" = "Wage earner x Increase",
      "employee:credit_benefit:after" =
        "Wage earner x Decrease x Credit period",
      "employee:after:credit_loss" =
        "Wage earner x Increase x Credit period",
      "employee:applicable" = "Wage earner x Log applicable price",
      "tinc_ln" = "Log income"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    output = "latex"
  ) %>%
  kableExtra::kable_styling(font_size = 8) %>%
  kableExtra::add_header_above(c(
    " " = 1, "Log donation" = 5
  )) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01.",
      "We use standard errors clustered at household level.",
      "An outcome variable is logged value of amount of charitable giving.",
      "For estimation, we use those whose amount of donation is positive.",
      "We control squared age (divided by 100), number of household members,",
      "a dummy that indicates having dependents, a set of dummies of industry,",
      "a set of dummies of residential area,",
      "and individual and time fixed effects."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(tab, out.file)
close(out.file)

#+ implied-elasticity
mu <- with(subset(use, type == "extensive"), mean(outcome))

impe_overall <- subset(est_femod, type == "extensive")$est[[1]][c(3, 5)] %>%
  purrr::map(function(x) {
    res <- subset(tidy(x), str_detect(term, "effective|applicable")) %>%
      mutate(
        estimate = estimate / mu,
        estimate = case_when(
          p.value < 0.01 ~ sprintf("%1.3f***", estimate),
          p.value < 0.05 ~ sprintf("%1.3f**", estimate),
          p.value < 0.1 ~ sprintf("%1.3f*", estimate),
          TRUE ~ sprintf("%1.3f", estimate)
        ),
        std.error = sprintf("(%1.3f)", std.error / mu)
      )

    tribble(
      ~term, ~mod,
      "Overall", res$estimate,
      "Overall se", res$std.error
    )
  })

impe_non_employee <- subset(est_femod, type == "extensive")$est[[1]][[4]] %>%
  tidy() %>%
  subset(str_detect(term, "applicable")) %>%
  subset(!str_detect(term, "employee")) %>%
  mutate(
    estimate = estimate / mu,
    estimate = case_when(
      p.value < 0.01 ~ sprintf("%1.3f***", estimate),
      p.value < 0.05 ~ sprintf("%1.3f**", estimate),
      p.value < 0.1 ~ sprintf("%1.3f*", estimate),
      TRUE ~ sprintf("%1.3f", estimate)
    ),
    std.error = sprintf("(%1.3f)", std.error / mu)
  ) %>% {
    tribble(
      ~term, ~mod,
      "Non wage earner", .$estimate,
      "Non wage earner se", .$std.error
    )
  }

impe_employee <- subset(est_femod, type == "extensive")$est[[1]][[4]] %>%
  tidy() %>%
  subset(str_detect(term, "employee|applicable")) %>%
  summarize(
    estimate = sum(estimate),
    std.error = sqrt(sum(std.error^2)),
    p.value = 2 * (pt(
      abs(estimate/std.error),
      df = attr(vcov(
        subset(est_femod, type == "extensive")$est[[1]][[4]],
        attr = TRUE
      ), "df.t"),
      lower.tail = FALSE
    ))
  ) %>%
  mutate(
    estimate = estimate / mu,
    estimate = case_when(
      p.value < 0.01 ~ sprintf("%1.3f***", estimate),
      p.value < 0.05 ~ sprintf("%1.3f**", estimate),
      p.value < 0.1 ~ sprintf("%1.3f*", estimate),
      TRUE ~ sprintf("%1.3f", estimate)
    ),
    std.error = sprintf("(%1.3f)", std.error / mu)
  ) %>% {
    tribble(
      ~term, ~mod,
      "Wage earner", .$estimate,
      "Wage earner se", .$std.error
    )
  }

impe_tab <- impe_overall[[1]] %>%
  full_join(bind_rows(impe_employee, impe_non_employee), by = "term") %>%
  left_join(impe_overall[[2]], by = "term") %>%
  right_join(
    tribble(
      ~term, ~bk1, ~bk2,
      "Overall", "", "",
      "Overall se", "", "",
      "Wage earner", "", "",
      "Wage earner se", "", "",
      "Non wage earner", "", "",
      "Non wage earner se", "", ""
    ),
    .,
    by = "term"
  ) %>%
  mutate(term = if_else(str_detect(term, "se"), "", term)) %>%
  mutate_at(vars(starts_with("mod")), list(~ifelse(is.na(.), "", .)))

attr(impe_tab, "position") <- 23:28

#+ fe-mod-ext
out.file <- file(here("export", "tables", "fe-model-ext.tex"), open = "w")

tab <- subset(est_femod, type == "extensive")$est[[1]] %>%
  setNames(paste0("(", seq(length(.)), ")")) %>%
  modelsummary(
    title = paste0(
      "Fixed Effect Model of Price Elasticity (Extensive-Margin)",
      "\\label{tab:fe-model-ext}"
    ),
    coef_map = c(
      "credit_benefit:after" = "Decrease x Credit period",
      "after:credit_loss" = "Increase x Credit period",
      "applicable" = "Log applicable price",
      "effective" = "Log effective last-price",
      "employee" = "Wage earner",
      "employee:credit_benefit" = "Wage earner x Decrease",
      "employee:credit_loss" = "Wage earner x Increase",
      "employee:credit_benefit:after" =
        "Wage earner x Decrease x Credit period",
      "employee:after:credit_loss" =
        "Wage earner x Increase x Credit period",
      "employee:applicable" = "Wage earner x Log applicable price",
      "tinc_ln" = "Log income"
    ),
    gof_omit = "R2 Pseudo|R2 Within|AIC|BIC|Log|Std|FE|R2",
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    add_rows = impe_tab,
    output = "latex"
  ) %>%
  kableExtra::kable_styling(font_size = 8) %>%
  kableExtra::add_header_above(c(
    " " = 1, "A dummy of donation" = 5
  )) %>%
  kableExtra::group_rows(
    "Implied price elasticity",
    23, 28,
    italic = TRUE, bold = FALSE
  ) %>%
  footnote(
    general_title = "",
    general = paste(
      "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01.",
      "We use standard errors clustered at household level.",
      "An outcome variable is a dummy indicating that",
      "an amount of charitable giving is positive.",
      "For estimation, we use those whose amount of donation is",
      "not only positive but also zero.",
      "We control squared age (divided by 100), number of household members,",
      "a dummy that indicates having dependents, a set of dummies of industry,",
      "a set of dummies of residential area,",
      "and individual and time fixed effects.",
      "We calculate implied price elasticities by",
      "dividing estimated coefficients by proportion of donors in our sample."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(tab, out.file)
close(out.file)