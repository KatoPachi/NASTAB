#' ---
#' title: |
#'   Estimating Conventional Price Elasticity of Charitable Giving (1)
#' author: Hiroki Kato
#' output:
#'   bookdown::html_document2:
#'     toc: true
#'     toc_float: true
#'     number_sections: false
#' params:
#'   preview: true
#' ---
#'
#+ include = FALSE, eval = params$preview
library(here)
source(here("R", "_library.r"))

#+ include = FALSE
source(here("R", "_html_header.r"))

#+ include = FALSE
rawdt <- readr::read_csv(
  here("data/shaped2_propensity.csv"),
  guess_max = 30000
)

use <- rawdt %>%
  mutate(
    applicable = price_ln,
    effective = d_relief_donate * price_ln,
    iv = employee * price_ln
  ) %>%
  pivot_longer(applicable:iv, names_to = "outcome", values_to = "value")

#+ include = FALSE
fixest::setFixest_fml(
  ..stage2 = ~ linc_ln + sqage + hh_num + have_dependents |
    year + pid + indust + area
)

#+ int-anatomy
reg_price_int <- use %>%
  dplyr::filter(d_donate == 1) %>%
  group_by(outcome) %>%
  do(est = feols(value ~ ..stage2, data = .)) %>%
  pull(est, name = outcome)

int_use <- use %>%
  dplyr::filter(d_donate == 1) %>%
  modelr::add_residuals(reg_price_int$applicable, var = "applicable") %>%
  modelr::add_residuals(reg_price_int$effective, var = "effective") %>%
  modelr::add_residuals(reg_price_int$iv, var = "iv") %>%
  mutate(
    residual = case_when(
      outcome == "applicable" ~ applicable,
      outcome == "effective" ~ effective,
      outcome == "iv" ~ iv
    )
  ) %>%
  select(-applicable, -effective, -iv)

reg_anatomy_int <- int_use %>%
  group_by(outcome) %>%
  do(
    est = feols(donate_ln ~ residual, data = ., cluster = ~ pid) %>% tidy()
  ) %>%
  summarize(
    price = outcome,
    label = sprintf(
      "Slope = %1.3f (s.e. = %1.3f)",
      est$estimate[2], est$std.error[2]
    )
  )

#+
int_use %>%
  dplyr::filter(outcome == "applicable") %>%
  dplyr::filter(!is.na(residual) & !is.na(donate_ln)) %>%
  group_by(d_relief_donate) %>%
  mutate(group = ntile(residual, 30)) %>%
  group_by(d_relief_donate, group) %>%
  summarize(
    min_residual = min(residual),
    max_residual = max(residual),
    residual = min_residual + (max_residual - min_residual) / 2,
    donate_ln = mean(donate_ln)
  ) %>%
  mutate(d_relief_donate = factor(
    d_relief_donate,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )) %>%
  ggplot(aes(x = residual, y = donate_ln, shape = d_relief_donate)) +
  geom_point(aes(shape = d_relief_donate), size = 5, color = "grey50") +
  geom_smooth(
    method = "lm", data = subset(int_use, outcome == "applicable"),
    se = FALSE, color = "black"
  ) +
  annotate(
    geom = "text",
    x = -0.12, y = 4.1, label = reg_anatomy_int[1, ]$label,
    size = 5
  ) +
  labs(
    x = "Residuals of log(first price)",
    y = "log(donate) conditional on givers",
    color = "Application of tax relief"
  ) +
  ggtemp()

#+
int_use %>%
  dplyr::filter(outcome == "effective") %>%
  dplyr::filter(!is.na(residual) & !is.na(donate_ln)) %>%
  group_by(d_relief_donate) %>%
  mutate(group = ntile(residual, 30)) %>%
  group_by(d_relief_donate, group) %>%
  summarize(
    min_residual = min(residual),
    max_residual = max(residual),
    residual = min_residual + (max_residual - min_residual) / 2,
    donate_ln = mean(donate_ln)
  ) %>%
  mutate(d_relief_donate = factor(
    d_relief_donate,
    levels = c(0, 1),
    labels = c("No", "Yes")
  )) %>%
  ggplot(aes(x = residual, y = donate_ln)) +
  geom_point(aes(shape = d_relief_donate), size = 5, color = "grey50") +
  geom_smooth(
    method = "lm", data = subset(int_use, outcome == "effective"),
    se = FALSE, color = "black"
  ) +
  annotate(
    geom = "text",
    x = -0.15, y = 4.4, label = reg_anatomy_int[2, ]$label,
    size = 5
  ) +
  labs(
    x = "Residuals of log(first price)\u00d7application",
    y = "log(donate) conditional on givers",
    size = "Application of tax relief"
  ) +
  ggtemp()

#'
#+ ext-anatomy
ext_anatomy_df <- estdf %>%
  mutate(
    applicable = price_ln,
    effective = d_relief_donate * price_ln
  )

est_ext_residual <- feols(applicable ~ ..stage2, data = ext_anatomy_df)
est_ext_resid2 <- feols(effective ~ ..stage2, data = ext_anatomy_df)

ext_anatomy_df <- ext_anatomy_df %>%
  modelr::add_residuals(est_ext_residual, var = "residual") %>%
  modelr::add_residuals(est_ext_resid2, var = "resid2")

ext_anatomy_df %>%
  mutate(d_relief_donate = factor(
    d_relief_donate,
    levels = c(0, 1),
    labels = c("Not", "Yes")
  )) %>%
  ggplot(aes(x = resid2, y = d_donate, color = d_relief_donate)) +
    geom_point(size = 3) +
    geom_smooth(se = FALSE, method = "lm", color = "black") +
    ggtemp()

# /*
#+
rmarkdown::render(
  here("R", "3-main-estimation.r"),
  output_dir = here("docs", "html-preview")
)
# */