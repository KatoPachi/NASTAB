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
estdf <- readr::read_csv(
  here("data/shaped2_propensity.csv"),
  guess_max = 30000
)

fixest::setFixest_fml(
  ..stage2 = ~ linc_ln + sqage + hh_num + have_dependents |
    year + pid + indust + area
)

#+ int-anatomy
est_anatomy_int <- estdf %>%
  dplyr::filter(d_donate == 1) %>%
  mutate(
    applicable = price_ln,
    effective = d_relief_donate * price_ln,
    iv = employee * price_ln
  ) %>%
  pivot_longer(applicable:iv, names_to = "outcome", values_to = "val") %>%
  group_by(outcome) %>%
  do(est = feols(val ~ ..stage2, data = .)) %>%
  pull(est, name = outcome)

int_anatomy_df <- estdf %>%
  dplyr::filter(d_donate == 1) %>%
  modelr::add_residuals(est_anatomy_int[[1]], var = "applicable") %>%
  modelr::add_residuals(est_anatomy_int[[2]], var = "effective") %>%
  modelr::add_residuals(est_anatomy_int[[3]], var = "iv")

est_anatomy_resid1 <- int_anatomy_df %>%
  feols(donate_ln ~ resid1, data = ., cluster = ~ pid) %>%
  tidy() %>%
  {
    sprintf("Slope = %1.3f (std.err = %1.3f)", .$estimate[2], .$std.error[2])
  }

est_anatomy_resid2 <- int_anatomy_df %>%
  feols(donate_ln ~ resid2, data = ., cluster = ~pid) %>%
  tidy() %>%
  {
    sprintf("Slope = %1.3f (std.err = %1.3f)", .$estimate[2], .$std.error[2])
  }

plot_int_resid1 <- int_anatomy_df %>%
  dplyr::filter(!is.na(resid1) & !is.na(donate_ln)) %>%
  mutate(group = ntile(resid1, 40)) %>%
  group_by(group) %>%
  summarize(
    min_resid1 = min(resid1),
    max_resid1 = max(resid1),
    resid1 = min_resid1 + (max_resid1 - min_resid1) / 2,
    donate_ln = mean(donate_ln),
    d_relief_donate = mean(d_relief_donate, na.rm = TRUE),
    n = n()
  ) %>%
  ggplot(aes(x = resid1, y = donate_ln)) +
  geom_point(aes(size = d_relief_donate), shape = 1) +
  geom_smooth(
    method = "lm", data = int_anatomy_df,
    se = FALSE, color = "black"
  ) +
  annotate(
    geom = "text",
    x = -0.12, y = 4.1, label = est_anatomy_resid1,
    size = 5
  ) +
  scale_size(range = c(.1, 15)) +
  labs(
    x = "Residuals of log(first price)",
    y = "log(donate) conditional on givers",
    size = "Application of tax relief"
  ) +
  ggtemp()
  
plot_int_resid2 <- int_anatomy_df %>%
  dplyr::filter(!is.na(resid2) & !is.na(donate_ln)) %>%
  mutate(group = ntile(resid2, 40)) %>%
  group_by(group) %>%
  summarize(
    min_resid2 = min(resid2),
    max_resid2 = max(resid2),
    resid2 = min_resid2 + (max_resid2 - min_resid2) / 2,
    donate_ln = mean(donate_ln),
    d_relief_donate = mean(d_relief_donate, na.rm = TRUE),
    n = n()
  ) %>%
  ggplot(aes(x = resid2, y = donate_ln)) +
  geom_point(aes(size = d_relief_donate), shape = 1) +
  geom_smooth(
    method = "lm", data = int_anatomy_df,
    se = FALSE, color = "black"
  ) +
  annotate(
    geom = "text",
    x = -0.12, y = 4.1, label = est_anatomy_resid2,
    size = 5
  ) +
  scale_size(range = c(3, 15)) +
  labs(
    x = "Residuals of log(first price)\u00d7application",
    y = "log(donate) conditional on givers",
    size = "Application of tax relief"
  ) +
  ggtemp()

list(plot_int_resid1, plot_int_resid2) %>%
  wrap_plots() +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

#'
#+ ext-anatomy
ext_anatomy_df <- estdf %>%
  mutate(
    applicable = price_ln,
    effective = d_relief_donate * price_ln
  )

est_ext_resid1 <- feols(applicable ~ ..stage2, data = ext_anatomy_df)
est_ext_resid2 <- feols(effective ~ ..stage2, data = ext_anatomy_df)

ext_anatomy_df <- ext_anatomy_df %>%
  modelr::add_residuals(est_ext_resid1, var = "resid1") %>%
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