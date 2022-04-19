#' ---
#' title: |
#'   Estimating Conventional Price Elasticity of Charitable Giving (1)
#' author: Hiroki Kato
#' bibliography: ../Rmarkdown/reference.bib
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
source("script/_html_header.r")
source("script/_libs.r")

#'
#+ include = FALSE
book <- readr::read_csv("data/codebook/shaped2_description.csv"); View(book)
df <- readr::read_csv("data/shaped2.csv")

#'
#+ kappa-intensive
intdf <- subset(df, d_donate == 1)

# estimate P(Z = 1|X) and P(Z = 1|X, Y)
fixest::setFixest_fml(..cov = ~ linc_ln + sqage | year + pid + indust + area)
z_mod <- employee ~ price_ln + ..cov
est_z_mod <- feglm(
  z_mod, data = intdf,
  family = binomial(link = "probit"),
  fixef.rm = "none"
)

# add Abadie's kappa
intdf <- intdf %>%
  modelr::add_predictions(est_z_mod) %>%
  mutate(
    kappa = 1 -
      ((d_relief_donate * (1 - employee)) / (1 - pred)) -
      (((1 - d_relief_donate) * employee) / pred),
    kappa = case_when(
      kappa < 0 | is.nan(kappa) ~ 0,
      kappa <= 1 ~ kappa,
      1 < kappa ~ 1
    )
  )

tab_int_data <- intdf %>%
  dplyr::filter(kappa < 1) %>%
  summarize(
    always = mean(d_relief_donate, na.rm = TRUE) *
      mean(intdf$kappa < 1, na.rm = TRUE),
    never = mean(1 - d_relief_donate, na.rm = TRUE) *
      mean(intdf$kappa < 1, na.rm = TRUE)
  ) %>%
  pivot_longer(always:never, names_to = "type", values_to = "prop") %>%
  bind_rows(tibble(type = "start", prop = mean(intdf$kappa, na.rm = TRUE)))

#+ kappa-extensive
# estimate P(Z = 1|X) and P(Z = 1|X, Y)
est_z_mod <- feglm(
  z_mod, data = df,
  family = binomial(link = "probit"),
  fixef.rm = "none"
)

# add Abadie's kappa
extdf <- df %>%
  modelr::add_predictions(est_z_mod) %>%
  mutate(
    kappa = 1 -
      ((d_relief_donate * (1 - employee)) / (1 - pred)) -
      (((1 - d_relief_donate) * employee) / pred),
    kappa = case_when(
      kappa < 0 | is.nan(kappa) ~ 0,
      kappa <= 1 ~ kappa,
      1 < kappa ~ 1
    )
  )

tab_ext_data <- extdf %>%
  dplyr::filter(kappa < 1) %>%
  summarize(
    always = mean(d_relief_donate, na.rm = TRUE) *
      mean(extdf$kappa < 1, na.rm = TRUE),
    never = mean(1 - d_relief_donate, na.rm = TRUE) *
      mean(extdf$kappa < 1, na.rm = TRUE)
  ) %>%
  pivot_longer(always:never, names_to = "type", values_to = "prop") %>%
  bind_rows(tibble(type = "start", prop = mean(extdf$kappa, na.rm = TRUE)))

#+ kappa-result
bind_cols(tab_int_data, c(outcome = "intensive")) %>%
  bind_rows(bind_cols(tab_ext_data, c(outcome = "extensive"))) %>%
  rename(outcome = "...3") %>%
  mutate(prop = 100 * prop) %>%
  mutate(
    type = recode(
      type,
      "always" = "$R_{it}(1) = R_{it}(0) = 1$",
      "never" = "$R_{it}(1) = R_{it}(0) = 0$",
      "start" = "$R_{it}(1) =1, R_{it}(0) = 0$"
    )
  ) %>%
  datasummary(
    (`Type of applicant` = type) ~
      (` ` = mean) * (` ` = prop) * outcome,
    data = .,
    align = "lcc"
  )
