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
library(here)
source(here("R", "_html_header.r"))

#+ include = FALSE
source(here("R", "_library.r"))

#'
#+ include = FALSE
estdf <- readr::read_csv(here("data/shaped2_propensity.csv"), guess_max = 30000)

#'
#' ```{asis, echo = output_type() == "slide"}
#' ## Expectated Results (1)
#'
#' - The larger the donation, the greater the tax savings
#' - Claim and the error term in the estimated model
#' should be positively correlated
#' - Thus, we expect the elasticity estimated from the FE model
#' to be more elastic than the elasticity estimated from the FE-2SLS model
#'   <!-- - Extensive-margin price elasticity is in line with our conjecture. -->
#'   <!-- - Intensive-margin price elasticity contradicts our expectation.  -->
#'
#' ## Expected Results (2)
#'
#' If we assume that price elasticity is heterogenous among tax-payers,
#' FE-2SLS estimates average price elasticity of
#' those who change their application behavior
#' depending on the value of IV (LATE).
#'
#' ## About LATE
#'
#' Let $R_{it}(z)$ be a dummy of application if $Z_{it} = z$.
#' Then, assuming monotonicity ($R_{it}(1) \ge R_{it}(0)$),
#' we classify tax-payers into three groups:
#'
#' 1. Never declarer: $R_{it}(1) = R_{it}(0) = 0$
#' 1. Always declarer: $R_{it}(1) = R_{it}(0) = 1$
#' 1. Start declarer: $R_{it}(1) =1, R_{it}(0) = 0$
#'
#' Price elasticity estimated by FE-2SLS is
#' average price elasticity among start declarers.
#'
#' ## Fraction of Applicants Type: Abadie's Theorem
#'
#' Abadie (2003) shows
#'
#' $$ Pr[R_{it}(1) =1, R_{it}(0) = 0] = E(\kappa_{it}) $$
#'
#' where
#'
#' $$
#' \kappa_{it} = 1
#' - \frac{R_{it}(1 - Z_{it})}{1 - P(Z_{it} = 1|X_{it})}
#' - \frac{(1 - R_{it})Z_{it}}{P(Z_{it} = 1|X_{it})}
#' $$
#'
#' ## Fraction of Applicants Type: Results
#' ```
#'
#+ kappa-intensive, include = FALSE
intdf <- subset(estdf, d_donate == 1)

# estimate P(Z = 1|X) and P(Z = 1|X, Y)
fixest::setFixest_fml(
  ..cov = ~ linc_ln + sqage + hh_num + have_dependents |
    year + pid + indust + area
)
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

#+ kappa-extensive, include = FALSE
# estimate P(Z = 1|X) and P(Z = 1|X, Y)
est_z_mod <- feglm(
  z_mod, data = estdf,
  family = binomial(link = "probit"),
  fixef.rm = "none"
)

# add Abadie's kappa
extdf <- estdf %>%
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
    align = "lcc",
    escape = FALSE
  )

#' ```{asis, echo = output_type() == "slide"}
#' ## Expected Price Elasticity of Three Types (1)
#'
#' - Never declarer
#'   - Since their giving price is always 1,
#'   intensive- and extensive-margin (within) price elasticity is infinite.
#' - Always declarer
#'   - Since they always donate,
#'   extensive-margin (within) price elasticity is zero.
#'   - Intensive-margin elasticity estimated by FE-2SLS
#'   is slightly less elastic than estimated by only applicants data.
#'   - Under monotonicity assumption, only applicants data includes
#'   always declarer and start declarer.
#'   - intensive-margin price elasticity among always declarers
#'   is slightly less elastic than among start declarers
#'
#' ## Expected Price Elasticity of Three Types (2)
#'
#' Relationship of price elasticity
#' (in terms of absolute term) among three types:
#'
#' $$ \text{Always} < \text{Start} < \text{Never} $$
#'
#' - We expect intensive-margin price elasticity estimated by FE
#' outweigh always declarer and start declarer
#'   - FE < FE-2SLS (in terms of absolute value)
#' - We expect extensive-margin price elasticity estimated by FE
#' outweigh start declarer and never declarer
#'   - FE-2SLS < FE (in terms of absolute value)
#'
#' ```
#' 