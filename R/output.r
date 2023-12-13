# Load reproduction function
library(here)
source(here("R/R6_StartAnalysis.r"))

# Create R6 class for analysis
use <- StartAnalysis$new(here("data/shaped2.csv"))

# Output options
options(
  knitr.table.format = "latex",
  knitr.kable.NA = "",
  modelsummary_stars_note = FALSE,
  modelsummary_factory_default = "latex"
)

# //NOTE: Descriptive statistics
# Table 2: Descriptive Statistics
data_summary <- use$summary()

out.file <- file(here("export", "tables", "summary-stats.tex"), open = "w")
tab <- data_summary$stats(
  title = "Descriptive Statistics",
  label = "summary-covariate",
  notes = "Notes: Our data is unbalanced panel data consisting of 8,441 unique individuals and 8 years period (2010--2017). The number of dependents in household do not include the number of children."
)
writeLines(tab, out.file)
close(out.file)

# Figure 1: Income Distribution in 2013 and Relative Giving Price
data_summary$income_dist()

ggsave(
  here("export", "figures", "price-income-dist.pdf"),
  width = 10,
  height = 6
)

# Figure 2: Average Giving Amount and Proportion of Donors
data_summary$ts_giving()

ggsave(
  here("export", "figures", "intensive-extensive-tax-reform.pdf"),
  width = 10,
  height = 6
)

# Figure 3: Proportion Having Applied for Tax Incentives
data_summary$ts_claim()

ggsave(
  here("export", "figures", "summary-tax-relief.pdf"),
  width = 10,
  height = 6
)

# Appendix ? (reproduce Figure 3 using a subset consisting donors)
data_summary$ts_claim(d_donate == 1)

ggsave(
  here("export", "figures", "summary-tax-relief-cond-donors.pdf"),
  width = 10,
  height = 6
)

# //NOTE Main results
# Table A1: First-Stage Models
est_fp <- use$first_price()

out.file <- file(here("export", "tables", "main-stage1.tex"), open = "w")
tab <- est_fp$stage1(
  title = "First-Stage Models",
  label = "main-stage1",
  notes = "Notes: * $p < 0.1$, ** $p < 0.05$, *** $p < 0.01$. Standard errors clustered at household level are in parentheses. The outcome variable is the logged value of the effective price. For estimation, Model (1) uses donors only (intensive-margin sample), and Model (2) uses not only donors but also non-donors (extensive-margin sample). In addition to logged income shown in table, covariates consist of squared age (divided by 100), number of household members, a dummy that indicates having dependents, a dummy that indicates a wage earner, a set of industry dummies, a set of residential area dummies, and individual and time fixed effects. The excluded instrument is the logged applicable price."
)
writeLines(tab, out.file)
close(out.file)

# Table 3: Estimation Results of Price Elasticities
out.file <- file(here("export", "tables", "main.tex"), open = "w")
tab <- est_fp$stage2(
  title = "Estimation Results of First-Price Elasticities",
  label = "main",
  notes = "Notes: * $p < 0.1$, ** $p < 0.05$, *** $p < 0.01$. Standard errors clustered at the household level are in parentheses. An outcome variable is the logged value of the effective price. For estimation, Models (1)--(3) use donors only (intensive-margin sample), and Models (4)--(6) use not only donors but also non-donors (extensive-margin sample). In addition to logged income, covariates consist of squared age (divided by 100), number of household members, a dummy that indicates having dependents, a dummy that indicates wage earner, a set of industry dummies, a set of residential area dummies, and individual and time fixed effects. The excluded instrument is a logged applicable price. The excluded instrument is a logged applicable price in Models (3) and (6)."
)
writeLines(tab, out.file)
close(out.file)

# //NOTE Robustness (Announcement effect)
# Table A2: Estimation of Price Elasticities Excluding Announcement Effect
out.file <- file(here("export", "tables", "announcement.tex"), open = "w")
tab <- est_fp$exclude_announcement(
  title = "First-Price Elasticities Excluding Announcement Effect",
  label = "announcement",
  notes = "Notes: * $p < 0.1$, ** $p < 0.05$, *** $p < 0.01$. Standard errors clustered at the household level are in parentheses. The outcome variable is the logged value of the amount of charitable giving in Models (1)--(3) and a donor dummy in Models (4)--(6). For estimation, Models (1)--(3) use donors only (intensive-margin sample), and Models (4)--(6) use not only donors but also non-donors (extensive-margin sample). To exclude the announcement effect, we exclude observations from 2013 and 2014. For the outcome equation, we control for squared age (divided by 100), number of household members, a dummy that indicates having dependents, a dummy that indicates a wage earner, a set of industry dummies, a set of residential area dummies, and individual and time fixed effects. For FE-2SLS, we use the logged applicable price as an instrument. To obtain the extensive-margin price elasticities in Models (4)--(6), we calculate implied price elasticities by dividing the estimated coefficient on price by the sample proportion of donors."
)
writeLines(tab, out.file)
close(out.file)

# //NOTE Robustness (Last-Price Elasticity)
# Table A3: Estimation Results of Intensive-Margin Last-Price Elasticities
lp <- use$last_price()
est_lp <- lp$fit()

out.file <- file(here("export", "tables", "last-int.tex"), open = "w")
tab <- est_lp$intensive(
  title = "Last-Price Elasticities for Intensive-Margin",
  label = "last-int",
  notes = "Notes: * $p < 0.1$, ** $p < 0.05$, *** $p < 0.01$. Standard errors clustered at the household level are in parenthesis. The outcome variable is the logged value of the amount of charitable giving. For estimation, we use donors only (intensive-margin sample). For the outcome equation, we control for squared age (divided by 100), number of household members, a dummy that indicates having dependents, a dummy that indicates a wage earner, a set of industry dummies, a set of residential area dummies, and individual and time fixed effects. For FE-2SLS, we use the logged applicable first price as an instrument."
)
writeLines(tab, out.file)
close(out.file)

# Table A4: Estimation Results of Extensive-Margin Last-Price Elasticities
out.file <- file(here("export", "tables", "last-ext.tex"), open = "w")
tab <- est_lp$extensive(
  title = "Last-Price Elasticities for Extensive-Margin",
  label = "last-ext",
  notes = "Notes: * $p < 0.1$, ** $p < 0.05$, *** $p < 0.01$. Standard errors clustered at the household level are in parentheses. The outcome variable is a dummy indicating a donor. For estimation, we use not only donors but also non-donors (extensive-margin sample). For the outcome equation, we control for squared age (divided by 100), number of household members, a dummy that indicates having dependents, a dummy that indicates a wage earner, a set of industry dummies, a set of residential area dummies, and individual and time fixed effects. For FE-2SLS, we use the logged applicable first price as an instrument. We calculate implied price elasticities by dividing the estimated coefficient on price by the sample proportion of donors."
)
writeLines(tab, out.file)
close(out.file)

# //NOTE Robustness (Bracket-shifting and permanent income problem?)
# Appendix ? (Estimation Results of First-Price Elasticities Removing Bracket Shifters)
use2 <- use$clone()
use2$remove_bracket_shift()

out.file <- file(here("export", "tables", "remove-bracket-shift.tex"), open = "w")
tab <- use2$first_price()$stage2(
  title = "First-Price Elasticities Removing Price Variation in Income Deduction Period",
  label = "remove-bracket-shift",
  notes = "Notes: * $p < 0.1$, ** $p < 0.05$, *** $p < 0.01$. Standard errors clustered at the household level are in parentheses. The outcome variable is the logged value of the amount of charitable giving in Models (1)--(3) and a donor dummy in Models (4)--(6). For estimation, Models (1)--(3) use donors only (intensive-margin sample), and Models (4)--(6) use not only donors but also non-donors (extensive-margin sample). We exclude those whose prices changed during the income deduction period. For the outcome equation, we control for squared age (divided by 100), number of household members, a dummy that indicates having dependents, a dummy that indicates a wage earner, a set of industry dummies, a set of residential area dummies, and individual and time fixed effects. For FE-2SLS, we use the logged applicable price as an instrument. To obtain the extensive-margin price elasticities in Models (4)--(6), we calculate implied price elasticities by dividing the estimated coefficient on price by the sample proportion of donors."
)
writeLines(tab, out.file)
close(out.file)

# Appendix ? (Estimation Results of Intensive-Margin Last-Price Elasticities Removing Bracket Shifters)
lp2_fit <- use2$last_price()$fit()
lp2_fit$intensive()

# Appendix ? (Estimation Results of Extensive-Margin Last-Price Elasticities Removing Bracket Shifters)
lp2_fit$extensive()

# Appendix ? (First-Price Elasticities Using 2012 and 2015 Data)
use3 <- use$clone()
use3$limit_2_year()

out.file <- file(here("export", "tables", "two-period.tex"), open = "w")
tab <- use3$first_price()$stage2(
  title = "First-Price Elasticities Using 2012 and 2015 Data",
  label = "two-period",
  notes = "Notes: * $p < 0.1$, ** $p < 0.05$, *** $p < 0.01$. Standard errors clustered at the household level are in parentheses. The outcome variable is the logged value of the amount of charitable giving in Models (1)--(3) and a donor dummy in Models (4)--(6). For estimation, Models (1)--(3) use donors only (intensive-margin sample), and Models (4)--(6) use not only donors but also non-donors (extensive-margin sample). We use only data from 2012 and 2015. For the outcome equation, we control for squared age (divided by 100), number of household members, a dummy that indicates having dependents, a dummy that indicates a wage earner, a set of industry dummies, a set of residential area dummies, and individual and time fixed effects. For FE-2SLS, we use the logged applicable price as an instrument. To obtain the extensive-margin price elasticities in Models (4)--(6), we calculate implied price elasticities by dividing the estimated coefficient on price by the sample proportion of donors."
)
writeLines(tab, out.file)
close(out.file)

# //NOTE Discussion
# Appendix ? (Intensive-Margin First-Price Elasticities among Claimants)
out.file <- file(here("export", "tables", "claimant-only.tex"), open = "w")
tab <- est_fp$claimant_only(
  title = "Intensive-Margin First-Price Elasticities for Claimants",
  label = "claimant-only",
  notes = "Notes: * $p < 0.1$, ** $p < 0.05$, *** $p < 0.01$. Standard errors clustered at the household level are in parentheses. The outcome variable is the logged value of the amount of charitable giving. For estimation, we use claimants only. We control for squared age (divided by 100), the number of household members, a dummy that indicates having dependents, a dummy that indicates a wage earner, a set of industry dummies, a set of residential area dummies, and individual and time fixed effects."
)
writeLines(tab, out.file)
close(out.file)

# Appendix ? (Price Elasticity of Claiming)
out.file <- file(here("export", "tables", "claim-elasticity.tex"), open = "w")
tab <- est_fp$claim_elasticity(
  title = "First-Price Elasticity of Declaration",
  label = "claim-elasticity",
  notes = "Notes: * $p < 0.1$, ** $p < 0.05$, *** $p < 0.01$. Standard errors clustered at the household level are in parentheses. The outcome variable is a declaration dummy. We control for squared age (divided by 100), the number of household members, a dummy that indicates having dependents, a dummy that indicates a wage earner, a set of industry dummies, a set of residential area dummies, and individual and time fixed effects. To obtain the price elasticity, we calculate implied price elasticities by dividing the estimated coefficient on price by the sample proportion of claimants."
)
writeLines(tab, out.file)
close(out.file)

# Table ? (Policy Effect of 2014 Tax Reform)
policy <- use$policy_effect()

out.file <- file(here("export", "tables", "policy-effect.tex"), open = "w")
tab <- policy$effective(
  intensive_elasticity = -1.527,
  extensive_elasticity = -1.903,
  font_size = 7,
  title = "Policy Effect of 2014 Tax Reform",
  label = "policy-effect",
  notes = "Notes: We use those whose declaration status is observed for 2013 and 2014. Column (1) shows the sample size by income bracket for 2013. Columns (2) and (3) are the declaration rates for each year. Columns (4) and (5) are the average effective price for each year. Column (6) reports the percentage change in the effective price. Column (8) shows the percentage change in donor contributions, which is the product of the value in Column (6) and the estimated intensive-margin effective price elasticity ($-1.56$). Column (10) shows the percentage change in the donor rate, which is the product of the value in Column (6) and the estimated extensive-margin effective price elasticities ($-2.647$). Columns (7) and (9) show the average donor contribution and the donor ratio in 2013, respectively. Columns (2)--(10) divide the sample by income bracket, claiming status in 2013, and claiming status in 2014, calculate the corresponding indicator in each subset, and then calculate the average of each indicator weighted by the subset sample size in each income bracket. The bottom row shows the average weighted by the bracket sample size."
)
writeLines(tab, out.file)
close(out.file)