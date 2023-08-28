# Load reproduction function
library(here)
source(here("R/R6_StartAnalysis.r"))

# Create R6 class for analysis
use <- StartAnalysis$new(here("data/shaped2.csv"))

# Output options
options(modelsummary_stars_note = FALSE)

# //NOTE: Descriptive statistics
# Table 2: Descriptive Statistics
data_summary <- use$summary()
data_summary$stats()

# Figure 1: Income Distribution in 2013 and Relative Giving Price
data_summary$income_dist()

# Figure 2: Average Giving Amount and Proportion of Donors
data_summary$ts_giving()

# Figure 3: Proportion Having Applied for Tax Incentives
data_summary$ts_claim()

# Appendix ? (reproduce Figure 3 using a subset consisting donors)
data_summary$ts_claim(d_donate == 1)

# //NOTE Main results
# Table A1: First-Stage Models
est_fp <- use$first_price()
est_fp$stage1()

# Table 3: Estimation Results of Price Elasticities
est_fp$stage2()

# //NOTE Robustness (Announcement effect)
# Table A2: Estimation of Price Elasticities Excluding Announcement Effect
est_fp$exclude_announcement()

# //NOTE Robustness (Last-Price Elasticity)
# Table A3: Estimation Results of Intensive-Margin Last-Price Elasticities
lp <- use$last_price()
est_lp <- lp$fit()
est_lp$intensive()

# Table A4: Estimation Results of Extensive-Margin Last-Price Elasticities
est_lp$extensive()

# //NOTE Robustness (Bracket-shifting and permanent income problem?)
# Appendix ? (Estimation Results of First-Price Elasticities Removing Bracket Shifters)
use2 <- use$clone()
use2$remove_bracket_shift()
use2$first_price()$stage2()

# Appendix ? (Estimation Results of Intensive-Margin Last-Price Elasticities Removing Bracket Shifters)
lp2_fit <- use2$last_price()$fit()
lp2_fit$intensive()

# Appendix ? (Estimation Results of Extensive-Margin Last-Price Elasticities Removing Bracket Shifters)
lp2_fit$extensive()

# //NOTE Discussion
# Appendix ? (Intensive-Margin First-Price Elasticities among Claimants)
est_fp$claimant_only()

# Appendix ? (Price Elasticity of Claiming)
est_fp$claim_elasticity()
