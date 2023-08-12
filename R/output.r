# Load reproduction function
library(here)
source(here("R/R6_StartAnalysis.r"))

# Create R6 class for analysis
use <- StartAnalysis$new(here("data/shaped2.csv"))

# //NOTE: Table 2: Descriptive Statistics
data_summary <- use$summary()
data_summary$stats()

# //NOTE: Figure 1: Income Distribution in 2013 and Relative Giving Price
data_summary$income_dist()

# //NOTE: Figure 2: Average Giving Amount and Proportion of Donors
data_summary$ts_giving()

# //NOTE: Figure 3: Proportion Having Applied for Tax Incentives
data_summary$ts_claim()

# //NOTE: Appendix ? (reproduce Figure 3 using a subset consisting donors)
data_summary$ts_claim(d_donate == 1)

# //NOTE: Table 3: Estimation Results of Price Elasticities
est_fp <- use$first_price()
est_fp$stage2()

# //NOTE: Table A1: First-Stage Models
est_fp$stage1()

# //NOTE: Table A2: Estimation of Price Elasticities Excluding Announcement Effect
est_fp$exclude_announcement()
