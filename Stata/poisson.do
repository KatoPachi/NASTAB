cd "C:/Users/vge00/Desktop/NASTAB/analysis/Stata"
use rawdata.dta, clear

// Remove highest brackets F & G
egen sum_experience_FG = total(experience_FG), by (pid)
keep if sum_experience_FG == 0

// Remove those whose donation exceeds incentive limit
keep if over_limit_incentive == 0

// Other variables
gen header = 0
replace header = 1 if family_position == 1

gen applicable = price_ln
gen effective = d_relief_donate * applicable

// Poisson regression
ppmlhdfe donate applicable after_tax_tinc_ln sqage hhnum hhnum_child ///
  dependent_num hh_max_inc header i.indust i.area, ///
  absorb(pid year) vce(cluster hhid) keepsing