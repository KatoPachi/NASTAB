cd "C:/Users/vge00/Desktop/NASTAB/analysis/Stata"
use rawdata.dta, clear

// Remove highest brackets F & G
egen sum_experience_FG = total(experience_FG), by (pid)
keep if sum_experience_FG == 0

// Transfomed outcome variable (Chen and Roth, 2024)
summarize donate if donate > 0, meanonly
local min_positive_donate = r(min)
gen norm_donate = donate / `min_positive_donate'
replace donate_ln = -1 if d_donate == 0
replace donate_ln = log(norm_donate) if d_donate != 0

// Set upper bound of incentive
gen amount_limit_incentive = log(taxable_tinc * 0.1 / `min_positive_donate')

// Remove those whose donation exceeded upper bound of incentive
keep if over_limit_incentive == 0
gen check = donate_ln <= amount_limit_incentive
tabulate check

// Other variables
gen header = 0
replace header = 1 if family_position == 1

gen applicable = price_ln
gen effective = d_relief_donate * applicable

// Demeand variables
egen mean_donate_ln = mean(donate_ln), by (pid)
egen mean_applicable = mean(applicable), by (pid)
egen mean_after_tax_tinc_ln = mean(after_tax_tinc_ln), by (pid)
egen mean_sqage = mean(sqage), by (pid)
egen mean_hhnum = mean(hhnum), by (pid)
egen mean_hhnum_child = mean(hhnum_child), by (pid)
egen mean_dependent_num = mean(dependent_num), by (pid)
egen mean_hh_max_inc = mean(hh_max_inc), by (pid)
egen mean_header = mean(header), by (pid)

gen demean_donate_ln = donate_ln - mean_donate_ln
gen demean_applicable = applicable - mean_applicable
gen demean_after_tax_tinc_ln = after_tax_tinc_ln - mean_after_tax_tinc_ln
gen demean_sqage = sqage - mean_sqage
gen demean_hhnum = hhnum - mean_hhnum
gen demean_hhnum_child = hhnum_child - mean_hhnum_child
gen demean_dependent_num = dependent_num - mean_dependent_num
gen demean_hh_max_inc = hh_max_inc - mean_hh_max_inc
gen demean_header = header - mean_header

// Tobit regression
tobit demean_donate_ln demean_applicable demean_after_tax_tinc_ln ///
  demean_sqage demean_hhnum demean_hhnum_child ///
  demean_dependent_num demean_hh_max_inc demean_header ///
  i.indust i.area i.year, ///
  ul(amount_limit_incentive) vce(cluster hhid)

estimates store tobit

// Fixed effect model (1st stage)
reghdfe effective applicable after_tax_tinc_ln sqage hhnum hhnum_child ///
  dependent_num hh_max_inc header i.indust i.area, ///
  absorb(pid year) vce(cluster hhid) keepsing

estimates store fe



