global path "C:/Users/vge00/Desktop/NASTAB/analysis"
cd "$path/Stata"
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
egen mean_effective = mean(effective), by (pid)
egen mean_after_tax_tinc_ln = mean(after_tax_tinc_ln), by (pid)
egen mean_sqage = mean(sqage), by (pid)
egen mean_hhnum = mean(hhnum), by (pid)
egen mean_hhnum_child = mean(hhnum_child), by (pid)
egen mean_dependent_num = mean(dependent_num), by (pid)
egen mean_hh_max_inc = mean(hh_max_inc), by (pid)
egen mean_header = mean(header), by (pid)

gen demean_donate_ln = donate_ln - mean_donate_ln
gen demean_applicable = applicable - mean_applicable
gen demean_effective = effective - mean_effective
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

// Tobit-IV regression
// * `ivtobit` requires constant limit
// ivtobit demean_donate_ln demean_after_tax_tinc_ln ///
//   demean_sqage demean_hhnum demean_hhnum_child ///
//   demean_dependent_num demean_hh_max_inc demean_header ///
//   i.indust i.area i.year ///
//   (demean_effective = demean_applicable), ///
//   ul(amount_limit_incentive) vce(cluster hhid)

// Truncated regression
truncreg demean_donate_ln demean_applicable demean_after_tax_tinc_ln ///
  demean_sqage demean_hhnum demean_hhnum_child ///
  demean_dependent_num demean_hh_max_inc demean_header ///
  i.indust i.area i.year, ///
  ul(amount_limit_incentive) vce(cluster hhid)

estimates store trunc 
  
// Fixed effect model (1st stage)
// reghdfe effective applicable after_tax_tinc_ln sqage hhnum hhnum_child ///
//   dependent_num hh_max_inc header i.indust i.area, ///
//   absorb(pid year) vce(cluster hhid) keepsing
//
// estimates store fe

//　Variable label
label variable applicable "Simulated price"
label variable demean_applicable "(Demeaned) simulated price"
label variable after_tax_tinc_ln "Log after-tax income"
label variable demean_after_tax_tinc_ln "(Demeaned) log after-tax income"

// Output
esttab tobit trunc using "$path/export/tables/tobit.tex", ///
  se star(* 0.1 ** 0.05 *** 0.01) b(3) ///
  keep(demean_applicable demean_after_tax_tinc_ln) ///
  scalars("ll Log Lik.") ///
  booktabs ///
  alignment(lcc) ///
  title(Tobit Regression) ///
  mtitles("Tobit" "Truncated") ///
  label ///
  nogaps ///
  nonote ///
  addnote("\parbox{15cm}{* p < 0.1, ** p < 0.05, *** p < 0.01. Standard errors are clustered at household level in parenethses. Censoring point and truncation point are 10 percent of taxable income (standarized so that the smallest positive value is one). An outcome variable is the transformed logged value of donation, which is defined as $\log(\tilde{g})$ for $g > 0$ and $-1$ for $g=0$, where $\tilde{g}=g/\min_{g>0}[g]$. The value of the minimum nonzero donation, $\min_{g>0}[g]$, in our data is 2,000KRW. In addition to logged income, covariates consist of squared age (divided by 100), number of household members, number of children, number of dependents, a dummy that indicates the highest income in a household, a dummy that indicates that a taxpayer is household head, a set of industry dummies, a set of residential area dummies, and time fixed effects. Instead of individual fixed effect, we used demeaned variables subtracted from each individual's mean except the set of industry dummies and the set of residential area dummies.}") ///
  replace
