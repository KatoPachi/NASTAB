cd "C:\Users\vge00\Desktop\nastab"  //root path

** ---- ReadData
use "data\shaped.dta", clear

gen price = .
replace price = 1 - mtr if year < 2014
replace price = 1 - 0.15 if year >= 2014
gen log_price = ln(price)
gen log_total_g = ln(i_total_giving + 1)
gen log_pinc_all = ln(lincome + 100000)
replace gender = gender - 1
gen univ = (educ == 3) if !missing(educ)
gen highschool = (educ == 2) if !missing(educ)
gen juniorhigh = (educ == 1) if !missing(educ)
gen sqage = age^2/100

** ---- LagOperation
tsset pid year

gen lag1_price = l.price
gen lag2_price = l2.price
gen lag3_price = l3.price
gen lag4_price = l4.price

gen lag1iv = ln(price/lag1_price)
gen lag2iv = ln(price/lag2_price)
gen lag3iv = ln(price/lag3_price)
gen lag4iv = ln(price/lag4_price)

keep if year >= 2012

** ---- ConstructTaxBalance
gen now_welfare = 1
replace now_welfare = 2 if avg_welfare_tax < 3 & avg_welfare_tax <= 6
replace now_welfare = 3 if avg_welfare_tax <= 3
replace now_welfare = . if missing(avg_welfare_tax)

gen now_tax = 1
replace now_tax = 2 if avg_welfare_tax == 2 | avg_welfare_tax == 5 | avg_welfare_tax == 8
replace now_tax = 3 if avg_welfare_tax == 3 | avg_welfare_tax == 6 | avg_welfare_tax == 9
replace now_tax = . if missing(avg_welfare_tax)

gen now_balance = 0
replace now_balance = 2 if avg_welfare_tax == 1
replace now_balance = 1 if avg_welfare_tax == 2 | avg_welfare_tax == 4
replace now_balance = -1 if avg_welfare_tax == 6 | avg_welfare_tax == 8
replace now_balance = -2 if avg_welfare_tax == 9
replace now_balance = . if missing(avg_welfare_tax)

gen now_balance3 = 0
replace now_balance3 = 1 if now_balance > 0
replace now_balance3 = -1 if now_balance < 0
replace now_balance3 = . if missing(now_balance)

** ---- EstimateTaxBalanceIndex
xtreg now_balance i.year##i.living_area if year >= 2015, fe
predict orgbalanceid, u

xtreg now_balance i.year##i.living_area if year == 2015 | year == 2016, fe
predict orgparkbalanceid, u

xtreg now_balance i.year##i.living_area if year == 2017 | year == 2018, fe
predict orgmoonbalanceid, u



* make trustid dataset
frame copy default balancedt
frame balancedt: {
	bysort pid: egen balanceid = mean(orgbalanceid)
	bysort pid: egen park_balanceid = mean(orgparkbalanceid) 
	bysort pid: egen moon_balanceid = mean(orgmoonbalanceid)
}
frame balancedt: keep pid balanceid park_balanceid moon_balanceid
frame balancedt: duplicates drop
frame balancedt: gen diff_balance = moon_balanceid - park_balanceid
frame balancedt: xtile balance5 = balanceid, nq(5) 
frame balancedt: xtile park_balance5 = park_balanceid, nq(5)
frame balancedt: {
	gen lessdiff1_balance = 0
	replace lessdiff1_balance = 1 if abs(diff_balance) < 1
	replace lessdiff1_balance = . if missing(diff_balance)
}
frame balancedt: {
	gen lessdiffhalf_balance = 0
	replace lessdiffhalf_balance = 1 if abs(diff_balance) < 0.5
	replace lessdiffhalf_balance = . if missing(diff_balance)
}
frame balancedt: save "data\shape\balanceid.dta", replace

** ---- merged with balancedt
merge m:1 pid using "data\shape\balanceid.dta"
drop _merge

********************************************************************************
* Efficent index summary
********************************************************************************

** ---- HistogramTaxBalanceIndex
frame copy default balancedt
frame balancedt {
    keep pid balanceid park_balanceid moon_balanceid diff_balance balance5 park_balance5 ///
		lessdiff1_balance lessdiffhalf_balance
	duplicates drop
}

frame balancedt: {
	twoway ///
	(histogram balanceid, freq yaxis(2) color(gs10%50) lcolor(black)), ///
	xtitle("Tax-welfare balance index") ///
	graphregion(fcolor(white))
}


** ---- Scatter1TaxBalanceIndex
frame balancedt: {
	twoway ///
	(scatter moon_balanceid park_balanceid, color(gs10%50)) ///
	(fpfit moon_balanceid park_balanceid, color(red)), ///
	xtitle("Park's tax-welfare balance index") ///
	ytitle("Moon's tax-welfare balance index") ///
	legend(off) ///
	graphregion(fcolor(white))
}

** ---- TtestPresidentTaxBalanceIndex
frame balancedt: ttest moon_balanceid == park_balanceid

forvalues i = 1(1)2 {
	mat group`i' = (r(mu_`i') \ r(sd_`i'))
	mat colnames group`i' = group`i'
	mat rownames group`i' = mu sd
}

mat diff = (group1[1,1] - group2[1,1] \ r(p))
mat colnames diff = diff
mat rownames diff = mu pval

mat_capp tabular : group1 group2
mat_capp tabular : tabular diff, miss(.)

mat list tabular

** ---- Scatter2TaxBalanceIndex
frame balancedt: {
	twoway ///
	(scatter balanceid diff_balance, color(gs10%50))  ///
	(fpfit balanceid diff_balance, color(red)), ///
	xtitle("Difference b/w president-specific tax-welfare balance index") ///
	ytitle("Tax-welfare balance index") ///
	legend(off)  ///
	graphregion(fcolor(white))
}

** ---- RegTrustidOnDiff2TaxBalanceIndex
frame balancedt: reg balanceid diff_balance
frame balancedt: reg balanceid diff_balance if abs(diff_balance) < 2
frame balancedt: reg balanceid diff_balance if abs(diff_balance) < 1
frame balancedt: reg balanceid diff_balance if abs(diff_balance) < 0.5


** ---- ScatterTaxBalanceIndexDonations
frame copy default scatdt
frame scatdt: bysort pid: egen avgdonate = mean(i_total_giving)
frame scatdt: keep pid balanceid avgdonate
frame scatdt: duplicates drop

frame scatdt: {
	twoway  ///
	(scatter avgdonate balanceid, color(gs10%50)),  ///
	xtitle("Tax-welfare balance index") ///
	ytitle("Individual average donations across time")  ///
	graphregion(fcolor(white))
}

** ---- PlotDiffDonationsbwTaxBalanceIndex
frame create coefplotdt
frame coefplotdt: {
	set obs 21
	gen effect = .
	gen se_effect = .
	gen cutoff = .
}

frame scatdt: gen high = .
local k = 1
forvalues i = 0(.1)2.1 {
	di "k = `k'"
	frame scatdt: replace high = 0 if balanceid <= `i'
	frame scatdt: replace high = 1 if balanceid > `i'
	frame scatdt: replace high = . if missing(balanceid)
	frame scatdt: reg avgdonate high 
	frame coefplotdt: replace effect = _b[high] if _n == `k'
	frame coefplotdt: replace se_effect = _se[high] if _n == `k'
	frame coefplotdt: replace cutoff = `i' if _n == `k'
	local k = `k' + 1
}

frame coefplotdt: gen lcoef = effect - 1.96*se_effect
frame coefplotdt: gen hcoef = effect + 1.96*se_effect

frame coefplotdt: {
	twoway ///
	(scatter effect cutoff, color(blue)) ///
	(line effect cutoff, color(blue))  ///
	(rcap hcoef lcoef cutoff, color(black)), ///
	yline(0, lcolor(red) lpattern(-))  ///
	xtitle("Threshold of tax-welfare balance index") ///
	ytitle("Difference in mean (+/- 1.96*se)") ///
	legend(off) ///
	graphregion(fcolor(white))
}


** ---- RegTaxBalanceIndexOnCovariate
reg balanceid gender log_pinc_all age sqage i.educ ib3.political_pref if year == 2018

mat coef = r(table)
mat coef = coef[.,1..12]
mat stat = (e(N) \ e(r2_a))
mat colnames stat = stat
mat rownames stat = N r2a
mat_capp model : coef stat, miss(.)
mat model = model'

mat list model


********************************************************************************
* Heterogenous price elasticity by trust index 
********************************************************************************

** ---- EstimateElasticityByEfficientGroup
forvalues i = 1(1)5 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance5 == `i', fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue","log_price"]
	mat colnames coef = model`i'
	mat stat = e(N) \ e(r2_a)
	mat colnames stat = model`i'
	mat rownames stat = N r2a
	mat_rapp model`i' : coef stat
	mat model`i' = model`i''
	
	if `i' == 1 {
	    mat tabular = model`i'
	}
	else {
	    mat_rapp tabular : tabular model`i'
	}
	
}

mat list tabular

** ---- EstimateElasticityExtensiveByEfficientGroup
forvalues i = 1(1)5 {
    
	* subgroup regression
	xtreg i_ext_giving log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance5 == `i', fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue","log_price"]
	mat colnames coef = model`i'
	mat stat = e(N) \ e(r2_a)
	mat colnames stat = model`i'
	mat rownames stat = N r2a
	
	* proportion of donors
	summarize i_ext_giving if balance5 == `i'
	local mu = r(mean)

	* implied elasticity
	lincom log_price*(1/`mu')
	mat elas = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas = model`i'
	mat rownames elas = e_b e_se e_pval
	
	* regression result for original5 == i
	mat_rapp model`i' : coef elas
	mat_rapp model`i' : model`i' stat
	mat model`i' = model`i''
	
	* combined with previous results
	if `i' == 1 {
	    mat tabular = model`i'
	}
	else {
	    mat_rapp tabular : tabular model`i'
	}
	
}

mat list tabular

** ---- EstimateElasticityIntensiveByEfficientGroup
forvalues i = 1(1)5 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance5 == `i' & i_ext_giving == 1, fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue","log_price"]
	mat colnames coef = model`i'
	mat stat = e(N) \ e(r2_a)
	mat colnames stat = model`i'
	mat rownames stat = N r2a
	mat_rapp model`i' : coef stat
	mat model`i' = model`i''
	
	if `i' == 1 {
	    mat tabular = model`i'
	}
	else {
	    mat_rapp tabular : tabular model`i'
	}
	
}

mat list tabular


** ---- EstimateInteractionByTaxBalanceIndexGroup
xtreg log_total_g c.log_price##ib3.balance5 log_pinc_all age i.living_area i.year##i.gender i.year##i.educ, ///
	fe vce(cluster pid)
	
** ---- Robust1EstimateInteractionByTaxBalanceIndexGroup
xtreg log_total_g c.log_price##ib3.balance5 log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	if year == 2013|year == 2014, fe vce(cluster pid)
xtreg log_total_g c.log_price##ib3.park_balance5 log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	if year == 2013|year == 2014, fe vce(cluster pid)	


xtreg log_total_g c.log_price##ib3.balance5 log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	if lessdiffhalf_balance == 1, fe vce(cluster pid)
