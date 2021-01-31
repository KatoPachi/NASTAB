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

gen benefit_group = .
replace benefit_group = 1 if credit_benefit == 1
replace benefit_group = 2 if credit_neutral == 1
replace benefit_group = 3 if credit_loss == 1

gen now_balance = 0
replace now_balance = 2 if avg_welfare_tax == 1
replace now_balance = 1 if avg_welfare_tax == 2 | avg_welfare_tax == 4
replace now_balance = -1 if avg_welfare_tax == 6 | avg_welfare_tax == 8
replace now_balance = -2 if avg_welfare_tax == 9
replace now_balance = . if missing(avg_welfare_tax)

gen ideal_balance = 0 
replace ideal_balance = 2 if opt_welfare_tax == 1
replace ideal_balance = 1 if opt_welfare_tax == 2 | opt_welfare_tax == 4
replace ideal_balance = -1 if opt_welfare_tax == 6 | opt_welfare_tax == 8
replace ideal_balance = -2 if opt_welfare_tax == 9
replace ideal_balance = . if missing(opt_welfare_tax) 

** ---- LagOperation
tsset pid year

forvalues k = 1(1)3 {
    gen diff`k'G = log_total_g - l`k'.log_total_g
	gen diff`k'G1 = i_ext_giving - l`k'.i_ext_giving
	gen diff`k'p = log_price - l`k'.log_price
	gen diff`k'I = log_pinc_all - l`k'.log_pinc_all
	gen diff`k'_age = age - l`k'.age
	gen diff`k'_sqage = sqage - l`k'.sqage
}

keep if year >= 2012

** ---- EstimateTaxBalanceIndex
* current efficient index
xtreg now_balance i.year##i.living_area if year >= 2015, fe
predict orgbalanceid, u

xtreg now_balance i.year##i.living_area if year == 2015 | year == 2016, fe
predict orgparkbalanceid, u

xtreg now_balance i.year##i.living_area if year == 2017 | year == 2018, fe
predict orgmoonbalanceid, u

* ideal efficient index 
xtreg ideal_balance i.year##i.living_area if year >= 2015, fe
predict ideal_orgbalanceid, u

xtreg ideal_balance i.year##i.living_area if year == 2015 | year == 2016, fe
predict ideal_orgparkbalanceid, u

xtreg ideal_balance i.year##i.living_area if year == 2017 | year == 2018, fe
predict ideal_orgmoonbalanceid, u


* make trustid dataset
frame copy default balancedt
frame balancedt: {
	bysort pid: egen balanceid = mean(orgbalanceid)
	bysort pid: egen park_balanceid = mean(orgparkbalanceid) 
	bysort pid: egen moon_balanceid = mean(orgmoonbalanceid)
	bysort pid: egen ideal_balanceid = mean(ideal_orgbalanceid)
	bysort pid: egen ideal_park_balanceid = mean(ideal_orgparkbalanceid)
	bysort pid: egen ideal_moon_balanceid = mean(ideal_orgmoonbalanceid)
}
frame balancedt: keep pid balanceid park_balanceid moon_balanceid ideal_balanceid ideal_park_balanceid ideal_moon_balanceid
frame balancedt: duplicates drop
frame balancedt {
	gen diff_balance = moon_balanceid - park_balanceid
	
	xtile balance5 = balanceid, nq(5)
	xtile park_balance5 = park_balanceid, nq(5)
	xtile balance4 = balanceid, nq(4)
	xtile park_balance4 = park_balanceid, nq(4)
	xtile balance3 = balanceid, nq(3)
	xtile park_balance3 = park_balanceid, nq(3)
	
	gen lessdiff1_balance = 0
	replace lessdiff1_balance = 1 if abs(diff_balance) < 1
	replace lessdiff1_balance = . if missing(diff_balance)
	
	gen lessdiffhalf_balance = 0
	replace lessdiffhalf_balance = 1 if abs(diff_balance) < 0.5
	replace lessdiffhalf_balance = . if missing(diff_balance)
}
frame balancedt: save "data\shape\balanceid.dta", replace
frame drop balancedt

** ---- merged with balancedt
merge m:1 pid using "data\shape\balanceid.dta"
drop _merge

********************************************************************************
* Summary of Efficent index summary
********************************************************************************

** ---- HistogramTaxBalanceIndex
frame copy default plotdt
frame plotdt {
	keep pid balanceid
	duplicates drop
}
frame plotdt: {
	twoway ///
	(histogram balanceid, freq yaxis(2) color(gs10%50) lcolor(black)), ///
	xtitle("Current efficient index") ///
	graphregion(fcolor(white))
}
frame drop plotdt

** ---- DensityTaxBalanceIndex
frame copy default plotdt
frame plotdt {
	keep pid balanceid ideal_balanceid
	duplicates drop
}
frame plotdt {
	twoway ///
	(kdensity balanceid, color(black)) ///
	(kdensity balanceid if ideal_balanceid > 0, color(black) lpattern(-)),  ///
	xtitle("Current efficient index")  ///
	ytitle("Density") ///
	legend(label(1 "Full sample") label(2 "Ideal efficient index {&gt} 0")) ///
	graphregion(fcolor(white))
}
frame drop plotdt

** ---- SummaryOutcomeByBenefitEfficientGroup
frame copy default plotdt
frame plotdt {
	by year benefit_group balance3, sort: egen meang = mean(i_total_giving)
	by year benefit_group balance3, sort: egen meanint = mean(i_total_giving) if i_ext_giving == 1
	by year benefit_group balance3, sort: egen meanext = mean(i_ext_giving)
}
frame plotdt {
	keep year benefit_group balance3 meang meanint meanext
	duplicates drop
	keep if !missing(benefit_group) & !missing(balance3) & !missing(meanint)
}

frame plotdt {
	twoway ///
	(connect meang year if benefit_group == 1 & balance3 == 1, msymbol(O) color(black))  ///
	(connect meang year if benefit_group == 3 & balance3 == 1, msymbol(T) color(black))  ///
	(connect meang year if benefit_group == 1 & balance3 == 3, msymbol(O) color(black) lpattern(-))  ///
	(connect meang year if benefit_group == 3 & balance3 == 3, msymbol(T) color(black) lpattern(-)), ///
	xline(2013.5, lcolor(red) lpattern(-)) ///
	xlab(2012(1)2018) xtitle("Year") ///
	ytitle("Average Donations")  ///
	legend(label(1 "Income {&lt} 1200 (1Q of efficent index)") label(2 "Income {&ge} 4600 (1Q of efficient index)") ///
		label(3 "Income {&lt} 1200 (3Q of efficent index)") label(4 "Income {&ge} 4600 (3Q of efficient index)") size(small) cols(1)) ///
	graphregion(fcolor(white%100))
}
frame plotdt {
	twoway ///
	(connect meanint year if benefit_group == 1 & balance3 == 1, msymbol(O) color(black))  ///
	(connect meanint year if benefit_group == 3 & balance3 == 1, msymbol(T) color(black))  ///
	(connect meanint year if benefit_group == 1 & balance3 == 3, msymbol(O) color(black) lpattern(-))  ///
	(connect meanint year if benefit_group == 3 & balance3 == 3, msymbol(T) color(black) lpattern(-)), ///
	xline(2013.5, lcolor(red) lpattern(-)) ///
	xlab(2012(1)2018) xtitle("Year") ///
	ytitle("Average Donations")  ///
	legend(label(1 "Income {&lt} 1200 (1Q of efficent index)") label(2 "Income {&ge} 4600 (1Q of efficient index)") ///
		label(3 "Income {&lt} 1200 (3Q of efficent index)") label(4 "Income {&ge} 4600 (3Q of efficient index)") size(small) cols(1)) ///
	graphregion(fcolor(white%100))
}
frame plotdt {
	twoway ///
	(connect meanext year if benefit_group == 1 & balance3 == 1, msymbol(O) color(black))  ///
	(connect meanext year if benefit_group == 3 & balance3 == 1, msymbol(T) color(black))  ///
	(connect meanext year if benefit_group == 1 & balance3 == 3, msymbol(O) color(black) lpattern(-))  ///
	(connect meanext year if benefit_group == 3 & balance3 == 3, msymbol(T) color(black) lpattern(-)), ///
	xline(2013.5, lcolor(red) lpattern(-)) ///
	xlab(2012(1)2018) xtitle("Year") ///
	ytitle("Average Donations")  ///
	legend(label(1 "Income {&lt} 1200 (1Q of efficent index)") label(2 "Income {&ge} 4600 (1Q of efficient index)") ///
		label(3 "Income {&lt} 1200 (3Q of efficent index)") label(4 "Income {&ge} 4600 (3Q of efficient index)") size(small) cols(1)) ///
	graphregion(fcolor(white%100))
}

frame drop plotdt

** ----tTestPresidentEfficientIndex
frame copy default tdt 
frame tdt {
	keep pid moon_balanceid park_balanceid ideal_moon_balanceid ideal_park_balanceid
	duplicates drop
}

frame tdt: ttest moon_balanceid == park_balanceid
mat test1 = r(mu_1) - r(mu_2) \ r(se) \ r(p)
mat colnames test1 = current
mat rownames test1 = diff se pval

frame tdt: ttest ideal_moon_balanceid == ideal_park_balanceid
mat test2 = r(mu_1) - r(mu_2) \ r(se) \ r(p)
mat colnames test2 = ideal
mat rownames test2 = diff se pval

mat_capp tabular : test1 test2

mat list tabular
frame drop tdt

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
* Heterogenous price elasticity by trust index (5 Groups)
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


********************************************************************************
* Heterogenous price elasticity by trust index (4 Groups)
********************************************************************************

** ---- EstimateElasticityByEfficientGroup4
forvalues i = 1(1)4 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance4 == `i', fe vce(cluster pid)
	
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
forvalues i = 1(1)4 {
    
	* subgroup regression
	xtreg i_ext_giving log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance4 == `i', fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue","log_price"]
	mat colnames coef = model`i'
	mat stat = e(N) \ e(r2_a)
	mat colnames stat = model`i'
	mat rownames stat = N r2a
	
	* proportion of donors
	summarize i_ext_giving if balance4 == `i'
	local mu = r(mean)

	* implied elasticity
	lincom log_price*(1/`mu')
	mat elas = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas = model`i'
	mat rownames elas = e_b e_se e_pval
	
	* regression result for balance4 == i
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

** ---- EstimateElasticityIntensiveByEfficientGroup4
forvalues i = 1(1)4 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance4 == `i' & i_ext_giving == 1, fe vce(cluster pid)
	
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


********************************************************************************
* Heterogenous price elasticity by trust index (3 groups)
********************************************************************************

** ---- EstimateElasticityByEfficientGroup3
forvalues i = 1(1)3 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance3 == `i', fe vce(cluster pid)
	
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

** ---- EstimateElasticityExtensiveByEfficientGroup3
forvalues i = 1(1)3 {
    
	* subgroup regression
	xtreg i_ext_giving log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance3 == `i', fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue","log_price"]
	mat colnames coef = model`i'
	mat stat = e(N) \ e(r2_a)
	mat colnames stat = model`i'
	mat rownames stat = N r2a
	
	* proportion of donors
	summarize i_ext_giving if balance3 == `i'
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

** ---- EstimateElasticityIntensiveByEfficientGroup3
forvalues i = 1(1)3 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance3 == `i' & i_ext_giving == 1, fe vce(cluster pid)
	
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


*************************************************************************************
* Heterogenous price elasticity by trust index (3 groups) using ideal efficienct > 0
*************************************************************************************

** ---- EstimateElasticityByIdealSubEfficientGroup3
forvalues i = 1(1)3 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance3 == `i' & ideal_balanceid > 0, fe vce(cluster pid)
	
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

** ---- EstimateElasticityExtensiveByIdealSubEfficientGroup3
forvalues i = 1(1)3 {
    
	* subgroup regression
	xtreg i_ext_giving log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance3 == `i' & ideal_balanceid > 0, fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue","log_price"]
	mat colnames coef = model`i'
	mat stat = e(N) \ e(r2_a)
	mat colnames stat = model`i'
	mat rownames stat = N r2a
	
	* proportion of donors
	summarize i_ext_giving if balance3 == `i'
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

** ---- EstimateElasticityIntensiveByIdealSubEfficientGroup3
forvalues i = 1(1)3 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance3 == `i' & i_ext_giving == 1 & ideal_balanceid > 0, fe vce(cluster pid)
	
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

********************************************************************************
* Heterogenous price elasticity by efficient index using year == 2013 | 2014
********************************************************************************

** ---- ShortEstimateElasticityByEfficientGroup
forvalues i = 1(1)5 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance5 == `i' & (year == 2013 | year == 2014), fe vce(cluster pid)
	
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

** ---- ShortEstimateElasticityExtensiveByEfficientGroup
forvalues i = 1(1)5 {
    
	* subgroup regression
	xtreg i_ext_giving log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance5 == `i' & (year == 2013 | year == 2014), fe vce(cluster pid)
	
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

** ---- ShortEstimateElasticityIntensiveByEfficientGroup
forvalues i = 1(1)5 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance5 == `i' & i_ext_giving == 1 & (year == 2013 | year == 2014), fe vce(cluster pid)
	
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


********************************************************************************
* Heterogenous price elasticity by trust index (3 groups) using year == 2013|2014
********************************************************************************

** ---- ShortEstimateElasticityByEfficientGroup3
forvalues i = 1(1)3 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance3 == `i' & (year == 2013 | year == 2014), fe vce(cluster pid)
	
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

** ---- ShortEstimateElasticityExtensiveByEfficientGroup3
forvalues i = 1(1)3 {
    
	* subgroup regression
	xtreg i_ext_giving log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance3 == `i' & (year == 2013 | year == 2014), fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue","log_price"]
	mat colnames coef = model`i'
	mat stat = e(N) \ e(r2_a)
	mat colnames stat = model`i'
	mat rownames stat = N r2a
	
	* proportion of donors
	summarize i_ext_giving if balance3 == `i'
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

** ---- ShortEstimateElasticityIntensiveByEfficientGroup3
forvalues i = 1(1)3 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance3 == `i' & i_ext_giving == 1 & (year == 2013 | year == 2014), fe vce(cluster pid)
	
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
