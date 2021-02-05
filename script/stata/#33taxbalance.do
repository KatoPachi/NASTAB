cd "C:\Users\vge00\Desktop\nastab"  //root path

** ---- ReadData
use "data\shaped.dta", clear
tsset pid year

gen log_price = ln(price)
gen log_lprice = ln(lprice)
gen log_iv1price = ln(iv1price)
gen log_iv2price = ln(iv2price)
gen log_iv3price = ln(iv3price)
gen log_total_g = ln(i_total_giving + 1)
gen log_pinc_all = ln(lincome + 100000)

forvalues i = 1(1)3 {
	gen lag`i'_log_total_g = l`i'.log_total_g
	gen lag`i'_log_pinc_all = l`i'.log_pinc_all
	gen lag`i'_age = l`i'.age
	gen lag`i'_sqage = l`i'.sqage
	
	gen log_diff`i'g = log_total_g - lag`i'_log_total_g
	gen log_diff`i'I = log_pinc_all - lag`i'_log_pinc_all
	gen diff`i'_age = age - lag`i'_age
	gen diff`i'_sqage = sqage - lag`i'_sqage
}

keep if year >= 2012 & age >= 24

********************************************************************************
* Construct Efficient index
********************************************************************************

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
	
	xtile balance3 = balanceid, nq(3)
	xtile park_balance3 = park_balanceid, nq(3)
	xtile balance10 = balanceid, nq(10)
	xtile park_balance10 = park_balanceid, nq(10)
	
	gen lessdiff1_balance = 0
	replace lessdiff1_balance = 1 if abs(diff_balance) < 1
	replace lessdiff1_balance = . if missing(diff_balance)
	
	gen lessdiffhalf_balance = 0
	replace lessdiffhalf_balance = 1 if abs(diff_balance) < 0.5
	replace lessdiffhalf_balance = . if missing(diff_balance)
}
frame balancedt: save "data\shape\balanceid.dta", replace
frame drop balancedt

********************************************************************************
* Merged with efficient index data
********************************************************************************

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
	xtitle("Efficient index") ///
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
	xtitle("Efficient index")  ///
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
* Heterogenous price elasticity by trust index (3 groups)
********************************************************************************

** ---- ElasticityByEfficientGroup
forvalues i = 1(1)3 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age sqage i.year##i.living_area i.year##i.gender i.year##i.educ ///
		if balance3 == `i', fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue","log_price".."log_pinc_all"]
	mat colnames coef = Logprice_M`i' Loginc_M`i'
	
	mat stat = e(N) \ e(r2)
	mat colnames stat = M`i'
	mat rownames stat = N r2
	
	if `i' == 1 {
	    mat coeftab = coef
		mat stattab = stat
	}
	else {
	    mat_capp coeftab : coeftab coef
		mat_capp stattab : stattab stat
	}
	
}

mat list coeftab
mat list stattab

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

** ---- EstimateElasticityByPositiveEfficientGroup3
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

** ---- EstimateElasticityExtensiveByPositiveEfficientGroup3
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

** ---- EstimateElasticityIntensiveByPositiveEfficientGroup3
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

*************************************************************************************
* Heterogenous price elasticity by trust index (3 groups) using ideal efficienct < 0
*************************************************************************************

** ---- EstimateElasticityByNegativeEfficientGroup3
forvalues i = 1(1)3 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance3 == `i' & ideal_balanceid < 0, fe vce(cluster pid)
	
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

** ---- EstimateElasticityExtensiveByNegativeEfficientGroup3
forvalues i = 1(1)3 {
    
	* subgroup regression
	xtreg i_ext_giving log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance3 == `i' & ideal_balanceid < 0, fe vce(cluster pid)
	
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

** ---- EstimateElasticityIntensiveByNegativeEfficientGroup3
forvalues i = 1(1)3 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance3 == `i' & i_ext_giving == 1 & ideal_balanceid < 0, fe vce(cluster pid)
	
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
* Heterogenous price elasticity by efficient index (3 groups) using Year == 2013|2014
********************************************************************************

** ---- ShortEstimateElasticityByEfficientGroup3
forvalues i = 1(1)3 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance3 == `i' & (year == 2013|year==2014) & ideal_balanceid > 0, fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue","log_price"]
	mat colnames coef = Q`i'k0
	mat stat = e(N) \ e(r2_a)
	mat colnames stat = Q`i'k0
	mat rownames stat = N r2a
	mat_rapp Q`i'k0 : coef stat
	mat Q`i'k0 = Q`i'k0'
	
	if `i' == 1 {
	    mat tabular = Q`i'k0
	}
	else {
	    mat_rapp tabular : tabular Q`i'k0
	}
	
}

forvalues i = 1(1)3 {
    
	forvalues k = 1(1)3 {
    
		di "lag = `k' with Group Q`i'"
	
		* first stage 
		xtreg log_price diff`k'p log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
			if balance3 == `i' & (year == 2013 | year == 2014) & ideal_balanceid > 0, fe vce(cluster pid)
	
		* result of first stage
		mat fstage = r(table)["b".."pvalue","diff`k'p"]
		mat fstage = fstage[1,1] \ fstage[3,1]^2
		mat colnames fstage = Q`i'k`k'
		mat rownames fstage = ivcoef ivf
	
		* second stage
		xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
			(log_price = diff`k'p) if balance3 == `i' & (year == 2013 | year == 2014) & ideal_balanceid > 0, fe vce(cluster pid)
	
		* result of second stage
		mat coef = r(table)["b".."pvalue","log_price"]
		mat colnames coef = Q`i'k`k'
		mat stat = e(N) \ e(r2_w)
		mat colnames stat = Q`i'k`k'
		mat rownames stat = N r2w
		mat_rapp Q`i'k`k' : coef stat
	
		* combined with first stage result
		mat_rapp Q`i'k`k' : Q`i'k`k' fstage
		mat Q`i'k`k' = Q`i'k`k''
		mat_rapp tabular : tabular Q`i'k`k', miss(.)
		
	}
}


mat list tabular

** ---- ShortEstimateElasticityExtensiveByTrustGroup3
forvalues i = 1(1)3 {
    
	* subgroup regression
	xtreg i_ext_giving log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance3 == `i' & (year == 2013 | year == 2014) & ideal_balanceid > 0, fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue","log_price"]
	mat colnames coef = Q`i'k0
	mat stat = e(N) \ e(r2_a)
	mat colnames stat = Q`i'k0
	mat rownames stat = N r2a
	
	* proportion of donors
	summarize i_ext_giving if balance3 == `i' & (year == 2013 | year == 2014) & ideal_balanceid > 0
	local mu = r(mean)

	* implied elasticity
	lincom log_price*(1/`mu')
	mat elas = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas = Q`i'k0
	mat rownames elas = e_b e_se e_pval
	
	* regression result for original5 == i
	mat_rapp Q`i'k0 : coef elas
	mat_rapp Q`i'k0 : Q`i'k0 stat
	mat Q`i'k0 = Q`i'k0'
	
	* combined with previous results
	if `i' == 1 {
	    mat tabular = Q`i'k0
	}
	else {
	    mat_rapp tabular : tabular Q`i'k0
	}
	
}

forvalues i = 1(1)3 {
    
	forvalues k = 1(1)3 {
    
		di "lag = `k' with group Q`i'"
	
		* first stage 
		xtreg log_price diff`k'p log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
			if balance3 == `i' & (year == 2013 | year == 2014) & ideal_balanceid > 0, fe vce(cluster pid)
	
		* result of first stage
		mat fstage = r(table)["b".."pvalue","diff`k'p"]
		mat fstage = fstage[1,1] \ fstage[3,1]^2
		mat colnames fstage = Q`i'k`k'
		mat rownames fstage = ivcoef ivf
	
		* second stage
		xtivreg i_ext_giving log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
			(log_price = diff`k'p) if balance3 == `i' & (year == 2013 | year == 2014) & ideal_balanceid > 0, fe vce(cluster pid)
	
		* result of second stage
		mat coef = r(table)["b".."pvalue","log_price"]
		mat colnames coef = Q`i'k`k'
		mat stat = e(N) \ e(r2_w)
		mat colnames stat = Q`i'k`k'
		mat rownames stat = N r2w
	
		*proportion of donors
		summarize i_ext_giving if balance3 == `i' & (year == 2013 | year == 2014) & ideal_balanceid > 0
		local mu = r(mean)
	
		*implied elasticity
		lincom log_price*(1/`mu')
		mat elas = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
		mat colnames elas = Q`i'k`k'
		mat rownames elas = e_b e_se e_pval
	
		* combined with first stage result
		mat_rapp Q`i'k`k' : coef elas
		mat_rapp Q`i'k`k' : Q`i'k`k' stat
		mat_rapp Q`i'k`k' : Q`i'k`k' fstage
		mat Q`i'k`k' = Q`i'k`k''
		mat_rapp tabular : tabular Q`i'k`k', miss(.)
	}
	
}

mat list tabular

** ---- ShortEstimateElasticityIntensiveByTrustGroup3
forvalues i = 1(1)3 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if balance3 == `i' & i_ext_giving == 1 & (year == 2013 | year == 2014) & ideal_balanceid > 0, fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue","log_price"]
	mat colnames coef = Q`i'k0
	mat stat = e(N) \ e(r2_a)
	mat colnames stat = Q`i'k0
	mat rownames stat = N r2a
	mat_rapp Q`i'k0 : coef stat
	mat Q`i'k0 = Q`i'k0'
	
	if `i' == 1 {
	    mat tabular = Q`i'k0
	}
	else {
	    mat_rapp tabular : tabular Q`i'k0
	}
	
}

forvalues i = 1(1)3 {
    
	forvalues k = 1(1)3 {
    
		di "lag = `k' with group Q`k'"
	
		* first stage 
		xtreg log_price diff`k'p log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
			if balance3 == `i' & i_ext_giving == 1 & (year == 2013 | year == 2014) & ideal_balanceid > 0, fe vce(cluster pid)
	
		* result of first stage
		mat fstage = r(table)["b".."pvalue","diff`k'p"]
		mat fstage = fstage[1,1] \ fstage[3,1]^2
		mat colnames fstage = Q`i'k`k'
		mat rownames fstage = ivcoef ivf
	
		* second stage
		xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
			(log_price = diff`k'p) ///
			if balance3 == `i' & i_ext_giving == 1 & (year == 2013 | year == 2014) & ideal_balanceid > 0, fe vce(cluster pid)
	
		* result of second stage
		mat coef = r(table)["b".."pvalue","log_price"]
		mat colnames coef = Q`i'k`k'
		mat stat = e(N) \ e(r2_w)
		mat colnames stat = Q`i'k`k'
		mat rownames stat = N r2w
		mat_rapp Q`i'k`k' : coef stat
	
		* combined with first stage result
		mat_rapp Q`i'k`k' : Q`i'k`k' fstage
		mat Q`i'k`k' = Q`i'k`k''
		mat_rapp tabular : tabular Q`i'k`k', miss(.)
	}
	
}

mat list tabular