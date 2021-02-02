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

********************************************************************************
* Construct Trust Index
********************************************************************************

** ---- EstimateTrustIndex
xtreg trust_politician i.year##i.living_area if year >= 2015, fe
predict orgtrustid, u

xtreg trust_politician i.year##i.living_area if year == 2015 | year == 2016, fe
predict orgparktrustid, u

xtreg trust_politician i.year##i.living_area if year == 2017 | year == 2018, fe
predict orgmoontrustid, u



* make trustid dataset
frame copy default trustdt
frame trustdt: {
	bysort pid: egen trustid = mean(orgtrustid)
	bysort pid: egen parktrustid = mean(orgparktrustid) 
	bysort pid: egen moontrustid = mean(orgmoontrustid)
}
frame trustdt: keep pid trustid parktrustid moontrustid
frame trustdt: duplicates drop
frame trustdt: gen diff = moontrustid - parktrustid
frame trustdt: xtile original5 = trustid, nq(5) 
frame trustdt: xtile park5 = parktrustid, nq(5)
frame trustdt: xtile original3 = trustid, nq(3)
frame trustdt: xtile park3 = parktrustid, nq(3)
frame trustdt: xtile original4 = trustid, nq(4)
frame trustdt: xtile park4 = parktrustid, nq(4)
frame trustdt: {
	gen lessdiff1 = 0
	replace lessdiff1 = 1 if abs(diff) < 1
	replace lessdiff1 = . if missing(diff)
}
frame trustdt: {
	gen lessdiffhalf = 0
	replace lessdiffhalf = 1 if abs(diff) < 0.5
	replace lessdiffhalf = . if missing(diff)
}
frame trustdt: save "data\shape\trustid.dta", replace
frame drop trustdt

********************************************************************************
* Merge with Trust index data
********************************************************************************

** ---- merged with trustdt
merge m:1 pid using "data\shape\trustid.dta"
drop _merge

********************************************************************************
* Trust index summary
********************************************************************************

** ---- HistogramTrustid
frame copy default plotdt
frame plotdt {
    keep pid trustid 
	duplicates drop
}
frame plotdt: {
	twoway ///
	(histogram trustid, freq yaxis(2) color(gs10%50) lcolor(black)), ///
	xtitle("Trust index") ///
	graphregion(fcolor(white))
}
frame drop plotdt

** ---- SummaryOutcomeByTrustGroup
frame copy default plotdt
frame plotdt {
	by year benefit_group original3, sort: egen meang = mean(i_total_giving)
	by year benefit_group original3, sort: egen meanint = mean(i_total_giving) if i_ext_giving == 1
	by year benefit_group original3, sort: egen meanext = mean(i_ext_giving)
}
frame plotdt {
	keep year benefit_group original3 meang meanint meanext
	duplicates drop
	keep if !missing(benefit_group) & !missing(original3) & !missing(meanint)
}

frame plotdt {
	twoway ///
	(connect meang year if benefit_group == 1 & original3 == 1, msymbol(O) color(black))  ///
	(connect meang year if benefit_group == 3 & original3 == 1, msymbol(T) color(black))  ///
	(connect meang year if benefit_group == 1 & original3 == 3, msymbol(O) color(black) lpattern(-))  ///
	(connect meang year if benefit_group == 3 & original3 == 3, msymbol(T) color(black) lpattern(-)), ///
	xline(2013.5, lcolor(red) lpattern(-)) ///
	xlab(2012(1)2018) xtitle("Year") ///
	ytitle("Average donations")  ///
	legend(label(1 "Income {&lt} 1200 (1Q of trust index)") label(2 "Income {&ge} 4600 (1Q of trust index)") ///
		label(3 "Income {&lt} 1200 (3Q of trust index)") label(4 "Income {&ge} 4600 (3Q of trust index)") size(small) cols(1)) ///
	graphregion(fcolor(white%100))
}
frame plotdt {
	twoway ///
	(connect meanint year if benefit_group == 1 & original3 == 1, msymbol(O) color(black))  ///
	(connect meanint year if benefit_group == 3 & original3 == 1, msymbol(T) color(black))  ///
	(connect meanint year if benefit_group == 1 & original3 == 3, msymbol(O) color(black) lpattern(-))  ///
	(connect meanint year if benefit_group == 3 & original3 == 3, msymbol(T) color(black) lpattern(-)), ///
	xline(2013.5, lcolor(red) lpattern(-)) ///
	xlab(2012(1)2018) xtitle("Year") ///
	ytitle("Average donations among donors")  ///
	legend(label(1 "Income {&lt} 1200 (1Q oftrust index)") label(2 "Income {&ge} 4600 (1Q of trust index)") ///
		label(3 "Income {&lt} 1200 (3Q oftrust index)") label(4 "Income {&ge} 4600 (3Q of trust index)") size(small) cols(1)) ///
	graphregion(fcolor(white%100))
}
frame plotdt {
	twoway ///
	(connect meanext year if benefit_group == 1 & original3 == 1, msymbol(O) color(black))  ///
	(connect meanext year if benefit_group == 3 & original3 == 1, msymbol(T) color(black))  ///
	(connect meanext year if benefit_group == 1 & original3 == 3, msymbol(O) color(black) lpattern(-))  ///
	(connect meanext year if benefit_group == 3 & original3 == 3, msymbol(T) color(black) lpattern(-)), ///
	xline(2013.5, lcolor(red) lpattern(-)) ///
	xlab(2012(1)2018) xtitle("Year") ///
	ytitle("Proportion of donors")  ///
	legend(label(1 "Income {&lt} 1200 (1Q oftrust index)") label(2 "Income {&ge} 4600 (1Q of trust index)") ///
		label(3 "Income {&lt} 1200 (3Q oftrust index)") label(4 "Income {&ge} 4600 (3Q of trust index)") size(small) cols(1)) ///
	graphregion(fcolor(white%100))
}

frame drop plotdt

** ---- tTestPresidentTrustid
frame copy default tdt
frame tdt {
    keep pid moontrustid parktrustid
	duplicates drop
}

frame tdt: ttest moontrustid == parktrustid

mat tabular = r(mu_1) - r(mu_2) \ r(se) \ r(p)
mat colnames tabular = diff
mat rownames tabular = diff se pval

mat list tabular

** ---- RegTrustidOnCovariate
reg trustid gender log_pinc_all age sqage i.educ ib3.political_pref if year == 2018

mat coef = r(table)
mat coef = coef[.,1..12]
mat stat = (e(N) \ e(r2_a))
mat colnames stat = stat
mat rownames stat = N r2a
mat_capp model : coef stat, miss(.)
mat model = model'

mat list model

********************************************************************************
* Heterogenous price elasticity by trust index (5 Gruops)
********************************************************************************

** ---- EstimateElasticityByTrustGroup
forvalues i = 1(1)5 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if original5 == `i', fe vce(cluster pid)
	
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

** ---- EstimateElasticityExtensiveByTrustGroup
forvalues i = 1(1)5 {
    
	* subgroup regression
	xtreg i_ext_giving log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if original5 == `i', fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue","log_price"]
	mat colnames coef = model`i'
	mat stat = e(N) \ e(r2_a)
	mat colnames stat = model`i'
	mat rownames stat = N r2a
	
	* proportion of donors
	summarize i_ext_giving if original5 == `i'
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

** ---- EstimateElasticityIntensiveByTrustGroup
forvalues i = 1(1)5 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if original5 == `i' & i_ext_giving == 1, fe vce(cluster pid)
	
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
* Heterogenous price elasticity by trust index (4 Gruops)
********************************************************************************

** ---- EstimateElasticityByTrustGroup4
forvalues i = 1(1)4 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if original4 == `i', fe vce(cluster pid)
	
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

** ---- EstimateElasticityExtensiveByTrustGroup4
forvalues i = 1(1)4 {
    
	* subgroup regression
	xtreg i_ext_giving log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if original4 == `i', fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue","log_price"]
	mat colnames coef = model`i'
	mat stat = e(N) \ e(r2_a)
	mat colnames stat = model`i'
	mat rownames stat = N r2a
	
	* proportion of donors
	summarize i_ext_giving if original4 == `i'
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

** ---- EstimateElasticityIntensiveByTrustGroup4
forvalues i = 1(1)4 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if original4 == `i' & i_ext_giving == 1, fe vce(cluster pid)
	
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

** ---- EstimateElasticityByTrustGroup3
forvalues i = 1(1)3 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if original3 == `i', fe vce(cluster pid)
	
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

** ---- EstimateElasticityExtensiveByTrustGroup3
forvalues i = 1(1)3 {
    
	* subgroup regression
	xtreg i_ext_giving log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if original3 == `i', fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue","log_price"]
	mat colnames coef = model`i'
	mat stat = e(N) \ e(r2_a)
	mat colnames stat = model`i'
	mat rownames stat = N r2a
	
	* proportion of donors
	summarize i_ext_giving if original5 == `i'
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

** ---- EstimateElasticityIntensiveByTrustGroup3
forvalues i = 1(1)3 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if original3 == `i' & i_ext_giving == 1, fe vce(cluster pid)
	
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
* Heterogenous price elasticity by trust index (3 groups) using Year == 2013|2014
********************************************************************************

** ---- ShortEstimateElasticityByTrustGroup3
forvalues i = 1(1)3 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if original3 == `i' & (year == 2013|year==2014), fe vce(cluster pid)
	
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
			if original3 == `i' & (year == 2013 | year == 2014), fe vce(cluster pid)
	
		* result of first stage
		mat fstage = r(table)["b".."pvalue","diff`k'p"]
		mat fstage = fstage[1,1] \ fstage[3,1]^2
		mat colnames fstage = Q`i'k`k'
		mat rownames fstage = ivcoef ivf
	
		* second stage
		xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
			(log_price = diff`k'p) if original3 == `i' & (year == 2013 | year == 2014), fe vce(cluster pid)
	
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
		if original3 == `i' & (year == 2013 | year == 2014), fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue","log_price"]
	mat colnames coef = Q`i'k0
	mat stat = e(N) \ e(r2_a)
	mat colnames stat = Q`i'k0
	mat rownames stat = N r2a
	
	* proportion of donors
	summarize i_ext_giving if original3 == `i' & (year == 2013 | year == 2014)
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
			if original3 == `i' & (year == 2013 | year == 2014), fe vce(cluster pid)
	
		* result of first stage
		mat fstage = r(table)["b".."pvalue","diff`k'p"]
		mat fstage = fstage[1,1] \ fstage[3,1]^2
		mat colnames fstage = Q`i'k`k'
		mat rownames fstage = ivcoef ivf
	
		* second stage
		xtivreg i_ext_giving log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
			(log_price = diff`k'p) if original3 == `i' & (year == 2013 | year == 2014), fe vce(cluster pid)
	
		* result of second stage
		mat coef = r(table)["b".."pvalue","log_price"]
		mat colnames coef = Q`i'k`k'
		mat stat = e(N) \ e(r2_w)
		mat colnames stat = Q`i'k`k'
		mat rownames stat = N r2w
	
		*proportion of donors
		summarize i_ext_giving if original3 == `i' & (year == 2013 | year == 2014)
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
		if original3 == `i' & i_ext_giving == 1 & (year == 2013 | year == 2014), fe vce(cluster pid)
	
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
			if original3 == `i' & i_ext_giving == 1 & (year == 2013 | year == 2014), fe vce(cluster pid)
	
		* result of first stage
		mat fstage = r(table)["b".."pvalue","diff`k'p"]
		mat fstage = fstage[1,1] \ fstage[3,1]^2
		mat colnames fstage = Q`i'k`k'
		mat rownames fstage = ivcoef ivf
	
		* second stage
		xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
			(log_price = diff`k'p) if original3 == `i' & i_ext_giving == 1 & (year == 2013 | year == 2014), fe vce(cluster pid)
	
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














