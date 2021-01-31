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

forvalues k = 1(1)3 {
    gen diff`k'G = log_total_g - l`k'.log_total_g
	gen diff`k'G1 = i_ext_giving - l`k'.i_ext_giving
	gen diff`k'p = log_price - l`k'.log_price
	gen diff`k'I = log_pinc_all - l`k'.log_pinc_all
	gen diff`k'_age = age - l`k'.age
	gen diff`k'_sqage = sqage - l`k'.sqage
}

keep if year >= 2012

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

** ---- merged with trustdt
merge m:1 pid using "data\shape\trustid.dta"
drop _merge

********************************************************************************
* Trust index summary
********************************************************************************

** ---- HistogramTrustid
frame copy default trustdt
frame trustdt {
    keep pid trustid parktrustid moontrustid diff original5 park5 lessdiff1 lessdiffhalf
	duplicates drop
}

frame trustdt: {
	twoway ///
	(histogram trustid, freq yaxis(2) color(gs10%50) lcolor(black)), ///
	xtitle("Trust index") ///
	graphregion(fcolor(white))
}


** ---- Scatter1Trustid
frame trustdt: {
	twoway ///
	(scatter moontrustid parktrustid, color(gs10%50)) ///
	(fpfit moontrustid parktrustid, color(red)), ///
	xtitle("Park's trust index") ytitle("Moon's trust index") ///
	legend(off) ///
	graphregion(fcolor(white))
}

** ---- TtestPresidentTrustid
frame trustdt: ttest moontrustid == parktrustid

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

** ---- Scatter2Trusid
frame trustdt: {
	twoway ///
	(scatter trustid diff, color(gs10%50))  ///
	(fpfit trustid diff, color(red)), ///
	xtitle("Difference b/w president-specific trust index") ///
	ytitle("Trust index") ///
	legend(off)  ///
	graphregion(fcolor(white))
}

** ---- RegTrustidOnDiff2Trustid
frame trustdt: reg trustid diff
frame trustdt: reg trustid diff if abs(diff) < 2
frame trustdt: reg trustid diff if abs(diff) < 1
frame trustdt: reg trustid diff if abs(diff) < 0.5


** ---- ScatterTrusidDonations
frame copy default scatdt
frame scatdt: bysort pid: egen avgdonate = mean(i_total_giving)
frame scatdt: keep pid trustid avgdonate
frame scatdt: duplicates drop

frame scatdt: {
	twoway  ///
	(scatter avgdonate trustid, color(gs10%50)),  ///
	xtitle("Trust index") ytitle("Individual average donations across time")  ///
	graphregion(fcolor(white))
}

** ---- PlotDiffDonationsbwTrustGroup
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
	frame scatdt: replace high = 0 if trustid <= `i'
	frame scatdt: replace high = 1 if trustid > `i'
	frame scatdt: replace high = . if missing(trustid)
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
	xtitle("Threshold of trust index") ///
	ytitle("Difference in mean (+/- 1.96*se)") ///
	legend(off) ///
	graphregion(fcolor(white))
}


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
* Heterogenous price elasticity by trust index using year == 2013 | 2014
********************************************************************************

** ---- ShortEstimateElasticityByTrustGroup
forvalues i = 1(1)5 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if original5 == `i' & (year == 2013 | year == 2014), fe vce(cluster pid)
	
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

** ---- ShortEstimateElasticityExtensiveByTrustGroup
forvalues i = 1(1)5 {
    
	* subgroup regression
	xtreg i_ext_giving log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if original5 == `i' & (year == 2013 | year == 2014), fe vce(cluster pid)
	
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

** ---- ShortEstimateElasticityIntensiveByTrustGroup
forvalues i = 1(1)5 {
    
	* subgroup regression
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if original5 == `i' & i_ext_giving == 1 & (year == 2013 | year == 2014), fe vce(cluster pid)
	
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

** ---- ShortEstimateElasticityExtensiveByTrustGroup3
forvalues i = 1(1)3 {
    
	* subgroup regression
	xtreg i_ext_giving log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if original3 == `i' & (year == 2013 | year == 2014), fe vce(cluster pid)
	
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
		if original3 == `i' & i_ext_giving == 1 & (year == 2013 | year == 2014), fe vce(cluster pid)
	
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



** ---- EstimateInteractionByTrustGroup
xtreg log_total_g c.log_price##ib3.original5 log_pinc_all age i.living_area i.year##i.gender i.year##i.educ, ///
	fe vce(cluster pid)
	
** ---- Robust1EstimateInteractionByTrustGroup
xtreg log_total_g c.log_price##ib3.original5 log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	if year == 2013|year == 2014, fe vce(cluster pid)
xtreg log_total_g c.log_price##ib3.park5 log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	if year == 2013|year == 2014, fe vce(cluster pid)	

xtreg log_total_g c.log_price##ib3.original5 log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	if lessdiff1 == 1, fe vce(cluster pid)
xtreg log_total_g c.log_price##ib3.original5 log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	if lessdiffhalf == 1, fe vce(cluster pid)

** ---- ClearEnv
frame change default
frame drop scatdt
frame drop coefplotdt
frame drop trustdt














