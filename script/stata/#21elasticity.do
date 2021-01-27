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

********************************************************************************
* Baseline
********************************************************************************

** ---- EstimateElasticity
label variable log_price "ln(giving price)"

* baseline
xtreg log_total_g log_price log_pinc_all i.year, fe vce(cluster pid)
mat coef = r(table)["b".."pvalue", "log_price"]
mat colnames coef = model0
mat stat = e(N) \ e(r2_a)
mat colnames stat = model0
mat rownames stat = N r2a
mat_rapp model0 : coef stat
mat tabular = model0'

* with covariates
local cov sqage i.year##i.educ i.year##i.gender i.living_area
local xvars 
local k = 1
foreach v of local cov {
	di "model `k'"
    
	*make set of covariates
	local xvars `xvars' `v'
	
	*estimate fixed effect model
	xtreg log_total_g log_price log_pinc_all i.year age `xvars', fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue","log_price"]
	mat colnames coef = model`k'
	mat stat = e(N) \ e(r2_a)
	mat colnames stat = model`k'
	mat rownames stat = N r2a
	mat_rapp model`k' : coef stat
	mat model`k' = model`k''
	
	*combined with previous results
	mat_rapp tabular : tabular model`k'

	local k = `++k'
}

mat list tabular

** ---- EstimateElasticityExtensive
label variable log_price "ln(giving price)"

* proportion of donors
summarize i_ext_giving
local mu = r(mean)

* baseline
xtreg i_ext_giving log_price log_pinc_all i.year, fe vce(cluster pid)
mat coef = r(table)["b".."pvalue", "log_price"]
mat colnames coef = model0
mat stat = e(N) \ e(r2_a)
mat colnames stat = model0
mat rownames stat = N r2a

* implied elasticity
lincom log_price*(1/`mu')
mat elas = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas = model0
mat rownames elas = e_b e_se e_pval

* baseline result
mat_rapp model0 : coef stat
mat_rapp model0 : model0 elas
mat tabular = model0'

* with covariates
local cov sqage i.year##i.educ i.year##i.gender i.living_area
local xvars 
local k = 1
foreach v of local cov {
	di "model `k'"
    
	*make set of covariates
	local xvars `xvars' `v'
	
	*estimate fixed effect model
	xtreg i_ext_giving log_price log_pinc_all i.year age `xvars', fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue","log_price"]
	mat colnames coef = model`k'
	mat stat = e(N) \ e(r2_a)
	mat colnames stat = model`k'
	mat rownames stat = N r2a
	
	*proportion of donors
	summarize i_ext_giving
	local mu = r(mean)
	
	*implied elasticity
	lincom log_price*(1/`mu')
	mat elas = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas = model`k'
	mat rownames elas = e_b e_se e_pval
	
	*regression result
	mat_rapp model`k' : coef stat
	mat_rapp model`k' : model`k' elas
	mat model`k' = model`k''
	
	*combined with previous results
	mat_rapp tabular : tabular model`k'

	local k = `++k'
}

mat list tabular

** ---- EstimateElasticityIntensive
label variable log_price "ln(giving price)"

* baseline
xtreg log_total_g log_price log_pinc_all i.year if i_ext_giving == 1, fe vce(cluster pid)
mat coef = r(table)["b".."pvalue", "log_price"]
mat colnames coef = model0
mat stat = e(N) \ e(r2_a)
mat colnames stat = model0
mat rownames stat = N r2a
mat_rapp model0 : coef stat
mat tabular = model0'

* with covariates
local cov sqage i.year##i.educ i.year##i.gender i.living_area
local xvars 
local k = 1
foreach v of local cov {
	di "model `k'"
    
	*make set of covariates
	local xvars `xvars' `v'
	
	*estimate fixed effect model
	xtreg log_total_g log_price log_pinc_all i.year age `xvars' if i_ext_giving == 1, /// 
		fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue","log_price"]
	mat colnames coef = model`k'
	mat stat = e(N) \ e(r2_a)
	mat colnames stat = model`k'
	mat rownames stat = N r2a
	mat_rapp model`k' : coef stat
	mat model`k' = model`k''
	
	*combined with previous results
	mat_rapp tabular : tabular model`k'

	local k = `++k'
}

mat list tabular

********************************************************************************
* Panel IV (instrument is log(p_it) - log(p_it-k))
********************************************************************************

** ---- PanelIVEstimateElasticity
forvalues k = 1(1)3 {
    
	di "lag = `k'"
	
	* first stage 
    xtreg log_price diff`k'p log_pinc_all age i.living_area i.year##i.gender i.year##i.educ, ///
		fe vce(cluster pid)
	
	* result of first stage
	mat fstage = r(table)["b".."pvalue","diff`k'p"]
	mat fstage = fstage[1,1] \ fstage[3,1]^2
	mat colnames fstage = model`k'
	mat rownames fstage = ivcoef ivf
	
	* second stage
	xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		(log_price = diff`k'p), fe vce(cluster pid)
	
	* result of second stage
	mat coef = r(table)["b".."pvalue","log_price"]
	mat colnames coef = model`k'
	mat stat = e(N) \ e(r2_w)
	mat colnames stat = model`k'
	mat rownames stat = N r2w
	mat_rapp model`k' : coef stat
	
	* combined with first stage result
	mat_rapp model`k' : model`k' fstage
	mat model`k' = model`k''
}

mat_rapp tabular : model1 model2
mat_rapp tabular : tabular model3

mat list tabular

** ---- PanelIVEstimateElasticityExtensive
forvalues k = 1(1)3 {
    
	di "lag = `k'"
	
	* first stage 
    xtreg log_price diff`k'p log_pinc_all age i.living_area i.year##i.gender i.year##i.educ, ///
		fe vce(cluster pid)
	
	* result of first stage
	mat fstage = r(table)["b".."pvalue","diff`k'p"]
	mat fstage = fstage[1,1] \ fstage[3,1]^2
	mat colnames fstage = model`k'
	mat rownames fstage = ivcoef ivf
	
	* second stage
	xtivreg i_ext_giving log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		(log_price = diff`k'p), fe vce(cluster pid)
	
	* result of second stage
	mat coef = r(table)["b".."pvalue","log_price"]
	mat colnames coef = model`k'
	mat stat = e(N) \ e(r2_w)
	mat colnames stat = model`k'
	mat rownames stat = N r2w
	
	*proportion of donors
	summarize i_ext_giving
	local mu = r(mean)
	
	*implied elasticity
	lincom log_price*(1/`mu')
	mat elas = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
	mat colnames elas = model`k'
	mat rownames elas = e_b e_se e_pval
	
	* combined with results
	mat_rapp model`k' : coef stat
	mat_rapp model`k' : model`k' elas
	mat_rapp model`k' : model`k' fstage
	mat model`k' = model`k''

}

mat_rapp tabular : model1 model2
mat_rapp tabular : tabular model3

mat list tabular

** ---- PanelIVEstimateElasticityIntensive
forvalues k = 1(1)3 {
    
	di "lag = `k'"
	
	* first stage 
    xtreg log_price diff`k'p log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if i_ext_giving == 1, fe vce(cluster pid)
	
	* result of first stage
	mat fstage = r(table)["b".."pvalue","diff`k'p"]
	mat fstage = fstage[1,1] \ fstage[3,1]^2
	mat colnames fstage = model`k'
	mat rownames fstage = ivcoef ivf
	
	* second stage
	xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		(log_price = diff`k'p) if i_ext_giving == 1, fe vce(cluster pid)
	
	* result of second stage
	mat coef = r(table)["b".."pvalue","log_price"]
	mat colnames coef = model`k'
	mat stat = e(N) \ e(r2_w)
	mat colnames stat = model`k'
	mat rownames stat = N r2w
	mat_rapp model`k' : coef stat
	
	* combined with first stage result
	mat_rapp model`k' : model`k' fstage
	mat model`k' = model`k''
}

mat_rapp tabular : model1 model2
mat_rapp tabular : tabular model3

mat list tabular

********************************************************************************
* k-Difference Estimation
********************************************************************************

** ---- kDiffEstimateElasticity
forvalues k = 1(1)3 {
    
	di "lag = `k'"
	
	* panel regression
	xtreg diff`k'G diff`k'p diff`k'I diff`k'_age diff`k'_sqage i.year, ///
		fe vce(cluster pid)
	
	* result of panel regression
	mat coef = r(table)["b".."pvalue","diff`k'p"]
	mat colnames coef = model`k'
	mat stat = e(N) \ e(r2_w)
	mat colnames stat = model`k'
	mat rownames stat = N r2w
	mat_rapp model`k' : coef stat
	mat model`k' = model`k''
}

mat_rapp tabular : model1 model2
mat_rapp tabular : tabular model3

mat list tabular

** ---- kDiffEstimateElasticityExtensive
forvalues k = 1(1)3 {
    
	di "lag = `k'"
	
	* panel regression
	xtreg diff`k'G1 diff`k'p diff`k'I diff`k'_age diff`k'_sqage i.year, ///
		fe vce(cluster pid)
	
	* result of panel regression
	mat coef = r(table)["b".."pvalue","diff`k'p"]
	mat colnames coef = model`k'
	mat stat = e(N) \ e(r2_w)
	mat colnames stat = model`k'
	mat rownames stat = N r2w
	
	
	*proportion of donors
	summarize diff`k'G1
	local mu = r(mean)
	
	*implied elasticity
	lincom diff`k'p*(1/`mu')
	mat elas = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas = model`k'
	mat rownames elas = e_b e_se e_pval
	
	*combined with result
	mat_rapp model`k' : coef elas
	mat_rapp model`k' : model`k' stat
	mat model`k' = model`k''
}

mat_rapp tabular : model1 model2
mat_rapp tabular : tabular model3

mat list tabular

** ---- kDiffEstimateElasticityIntensive
forvalues k = 1(1)3 {
    
	di "lag = `k'"
	
	* panel regression
	xtreg diff`k'G diff`k'p diff`k'I diff`k'_age diff`k'_sqage i.year if i_ext_giving == 1, ///
		fe vce(cluster pid)
	
	* result of panel regression
	mat coef = r(table)["b".."pvalue","diff`k'p"]
	mat colnames coef = model`k'
	mat stat = e(N) \ e(r2_w)
	mat colnames stat = model`k'
	mat rownames stat = N r2w
	mat_rapp model`k' : coef stat
	mat model`k' = model`k''
}

mat_rapp tabular : model1 model2
mat_rapp tabular : tabular model3

mat list tabular

********************************************************************************
* Baseline and Panel IV using Year == 2013 | Year == 2014
********************************************************************************

** ---- ShortEstimateElasticity
xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	if year == 2013|year == 2014, fe vce(cluster pid)
xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	(log_price = lag1iv) if year == 2013|year == 2014, fe first vce(cluster pid)
xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	(log_price = lag2iv) if year == 2013|year == 2014, fe first vce(cluster pid)
xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	(log_price = lag3iv) if year == 2013|year == 2014, fe first vce(cluster pid)
xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	(log_price = lag4iv) if year == 2013|year == 2014, fe first vce(cluster pid)