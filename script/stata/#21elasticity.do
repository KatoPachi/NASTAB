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
local cov age i.year##i.educ i.year##i.gender i.living_area
local xvars 
local k = 1
foreach v of local cov {
	di "model `k'"
    
	*make set of covariates
	local xvars `xvars' `v'
	
	*estimate fixed effect model
	xtreg log_total_g log_price log_pinc_all i.year `xvars', fe vce(cluster pid)
	
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
local cov age i.year##i.educ i.year##i.gender i.living_area
local xvars 
local k = 1
foreach v of local cov {
	di "model `k'"
	
	* proportion of donors
	summarize i_ext_giving
	local mu = r(mean)
    
	*make set of covariates
	local xvars `xvars' `v'
	
	*estimate fixed effect model
	xtreg i_ext_giving log_price log_pinc_all i.year `xvars', fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue","log_price"]
	mat colnames coef = model`k'
	mat stat = e(N) \ e(r2_a)
	mat colnames stat = model`k'
	mat rownames stat = N r2a
	
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
local cov age i.year##i.educ i.year##i.gender i.living_area
local xvars 
local k = 1
foreach v of local cov {
	di "model `k'"
    
	*make set of covariates
	local xvars `xvars' `v'
	
	*estimate fixed effect model
	xtreg log_total_g log_price log_pinc_all i.year `xvars' if i_ext_giving == 1, /// 
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

** ---- Robust1EstimateElasticity
xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	(log_price = lag1iv), fe first vce(cluster pid)
xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	(log_price = lag2iv), fe first vce(cluster pid)
xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	(log_price = lag3iv), fe first vce(cluster pid)
xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	(log_price = lag4iv), fe first vce(cluster pid)

** ---- Robust2EstimateElasticity
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