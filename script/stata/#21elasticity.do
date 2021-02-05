cd "C:\Users\vge00\Desktop\nastab"  //root path

** ---- ReadData
use "data\shaped.dta", clear

gen log_price = ln(price)
gen log_lprice = ln(lprice)
gen log_iv1price = ln(iv1price)
gen log_iv2price = ln(iv2price)
gen log_iv3price = ln(iv3price)
gen log_total_g = ln(i_total_giving + 1)
gen log_pinc_all = ln(lincome + 100000)

tsset pid year
keep if year >= 2012 & age >= 24

********************************************************************************
* Baseline
********************************************************************************

** ---- Elasticity
xtreg log_total_g log_price log_pinc_all i.year, fe vce(cluster pid)

mat coeftab = r(table)["b".."pvalue", "log_price".."log_pinc_all"]
mat colnames coeftab = Logprice_M0 Loginc_M0

mat stattab = e(N) \ e(r2)
mat colnames stattab = M0
mat rownames stattab = N r2

predict resid, e

* with covariates
local cov sqage i.year##i.educ i.year##i.gender i.year##i.living_area
local xvars 
local k = 1
foreach v of local cov {
	di "model `k'"
    
	*make set of covariates
	local xvars `xvars' `v'
	
	*estimate fixed effect model
	xtreg log_total_g log_price log_pinc_all i.year age `xvars', fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue","log_price".."log_pinc_all"]
	mat colnames coef = Logprice_M`k' Loginc_M`k'
	
	mat stat = e(N) \ e(r2)
	mat colnames stat = M`k'
	mat rownames stat = N r2
	
	*combined with previous results
	mat_capp coeftab : coeftab coef
	mat_capp stattab : stattab stat

	local k = `++k'
}

mat list coeftab
mat list stattab

* residuals plot
frame copy default plotdt
frame plotdt {
	gen benefit_group = .
	replace benefit_group = 1 if credit_benefit == 1
	replace benefit_group = 2 if credit_neutral == 1
	replace benefit_group = 3 if credit_loss == 1
	
	by year benefit_group, sort: egen meang = mean(resid)
	
	keep meang year benefit_group
	duplicates drop
	keep if !missing(benefit_group)
	
	gen meang_norm = meang if year == 2013
	by benefit_group, sort: egen meang13 = mean(meang_norm) 
	gen meang_n = meang/meang13 - 1
}

frame plotdt {
	twoway ///
	(connected meang_n year if benefit_group == 1, sort msymbol(O) color(black))  ///
	(connected meang_n year if benefit_group == 2, sort msymbol(T) color(black))  ///
	(connected meang_n year if benefit_group == 3, sort msymbol(S) color(black)),  ///
	xline(2013.5, lcolor(red) lpattern(-)) ///
	xlabel(2012(1)2018) xtitle("Year")  ///
	ytitle("Average Residuals (Normalized)")  ///
	legend(label(1 "Income {&lt} 1200") label(2 " Income in [1200, 4600)") label(3 "Income {&ge} 4600"))  ///
	graphregion(fcolor(white))
}

frame drop plotdt

** ---- ExtElasticity
* baseline
xtreg i_ext_giving log_price log_pinc_all i.year, fe vce(cluster pid)
predict residext, e

mat coeftab = r(table)["b".."pvalue", "log_price".."log_pinc_all"]
mat colnames coeftab = Logprice_M0 Loginc_M0

mat stattab = e(N) \ e(r2)
mat colnames stattab = M0
mat rownames stattab = N r

* price elasticity evaluated at mean
summarize i_ext_giving
local mu = r(mean)
lincom log_price*(1/`mu')

mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas1 = Logprice_M0
mat rownames elas1 = e_b e_se e_pval

* price elasticity evaluated at mean
summarize i_ext_giving
local mu = r(mean)
lincom log_pinc_all*(1/`mu')

mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas2 = Loginc_M0
mat rownames elas2 = e_b e_se e_pval

* baseline result
mat_capp elas : elas1 elas2
mat_rapp coeftab : coeftab elas


* with covariates
local cov sqage i.year##i.educ i.year##i.gender i.year##i.living_area
local xvars 
local k = 1
foreach v of local cov {
	di "model `k'"
    
	*make set of covariates
	local xvars `xvars' `v'
	
	*estimate fixed effect model
	xtreg i_ext_giving log_price log_pinc_all i.year age `xvars', fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue", "log_price".."log_pinc_all"]
	mat colnames coef = Logprice_M`k' Loginc_M`k'

	mat stat = e(N) \ e(r2)
	mat colnames stat = M0
	mat rownames stat = N r
	
	*proportion of donors
	summarize i_ext_giving
	local mu = r(mean)
	
	* price elasticity evaluated at mean
	summarize i_ext_giving
	local mu = r(mean)
	lincom log_price*(1/`mu')

	mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas1 = Logprice_M`k'
	mat rownames elas1 = e_b e_se e_pval

	* price elasticity evaluated at mean
	summarize i_ext_giving
	local mu = r(mean)
	lincom log_pinc_all*(1/`mu')

	mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas2 = Loginc_M`k'
	mat rownames elas2 = e_b e_se e_pval
	
	*combined with previous results
	mat_capp elas : elas1 elas2
	mat_rapp coef : coef elas
	mat_capp coeftab : coeftab coef
	
	mat_capp stattab : stattab stat

	local k = `++k'
}

mat list coeftab
mat list stattab

* residuals plot
frame copy default plotdt
frame plotdt {
	gen benefit_group = .
	replace benefit_group = 1 if credit_benefit == 1
	replace benefit_group = 2 if credit_neutral == 1
	replace benefit_group = 3 if credit_loss == 1
	
	by year benefit_group, sort: egen meang = mean(residext)
	
	keep meang year benefit_group
	duplicates drop
	keep if !missing(benefit_group)
	
	gen meang_norm = meang if year == 2013
	by benefit_group, sort: egen meang13 = mean(meang_norm) 
	gen meang_n = meang/meang13 - 1
}

frame plotdt {
	twoway ///
	(connected meang_n year if benefit_group == 1, sort msymbol(O) color(black))  ///
	(connected meang_n year if benefit_group == 2, sort msymbol(T) color(black))  ///
	(connected meang_n year if benefit_group == 3, sort msymbol(S) color(black)),  ///
	xline(2013.5, lcolor(red) lpattern(-)) ///
	xlabel(2012(1)2018) xtitle("Year")  ///
	ytitle("Average Residuals (Normalized)")  ///
	legend(label(1 "Income {&lt} 1200") label(2 " Income in [1200, 4600)") label(3 "Income {&ge} 4600"))  ///
	graphregion(fcolor(white))
}

frame drop plotdt

** ---- IntElasticity
* baseline
xtreg log_total_g log_price log_pinc_all i.year if i_ext_giving == 1, fe vce(cluster pid)

mat coeftab = r(table)["b".."pvalue", "log_price".."log_pinc_all"]
mat colnames coeftab = Logprice_M0 Loginc_M0

mat stattab = e(N) \ e(r2)
mat colnames stattab = M0
mat rownames stattab = N r2

predict residint, e

* with covariates
local cov sqage i.year##i.educ i.year##i.gender i.year##i.living_area
local xvars 
local k = 1
foreach v of local cov {
	di "model `k'"
    
	*make set of covariates
	local xvars `xvars' `v'
	
	*estimate fixed effect model
	xtreg log_total_g log_price log_pinc_all i.year age `xvars' if i_ext_giving == 1, fe vce(cluster pid)
	
	*matrix of regression result
	mat coef = r(table)["b".."pvalue","log_price".."log_pinc_all"]
	mat colnames coef = Logprice_M`k' Loginc_M`k'
	
	mat stat = e(N) \ e(r2)
	mat colnames stat = M`k'
	mat rownames stat = N r2
	
	*combined with previous results
	mat_capp coeftab : coeftab coef
	mat_capp stattab : stattab stat

	local k = `++k'
}

mat list coeftab
mat list stattab

* residuals plot
frame copy default plotdt
frame plotdt {
	gen benefit_group = .
	replace benefit_group = 1 if credit_benefit == 1
	replace benefit_group = 2 if credit_neutral == 1
	replace benefit_group = 3 if credit_loss == 1
	
	by year benefit_group, sort: egen meang = mean(residint)
	
	keep meang year benefit_group
	duplicates drop
	keep if !missing(benefit_group)
	
	gen meang_norm = meang if year == 2013
	by benefit_group, sort: egen meang13 = mean(meang_norm) 
	gen meang_n = meang/meang13 - 1
}

frame plotdt {
	twoway ///
	(connected meang_n year if benefit_group == 1, sort msymbol(O) color(black))  ///
	(connected meang_n year if benefit_group == 2, sort msymbol(T) color(black))  ///
	(connected meang_n year if benefit_group == 3, sort msymbol(S) color(black)),  ///
	xline(2013.5, lcolor(red) lpattern(-)) ///
	xlabel(2012(1)2018) xtitle("Year")  ///
	ytitle("Average Residuals (Normalized)")  ///
	legend(label(1 "Income {&lt} 1200") label(2 " Income in [1200, 4600)") label(3 "Income {&ge} 4600"))  ///
	graphregion(fcolor(white))
}

frame drop plotdt


********************************************************************************
* Robustness 1: Limited by year
********************************************************************************

** ---- ShortElasticity
* M0: year >= 2013 without cov
xtreg log_total_g log_price log_pinc_all i.year if year >= 2013, fe vce(cluster pid)

mat coeftab0 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]
mat colnames coeftab0 = Logprice_M0 Loginc_M0

mat stattab0 = e(N) \ e(r2)
mat colnames stattab0 = M0
mat rownames stattab0 = N r2

* M1: year >= 2013 with cov
xtreg log_total_g log_price log_pinc_all i.year age sqage i.year##i.educ i.year##i.gender i.year##i.living_area ///
	if year >= 2013, fe vce(cluster pid)

mat coeftab1 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]
mat colnames coeftab1 = Logprice_M1 Loginc_M1

mat stattab1 = e(N) \ e(r2)
mat colnames stattab1 = M1
mat rownames stattab1 = N r2

* M2: year == 2013 | 2014 without cov
xtreg log_total_g log_price log_pinc_all i.year if year == 2013 | year == 2014, fe vce(cluster pid)

mat coeftab2 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]
mat colnames coeftab2 = Logprice_M2 Loginc_M2

mat stattab2 = e(N) \ e(r2)
mat colnames stattab2 = M2
mat rownames stattab2 = N r2

* M3: year == 2013 | 2014 with cov
xtreg log_total_g log_price log_pinc_all i.year age sqage i.year##i.educ i.year##i.gender i.year##i.living_area ///
	if year == 2013 | year == 2014, fe vce(cluster pid)

mat coeftab3 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]
mat colnames coeftab3 = Logprice_M3 Loginc_M3

mat stattab3 = e(N) \ e(r2)
mat colnames stattab3 = M3
mat rownames stattab3 = N r2

mat_capp coeftab : coeftab0 coeftab1
mat_capp coeftab : coeftab coeftab2
mat_capp coeftab : coeftab coeftab3

mat_capp stattab : stattab0 stattab1
mat_capp stattab : stattab stattab2
mat_capp stattab : stattab stattab3

mat list coeftab
mat list stattab

** ---- ShortIntElasticity
* M0: year >= 2013 without cov
xtreg log_total_g log_price log_pinc_all i.year if year >= 2013 & i_ext_giving == 1, fe vce(cluster pid)

mat coeftab0 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]
mat colnames coeftab0 = Logprice_M0 Loginc_M0

mat stattab0 = e(N) \ e(r2)
mat colnames stattab0 = M0
mat rownames stattab0 = N r2

* M1: year >= 2013 with cov
xtreg log_total_g log_price log_pinc_all i.year age sqage i.year##i.educ i.year##i.gender i.year##i.living_area ///
	if year >= 2013 & i_ext_giving == 1, fe vce(cluster pid)

mat coeftab1 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]
mat colnames coeftab1 = Logprice_M1 Loginc_M1

mat stattab1 = e(N) \ e(r2)
mat colnames stattab1 = M1
mat rownames stattab1 = N r2

* M2: year == 2013 | 2014 without cov
xtreg log_total_g log_price log_pinc_all i.year if (year == 2013 | year == 2014) & i_ext_giving == 1, fe vce(cluster pid)

mat coeftab2 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]
mat colnames coeftab2 = Logprice_M2 Loginc_M2

mat stattab2 = e(N) \ e(r2)
mat colnames stattab2 = M2
mat rownames stattab2 = N r2

* M3: year == 2013 | 2014 with cov
xtreg log_total_g log_price log_pinc_all i.year age sqage i.year##i.educ i.year##i.gender i.year##i.living_area ///
	if (year == 2013 | year == 2014) & i_ext_giving == 1, fe vce(cluster pid)

mat coeftab3 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]
mat colnames coeftab3 = Logprice_M3 Loginc_M3

mat stattab3 = e(N) \ e(r2)
mat colnames stattab3 = M3
mat rownames stattab3 = N r2

mat_capp coeftab : coeftab0 coeftab1
mat_capp coeftab : coeftab coeftab2
mat_capp coeftab : coeftab coeftab3

mat_capp stattab : stattab0 stattab1
mat_capp stattab : stattab stattab2
mat_capp stattab : stattab stattab3

mat list coeftab
mat list stattab

** ---- ShortExtElasticity
** M0: year >= 2013 without cov
xtreg i_ext_giving log_price log_pinc_all i.year if year >= 2013, fe vce(cluster pid)

mat coeftab0 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]
mat colnames coeftab0 = Logprice_M0 Loginc_M0

mat stattab0 = e(N) \ e(r2)
mat colnames stattab0 = M0
mat rownames stattab0 = N r2

* price elasticity evaluated at mean
summarize i_ext_giving if year >= 2013
local mu = r(mean)
lincom log_price*(1/`mu')

mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas1 = Logprice_M0
mat rownames elas1 = e_b e_se e_pval

* price elasticity evaluated at mean
summarize i_ext_giving if year >= 2013
local mu = r(mean)
lincom log_pinc_all*(1/`mu')

mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas2 = Loginc_M0
mat rownames elas2 = e_b e_se e_pval

mat_capp elas : elas1 elas2
mat_rapp coeftab0 : coeftab0 elas

** M1: year >= 2013 with cov
xtreg i_ext_giving log_price log_pinc_all i.year age sqage i.year##i.educ i.year##i.gender i.year##i.living_area ///
	if year >= 2013, fe vce(cluster pid)

mat coeftab1 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]
mat colnames coeftab1 = Logprice_M1 Loginc_M1

mat stattab1 = e(N) \ e(r2)
mat colnames stattab1 = M1
mat rownames stattab1 = N r2

* price elasticity evaluated at mean
summarize i_ext_giving if year >= 2013
local mu = r(mean)
lincom log_price*(1/`mu')

mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas1 = Logprice_M1
mat rownames elas1 = e_b e_se e_pval

* price elasticity evaluated at mean
summarize i_ext_giving if year >= 2013
local mu = r(mean)
lincom log_pinc_all*(1/`mu')

mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas2 = Loginc_M1
mat rownames elas2 = e_b e_se e_pval

mat_capp elas : elas1 elas2
mat_rapp coeftab1 : coeftab1 elas

** M2: year == 2013 | 2014 without cov
xtreg i_ext_giving log_price log_pinc_all i.year if year == 2013 | year == 2014, fe vce(cluster pid)

mat coeftab2 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]
mat colnames coeftab2 = Logprice_M2 Loginc_M2

mat stattab2 = e(N) \ e(r2)
mat colnames stattab2 = M2
mat rownames stattab2 = N r2

* price elasticity evaluated at mean
summarize i_ext_giving if year == 2013 | year == 2014
local mu = r(mean)
lincom log_price*(1/`mu')

mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas1 = Logprice_M2
mat rownames elas1 = e_b e_se e_pval

* price elasticity evaluated at mean
summarize i_ext_giving if year == 2013 | year == 2014
local mu = r(mean)
lincom log_pinc_all*(1/`mu')

mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas2 = Loginc_M2
mat rownames elas2 = e_b e_se e_pval

mat_capp elas : elas1 elas2
mat_rapp coeftab2 : coeftab2 elas

** M3: year == 2013 | 2014 with cov
xtreg i_ext_giving log_price log_pinc_all i.year age sqage i.year##i.educ i.year##i.gender i.year##i.living_area ///
	if year == 2013 | year == 2014, fe vce(cluster pid)

mat coeftab3 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]
mat colnames coeftab3 = Logprice_M3 Loginc_M3

mat stattab3 = e(N) \ e(r2)
mat colnames stattab3 = M3
mat rownames stattab3 = N r2

* price elasticity evaluated at mean
summarize i_ext_giving if year == 2013 | year == 2014
local mu = r(mean)
lincom log_price*(1/`mu')

mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas1 = Logprice_M3
mat rownames elas1 = e_b e_se e_pval

* price elasticity evaluated at mean
summarize i_ext_giving if year == 2013 | year == 2014
local mu = r(mean)
lincom log_pinc_all*(1/`mu')

mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas2 = Loginc_M3
mat rownames elas2 = e_b e_se e_pval

mat_capp elas : elas1 elas2
mat_rapp coeftab3 : coeftab3 elas

mat_capp coeftab : coeftab0 coeftab1
mat_capp coeftab : coeftab coeftab2
mat_capp coeftab : coeftab coeftab3

mat_capp stattab : stattab0 stattab1
mat_capp stattab : stattab stattab2
mat_capp stattab : stattab stattab3

mat list coeftab
mat list stattab


********************************************************************************
* Robustness 2: Last Price Elasticity
********************************************************************************

** ---- LastElasticity
* first stage
xtreg log_lprice log_price log_pinc_all i.year, fe vce(cluster pid)

mat fstage = r(table)["b".."pvalue","log_price"]
mat fstage = fstage[1,1] \ fstage[3,1]^2
mat colnames fstage = M0
mat rownames fstage = ivcoef ivf

* second stage
xtivreg log_total_g log_pinc_all i.year (log_lprice = log_price), fe vce(cluster pid)

mat coeftab = r(table)["b".."pvalue", "log_lprice".."log_pinc_all"]
mat colnames coeftab = Loglprice_M0 Loginc_M0

mat stattab = e(N) \ e(r2)
mat colnames stattab = M0
mat rownames stattab = N r2

mat_rapp stattab : stattab fstage

* with covariates
local cov sqage i.year##i.educ i.year##i.gender i.year##i.living_area
local xvars 
local k = 1
foreach v of local cov {
	di "model `k'"
    
	*make set of covariates
	local xvars `xvars' `v'
	
	*first stage
	xtreg log_lprice log_price log_pinc_all i.year age `xvars', fe vce(cluster pid)
	
	mat fstage = r(table)["b".."pvalue","log_price"]
	mat fstage = fstage[1,1] \ fstage[3,1]^2
	mat colnames fstage = M`k'
	mat rownames fstage = ivcoef ivf
	
	*second stage
	xtivreg log_total_g log_pinc_all i.year age `xvars' (log_lprice = log_price), fe vce(cluster pid)
	
	mat coef = r(table)["b".."pvalue", "log_lprice".."log_pinc_all"]
	mat colnames coef = Loglprice_M`k' Loginc_M`k'

	mat stat = e(N) \ e(r2)
	mat colnames stat = M`k'
	mat rownames stat = N r2
	
	mat_rapp stat : stat fstage
	
	*combined with previous results
	mat_capp coeftab : coeftab coef
	mat_capp stattab : stattab stat

	local k = `++k'
}

mat list coeftab
mat list stattab

** ---- LastExtElasticity
* first stage
xtreg log_lprice log_price log_pinc_all i.year, fe vce(cluster pid)

mat fstage = r(table)["b".."pvalue","log_price"]
mat fstage = fstage[1,1] \ fstage[3,1]^2
mat colnames fstage = M0
mat rownames fstage = ivcoef ivf

* second stage
xtivreg i_ext_giving log_pinc_all i.year (log_lprice = log_price), fe vce(cluster pid)

mat coeftab = r(table)["b".."pvalue", "log_lprice".."log_pinc_all"]
mat colnames coeftab = Loglprice_M0 Loginc_M0

mat stattab = e(N) \ e(r2)
mat colnames stattab = M0
mat rownames stattab = N r2

* price elasticity evaluated at mean
summarize i_ext_giving
local mu = r(mean)
lincom log_lprice*(1/`mu')

mat elas1 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas1 = Loglprice_M0
mat rownames elas1 = e_b e_se e_pval

* price elasticity evaluated at mean
summarize i_ext_giving
local mu = r(mean)
lincom log_pinc_all*(1/`mu')

mat elas2 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas2 = Loginc_M0
mat rownames elas2 = e_b e_se e_pval

mat_capp elas : elas1 elas2
mat_rapp coeftab : coeftab elas
mat_rapp stattab : stattab fstage

* with covariates
local cov sqage i.year##i.educ i.year##i.gender i.year##i.living_area
local xvars 
local k = 1
foreach v of local cov {
	di "model `k'"
    
	*make set of covariates
	local xvars `xvars' `v'
	
	*first stage
	xtreg log_lprice log_price log_pinc_all i.year age `xvars', fe vce(cluster pid)
	
	mat fstage = r(table)["b".."pvalue","log_price"]
	mat fstage = fstage[1,1] \ fstage[3,1]^2
	mat colnames fstage = M`k'
	mat rownames fstage = ivcoef ivf
	
	*second stage
	xtivreg i_ext_giving log_pinc_all i.year age `xvars' (log_lprice = log_price), fe vce(cluster pid)
	
	mat coef = r(table)["b".."pvalue", "log_lprice".."log_pinc_all"]
	mat colnames coef = Loglprice_M`k' Loginc_M`k'

	mat stat = e(N) \ e(r2)
	mat colnames stat = M`k'
	mat rownames stat = N r2
	
	* price elasticity evaluated at mean
	summarize i_ext_giving
	local mu = r(mean)
	lincom log_lprice*(1/`mu')

	mat elas1 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
	mat colnames elas1 = Loglprice_M`k'
	mat rownames elas1 = e_b e_se e_pval

	* price elasticity evaluated at mean
	summarize i_ext_giving
	local mu = r(mean)
	lincom log_pinc_all*(1/`mu')

	mat elas2 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
	mat colnames elas2 = Loginc_M`k'
	mat rownames elas2 = e_b e_se e_pval

	mat_capp elas : elas1 elas2
	mat_rapp coef : coef elas
	mat_rapp stat : stat fstage
	
	*combined with previous results
	mat_capp coeftab : coeftab coef
	mat_capp stattab : stattab stat

	local k = `++k'
}

mat list coeftab
mat list stattab

** ---- LastIntElasticity
* first stage
xtreg log_lprice log_price log_pinc_all i.year if i_ext_giving == 1, fe vce(cluster pid)

mat fstage = r(table)["b".."pvalue","log_price"]
mat fstage = fstage[1,1] \ fstage[3,1]^2
mat colnames fstage = M0
mat rownames fstage = ivcoef ivf

* second stage
xtivreg log_total_g log_pinc_all i.year (log_lprice = log_price) if i_ext_giving == 1, fe vce(cluster pid)

mat coeftab = r(table)["b".."pvalue", "log_lprice".."log_pinc_all"]
mat colnames coeftab = Loglprice_M0 Loginc_M0

mat stattab = e(N) \ e(r2)
mat colnames stattab = M0
mat rownames stattab = N r2

mat_rapp stattab : stattab fstage

* with covariates
local cov sqage i.year##i.educ i.year##i.gender i.year##i.living_area
local xvars 
local k = 1
foreach v of local cov {
	di "model `k'"
    
	*make set of covariates
	local xvars `xvars' `v'
	
	*first stage
	xtreg log_lprice log_price log_pinc_all i.year age `xvars' if i_ext_giving == 1, fe vce(cluster pid)
	
	mat fstage = r(table)["b".."pvalue","log_price"]
	mat fstage = fstage[1,1] \ fstage[3,1]^2
	mat colnames fstage = M`k'
	mat rownames fstage = ivcoef ivf
	
	*second stage
	xtivreg log_total_g log_pinc_all i.year age `xvars' (log_lprice = log_price) if i_ext_giving == 1, fe vce(cluster pid)
	
	mat coef = r(table)["b".."pvalue", "log_lprice".."log_pinc_all"]
	mat colnames coef = Loglprice_M`k' Loginc_M`k'

	mat stat = e(N) \ e(r2)
	mat colnames stat = M`k'
	mat rownames stat = N r2
	
	mat_rapp stat : stat fstage
	
	*combined with previous results
	mat_capp coeftab : coeftab coef
	mat_capp stattab : stattab stat

	local k = `++k'
}

mat list coeftab
mat list stattab


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

