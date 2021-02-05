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
	by year benefit_group, sort: egen meang = mean(resid)
	
	keep meang year benefit_group
	duplicates drop
	keep if !missing(benefit_group)
	
	gen meang_norm = meang if year == 2013
	by benefit_group, sort: egen meang13 = mean(meang_norm) 
	gen meang_n = meang - meang13
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
	by year benefit_group, sort: egen meang = mean(residext)
	
	keep meang year benefit_group
	duplicates drop
	keep if !missing(benefit_group)
	
	gen meang_norm = meang if year == 2013
	by benefit_group, sort: egen meang13 = mean(meang_norm) 
	gen meang_n = meang - meang13
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
	by year benefit_group, sort: egen meang = mean(residint)
	
	keep meang year benefit_group
	duplicates drop
	keep if !missing(benefit_group)
	
	gen meang_norm = meang if year == 2013
	by benefit_group, sort: egen meang13 = mean(meang_norm) 
	gen meang_n = meang - meang13
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
* Robust3: k-Difference Estimation
********************************************************************************

** ---- kDiffElasticity
forvalues k = 1(1)3 {
    
	di "lag = `k'"
	
	* panel regression
	xtreg log_diff`k'g log_iv`k'price log_diff`k'I diff`k'_age diff`k'_sqage i.year##i.educ i.year##i.gender i.year##i.living_area, ///
		fe vce(cluster pid)
	
	* result of panel regression
	mat coef = r(table)["b".."pvalue","log_iv`k'price".."log_diff`k'I"]
	mat colnames coef = Logdiffprice_M`k' Logdiffinc_M`k'
	
	mat stat = e(N) \ e(r2)
	mat colnames stat = M`k'
	mat rownames stat = N r2
	
	if `k' == 1 {
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

** ---- kDiffIntElasticity
forvalues k = 1(1)3 {
    
	di "lag = `k'"
	
	* panel regression
	xtreg log_diff`k'g log_iv`k'price log_diff`k'I diff`k'_age diff`k'_sqage i.year##i.educ i.year##i.gender i.year##i.living_area ///
		if i_ext_giving == 1, fe vce(cluster pid)
	
	* result of panel regression
	mat coef = r(table)["b".."pvalue","log_iv`k'price".."log_diff`k'I"]
	mat colnames coef = Logdiffprice_M`k' Logdiffinc_M`k'
	
	mat stat = e(N) \ e(r2)
	mat colnames stat = M`k'
	mat rownames stat = N r2
	
	if `k' == 1 {
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
