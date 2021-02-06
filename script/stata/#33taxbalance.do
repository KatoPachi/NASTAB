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
	xtile balance3 = balanceid, nq(3)
	xtile park_balance3 = park_balanceid, nq(3)
}
frame balancedt: save "data\shape\balanceid.dta", replace
frame drop balancedt

********************************************************************************
* Merged with efficient index data
********************************************************************************

** ---- merged with balancedt
merge m:1 pid using "data\shape\balanceid.dta"
drop _merge

foreach v in log_price log_lprice log_iv1price log_iv2price log_iv3price {
    gen `v'_int2 = `v' * (balance3 == 2) if !missing(balance3)
	gen `v'_int3 = `v' * (balance3 == 3) if !missing(balance3)
}

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
* Heterogenous price elasticity by efficient index (3 quanitle)
********************************************************************************

** ---- HeteroElasticity
* overall
xtreg log_total_g log_price log_price_int2 log_price_int3   ///
	log_pinc_all age sqage i.year##i.living_area i.year##i.gender i.year##i.educ, ///
	fe vce(cluster pid)

mat coef0 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]	
mat colnames coef0 = Logprice_M0 Logprice2_M0 Logprice3_M0 Loginc_M0

mat stat0 = e(N) \ e(r2)
mat colnames stat0 = M0
mat rownames stat0 = N r2

lincom log_price
mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas1 = Logprice_M0
mat rownames elas1 = e_b e_se e_pval	

lincom log_price + log_price_int2
mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas2 = Logprice2_M0
mat rownames elas2 = e_b e_se e_pval	

lincom log_price + log_price_int3
mat elas3 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas3 = Logprice3_M0
mat rownames elas3 = e_b e_se e_pval	

lincom log_pinc_all
mat elas4 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas4 = Loginc_M0
mat rownames elas4 = e_b e_se e_pval	

mat_capp elas : elas1 elas2
mat_capp elas : elas elas3
mat_capp elas : elas elas4

mat_rapp coef0 : coef0 elas

mat list coef0 
mat list stat0

* extensive
xtreg i_ext_giving log_price log_price_int2 log_price_int3   ///
	log_pinc_all age sqage i.year##i.living_area i.year##i.gender i.year##i.educ, ///
	fe vce(cluster pid)

mat coef1 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]	
mat colnames coef1 = Logprice_M1 Logprice2_M1 Logprice3_M1 Loginc_M1

mat stat1 = e(N) \ e(r2)
mat colnames stat1 = M1
mat rownames stat1 = N r2

sum i_ext_giving
local mu = r(mean)

lincom log_price * (1/`mu')
mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas1 = Logprice_M1
mat rownames elas1 = e_b e_se e_pval	

lincom (log_price + log_price_int2) * (1/`mu')
mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas2 = Logprice2_M1
mat rownames elas2 = e_b e_se e_pval

lincom (log_price + log_price_int3) * (1/`mu')
mat elas3 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas3 = Logprice3_M1
mat rownames elas3 = e_b e_se e_pval	

lincom log_pinc_all * (1/`mu')
mat elas4 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas4 = Loginc_M1
mat rownames elas4 = e_b e_se e_pval	
	
mat_capp elas : elas1 elas2
mat_capp elas : elas elas3
mat_capp elas : elas elas4

mat_rapp coef1 : coef1 elas

mat list coef1 
mat list stat1

* intensive
xtreg log_total_g log_price log_price_int2 log_price_int3   ///
	log_pinc_all age sqage i.year##i.living_area i.year##i.gender i.year##i.educ ///
	if i_ext_giving == 1, fe vce(cluster pid)

mat coef2 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]	
mat colnames coef2 = Logprice_M2 Logprice2_M2 Logprice3_M2 Loginc_M2

mat stat2 = e(N) \ e(r2)
mat colnames stat2 = M2
mat rownames stat2 = N r2

lincom log_price
mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas1 = Logprice_M2
mat rownames elas1 = e_b e_se e_pval	

lincom log_price + log_price_int2
mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas2 = Logprice2_M2
mat rownames elas2 = e_b e_se e_pval	

lincom log_price + log_price_int3
mat elas3 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas3 = Logprice3_M2
mat rownames elas3 = e_b e_se e_pval	

lincom log_pinc_all
mat elas4 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas4 = Loginc_M2
mat rownames elas4 = e_b e_se e_pval	

mat_capp elas : elas1 elas2
mat_capp elas : elas elas3
mat_capp elas : elas elas4

mat_rapp coef2 : coef2 elas

mat list coef2 
mat list stat2

* combined result
mat_capp coeftab : coef0 coef1
mat_capp coeftab : coeftab coef2

mat_capp stattab : stat0 stat1
mat_capp stattab : stattab stat2

mat list coeftab
mat list stattab


*************************************************************************************
* Robust 1: Heterogenous price elasticity by efficient index using ideal efficienct > 0
*************************************************************************************

** ---- SubsetHeteroElasticity
* overall
xtreg log_total_g log_price log_price_int2 log_price_int3   ///
	log_pinc_all age sqage i.year##i.living_area i.year##i.gender i.year##i.educ ///
	if ideal_balanceid > 0, fe vce(cluster pid)

mat coef0 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]	
mat colnames coef0 = Logprice_M0 Logprice2_M0 Logprice3_M0 Loginc_M0

mat stat0 = e(N) \ e(r2)
mat colnames stat0 = M0
mat rownames stat0 = N r2

lincom log_price
mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas1 = Logprice_M0
mat rownames elas1 = e_b e_se e_pval	

lincom log_price + log_price_int2
mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas2 = Logprice2_M0
mat rownames elas2 = e_b e_se e_pval	

lincom log_price + log_price_int3
mat elas3 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas3 = Logprice3_M0
mat rownames elas3 = e_b e_se e_pval	

lincom log_pinc_all
mat elas4 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas4 = Loginc_M0
mat rownames elas4 = e_b e_se e_pval	

mat_capp elas : elas1 elas2
mat_capp elas : elas elas3
mat_capp elas : elas elas4

mat_rapp coef0 : coef0 elas

mat list coef0 
mat list stat0

* extensive
xtreg i_ext_giving log_price log_price_int2 log_price_int3   ///
	log_pinc_all age sqage i.year##i.living_area i.year##i.gender i.year##i.educ ///
	if ideal_balanceid > 0, fe vce(cluster pid)

mat coef1 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]	
mat colnames coef1 = Logprice_M1 Logprice2_M1 Logprice3_M1 Loginc_M1

mat stat1 = e(N) \ e(r2)
mat colnames stat1 = M1
mat rownames stat1 = N r2

sum i_ext_giving
local mu = r(mean)

lincom log_price * (1/`mu')
mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas1 = Logprice_M1
mat rownames elas1 = e_b e_se e_pval	

lincom (log_price + log_price_int2) * (1/`mu')
mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas2 = Logprice2_M1
mat rownames elas2 = e_b e_se e_pval

lincom (log_price + log_price_int3) * (1/`mu')
mat elas3 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas3 = Logprice3_M1
mat rownames elas3 = e_b e_se e_pval	

lincom log_pinc_all * (1/`mu')
mat elas4 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas4 = Loginc_M1
mat rownames elas4 = e_b e_se e_pval	
	
mat_capp elas : elas1 elas2
mat_capp elas : elas elas3
mat_capp elas : elas elas4

mat_rapp coef1 : coef1 elas

mat list coef1 
mat list stat1

* intensive
xtreg log_total_g log_price log_price_int2 log_price_int3   ///
	log_pinc_all age sqage i.year##i.living_area i.year##i.gender i.year##i.educ ///
	if i_ext_giving == 1 & ideal_balanceid > 0, fe vce(cluster pid)

mat coef2 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]	
mat colnames coef2 = Logprice_M2 Logprice2_M2 Logprice3_M2 Loginc_M2

mat stat2 = e(N) \ e(r2)
mat colnames stat2 = M2
mat rownames stat2 = N r2

lincom log_price
mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas1 = Logprice_M2
mat rownames elas1 = e_b e_se e_pval	

lincom log_price + log_price_int2
mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas2 = Logprice2_M2
mat rownames elas2 = e_b e_se e_pval	

lincom log_price + log_price_int3
mat elas3 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas3 = Logprice3_M2
mat rownames elas3 = e_b e_se e_pval	

lincom log_pinc_all
mat elas4 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas4 = Loginc_M2
mat rownames elas4 = e_b e_se e_pval	

mat_capp elas : elas1 elas2
mat_capp elas : elas elas3
mat_capp elas : elas elas4

mat_rapp coef2 : coef2 elas

mat list coef2 
mat list stat2

* combined result
mat_capp coeftab : coef0 coef1
mat_capp coeftab : coeftab coef2

mat_capp stattab : stat0 stat1
mat_capp stattab : stattab stat2

mat list coeftab
mat list stattab


*************************************************************************************
* Robust 2: Heterogenous last price elasticity by efficient index
*************************************************************************************

** ---- HeteroLastElasticity
* overall 
xtivreg log_total_g log_pinc_all age sqage i.year##i.living_area i.year##i.gender i.year##i.educ ///
	(log_lprice log_lprice_int2 log_lprice_int3 = log_price log_price_int2 log_price_int3), ///
	fe vce(cluster pid)

mat coef0 = r(table)["b".."pvalue", "log_lprice".."log_pinc_all"]	
mat colnames coef0 = Loglprice_M0 Loglprice2_M0 Loglprice3_M0 Loginc_M0

mat stat0 = e(N) \ e(r2)
mat colnames stat0 = M0
mat rownames stat0 = N r2

lincom log_lprice
mat elas1 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas1 = Loglprice_M0
mat rownames elas1 = e_b e_se e_pval	

lincom log_lprice + log_lprice_int2
mat elas2 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas2 = Loglprice2_M0
mat rownames elas2 = e_b e_se e_pval	

lincom log_lprice + log_lprice_int3
mat elas3 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas3 = Loglprice3_M0
mat rownames elas3 = e_b e_se e_pval	

lincom log_pinc_all
mat elas4 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas4 = Loginc_M0
mat rownames elas4 = e_b e_se e_pval	

mat_capp elas : elas1 elas2
mat_capp elas : elas elas3
mat_capp elas : elas elas4

mat_rapp coef0 : coef0 elas

mat list coef0 
mat list stat0

* extensive
xtivreg i_ext_giving log_pinc_all age sqage i.year##i.living_area i.year##i.gender i.year##i.educ ///
	(log_lprice log_lprice_int2 log_lprice_int3 = log_price log_price_int2 log_price_int3), ///
	fe vce(cluster pid)

mat coef1 = r(table)["b".."pvalue", "log_lprice".."log_pinc_all"]	
mat colnames coef1 = Loglprice_M1 Loglprice2_M1 Loglprice3_M1 Loginc_M1

mat stat1 = e(N) \ e(r2)
mat colnames stat1 = M1
mat rownames stat1 = N r2

sum i_ext_giving
local mu = r(mean)

lincom log_lprice * (1/`mu')
mat elas1 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas1 = Loglprice_M1
mat rownames elas1 = e_b e_se e_pval	

lincom (log_lprice + log_lprice_int2) * (1/`mu')
mat elas2 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas2 = Loglprice2_M1
mat rownames elas2 = e_b e_se e_pval

lincom (log_lprice + log_lprice_int3) * (1/`mu')
mat elas3 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas3 = Loglprice3_M1
mat rownames elas3 = e_b e_se e_pval	

lincom log_pinc_all * (1/`mu')
mat elas4 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas4 = Loginc_M1
mat rownames elas4 = e_b e_se e_pval	
	
mat_capp elas : elas1 elas2
mat_capp elas : elas elas3
mat_capp elas : elas elas4

mat_rapp coef1 : coef1 elas

mat list coef1 
mat list stat1
	
* intensive
xtivreg log_total_g log_pinc_all age sqage i.year##i.living_area i.year##i.gender i.year##i.educ ///
	(log_lprice log_lprice_int2 log_lprice_int3 = log_price log_price_int2 log_price_int3) ///
	if i_ext_giving == 1, fe vce(cluster pid)

mat coef2 = r(table)["b".."pvalue", "log_lprice".."log_pinc_all"]	
mat colnames coef2 = Loglprice_M2 Loglprice2_M2 Loglprice3_M2 Loginc_M2

mat stat2 = e(N) \ e(r2)
mat colnames stat2 = M2
mat rownames stat2 = N r2

lincom log_lprice
mat elas1 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas1 = Loglprice_M2
mat rownames elas1 = e_b e_se e_pval	

lincom log_lprice + log_lprice_int2
mat elas2 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas2 = Loglprice2_M2
mat rownames elas2 = e_b e_se e_pval	

lincom log_lprice + log_lprice_int3
mat elas3 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas3 = Loglprice3_M2
mat rownames elas3 = e_b e_se e_pval	

lincom log_pinc_all
mat elas4 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas4 = Loginc_M2
mat rownames elas4 = e_b e_se e_pval	

mat_capp elas : elas1 elas2
mat_capp elas : elas elas3
mat_capp elas : elas elas4

mat_rapp coef2 : coef2 elas

mat list coef2 
mat list stat2

* overall (ideal_balanceid >0)
xtivreg log_total_g log_pinc_all age sqage i.year##i.living_area i.year##i.gender i.year##i.educ ///
	(log_lprice log_lprice_int2 log_lprice_int3 = log_price log_price_int2 log_price_int3) ///
	if ideal_balanceid > 0, fe vce(cluster pid)

mat coef3 = r(table)["b".."pvalue", "log_lprice".."log_pinc_all"]	
mat colnames coef3 = Loglprice_M3 Loglprice2_M3 Loglprice3_M3 Loginc_M3

mat stat3 = e(N) \ e(r2)
mat colnames stat3 = M3
mat rownames stat3 = N r2

lincom log_lprice
mat elas1 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas1 = Loglprice_M3
mat rownames elas1 = e_b e_se e_pval	

lincom log_lprice + log_lprice_int2
mat elas2 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas2 = Loglprice2_M3
mat rownames elas2 = e_b e_se e_pval	

lincom log_lprice + log_lprice_int3
mat elas3 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas3 = Loglprice3_M3
mat rownames elas3 = e_b e_se e_pval	

lincom log_pinc_all
mat elas4 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas4 = Loginc_M3
mat rownames elas4 = e_b e_se e_pval	

mat_capp elas : elas1 elas2
mat_capp elas : elas elas3
mat_capp elas : elas elas4

mat_rapp coef3 : coef3 elas

mat list coef3 
mat list stat3

* extensive (ideal_balanceid > 0)
xtivreg i_ext_giving log_pinc_all age sqage i.year##i.living_area i.year##i.gender i.year##i.educ ///
	(log_lprice log_lprice_int2 log_lprice_int3 = log_price log_price_int2 log_price_int3) ///
	if ideal_balanceid > 0, fe vce(cluster pid)

mat coef4 = r(table)["b".."pvalue", "log_lprice".."log_pinc_all"]	
mat colnames coef4 = Loglprice_M4 Loglprice2_M4 Loglprice3_M4 Loginc_M4

mat stat4 = e(N) \ e(r2)
mat colnames stat4 = M4
mat rownames stat4 = N r2

sum i_ext_giving if ideal_balanceid > 0
local mu = r(mean)

lincom log_lprice * (1/`mu')
mat elas1 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas1 = Loglprice_M4
mat rownames elas1 = e_b e_se e_pval	

lincom (log_lprice + log_lprice_int2) * (1/`mu')
mat elas2 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas2 = Loglprice2_M4
mat rownames elas2 = e_b e_se e_pval

lincom (log_lprice + log_lprice_int3) * (1/`mu')
mat elas3 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas3 = Loglprice3_M4
mat rownames elas3 = e_b e_se e_pval	

lincom log_pinc_all * (1/`mu')
mat elas4 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas4 = Loginc_M4
mat rownames elas4 = e_b e_se e_pval	
	
mat_capp elas : elas1 elas2
mat_capp elas : elas elas3
mat_capp elas : elas elas4

mat_rapp coef4 : coef4 elas

mat list coef4
mat list stat4
	
* intensive (ideal_balanceid > 0)
xtivreg log_total_g log_pinc_all age sqage i.year##i.living_area i.year##i.gender i.year##i.educ ///
	(log_lprice log_lprice_int2 log_lprice_int3 = log_price log_price_int2 log_price_int3) ///
	if i_ext_giving == 1 & ideal_balanceid > 0, fe vce(cluster pid)

mat coef5 = r(table)["b".."pvalue", "log_lprice".."log_pinc_all"]	
mat colnames coef5 = Loglprice_M5 Loglprice2_M5 Loglprice3_M5 Loginc_M5

mat stat5 = e(N) \ e(r2)
mat colnames stat5 = M5
mat rownames stat5 = N r2

lincom log_lprice
mat elas1 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas1 = Loglprice_M5
mat rownames elas1 = e_b e_se e_pval	

lincom log_lprice + log_lprice_int2
mat elas2 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas2 = Loglprice2_M5
mat rownames elas2 = e_b e_se e_pval	

lincom log_lprice + log_lprice_int3
mat elas3 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas3 = Loglprice3_M5
mat rownames elas3 = e_b e_se e_pval	

lincom log_pinc_all
mat elas4 = r(estimate) \ r(se) \ (1 - normal(abs(r(estimate)/r(se))))*2
mat colnames elas4 = Loginc_M5
mat rownames elas4 = e_b e_se e_pval	

mat_capp elas : elas1 elas2
mat_capp elas : elas elas3
mat_capp elas : elas elas4

mat_rapp coef5 : coef5 elas

mat list coef5 
mat list stat5

* combined result
mat_capp coeftab : coef0 coef1
forvalues i = 2(1)5 {
    mat_capp coeftab : coeftab coef`i'
}

mat_capp stattab : stat0 stat1
forvalues i = 2(1)5 {
    mat_capp stattab : stattab stat`i'
}

mat list coeftab
mat list stattab


********************************************************************************
* Robustness 3: Hetegenous Elasticity Limited by year
********************************************************************************
** ---- HeteroShortElasticity
* overall 
xtreg log_total_g log_price log_price_int2 log_price_int3 log_pinc_all ///
	age sqage i.year##i.living_area i.year##i.gender i.year##i.educ ///
	if year > 2012, ///
	fe vce(cluster pid)

mat coef0 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]	
mat colnames coef0 = Logprice_M0 Logprice2_M0 Logprice3_M0 Loginc_M0

mat stat0 = e(N) \ e(r2)
mat colnames stat0 = M0
mat rownames stat0 = N r2

lincom log_price
mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas1 = Logprice_M0
mat rownames elas1 = e_b e_se e_pval	

lincom log_price + log_price_int2
mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas2 = Logprice2_M0
mat rownames elas2 = e_b e_se e_pval	

lincom log_price + log_price_int3
mat elas3 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas3 = Logprice3_M0
mat rownames elas3 = e_b e_se e_pval	

lincom log_pinc_all
mat elas4 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas4 = Loginc_M0
mat rownames elas4 = e_b e_se e_pval	

mat_capp elas : elas1 elas2
mat_capp elas : elas elas3
mat_capp elas : elas elas4

mat_rapp coef0 : coef0 elas

mat list coef0 
mat list stat0

* extensive
xtreg i_ext_giving log_price log_price_int2 log_price_int3 log_pinc_all ///
	age sqage i.year##i.living_area i.year##i.gender i.year##i.educ ///
	if year > 2012, ///
	fe vce(cluster pid)

mat coef1 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]	
mat colnames coef1 = Logprice_M1 Logprice2_M1 Logprice3_M1 Loginc_M1

mat stat1 = e(N) \ e(r2)
mat colnames stat1 = M1
mat rownames stat1 = N r2

sum i_ext_giving if year > 2012
local mu = r(mean)

lincom log_price * (1/`mu')
mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas1 = Logprice_M1
mat rownames elas1 = e_b e_se e_pval	

lincom (log_price + log_price_int2) * (1/`mu')
mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas2 = Logprice2_M1
mat rownames elas2 = e_b e_se e_pval

lincom (log_price + log_price_int3) * (1/`mu')
mat elas3 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas3 = Logprice3_M1
mat rownames elas3 = e_b e_se e_pval	

lincom log_pinc_all * (1/`mu')
mat elas4 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas4 = Loginc_M1
mat rownames elas4 = e_b e_se e_pval	
	
mat_capp elas : elas1 elas2
mat_capp elas : elas elas3
mat_capp elas : elas elas4

mat_rapp coef1 : coef1 elas

mat list coef1 
mat list stat1
	
* intensive
xtreg log_total_g log_price log_price_int2 log_price_int3 log_pinc_all ///
	age sqage i.year##i.living_area i.year##i.gender i.year##i.educ ///
	if i_ext_giving == 1 & year >= 2012, fe vce(cluster pid)

mat coef2 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]	
mat colnames coef2 = Logprice_M2 Logprice2_M2 Logprice3_M2 Loginc_M2

mat stat2 = e(N) \ e(r2)
mat colnames stat2 = M2
mat rownames stat2 = N r2

lincom log_price
mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas1 = Logprice_M2
mat rownames elas1 = e_b e_se e_pval	

lincom log_price + log_price_int2
mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas2 = Logprice2_M2
mat rownames elas2 = e_b e_se e_pval	

lincom log_price + log_price_int3
mat elas3 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas3 = Logprice3_M2
mat rownames elas3 = e_b e_se e_pval	

lincom log_pinc_all
mat elas4 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas4 = Loginc_M2
mat rownames elas4 = e_b e_se e_pval	

mat_capp elas : elas1 elas2
mat_capp elas : elas elas3
mat_capp elas : elas elas4

mat_rapp coef2 : coef2 elas

mat list coef2 
mat list stat2

* overall (ideal_balanceid >0)
xtreg log_total_g log_price log_price_int2 log_price_int3 log_pinc_all ///
	age sqage i.year##i.living_area i.year##i.gender i.year##i.educ ///
	if ideal_balanceid > 0 & year >= 2012, fe vce(cluster pid)

mat coef3 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]	
mat colnames coef3 = Logprice_M3 Logprice2_M3 Logprice3_M3 Loginc_M3

mat stat3 = e(N) \ e(r2)
mat colnames stat3 = M3
mat rownames stat3 = N r2

lincom log_price
mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas1 = Logprice_M3
mat rownames elas1 = e_b e_se e_pval	

lincom log_price + log_price_int2
mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas2 = Logprice2_M3
mat rownames elas2 = e_b e_se e_pval	

lincom log_price + log_price_int3
mat elas3 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas3 = Logprice3_M3
mat rownames elas3 = e_b e_se e_pval	

lincom log_pinc_all
mat elas4 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas4 = Loginc_M3
mat rownames elas4 = e_b e_se e_pval	

mat_capp elas : elas1 elas2
mat_capp elas : elas elas3
mat_capp elas : elas elas4

mat_rapp coef3 : coef3 elas

mat list coef3 
mat list stat3

* extensive (ideal_balanceid > 0)
xtreg i_ext_giving log_price log_price_int2 log_price_int3 log_pinc_all ///
	age sqage i.year##i.living_area i.year##i.gender i.year##i.educ ///
	if ideal_balanceid > 0 & year >= 2012, fe vce(cluster pid)

mat coef4 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]	
mat colnames coef4 = Logprice_M4 Logprice2_M4 Logprice3_M4 Loginc_M4

mat stat4 = e(N) \ e(r2)
mat colnames stat4 = M4
mat rownames stat4 = N r2

sum i_ext_giving if ideal_balanceid > 0 & year >= 2012
local mu = r(mean)

lincom log_price * (1/`mu')
mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas1 = Logprice_M4
mat rownames elas1 = e_b e_se e_pval	

lincom (log_price + log_price_int2) * (1/`mu')
mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas2 = Logprice2_M4
mat rownames elas2 = e_b e_se e_pval

lincom (log_price + log_price_int3) * (1/`mu')
mat elas3 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas3 = Logprice3_M4
mat rownames elas3 = e_b e_se e_pval	

lincom log_pinc_all * (1/`mu')
mat elas4 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas4 = Loginc_M4
mat rownames elas4 = e_b e_se e_pval	
	
mat_capp elas : elas1 elas2
mat_capp elas : elas elas3
mat_capp elas : elas elas4

mat_rapp coef4 : coef4 elas

mat list coef4
mat list stat4
	
* intensive (ideal_balanceid > 0)
xtreg log_total_g log_price log_price_int2 log_price_int3 log_pinc_all ///
	age sqage i.year##i.living_area i.year##i.gender i.year##i.educ ///
	if i_ext_giving == 1 & ideal_balanceid > 0 & year >= 2012, fe vce(cluster pid)

mat coef5 = r(table)["b".."pvalue", "log_price".."log_pinc_all"]	
mat colnames coef5 = Logprice_M5 Logprice2_M5 Logprice3_M5 Loginc_M5

mat stat5 = e(N) \ e(r2)
mat colnames stat5 = M5
mat rownames stat5 = N r2

lincom log_price
mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas1 = Logprice_M5
mat rownames elas1 = e_b e_se e_pval	

lincom log_price + log_price_int2
mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas2 = Logprice2_M5
mat rownames elas2 = e_b e_se e_pval	

lincom log_price + log_price_int3
mat elas3 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas3 = Logprice3_M5
mat rownames elas3 = e_b e_se e_pval	

lincom log_pinc_all
mat elas4 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
mat colnames elas4 = Loginc_M5
mat rownames elas4 = e_b e_se e_pval	

mat_capp elas : elas1 elas2
mat_capp elas : elas elas3
mat_capp elas : elas elas4

mat_rapp coef5 : coef5 elas

mat list coef5 
mat list stat5

* combined result
mat_capp coeftab : coef0 coef1
forvalues i = 2(1)5 {
    mat_capp coeftab : coeftab coef`i'
}

mat_capp stattab : stat0 stat1
forvalues i = 2(1)5 {
    mat_capp stattab : stattab stat`i'
}

mat list coeftab
mat list stattab

********************************************************************************
* Robustness 4: Hetegenous Elasticity Limited by year
********************************************************************************
** ---- HeterokDiffElasticity
* overall 
forvalues k = 1(1)3 {
    
	di "lag = `k'"
	
    xtreg log_diff`k'g log_iv`k'price log_iv`k'price_int2 log_iv`k'price_int3 log_diff`k'I ///
		diff`k'_age diff`k'_sqage i.year##i.educ i.year##i.gender i.year##i.living_area, ///
		fe vce(cluster pid)
	
	mat coef = r(table)["b".."pvalue","log_iv`k'price".."log_diff`k'I"]
	mat colnames coef = Logdiffprice_M`k' Logdiffprice2_M`k' Logdiffprice3_M`k' Logdiffinc_M`k'
	
	mat stat = e(N) \ e(r2)
	mat colnames stat = M`k'
	mat rownames stat = N r2
	
	lincom log_iv`k'price
	mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas1 = Logdiffprice_M`k'
	mat rownames elas1 = e_b e_se e_pval	

	lincom log_iv`k'price + log_iv`k'price_int2
	mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas2 = Logdiffprice2_M`k'
	mat rownames elas2 = e_b e_se e_pval	

	lincom log_iv`k'price + log_iv`k'price_int3
	mat elas3 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas3 = Logdiffprice3_M`k'
	mat rownames elas3 = e_b e_se e_pval	

	lincom log_diff`k'I
	mat elas4 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas4 = Logdiffinc_M`k'
	mat rownames elas4 = e_b e_se e_pval	

	mat_capp elas : elas1 elas2
	mat_capp elas : elas elas3
	mat_capp elas : elas elas4
	
	mat_rapp coef : coef elas
	
	if `k' == 1 {
	    mat coeftab = coef
		mat stattab = stat
	}
	else {
	    mat_capp coeftab : coeftab coef
		mat_capp stattab : stattab stat
	}
	
}

* intensive
local j = 4
forvalues k = 1(1)3 {
    
	di "Model `j': lag = `k'"
	
    xtreg log_diff`k'g log_iv`k'price log_iv`k'price_int2 log_iv`k'price_int3 log_diff`k'I ///
		diff`k'_age diff`k'_sqage i.year##i.educ i.year##i.gender i.year##i.living_area ///
		if i_ext_giving == 1, fe vce(cluster pid)
	
	mat coef = r(table)["b".."pvalue","log_iv`k'price".."log_diff`k'I"]
	mat colnames coef = Logdiffprice_M`j' Logdiffprice2_M`j' Logdiffprice3_M`j' Logdiffinc_M`j'
	
	mat stat = e(N) \ e(r2)
	mat colnames stat = M`j'
	mat rownames stat = N r2
	
	lincom log_iv`k'price
	mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas1 = Logdiffprice_M`j'
	mat rownames elas1 = e_b e_se e_pval	

	lincom log_iv`k'price + log_iv`k'price_int2
	mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas2 = Logdiffprice2_M`j'
	mat rownames elas2 = e_b e_se e_pval	

	lincom log_iv`k'price + log_iv`k'price_int3
	mat elas3 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas3 = Logdiffprice3_M`j'
	mat rownames elas3 = e_b e_se e_pval	

	lincom log_diff`k'I
	mat elas4 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas4 = Logdiffinc_M`j'
	mat rownames elas4 = e_b e_se e_pval	

	mat_capp elas : elas1 elas2
	mat_capp elas : elas elas3
	mat_capp elas : elas elas4
	
	mat_rapp coef : coef elas
	
	mat_capp coeftab : coeftab coef
	mat_capp stattab : stattab stat
	
	local j = `++j'
	
}

mat list coeftab
mat list stattab

** ---- SubsetHeterokDiffElasticity
* overall 
forvalues k = 1(1)3 {
    
	di "lag = `k'"
	
    xtreg log_diff`k'g log_iv`k'price log_iv`k'price_int2 log_iv`k'price_int3 log_diff`k'I ///
		diff`k'_age diff`k'_sqage i.year##i.educ i.year##i.gender i.year##i.living_area ///
		if ideal_balanceid > 0, fe vce(cluster pid)
	
	mat coef = r(table)["b".."pvalue","log_iv`k'price".."log_diff`k'I"]
	mat colnames coef = Logdiffprice_M`k' Logdiffprice2_M`k' Logdiffprice3_M`k' Logdiffinc_M`k'
	
	mat stat = e(N) \ e(r2)
	mat colnames stat = M`k'
	mat rownames stat = N r2
	
	lincom log_iv`k'price
	mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas1 = Logdiffprice_M`k'
	mat rownames elas1 = e_b e_se e_pval	

	lincom log_iv`k'price + log_iv`k'price_int2
	mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas2 = Logdiffprice2_M`k'
	mat rownames elas2 = e_b e_se e_pval	

	lincom log_iv`k'price + log_iv`k'price_int3
	mat elas3 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas3 = Logdiffprice3_M`k'
	mat rownames elas3 = e_b e_se e_pval	

	lincom log_diff`k'I
	mat elas4 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas4 = Logdiffinc_M`k'
	mat rownames elas4 = e_b e_se e_pval	

	mat_capp elas : elas1 elas2
	mat_capp elas : elas elas3
	mat_capp elas : elas elas4
	
	mat_rapp coef : coef elas
	
	if `k' == 1 {
	    mat coeftab = coef
		mat stattab = stat
	}
	else {
	    mat_capp coeftab : coeftab coef
		mat_capp stattab : stattab stat
	}
	
}

* intensive
local j = 4
forvalues k = 1(1)3 {
    
	di "Model `j': lag = `k'"
	
    xtreg log_diff`k'g log_iv`k'price log_iv`k'price_int2 log_iv`k'price_int3 log_diff`k'I ///
		diff`k'_age diff`k'_sqage i.year##i.educ i.year##i.gender i.year##i.living_area ///
		if i_ext_giving == 1 & ideal_balanceid > 0, fe vce(cluster pid)
	
	mat coef = r(table)["b".."pvalue","log_iv`k'price".."log_diff`k'I"]
	mat colnames coef = Logdiffprice_M`j' Logdiffprice2_M`j' Logdiffprice3_M`j' Logdiffinc_M`j'
	
	mat stat = e(N) \ e(r2)
	mat colnames stat = M`j'
	mat rownames stat = N r2
	
	lincom log_iv`k'price
	mat elas1 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas1 = Logdiffprice_M`j'
	mat rownames elas1 = e_b e_se e_pval	

	lincom log_iv`k'price + log_iv`k'price_int2
	mat elas2 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas2 = Logdiffprice2_M`j'
	mat rownames elas2 = e_b e_se e_pval	

	lincom log_iv`k'price + log_iv`k'price_int3
	mat elas3 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas3 = Logdiffprice3_M`j'
	mat rownames elas3 = e_b e_se e_pval	

	lincom log_diff`k'I
	mat elas4 = r(estimate) \ r(se) \ ttail(r(df), abs(r(estimate)/r(se)))*2
	mat colnames elas4 = Logdiffinc_M`j'
	mat rownames elas4 = e_b e_se e_pval	

	mat_capp elas : elas1 elas2
	mat_capp elas : elas elas3
	mat_capp elas : elas elas4
	
	mat_rapp coef : coef elas
	
	mat_capp coeftab : coeftab coef
	mat_capp stattab : stattab stat
	
	local j = `++j'
	
}

mat list coeftab
mat list stattab
