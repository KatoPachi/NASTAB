
cd "C:\Users\vge00\Desktop\nastab"  //root path

* call data
use "data\shaped.dta", clear

* variables
gen price = .
replace price = 1 - mtr if year < 2014
replace price = 1 - 0.15 if year >= 2014

gen log_price = ln(price)
gen log_total_g = ln(i_total_giving + 1)
gen log_pinc_all = ln(lincome + 100000)

gen welfare_level_3scale = .
replace welfare_level_3scale = 1 if welfare_level < 3
replace welfare_level_3scale = 2 if welfare_level == 3
replace welfare_level_3scale = 3 if welfare_level > 3

* panel data
xtset pid

/***************

Use original scale of perceived welfare level (5-Likert Scale)

***************/


/*
Total effect during year >=2012
*/

* baseline: individual FE and time FE
xtreg log_total_g i.welfare_level_3scale log_price log_pinc_all i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)
estimates store twoFE

* robustness 1: + age
xtreg log_total_g i.welfare_level_3scale log_price log_pinc_all age i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)
estimates store Add_Age
	
* robustness 2: + education-time FE
xtreg log_total_g i.welfare_level_3scale log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)
estimates store Add_EductationTimeFE
	
* robustness 3: + gender-time FE	
xtreg log_total_g i.welfare_level_3scale log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)
estimates store Add_GenderTimeFE
	
* robustness 4: + living-time FE	
xtreg log_total_g i.welfare_level_3scale log_price log_pinc_all age i.year##i.gender i.year##i.educ i.year##i.living_area ///
	if year >= 2012, ///
	fe vce(cluster pid)
estimates store Add_LivingTimeFE

estimates table twoFE Add_Age Add_GenderTimeFE Add_LivingTimeFE, ///
	keep(i.welfare_level log_price log_pinc_all) ///
	b(%9.3f) se(%9.3f) p(%9.3f) stats(N r2) ///
	title("Intensive + Extensive Margin")  ///
	modelwidth(18) varwidth(15)
estimates clear
	
/*
Extensive Margin during year >=2012
*/

* baseline: individual FE and time FE
xtreg i_ext_giving i.welfare_level_3scale log_price log_pinc_all i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)
estimates store twoFE

* robustness 1: + age
xtreg i_ext_giving i.welfare_level_3scale log_price log_pinc_all age i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)
estimates store Add_Age
	
* robustness 2: + education-time FE
xtreg i_ext_giving i.welfare_level_3scale log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)
estimates store Add_EductationTimeFE
	
* robustness 3: + gender-time FE	
xtreg i_ext_giving i.welfare_level_3scale log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)
estimates store Add_GenderTimeFE
	
* robustness 4: + living-time FE	
xtreg i_ext_giving i.welfare_level_3scale log_price log_pinc_all age i.year##i.gender i.year##i.educ i.year##i.living_area ///
	if year >= 2012, ///
	fe vce(cluster pid)
estimates store Add_LivingTimeFE

estimates table twoFE Add_Age Add_GenderTimeFE Add_LivingTimeFE, ///
	keep(i.welfare_level log_price log_pinc_all) ///
	b(%9.3f) se(%9.3f) p(%9.3f) stats(N r2) ///
	title("Extensive Margin")  ///
	modelwidth(18) varwidth(15)
estimates clear

/*
Intensive Margin during year >=2012
*/

* baseline: individual FE and time FE
xtreg log_total_g i.welfare_level_3scale log_price log_pinc_all i.year ///
	if year >= 2012 & i_ext_giving == 1, ///
	fe vce(cluster pid)
estimates store twoFE

* robustness 1: + age
xtreg log_total_g i.welfare_level_3scale log_price log_pinc_all age i.year ///
	if year >= 2012 & i_ext_giving == 1, ///
	fe vce(cluster pid)
estimates store Add_Age
	
* robustness 2: + education-time FE
xtreg log_total_g i.welfare_level_3scale log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012 & i_ext_giving == 1, ///
	fe vce(cluster pid)
estimates store Add_EductationTimeFE
	
* robustness 3: + gender-time FE	
xtreg log_total_g i.welfare_level_3scale log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012 & i_ext_giving == 1, ///
	fe vce(cluster pid)
estimates store Add_GenderTimeFE
	
* robustness 4: + living-time FE	
xtreg log_total_g i.welfare_level_3scale log_price log_pinc_all age i.year##i.gender i.year##i.educ i.year##i.living_area ///
	if year >= 2012 & i_ext_giving == 1, ///
	fe vce(cluster pid)
estimates store Add_LivingTimeFE

estimates table twoFE Add_Age Add_GenderTimeFE Add_LivingTimeFE, ///
	keep(i.welfare_level log_price log_pinc_all) ///
	b(%9.3f) se(%9.3f) p(%9.3f) stats(N r2) ///
	title("Intensive Margin")  ///
	modelwidth(18) varwidth(15)
estimates clear

/*
Heterogenous effect of logged price by subset analysis
*/

* Intensive + Extensive margin
xtreg log_total_g log_price log_pinc_all i.year ///
	if year >= 2012 & welfare_level_3scale == 1, ///
	fe vce(cluster pid)
estimates store Low

xtreg log_total_g log_price log_pinc_all i.year ///
	if year >= 2012 & welfare_level_3scale == 2, ///
	fe vce(cluster pid)
estimates store Neutral

xtreg log_total_g log_price log_pinc_all i.year ///
	if year >= 2012 & welfare_level_3scale == 3, ///
	fe vce(cluster pid)
estimates store High

estimates table Low Neutral High, ///
	keep(log_price log_pinc_all) ///
	b(%9.3f) se(%9.3f) p(%9.3f) stats(N r2) ///
	title("Intensive + Extensive Margin Grouped by Perceived Welfare Level")  ///
	modelwidth(15) varwidth(15)
estimates clear

* Extensive Margin
xtreg i_ext_giving log_price log_pinc_all i.year ///
	if year >= 2012 & welfare_level_3scale == 1, ///
	fe vce(cluster pid)
estimates store Low

xtreg i_ext_giving log_price log_pinc_all i.year ///
	if year >= 2012 & welfare_level_3scale == 2, ///
	fe vce(cluster pid)
estimates store Neutral

xtreg i_ext_giving log_price log_pinc_all i.year ///
	if year >= 2012 & welfare_level_3scale == 3, ///
	fe vce(cluster pid)
estimates store High

estimates table Low Neutral High, ///
	keep(log_price log_pinc_all) ///
	b(%9.3f) se(%9.3f) p(%9.3f) stats(N r2) ///
	title("Extensive Margin Grouped by Perceived Welfare Level")  ///
	modelwidth(15) varwidth(15)
estimates clear

* Intensive Margin
xtreg log_total_g log_price log_pinc_all i.year ///
	if year >= 2012 & welfare_level_3scale == 1 & i_ext_giving == 1, ///
	fe vce(cluster pid)
estimates store Low

xtreg log_total_g log_price log_pinc_all i.year ///
	if year >= 2012 & welfare_level_3scale == 2 & i_ext_giving == 1, ///
	fe vce(cluster pid)
estimates store Neutral

xtreg log_total_g log_price log_pinc_all i.year ///
	if year >= 2012 & welfare_level_3scale == 3 & i_ext_giving == 1, ///
	fe vce(cluster pid)
estimates store High

estimates table Low Neutral High, ///
	keep(log_price log_pinc_all) ///
	b(%9.3f) se(%9.3f) p(%9.3f) stats(N r2) ///
	title("Intensive Margin Grouped by Perceived Welfare Level")  ///
	modelwidth(15) varwidth(15)
estimates clear

/*
Heterogenous effect of logged price (interaction model)
*/

* Intensive + Extensive margin
xtreg log_total_g i.welfare_level_3scale##c.log_price log_pinc_all i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)
estimates store twoFE

xtreg log_total_g i.welfare_level_3scale##c.log_price log_pinc_all age i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)
estimates store Add_Age
	
xtreg log_total_g i.welfare_level_3scale##c.log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)
estimates store Add_EductationTimeFE
	
xtreg log_total_g i.welfare_level_3scale##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)
estimates store Add_GenderTimeFE
	
xtreg log_total_g i.welfare_level_3scale##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ i.year##i.living_area ///
	if year >= 2012, ///
	fe vce(cluster pid)
estimates store Add_LivingTimeFE

estimates table twoFE Add_Age Add_GenderTimeFE Add_LivingTimeFE, ///
	keep(log_price welfare_level_3scale#c.log_price log_pinc_all) ///
	b(%9.3f) se(%9.3f) p(%9.3f) stats(N r2) ///
	title("Heterogenous Effect of Logged Price: Intensive + Extensive Margin")  ///
	modelwidth(18) varwidth(20)
estimates clear

* Extensive margin
xtreg i_ext_giving i.welfare_level_3scale##c.log_price log_pinc_all i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)
estimates store twoFE

xtreg i_ext_giving i.welfare_level_3scale##c.log_price log_pinc_all age i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)
estimates store Add_Age
	
xtreg i_ext_giving i.welfare_level_3scale##c.log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)
estimates store Add_EductationTimeFE
	
xtreg i_ext_giving i.welfare_level_3scale##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)
estimates store Add_GenderTimeFE
	
xtreg i_ext_giving i.welfare_level_3scale##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ i.year##i.living_area ///
	if year >= 2012, ///
	fe vce(cluster pid)
estimates store Add_LivingTimeFE

estimates table twoFE Add_Age Add_GenderTimeFE Add_LivingTimeFE, ///
	keep(log_price welfare_level_3scale#c.log_price log_pinc_all) ///
	b(%9.3f) se(%9.3f) p(%9.3f) stats(N r2) ///
	title("Heterogenous Effect of Logged Price: Extensive Margin")  ///
	modelwidth(18) varwidth(20)
estimates clear

* Intensive margin
xtreg log_total_g i.welfare_level_3scale##c.log_price log_pinc_all i.year ///
	if year >= 2012 & i_ext_giving == 1, ///
	fe vce(cluster pid)
estimates store twoFE

xtreg log_total_g i.welfare_level_3scale##c.log_price log_pinc_all age i.year ///
	if year >= 2012 & i_ext_giving == 1, ///
	fe vce(cluster pid)
estimates store Add_Age
	
xtreg log_total_g i.welfare_level_3scale##c.log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012 & i_ext_giving == 1, ///
	fe vce(cluster pid)
estimates store Add_EductationTimeFE
	
xtreg log_total_g i.welfare_level_3scale##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012 & i_ext_giving == 1, ///
	fe vce(cluster pid)
estimates store Add_GenderTimeFE
	
xtreg log_total_g i.welfare_level_3scale##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ i.year##i.living_area ///
	if year >= 2012 & i_ext_giving == 1, ///
	fe vce(cluster pid)
estimates store Add_LivingTimeFE

estimates table twoFE Add_Age Add_GenderTimeFE Add_LivingTimeFE, ///
	keep(log_price welfare_level#c.log_price log_pinc_all) ///
	b(%9.3f) se(%9.3f) p(%9.3f) stats(N r2) ///
	title("Heterogenous Effect of Logged Price: Intensive Margin")  ///
	modelwidth(18) varwidth(20)
estimates clear