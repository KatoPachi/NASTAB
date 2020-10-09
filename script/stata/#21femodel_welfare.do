
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

gen welfare_level = .
replace welfare_level = 1 if welfare_level > 3
replace welfare_level = 0 if welfare_level <= 3

gen low_welfare_level = .
replace low_welfare_level = 1 if welfare_level < 3
replace low_welfare_level = 0 if welfare_level >= 3

gen neutral_welfare_level = .
replace neutral_welfare_level = 1 if welfare_level == 3
replace neutral_welfare_level = 0 if welfare_level != 3

* panel data
xtset pid

/***************

Use original scale of perceived welfare level (5-Likert Scale)

***************/

/*
Total effect during year >=2012
*/

* baseline: individual FE and time FE
xtreg log_total_g welfare_level log_price log_pinc_all i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/totalef.txt", replace ///
	keep(welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, N, Education-Time FE, N, Gender-Time FE, N, Living-Time FE, N)

* robustness 1: + age
xtreg log_total_g welfare_level log_price log_pinc_all age i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/totalef.txt", ///
	append keep(welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, N, Gender-Time FE, N, Living-Time FE, N)
	
* robustness 2: + education-time FE
xtreg log_total_g welfare_level log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/totalef.txt", ///
	append keep(welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, N, Living-Time FE, N)
	
* robustness 3: + gender-time FE	
xtreg log_total_g welfare_level log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/totalef.txt", ///
	append keep(welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, Y, Living-Time FE, N)

* robustness 4: + living-time FE	
xtreg log_total_g welfare_level log_price log_pinc_all age i.year##i.gender i.year##i.educ i.year##i.living_area ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/totalef.txt", ///
	append keep(welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, Y, Living-Time FE, Y)

* heterogeneity: individual FE and time FE
xtreg log_total_g c.welfare_level##c.log_price log_pinc_all i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/totalef_hetero.txt", replace ///
	keep(welfare_level log_price c.welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, N, Education-Time FE, N, Gender-Time FE, N, Living-Time FE, N)

* robust hetero 1: + age
xtreg log_total_g c.welfare_level##c.log_price log_pinc_all age i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/totalef_hetero.txt", append ///
	keep(welfare_level log_price c.welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, N, Gender-Time FE, N, Living-Time FE, N)

* robust hetero 2: + education-time FE
xtreg log_total_g c.welfare_level##c.log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/totalef_hetero.txt", append ///
	keep(c.welfare_level log_price c.welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, N, Living-Time FE, N)

* robust hetero 3: + gender-time FE	
xtreg log_total_g c.welfare_level##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/totalef_hetero.txt", append ///
	keep(welfare_level log_price c.welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, Y, Living-Time FE, N)
	
* robust hetero 4: + living-time FE	
xtreg log_total_g c.welfare_level##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ i.year##i.living_area ///
	if year >= 2012, ///
	fe vce(cluster pid)
	
outreg2 using "_assets/stata/totalef_hetero.txt", append ///
	keep(welfare_level log_price c.welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, Y, Living-Time FE, Y)
	

	
/*
Extensive margin during year >=2012
*/

* baseline: individual FE and time FE
xtreg i_ext_giving welfare_level log_price log_pinc_all i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/extensive.txt", replace ///
	keep(welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, N, Education-Time FE, N, Gender-Time FE, N, Living-Time FE, N)

* robustness 1: + age
xtreg i_ext_giving welfare_level log_price log_pinc_all age i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/extensive.txt", ///
	append keep(welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, N, Gender-Time FE, N, Living-Time FE, N)
	
* robustness 2: + education-time FE
xtreg i_ext_giving welfare_level log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/extensive.txt", ///
	append keep(welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, N, Living-Time FE, N)
	
* robustness 3: + gender-time FE	
xtreg i_ext_giving welfare_level log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/extensive.txt", ///
	append keep(welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, Y, Living-Time FE, N)

* robustness 4: + living-time FE	
xtreg i_ext_giving welfare_level log_price log_pinc_all age i.year##i.gender i.year##i.educ i.year##i.living_area ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/extensive.txt", ///
	append keep(welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, Y, Living-Time FE, Y)

* heterogeneity: individual FE and time FE
xtreg i_ext_giving c.welfare_level##c.log_price log_pinc_all i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/extensive_hetero.txt", replace ///
	keep(welfare_level log_price c.welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, N, Education-Time FE, N, Gender-Time FE, N, Living-Time FE, N)

* robust hetero 1: + age
xtreg i_ext_giving c.welfare_level##c.log_price log_pinc_all age i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/extensive_hetero.txt", append ///
	keep(welfare_level log_price c.welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, N, Gender-Time FE, N, Living-Time FE, N)

* robust hetero 2: + education-time FE
xtreg i_ext_giving c.welfare_level##c.log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/extensive_hetero.txt", append ///
	keep(welfare_level log_price c.welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, N, Living-Time FE, N)

* robust hetero 3: + gender-time FE	
xtreg i_ext_giving c.welfare_level##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/extensive_hetero.txt", append ///
	keep(welfare_level log_price c.welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, Y, Living-Time FE, N)
	
* robust hetero 4: + living-time FE	
xtreg i_ext_giving c.welfare_level##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ i.year##i.living_area ///
	if year >= 2012, ///
	fe vce(cluster pid)
	
outreg2 using "_assets/stata/extensive_hetero.txt", append ///
	keep(welfare_level log_price c.welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, Y, Living-Time FE, Y)
	
	
	

/*
Intensive margin during year >=2012
*/

* baseline: individual FE and time FE
xtreg log_total_g welfare_level log_price log_pinc_all i.year ///
	if year >= 2012 & i_ext_giving == 1, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/intensive.txt", replace ///
	keep(welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, N, Education-Time FE, N, Gender-Time FE, N, Living-Time FE, N)

* robustness 1: + age
xtreg log_total_g welfare_level log_price log_pinc_all age i.year ///
	if year >= 2012 & i_ext_giving == 1, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/intensive.txt", ///
	append keep(welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, N, Gender-Time FE, N, Living-Time FE, N)
	
* robustness 2: + education-time FE
xtreg log_total_g welfare_level log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012 & i_ext_giving == 1, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/intensive.txt", ///
	append keep(welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, N, Living-Time FE, N)
	
* robustness 3: + gender-time FE	
xtreg log_total_g welfare_level log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012 & i_ext_giving == 1, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/intensive.txt", ///
	append keep(welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, Y, Living-Time FE, N)

* robustness 4: + living-time FE	
xtreg log_total_g welfare_level log_price log_pinc_all age i.year##i.gender i.year##i.educ i.year##i.living_area ///
	if year >= 2012 & i_ext_giving == 1, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/intensive.txt", ///
	append keep(welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, Y, Living-Time FE, Y)

* heterogeneity: individual FE and time FE
xtreg log_total_g c.welfare_leve##c.log_price log_pinc_all i.year ///
	if year >= 2012 & i_ext_giving == 1, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/intensive_hetero.txt", replace ///
	keep(welfare_level log_price c.welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, N, Education-Time FE, N, Gender-Time FE, N, Living-Time FE, N)

* robust hetero 1: + age
xtreg log_total_g c.welfare_level##c.log_price log_pinc_all age i.year ///
	if year >= 2012 & i_ext_giving == 1, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/intensive_hetero.txt", append ///
	keep(welfare_level log_price c.welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, N, Gender-Time FE, N, Living-Time FE, N)

* robust hetero 2: + education-time FE
xtreg log_total_g c.welfare_level##c.log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012 & i_ext_giving == 1, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/intensive_hetero.txt", append ///
	keep(welfare_level log_price c.welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, N, Living-Time FE, N)

* robust hetero 3: + gender-time FE	
xtreg log_total_g c.welfare_level##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012 & i_ext_giving == 1, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/intensive_hetero.txt", append ///
	keep(welfare_level log_price c.welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, Y, Living-Time FE, N)
	
* robust hetero 4: + living-time FE	
xtreg log_total_g c.welfare_level##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ i.year##i.living_area ///
	if year >= 2012 & i_ext_giving == 1, ///
	fe vce(cluster pid)
	
outreg2 using "_assets/stata/intensive_hetero.txt", append ///
	keep(welfare_level log_price c.welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, Y, Living-Time FE, Y)	
	
	
	
	