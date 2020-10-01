
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

gen high_welfare_level = .
replace high_welfare_level = 1 if welfare_level >= 3
replace high_welfare_level = 0 if welfare_level < 3
replace high_welfare_level = . if welfare_level == .

gen bin_trust = .
replace bin_trust = 1 if trust_politician >= 3
replace bin_trust = 0 if trust_politician < 3
replace bin_trust = . if trust_politician == .

* panel data
xtset pid


/*
Total effect during year >=2012
*/

* baseline: individual FE and time FE
xtreg log_total_g high_welfare_level log_price log_pinc_all i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/totalef.txt", replace ///
	keep(high_welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, N, Education-Time FE, N, Gender-Time FE, N, Living-Time FE, N)

* robustness 1: + age
xtreg log_total_g high_welfare_level log_price log_pinc_all age i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/totalef.txt", ///
	append keep(high_welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, N, Gender-Time FE, N, Living-Time FE, N)
	
* robustness 2: + education-time FE
xtreg log_total_g high_welfare_leve log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/totalef.txt", ///
	append keep(high_welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, N, Living-Time FE, N)
	
* robustness 3: + gender-time FE	
xtreg log_total_g high_welfare_level log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/totalef.txt", ///
	append keep(high_welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, Y, Living-Time FE, N)

* robustness 4: + living-time FE	
xtreg log_total_g high_welfare_level log_price log_pinc_all age i.year##i.gender i.year##i.educ i.year##i.living_area ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/totalef.txt", ///
	append keep(high_welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, Y, Living-Time FE, Y)

* heterogeneity: individual FE and time FE
xtreg log_total_g high_welfare_leve##c.log_price log_pinc_all i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/totalef_hetero.txt", replace ///
	keep(1.high_welfare_level log_price 1.high_welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, N, Education-Time FE, N, Gender-Time FE, N, Living-Time FE, N)

* robust hetero 1: + age
xtreg log_total_g high_welfare_level##c.log_price log_pinc_all age i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/totalef_hetero.txt", append ///
	keep(1.high_welfare_level log_price 1.high_welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, N, Gender-Time FE, N, Living-Time FE, N)

* robust hetero 2: + education-time FE
xtreg log_total_g high_welfare_lev##c.log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/totalef_hetero.txt", append ///
	keep(1.high_welfare_level log_price 1.high_welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, N, Living-Time FE, N)

* robust hetero 3: + gender-time FE	
xtreg log_total_g high_welfare_leve##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/totalef_hetero.txt", append ///
	keep(1.high_welfare_level log_price 1.high_welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, Y, Living-Time FE, N)
	
* robust hetero 4: + living-time FE	
xtreg log_total_g high_welfare_level##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ i.year##i.living_area ///
	if year >= 2012, ///
	fe vce(cluster pid)
	
outreg2 using "_assets/stata/totalef_hetero.txt", append ///
	keep(1.high_welfare_level log_price 1.high_welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, Y, Living-Time FE, Y)
	

	
/*
Extensive margin during year >=2012
*/

* baseline: individual FE and time FE
xtreg i_ext_giving high_welfare_level log_price log_pinc_all i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/extensive.txt", replace ///
	keep(high_welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, N, Education-Time FE, N, Gender-Time FE, N, Living-Time FE, N)

* robustness 1: + age
xtreg i_ext_giving high_welfare_level log_price log_pinc_all age i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/extensive.txt", ///
	append keep(high_welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, N, Gender-Time FE, N, Living-Time FE, N)
	
* robustness 2: + education-time FE
xtreg i_ext_giving high_welfare_leve log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/extensive.txt", ///
	append keep(high_welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, N, Living-Time FE, N)
	
* robustness 3: + gender-time FE	
xtreg i_ext_giving high_welfare_level log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/extensive.txt", ///
	append keep(high_welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, Y, Living-Time FE, N)

* robustness 4: + living-time FE	
xtreg i_ext_giving high_welfare_level log_price log_pinc_all age i.year##i.gender i.year##i.educ i.year##i.living_area ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/extensive.txt", ///
	append keep(high_welfare_level log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, Y, Living-Time FE, Y)

* heterogeneity: individual FE and time FE
xtreg i_ext_giving high_welfare_leve##c.log_price log_pinc_all i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/extensive_hetero.txt", replace ///
	keep(1.high_welfare_level log_price 1.high_welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, N, Education-Time FE, N, Gender-Time FE, N, Living-Time FE, N)

* robust hetero 1: + age
xtreg i_ext_giving high_welfare_level##c.log_price log_pinc_all age i.year ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/extensive_hetero.txt", append ///
	keep(1.high_welfare_level log_price 1.high_welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, N, Gender-Time FE, N, Living-Time FE, N)

* robust hetero 2: + education-time FE
xtreg i_ext_giving high_welfare_lev##c.log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/extensive_hetero.txt", append ///
	keep(1.high_welfare_level log_price 1.high_welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, N, Living-Time FE, N)

* robust hetero 3: + gender-time FE	
xtreg i_ext_giving high_welfare_leve##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/extensive_hetero.txt", append ///
	keep(1.high_welfare_level log_price 1.high_welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, Y, Living-Time FE, N)
	
* robust hetero 4: + living-time FE	
xtreg i_ext_giving high_welfare_level##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ i.year##i.living_area ///
	if year >= 2012, ///
	fe vce(cluster pid)
	
outreg2 using "_assets/stata/extensive_hetero.txt", append ///
	keep(1.high_welfare_level log_price 1.high_welfare_level##c.log_price log_pinc_all) ///
	addtext(Time FE, Y, Individual FE, Y, Age, Y, Education-Time FE, Y, Gender-Time FE, Y, Living-Time FE, Y)
	
	
	
	
	
	
	
	