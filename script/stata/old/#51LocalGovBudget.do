
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
gen log_PPP_pubbdg = ln(pubbdg + 1)
gen sqlog_PPP_pubpdg = log_PPP_pubbdg^2
gen log_PPP_healthbdg = ln(healthbdg + 1)
gen sqlog_PPP_healthpdg = log_PPP_healthbdg^2
gen welfarebdg = pubbdg + healthbdg
gen PPP_welfarebdg = welfarebdg/pop
gen log_PPP_welfarebdg = ln(PPP_welfarebdg + 1)

* panel data
xtset pid

/*******************************************************************************

	Use local government's budget for welfare
	This is geographical variation within year

*******************************************************************************/

/* intensive + extensive margin (public services)*/

* model 1: baseline
xtreg log_total_g log_PPP_pubbdg log_price log_pinc_all i.year ///
	if year >= 2012, fe vce(cluster pid)

estimates store twoFE

* model 2: + age
xtreg log_total_g log_PPP_pubbdg log_price log_pinc_all age i.year ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddAge

* model 3: + education-time dummies
xtreg log_total_g log_PPP_pubbdg log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddEduc

* model 4: + gender-time dummies
xtreg log_total_g log_PPP_pubbdg log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddGender1

* model 5: + squared logged government budge
xtreg log_total_g log_PPP_pubbdg sqlog_PPP_pubpdg log_price log_pinc_all ///
	age i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddGender2

* graphical representation
gen pred_e_health = _b[log_PPP_healthbdg] + 2 * _b[sqlog_PPP_healthpdg] * log_PPP_healthbdg
table living_area year if pred_e_pub > 0 & year >= 2012

gen special_area = 3
replace special_area = 1 if living_area == 11
replace special_area = 2 if living_area == 31 

twoway  (scatter pred_e_pub PPP_pubbdg if year >= 2012 & special_area == 1) ///
	    (scatter pred_e_pub PPP_pubbdg if year >= 2012 & special_area == 2) ///
	    (scatter pred_e_pub PPP_pubbdg if year >= 2012 & special_area == 3), ///
		yline(0, lpattern(dash) lcolor(red)) ///
		legend(label(1 "living area = 11") label(2 "living area = 31") label(3 "Other living areas")) ///
		xtitle("Social Welfare Budget per capita")  ///
		ytitle("Predicted Elasticity")  ///
		title("Predicted Budget-Elasticity of Donations")  

* regression table
estimates table twoFE AddAge AddEduc AddGender1 AddGender2, ///
	keep(log_PPP_pubbdg sqlog_PPP_pubpdg log_price log_pinc_all) ///
	b(%9.3f) se(%9.3f) p(%9.3f) stats(N r2) ///
	title("Intensive + Extensive Margin")  ///
	modelwidth(12) varwidth(18)
estimates clear

/* extensive margin (public services)*/

* model 1: baseline
xtreg i_ext_giving log_PPP_pubbdg log_price log_pinc_all i.year ///
	if year >= 2012, fe vce(cluster pid)

estimates store twoFE

* model 2: + age
xtreg i_ext_giving log_PPP_pubbdg log_price log_pinc_all age i.year ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddAge

* model 3: + education-time dummies
xtreg i_ext_giving log_PPP_pubbdg log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddEduc

* model 4: + gender-time dummies
xtreg i_ext_giving log_PPP_pubbdg log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddGender1

* model 5: + squared logged government budge
xtreg i_ext_giving log_PPP_pubbdg sqlog_PPP_pubpdg log_price log_pinc_all ///
	age i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddGender2

* regression table
estimates table twoFE AddAge AddEduc AddGender1 AddGender2, ///
	keep(log_PPP_pubbdg sqlog_PPP_pubpdg log_price log_pinc_all) ///
	b(%9.3f) se(%9.3f) p(%9.3f) stats(N r2) ///
	title("Extensive Margin")  ///
	modelwidth(12) varwidth(18)
estimates clear

/* intensive margin (public services)*/

* model 1: baseline
xtreg log_total_g log_PPP_pubbdg log_price log_pinc_all i.year ///
	if i_ext_giving == 1 & year >= 2012, fe vce(cluster pid)

estimates store twoFE

* model 2: + age
xtreg log_total_g log_PPP_pubbdg log_price log_pinc_all age i.year ///
	if i_ext_giving == 1 & year >= 2012, fe vce(cluster pid)

estimates store AddAge

* model 3: + education-time dummies
xtreg log_total_g log_PPP_pubbdg log_price log_pinc_all age i.year##i.educ ///
	if i_ext_giving == 1 & year >= 2012, fe vce(cluster pid)

estimates store AddEduc

* model 4: + gender-time dummies
xtreg log_total_g log_PPP_pubbdg log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if i_ext_giving == 1 & year >= 2012, fe vce(cluster pid)

estimates store AddGender1

* model 5: + squared logged government budge
xtreg log_total_g log_PPP_pubbdg sqlog_PPP_pubpdg log_price log_pinc_all ///
	age i.year##i.gender i.year##i.educ ///
	if i_ext_giving == 1 & year >= 2012, fe vce(cluster pid)

estimates store AddGender2

* regression table
estimates table twoFE AddAge AddEduc AddGender1 AddGender2, ///
	keep(log_PPP_pubbdg sqlog_PPP_pubpdg log_price log_pinc_all) ///
	b(%9.3f) se(%9.3f) p(%9.3f) stats(N r2) ///
	title("Intensive Margin")  ///
	modelwidth(12) varwidth(18)
estimates clear


/* intensive + extensive margin (healthcare services)*/

* model 1: baseline
xtreg log_total_g log_PPP_healthbdg log_price log_pinc_all i.year ///
	if year >= 2012, fe vce(cluster pid)

estimates store twoFE

* model 2: + age
xtreg log_total_g log_PPP_healthbdg log_price log_pinc_all age i.year ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddAge

* model 3: + education-time dummies
xtreg log_total_g log_PPP_healthbdg log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddEduc

* model 4: + gender-time dummies
xtreg log_total_g log_PPP_healthbdg log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddGender1

* model 5: + squared logged government budge
xtreg log_total_g log_PPP_healthbdg sqlog_PPP_healthpdg log_price log_pinc_all ///
	age i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddGender2

* graphical representation
table living_area year if pred_e_health > 0 & year >= 2012

twoway  (scatter pred_e_health PPP_healthbdg if year >= 2012 & special_area == 1)  ///
		(scatter pred_e_health PPP_healthbdg if year >= 2012 & special_area == 2)  ///
		(scatter pred_e_health PPP_healthbdg if year >= 2012 & special_area == 3), ///
		yline(0, lpattern(dash) lcolor(red))  ///
		legend(label(1 "living area = 11") label(2 "living area = 31") label(3 "Other living areas")) ///
		xtitle("Healthcare Budget per capita")  ///
		ytitle("Predicted Elasticity")  ///
		title("Predicted Budget-Elasticity of Donations")  

* regression table
estimates table twoFE AddAge AddEduc AddGender1 AddGender2, ///
	keep(log_PPP_healthbdg sqlog_PPP_healthpdg log_price log_pinc_all) ///
	b(%9.3f) se(%9.3f) p(%9.3f) stats(N r2) ///
	title("Intensive + Extensive Margin")  ///
	modelwidth(12) varwidth(20)
estimates clear

/* extensive margin (healthcare services)*/

* model 1: baseline
xtreg i_ext_giving log_PPP_healthbdg log_price log_pinc_all i.year ///
	if year >= 2012, fe vce(cluster pid)

estimates store twoFE

* model 2: + age
xtreg i_ext_giving log_PPP_healthbdg log_price log_pinc_all age i.year ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddAge

* model 3: + education-time dummies
xtreg i_ext_giving log_PPP_healthbdg log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddEduc

* model 4: + gender-time dummies
xtreg i_ext_giving log_PPP_healthbdg log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddGender1

* model 5: + squared logged government budge
xtreg i_ext_giving log_PPP_healthbdg sqlog_PPP_healthpdg log_price log_pinc_all ///
	age i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddGender2

* regression table
estimates table twoFE AddAge AddEduc AddGender1 AddGender2, ///
	keep(log_PPP_healthbdg sqlog_PPP_healthpdg log_price log_pinc_all) ///
	b(%9.3f) se(%9.3f) p(%9.3f) stats(N r2) ///
	title("Extensive Margin")  ///
	modelwidth(12) varwidth(20)
estimates clear

/* intensive margin (healthcare services) */

* model 1: baseline
xtreg log_total_g log_PPP_healthbdg log_price log_pinc_all i.year ///
	if i_ext_giving == 1 & year >= 2012, fe vce(cluster pid)

estimates store twoFE

* model 2: + age
xtreg log_total_g log_PPP_healthbdg log_price log_pinc_all age i.year ///
	if i_ext_giving == 1 & year >= 2012, fe vce(cluster pid)

estimates store AddAge

* model 3: + education-time dummies
xtreg log_total_g log_PPP_healthbdg log_price log_pinc_all age i.year##i.educ ///
	if i_ext_giving == 1 & year >= 2012, fe vce(cluster pid)

estimates store AddEduc

* model 4: + gender-time dummies
xtreg log_total_g log_PPP_healthbdg log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if i_ext_giving == 1 & year >= 2012, fe vce(cluster pid)

estimates store AddGender1

* model 5: + squared logged government budge
xtreg log_total_g log_PPP_healthbdg sqlog_PPP_healthpdg log_price log_pinc_all ///
	age i.year##i.gender i.year##i.educ ///
	if i_ext_giving == 1 & year >= 2012, fe vce(cluster pid)

estimates store AddGender2

* regression table
estimates table twoFE AddAge AddEduc AddGender1 AddGender2, ///
	keep(log_PPP_healthbdg sqlog_PPP_healthpdg log_price log_pinc_all) ///
	b(%9.3f) se(%9.3f) p(%9.3f) stats(N r2) ///
	title("Intensive Margin")  ///
	modelwidth(12) varwidth(18)
estimates clear


/****** 
Heterogenous effect of price
******/

/* intensive + extensive margin (public services)*/

* model 1: baseline
xtreg log_total_g c.log_PPP_pubbdg##c.log_price log_pinc_all i.year ///
	if year >= 2012, fe vce(cluster pid)

estimates store twoFE

* model 2: + age
xtreg log_total_g c.log_PPP_pubbdg##c.log_price log_pinc_all age i.year ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddAge

* model 3: + education-time dummies
xtreg log_total_g c.log_PPP_pubbdg##c.log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddEduc

* model 4: + gender-time dummies
xtreg log_total_g c.log_PPP_pubbdg##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddGender1

* regression table
estimates table twoFE AddAge AddEduc AddGender1, ///
	keep(c.log_PPP_pubbd##c.log_price log_pinc_all) ///
	b(%9.3f) se(%9.3f) p(%9.3f) stats(N r2) ///
	title("Intensive + Extensive Margin")  ///
	modelwidth(12) varwidth(18)
estimates clear

/* extensive margin (public services)*/

* model 1: baseline
xtreg i_ext_giving c.log_PPP_pubbdg##c.log_price log_pinc_all i.year ///
	if year >= 2012, fe vce(cluster pid)

estimates store twoFE

* model 2: + age
xtreg i_ext_giving c.log_PPP_pubbdg##c.log_price log_pinc_all age i.year ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddAge

* model 3: + education-time dummies
xtreg i_ext_giving c.log_PPP_pubbdg##c.log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddEduc

* model 4: + gender-time dummies
xtreg i_ext_giving c.log_PPP_pubbdg##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddGender1

* regression table
estimates table twoFE AddAge AddEduc AddGender1, ///
	keep(c.log_PPP_pubbdg##c.log_price log_pinc_all) ///
	b(%9.3f) se(%9.3f) p(%9.3f) stats(N r2) ///
	title("Extensive Margin")  ///
	modelwidth(12) varwidth(18)
estimates clear

/* intensive margin (public services)*/

* model 1: baseline
xtreg log_total_g c.log_PPP_pubbdg##c.log_price log_pinc_all i.year ///
	if i_ext_giving == 1 & year >= 2012, fe vce(cluster pid)

estimates store twoFE

* model 2: + age
xtreg log_total_g c.log_PPP_pubbdg##c.log_price log_pinc_all age i.year ///
	if i_ext_giving == 1 & year >= 2012, fe vce(cluster pid)

estimates store AddAge

* model 3: + education-time dummies
xtreg log_total_g c.log_PPP_pubbdg##c.log_price log_pinc_all age i.year##i.educ ///
	if i_ext_giving == 1 & year >= 2012, fe vce(cluster pid)

estimates store AddEduc

* model 4: + gender-time dummies
xtreg log_total_g c.log_PPP_pubbdg##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if i_ext_giving == 1 & year >= 2012, fe vce(cluster pid)

estimates store AddGender1

* regression table
estimates table twoFE AddAge AddEduc AddGender1, ///
	keep(c.log_PPP_pubbdg##c.log_price log_pinc_all) ///
	b(%9.3f) se(%9.3f) p(%9.3f) stats(N r2) ///
	title("Intensive Margin")  ///
	modelwidth(12) varwidth(18)
estimates clear

/* intensive + extensive margin (healthcare services)*/

* model 1: baseline
xtreg log_total_g c.log_PPP_healthbdg##c.log_price log_pinc_all i.year ///
	if year >= 2012, fe vce(cluster pid)

estimates store twoFE

* model 2: + age
xtreg log_total_g c.log_PPP_healthbdg##c.log_price log_pinc_all age i.year ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddAge

* model 3: + education-time dummies
xtreg log_total_g c.log_PPP_healthbdg##c.log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddEduc

* model 4: + gender-time dummies
xtreg log_total_g c.log_PPP_healthbdg##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddGender1

* regression table
estimates table twoFE AddAge AddEduc AddGender1, ///
	keep(c.log_PPP_healthbdg##c.log_price log_pinc_all) ///
	b(%9.3f) se(%9.3f) p(%9.3f) stats(N r2) ///
	title("Intensive + Extensive Margin")  ///
	modelwidth(12) varwidth(20)
estimates clear

/* extensive margin (healthcare services)*/

* model 1: baseline
xtreg i_ext_giving c.log_PPP_healthbdg##c.log_price log_pinc_all i.year ///
	if year >= 2012, fe vce(cluster pid)

estimates store twoFE

* model 2: + age
xtreg i_ext_giving c.log_PPP_healthbdg##c.log_price log_pinc_all age i.year ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddAge

* model 3: + education-time dummies
xtreg i_ext_giving c.log_PPP_healthbdg##c.log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddEduc

* model 4: + gender-time dummies
xtreg i_ext_giving c.log_PPP_healthbdg##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

estimates store AddGender1

* regression table
estimates table twoFE AddAge AddEduc AddGender1, ///
	keep(c.log_PPP_healthbdg##c.log_price log_pinc_all) ///
	b(%9.3f) se(%9.3f) p(%9.3f) stats(N r2) ///
	title("Extensive Margin")  ///
	modelwidth(12) varwidth(20)
estimates clear

/* intensive margin (healthcare services) */

* model 1: baseline
xtreg log_total_g c.log_PPP_healthbdg##c.log_price log_pinc_all i.year ///
	if i_ext_giving == 1 & year >= 2012, fe vce(cluster pid)

estimates store twoFE

* model 2: + age
xtreg log_total_g c.log_PPP_healthbdg##c.log_price log_pinc_all age i.year ///
	if i_ext_giving == 1 & year >= 2012, fe vce(cluster pid)

estimates store AddAge

* model 3: + education-time dummies
xtreg log_total_g c.log_PPP_healthbdg##c.log_price log_pinc_all age i.year##i.educ ///
	if i_ext_giving == 1 & year >= 2012, fe vce(cluster pid)

estimates store AddEduc

* model 4: + gender-time dummies
xtreg log_total_g c.log_PPP_healthbdg##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if i_ext_giving == 1 & year >= 2012, fe vce(cluster pid)

estimates store AddGender1

* regression table
estimates table twoFE AddAge AddEduc AddGender1, ///
	keep(c.log_PPP_healthbdg##c.log_price log_pinc_all) ///
	b(%9.3f) se(%9.3f) p(%9.3f) stats(N r2) ///
	title("Intensive Margin")  ///
	modelwidth(12) varwidth(20)
estimates clear