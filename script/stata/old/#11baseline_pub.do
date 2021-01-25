cd "C:\Users\vge00\Desktop\nastab"  //root path

eststo clear
use "data\shaped.dta", clear

gen price = .
replace price = 1 - mtr if year < 2014
replace price = 1 - 0.15 if year >= 2014

gen log_price = ln(price)
gen log_total_g = ln(i_total_giving + 1)
gen log_pinc_all = ln(lincome + 100000)
gen log_PPP_pubbdg = ln(PPP_pubbdg + 1)
gen sqlog_PPP_pubpdg = log_PPP_pubbdg^2
gen log_PPP_healthbdg = ln(PPP_healthbdg + 1)
gen sqlog_PPP_healthpdg = log_PPP_healthbdg^2

xtset pid

/*******************************************************************************

Baseline results using budge for social welfare

*******************************************************************************/

* intensive + extensive margin (public services)

* model 1: baseline
eststo: xtreg log_total_g log_PPP_pubbdg log_price log_pinc_all i.year ///
	if year >= 2012, fe vce(cluster pid)

* model 2: + age
eststo: xtreg log_total_g log_PPP_pubbdg log_price log_pinc_all age i.year ///
	if year >= 2012, fe vce(cluster pid)

* model 3: + education-time dummies
eststo: xtreg log_total_g log_PPP_pubbdg log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

* model 4: + gender-time dummies
eststo: xtreg log_total_g log_PPP_pubbdg log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

* model 5: + living_area
eststo: xtreg log_total_g log_PPP_pubbdg log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

* model 6: + squared logged government budge
eststo: xtreg log_total_g log_PPP_pubbdg sqlog_PPP_pubpdg log_price log_pinc_all ///
	age i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

gen pred_e_pub = _b[log_PPP_pubbdg] + 2 * _b[sqlog_PPP_pubpdg] * log_PPP_pubbdg

esttab using "_assets\baseline1.html", ///
	replace se keep(log_PPP_pubbdg sqlog_PPP_pubpdg log_price log_pinc_all)  ///
	order(log_PPP_pubbdg sqlog_PPP_pubpdg log_price log_pinc_all) ///
	title("Social Welfare Expenditure and Monetary Donations") 
eststo clear

/*******************************************************************************

Government Trust and Crowd-in
Since trust for politicians can be observed if year >= 2015
We omit price of charitable giving

*******************************************************************************/

* intensive + extensive margin (public services)

eststo: xtreg log_total_g c.log_PPP_pubbdg##i.trust_politician c.sqlog_PPP_pubpdg##i.trust_politician ///
	log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2015, fe vce(cluster pid)

gen pred_e_pub_sep = .
replace pred_e_pub_sep = _b[log_PPP_pubbdg] + 2 * _b[sqlog_PPP_pubpdg] * log_PPP_pubbdg if trust_politician == 1
replace pred_e_pub_sep = (_b[log_PPP_pubbdg] + _b[2.trust_politician#c.log_PPP_pubbdg]) + ///
	2 * (_b[sqlog_PPP_pubpdg] + _b[2.trust_politician#c.sqlog_PPP_pubpdg]) * log_PPP_pubbdg if trust_politician == 2
replace pred_e_pub_sep = (_b[log_PPP_pubbdg] + _b[3.trust_politician#c.log_PPP_pubbdg]) + ///
	2 * (_b[sqlog_PPP_pubpdg] + _b[3.trust_politician#c.sqlog_PPP_pubpdg]) * log_PPP_pubbdg if trust_politician == 3


esttab using "_assets\trust_elast.html", ///
	replace se ///
	keep(log_PPP_pubbdg sqlog_PPP_pubpdg ///
	2.trust_politician#c.log_PPP_pubbdg ///
	3.trust_politician#c.log_PPP_pubbdg ///
	4.trust_politician#c.log_PPP_pubbdg ///
	5.trust_politician#c.log_PPP_pubbdg ///
	2.trust_politician#c.sqlog_PPP_pubpdg ///
	3.trust_politician#c.sqlog_PPP_pubpdg ///
	4.trust_politician#c.sqlog_PPP_pubpdg ///
	5.trust_politician#c.sqlog_PPP_pubpdg)  ///
	title("Political Trust and Crowd-in") ///
	label star(* 0.1 ** 0.05 *** 0.01)
eststo clear

/*******************************************************************************

Interactions b/w giving prices and government expenditure

*******************************************************************************/

* intensive + extensive margin (public services)

* model 1: baseline
eststo: xtreg log_total_g c.log_PPP_pubbdg##c.log_price log_pinc_all i.year ///
	if year >= 2012, fe vce(cluster pid)

* model 2: + age
eststo: xtreg log_total_g c.log_PPP_pubbdg##c.log_price log_pinc_all age i.year ///
	if year >= 2012, fe vce(cluster pid)

* model 3: + education-time dummies
eststo: xtreg log_total_g c.log_PPP_pubbdg##c.log_price log_pinc_all age i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

* model 4: + gender-time dummies
eststo: xtreg log_total_g c.log_PPP_pubbdg##c.log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

* model 5: + living_area
eststo: xtreg log_total_g c.log_PPP_pubbdg##c.log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

* model 6: + squared logged government budge
eststo: xtreg log_total_g c.log_PPP_pubbdg##c.log_price sqlog_PPP_pubpdg c.sqlog_PPP_pubpdg#c.log_price log_pinc_all ///
	age i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

esttab using "_assets\price_expenditure.html", ///
	replace se ///
	keep(log_PPP_pubbdg sqlog_PPP_pubpdg log_price ///
	c.log_PPP_pubbdg#c.log_price c.sqlog_PPP_pubpdg#c.log_price log_pinc_all)  ///
	order(log_PPP_pubbdg sqlog_PPP_pubpdg log_price ///
	c.log_PPP_pubbdg#c.log_price c.sqlog_PPP_pubpdg#c.log_price log_pinc_all) ///
	title("Interactions between Giving Prices and Government Expenditure") 

/*******************************************************************************

Set of Plots

*******************************************************************************/

keep pred_e_pub pred_e_pub_sep trust_politician PPP_pubbdg year
keep if year >= 2012
duplicates drop

* Predicted elasiticity of donations
twoway	(scatter pred_e_pub PPP_pubbdg, mcolor(%30)), ///
        yline(0, lpattern(dash) lcolor(red)) ///
        xtitle("Social Welfare Budget per capita")  ///
        ytitle("Predicted Elasticity")  ///
        title("Predicted Budget-Elasticity of Donations")  
graph export "_assets\baseline1.png", width(700) replace


* Predicted elasiticity of donations grouped by political trust
twoway (scatter pred_e_pub_sep PPP_pubbdg if year >= 2015 & trust_politician == 1, mcolor(%30))   ///
	   (scatter pred_e_pub_sep PPP_pubbdg if year >= 2015 & trust_politician == 2, mcolor(%30))  ///
	   (scatter pred_e_pub_sep PPP_pubbdg if year >= 2015 & trust_politician == 3, mcolor(%30)),  ///
	   yline(0, lpattern(dash) lcolor(red))  ///
	   xtitle("Social Welfare Budge per capita") ///
	   ytitle("Predicted Elasticity")  ///
	   legend(lab(1 "Lowest trust") lab(2 "Lower trust") lab(3 "Neutral") ///
	   title("Trust for Politicians", size(medium)))  ///
	   title("Predicted Budget-Elasticity of Donations Grouped by Trust")  
graph export "_assets\trust_elast.png", width(700) replace











