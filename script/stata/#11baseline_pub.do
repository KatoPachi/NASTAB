cd "C:\Users\vge00\Desktop\nastab"  //root path
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

esttab using "_assets\baseline1.html", ///
	replace se keep(log_PPP_pubbdg sqlog_PPP_pubpdg log_price log_pinc_all)  ///
  order(log_PPP_pubbdg sqlog_PPP_pubpdg log_price log_pinc_all) ///
	title("Social Welfare Expenditure and Monetary Donations") 

gen pred_e_pub = _b[log_PPP_pubbdg] + 2 * _b[sqlog_PPP_pubpdg] * log_PPP_pubbdg
keep pred_e_pub PPP_pubbdg year
keep if year >= 2012
duplicates drop

twoway	(scatter pred_e_pub PPP_pubbdg, mcolor(%30)), ///
        yline(0, lpattern(dash) lcolor(red)) ///
        xtitle("Social Welfare Budget per capita")  ///
        ytitle("Predicted Elasticity")  ///
        title("Predicted Budget-Elasticity of Donations")  
graph export "_assets\baseline1.png", width(700) replace