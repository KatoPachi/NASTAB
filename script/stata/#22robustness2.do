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

egen group_id = group(living_area)

gen PPP_pub_j = .
forvalues i = 2007(1)2018 {
	forvalues j = 1(1)17{
		qui sum PPP_pubbdg if year == `i' & group_id != `j'
		replace PPP_pub_j = r(mean) if year == `i' & group_id == `j'
	}
}

gen PPP_health_j = .
forvalues i = 2007(1)2018 {
	forvalues j = 1(1)17{
		qui sum PPP_healthbdg if year == `i' & group_id != `j'
		replace PPP_health_j = r(mean) if year == `i' & group_id == `j'
	}
}

gen log_PPP_pub_j = log(PPP_pub_j + 1)
gen log_PPP_health_j = log(PPP_health_j + 1)
gen sqlog_PPP_pub_j = log_PPP_pub_j^2
gen sqlog_PPP_health_j = log_PPP_health_j^2

xtset pid

/*******************************************************************************

Confounder check: 
Do responders donate charities which is located outside living area?

To show it, we regress logged value of outside local government on donations

*******************************************************************************/

* model 1: log_total_g ~ log_PPP_pub_j + cov
eststo: xtreg log_total_g log_PPP_pub_j log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

* model 2: + sq_log_PPP_pub_j + cov
eststo: xtreg log_total_g log_PPP_pub_j sqlog_PPP_pub_j sqlog_PPP_healthpdg log_price log_pinc_all ///
	age i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

* model 3: log_total_g ~ log_PPP_health_j + cov
eststo: xtreg log_total_g log_PPP_health_j log_price log_pinc_all age i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)

* model 4: + sq_log_PPP_health_j + cov
eststo: xtreg log_total_g log_PPP_health_j sqlog_PPP_health_j sqlog_PPP_healthpdg log_price log_pinc_all ///
	age i.year##i.gender i.year##i.educ ///
	if year >= 2012, fe vce(cluster pid)	

esttab	using "_assets/robustreg2.html", ///
		se replace ///
		keep(log_PPP_pub_j sqlog_PPP_pub_j log_PPP_health_j sqlog_PPP_health_j log_price log_pinc_all) ///
		order(log_PPP_pub_j sqlog_PPP_pub_j log_PPP_health_j sqlog_PPP_health_j log_price log_pinc_all)
	