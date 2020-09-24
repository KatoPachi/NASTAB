
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
replace high_welfare_level = 1 if welfare_level > 3
replace high_welfare_level = 0 if welfare_level <= 3
replace high_welfare_level = . if welfare_level == .

gen bin_trust = .
replace bin_trust = 1 if trust_politician >= 3
replace bin_trust = 0 if trust_politician < 3
replace bin_trust = . if trust_politician == .

* panel data
xtset pid

/*
Welfare Level
*/

* baseline
xtreg log_total_g high_welfare_level log_price log_pinc_all i.year, fe vce(cluster pid)

xtreg log_total_g high_welfare_level##c.log_price log_pinc_all i.year, fe vce(cluster pid)

* decompose: Extensive margin
xtreg i_ext_giving high_welfare_level log_price log_pinc_all i.year, fe vce(cluster pid)

xtreg i_ext_giving high_welfare_level##c.log_price log_pinc_all i.year, fe vce(cluster pid)


* decompose: Intensive margin
xtreg log_total_g high_welfare_level log_price log_pinc_all i.year if i_ext_giving == 1, ///
	fe vce(cluster pid)

xtreg log_total_g high_welfare_level##c.log_price log_pinc_all i.year if i_ext_giving == 1, ///
	fe vce(cluster pid)

/*
Political Trust
*/

* baseline
xtreg log_total_g bin_trust log_pinc_all i.year, fe vce(cluster hhid)

* decompose: Extensive margin
xtreg i_ext_giving bin_trust log_pinc_all i.year, fe vce(cluster hhid)

* decompose: Intensive margin
xtreg log_total_g bin_trust log_pinc_all i.year if i_ext_giving == 1, ///
	fe vce(cluster hhid)
	


