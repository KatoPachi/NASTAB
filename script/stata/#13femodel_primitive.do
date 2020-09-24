
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

gen bin_welfare_level = .
replace bin_welfare_level = 1 if welfare_level > 3
replace bin_welfare_level = 0 if welfare_level <= 3

gen int_welfare_price = bin_welfare_level * log_price

* panel data
xtset pid

/*
Full time length
*/

* baseline
xtreg log_total_g bin_welfare_level log_price log_pinc_all i.year, fe vce(cluster pid)

xtreg log_total_g bin_welfare_level log_price int_welfare_price log_pinc_all i.year, fe vce(cluster pid)

* decompose: Extensive margin
xtreg i_ext_giving bin_welfare_level log_price log_pinc_all i.year, fe vce(cluster pid)

xtreg i_ext_giving bin_welfare_level log_price int_welfare_price log_pinc_all i.year, fe vce(cluster pid)


* decompose: Intensive margin
xtreg log_total_g bin_welfare_level log_price log_pinc_all i.year if i_ext_giving == 1, ///
	fe vce(cluster pid)

xtreg log_total_g bin_welfare_level log_price int_welfare_price log_pinc_all i.year if i_ext_giving == 1, ///
	fe vce(cluster pid)

/*
use data year > 2011 
because marginal tax rate in 2012 is same as in 2013
*/

* Intensive margin
xtreg log_total_g log_price log_pinc_all i.year if total_g > 0 & year > 2011, ///
	fe vce(cluster pid)

* Extensive margin 
xtreg extensive log_price log_pinc_all i.year if year > 2011, ///
	fe vce(cluster pid)
	

/*
use data year == 2013 and 2014
Pure DID?
*/

* Intensive margin
xtreg log_total_g log_price log_pinc_all i.year ///
	if total_g > 0 & 2015 > year & year > 2012, fe vce(cluster pid)

* Extensive margin 
xtreg extensive log_price log_pinc_all i.year ///
	if 2015 > year & year > 2012, fe vce(cluster pid)


