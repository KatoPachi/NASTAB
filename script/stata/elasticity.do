
* directory
cd "C:\Users\vge00\Desktop\nastab"

* call shaped data.
* This data is made by R. I will replace latter.
use "data\shapedt.dta", clear

* variables
gen price = .
replace price = 1 - mtr if year < 2014
replace price = 1 - 0.15 if year >= 2014

gen log_price = ln(price)
gen log_total_g = ln(total_g + 1)
gen log_pinc_all = ln(pinc_all + 100000)

gen extensive = .
replace extensive = 1 if total_g > 0
replace extensive = 0 if total_g == 0

* panel data
xtset pid

/*
Full model
*/

* Intensive margin
xtreg log_total_g log_price log_pinc_all i.year if total_g > 0, ///
	fe vce(cluster pid)

* Extensive margin
xtreg extensive log_price log_pinc_all i.year, ///
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
	if 2015 > year & year > 2013, fe vce(cluster pid)


