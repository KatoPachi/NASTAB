
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

* president variable 
gen president = ""
replace president = "Roh Moo-hyun" if year == 2007
replace president = "Lee Myung-bak" if 2008 <= year & year <= 2012
replace president = "Park Geun-hye" if 2013 <= year & year <= 2016
replace president = "Moon Jae-in" if 2017 <= year

gen wing_president = ""
replace wing_president = "right" if president == "Lee Myung-bak" | president == "Park Geun-hye"
replace wing_president = "left" if president == "Roh Moo-hyun" | president == "Moon Jae-in"

gen wing_politics = ""
replace wing_politics = "right" if political_pref > 3
replace wing_politics = "neutral" if political_pref == 3
replace wing_politics = "left" if political_pref < 3

gen wing_match = .
replace wing_match = 1 if wing_politics == wing_president
replace wing_match = 0 if wing_politics != wing_president

gen wing_right = .
replace wing_right = 1 if wing_politics == "right"
replace wing_right = 0 if wing_politics != "right"

gen wing_left = .
replace wing_left = 1 if wing_politics == "left"
replace wing_left = 0 if wing_politics != "left"

* regiem change
* regiem1: from Roh Mom-hyun to Lee Myung-bak
* regiem2: from Lee Myung-bak to Park Geun-hye
* regiem3: from Park Geun-hye to Moon Jae-in

gen regiem1 = .
replace regiem1 = 0 if year == 2008
replace regiem1 = year - 2008 if year != 2008

gen regiem2 = .
replace regiem2 = 0 if year == 2013
replace regiem2 = year - 2013 if year != 2013

gen regiem3 = .
replace regiem3 = 0 if year == 2017
replace regiem3 = year - 2017 if year != 2017

* panel data
xtset pid

* summary data
table year wing_match wing_politics

* fixed effect model 
xtreg log_total_g wing_match log_price log_pinc_all i.year, fe vce(cluster pid)

xtreg log_total_g wing_match log_price log_pinc_all i.year if i_total_giving > 0, fe vce(cluster pid)

xtreg i_ext_giving wing_match log_price log_pinc_all i.year, fe vce(cluster pid)


* Focus on regiem change from right to right (regiem 2)
*right wing
xtreg log_total_g wing_right##ib2013.year log_price log_pinc_all ///
	if 2008 <= year & year <= 2016, fe vce(cluster pid)

xtreg log_total_g wing_right##ib2013.year log_price log_pinc_all ///
	if 2008 <= year & year <= 2016 & i_total_giving > 0, fe vce(cluster pid)

xtreg i_ext_giving wing_right##ib2013.year log_price log_pinc_all ///
	if 2008 <= year & year <= 2016, fe vce(cluster pid)

*left wing
xtreg log_total_g wing_left##ib2013.year log_price log_pinc_all ///
	if 2008 <= year & year <= 2016, fe vce(cluster pid)

xtreg log_total_g wing_left##ib2013.year log_price log_pinc_all ///
	if 2008 <= year & year <= 2016 & i_total_giving > 0, fe vce(cluster pid)

xtreg i_ext_giving wing_left##ib2013.year log_price log_pinc_all ///
	if 2008 <= year & year <= 2016, fe vce(cluster pid)


* Focus on regiem change from right to left (regiem 3)
*right wing
xtreg log_total_g wing_right##ib2017.year log_price log_pinc_all ///
	if 2013 <= year, fe vce(cluster pid)

xtreg log_total_g wing_right##ib2017.year log_price log_pinc_all ///
	if 2013 <= year & i_total_giving > 0, fe vce(cluster pid)

xtreg i_ext_giving wing_right##ib2017.year log_price log_pinc_all ///
	if 2013 <= year, fe vce(cluster pid)

*left wing
xtreg log_total_g wing_left##ib2017.year log_price log_pinc_all ///
	if 2013 <= year, fe vce(cluster pid)

xtreg log_total_g wing_left##ib2017.year log_price log_pinc_all ///
	if 2013 <= year & i_total_giving > 0, fe vce(cluster pid)

xtreg i_ext_giving wing_left##ib2017.year log_price log_pinc_all ///
	if 2013 <= year, fe vce(cluster pid)















