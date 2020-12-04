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

Confounder check: 
Does budget per capita truely represents the size of public goods?

To show it, we make time-series plot of local government budge by 
subjective welfare size

*******************************************************************************/

gen welfare_level_3scale = .
replace welfare_level_3scale = 1 if welfare_level < 3
replace welfare_level_3scale = 2 if welfare_level == 3
replace welfare_level_3scale = 3 if welfare_level > 3
replace welfare_level_3scale = . if welfare_level == .

bysort year welfare_level_3scale: egen meanpub = mean(PPP_pubbdg)
bysort year welfare_level_3scale: egen meanhealth = mean(PPP_healthbdg)
keep year welfare_level_3scale meanpub meanhealth
keep if welfare_level_3scale != .
duplicates drop

* Social welfare budget
twoway 	(line meanpub year if welfare_level == 1)  ///
		(line meanpub year if welfare_level == 2)  ///
		(line meanpub year if welfare_level == 3),  ///
		legend(label(1 "Small") label(2 "Neutral") label(3 "Large") ///
		subtitle("Perceived Welfare Size") rows(1))  ///
		xtitle("Year") ytitle("Social Welfare Budget per capita") ///
		title("Time-series of Local Governement Budget")

graph export "_assets/robust1.png", width(700) replace

* Healthcare budget
twoway 	(line meanhealth year if welfare_level == 1)  ///
		(line meanhealth year if welfare_level == 2)  ///
		(line meanhealth year if welfare_level == 3),  ///
		legend(label(1 "Small") label(2 "Neutral") label(3 "Large") ///
		subtitle("Perceived Welfare Size") rows(1))  ///
		xtitle("Year") ytitle("Healthcare Budget per capita") ///
		title("Time-series of Local Governement Budget")
	
graph export "_assets/robust2.png", width(700) replace