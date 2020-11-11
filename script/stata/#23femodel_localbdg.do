
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
gen log_PPP_healthbdg = ln(healthbdg + 1)
gen welfarebdg = pubbdg + healthbdg
gen PPP_welfarebdg = welfarebdg/pop
gen log_PPP_welfarebdg = ln(PPP_welfarebdg + 1)

* panel data
xtset pid

/*******************************************************************************

	Use local government's budget for welfare
	Geographical variation within year

*******************************************************************************/

/*
Baseline 1: for public services (2FE)
*/

xtreg log_total_g log_PPP_pubbdg  log_price log_pinc_all i.year, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/main_pubbdg.txt", replace ///
	keep(log_PPP_pubbdg log_price log_pinc_all) ///
	ctitle(Overall) ///
	addtext(Individual FE, Yes, Year FE, Yes)

xtreg i_ext_giving log_PPP_pubbdg log_price log_pinc_all i.year, ///
	fe vce(cluster pid)	

outreg2 using "_assets/stata/main_pubbdg.txt", append ///
	keep(log_PPP_pubbdg log_price log_pinc_all) ///
	ctitle(Extensive Margin) ///
	addtext(Individual FE, Yes, Year FE, Yes)
		
xtreg log_total_g log_PPP_pubbdg log_price log_pinc_all i.year if i_ext_giving == 1, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/main_pubbdg.txt", append ///
	keep(log_PPP_pubbdg log_price log_pinc_all) ///
	ctitle(Intensive Margin) ///
	addtext(Individual FE, Yes, Year FE, Yes)

/*
Baseline 2: for health services (2FE)
*/

xtreg log_total_g log_PPP_healthbdg  log_price log_pinc_all i.year, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/main_healthbdg.txt", replace ///
	keep(log_PPP_healthbdg log_price log_pinc_all) ///
	ctitle(Overall) ///
	addtext(Individual FE, Yes, Year FE, Yes)

xtreg i_ext_giving log_PPP_healthbdg log_price log_pinc_all i.year, ///
	fe vce(cluster pid)	

outreg2 using "_assets/stata/main_healthbdg.txt", append ///
	keep(log_PPP_healthbdg log_price log_pinc_all) ///
	ctitle(Extensive Margin) ///
	addtext(Individual FE, Yes, Year FE, Yes)
		
xtreg log_total_g log_PPP_healthbdg log_price log_pinc_all i.year if i_ext_giving == 1, ///
	fe vce(cluster pid)

outreg2 using "_assets/stata/main_healthbdg.txt", append ///
	keep(log_PPP_healthbdg log_price log_pinc_all) ///
	ctitle(Intensive Margin) ///
	addtext(Individual FE, Yes, Year FE, Yes)

















