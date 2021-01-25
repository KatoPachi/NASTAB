cd "C:\Users\vge00\Desktop\nastab"  //root path

** ---- ReadData
use "data\shaped.dta", clear

gen price = .
replace price = 1 - mtr if year < 2014
replace price = 1 - 0.15 if year >= 2014

gen log_price = ln(price + 1)
gen log_total_g = ln(i_total_giving + 1)
gen log_pinc_all = ln(lincome + 100000)

keep if year >= 2012

xtset pid

** ---- SummaryOutcome
frame copy default avgdt
frame avgdt: by year, sort: egen meanext = mean(i_ext_giving)
frame avgdt: by year, sort: egen meanint = mean(i_total_giving) if i_ext_giving == 1
frame avgdt: keep year meanext meanint
frame avgdt: duplicates drop
frame avgdt: keep if meanint != .

frame avgdt: {
    twoway ///
	(bar meanext year, color(gs10) lcolor(black) barwidth(0.9) yaxis(1)) ///
	(connected meanint year, color(blue) yaxis(2)), ///
	xline(2013.5, lcolor(red) lpattern(-)) ///
	yscale(range(0 .) axis(1)) ///
	yscale(range(0 .) axis(2)) ///
	ylabel(0(.05).2, axis(1)) ///
	ylabel(0(50)200, axis(2)) ///
	ytitle("Proportion of donors", axis(1)) ///
	ytitle("Average donations among donors", axis(2)) ///
	xlabel(2012(1)2018) xtitle("Year")  ///
	legend(label(1 "Extensive margin") label(2 "Intensive margin")) ///
	graphregion(fcolor(white))
}

** ---- ClearEnv
frame change default
frame drop avgdt