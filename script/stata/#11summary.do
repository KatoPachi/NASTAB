cd "C:\Users\vge00\Desktop\nastab"  //root path

** ---- ReadData
use "data\shaped.dta", clear

gen price = .
replace price = 1 - mtr if year < 2014
replace price = 1 - 0.15 if year >= 2014
gen log_price = ln(price + 1)
gen log_total_g = ln(i_total_giving + 1)
gen log_pinc_all = ln(lincome + 100000)
replace gender = gender - 1
gen univ = (educ == 3) if !missing(educ)
gen highschool = (educ == 2) if !missing(educ)
gen juniorhigh = (educ == 1) if !missing(educ)

gen now_balance = 0
replace now_balance = 2 if avg_welfare_tax == 1
replace now_balance = 1 if avg_welfare_tax == 2 | avg_welfare_tax == 4
replace now_balance = -1 if avg_welfare_tax == 6 | avg_welfare_tax == 8
replace now_balance = -2 if avg_welfare_tax == 9
replace now_balance = . if missing(avg_welfare_tax)

gen ideal_balance = 0 
replace ideal_balance = 2 if opt_welfare_tax == 1
replace ideal_balance = 1 if opt_welfare_tax == 2 | opt_welfare_tax == 4
replace ideal_balance = -1 if opt_welfare_tax == 6 | opt_welfare_tax == 8
replace ideal_balance = -2 if opt_welfare_tax == 9
replace ideal_balance = . if missing(opt_welfare_tax) 

keep if year >= 2012 & age >= 24
tsset pid year

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
	ytitle("Average donations among donors (10,000KRW)", axis(2)) ///
	xlabel(2012(1)2018) xtitle("Year")  ///
	legend(label(1 "Extensive margin") label(2 "Intensive margin")) ///
	graphregion(fcolor(white))
}

frame drop avgdt

** ---- SummaryCovariate
local k = 0
foreach v in lincome price i_total_giving i_ext_giving now_balance ideal_balance age gender univ highschool juniorhigh {
	local k = `++k'
	sum `v', detail
	mat `v' = r(N) \ r(mean) \ r(sd) \ r(min) \ r(p25) \ r(p50) \ r(p75) \ r(max)
	mat colnames `v' = `v'
	mat rownames `v' = N mean sd min p25 median p75 max
	
	if `k' == 1 {
	    mat tabular = `v'
	}
	else {
	    mat_capp tabular : tabular `v'
	}
}

mat tabular = tabular'
mat list tabular



** ---- SummaryPriceChange
frame copy default sump
frame sump: keep if year == 2013
frame sump: keep lincome price

frame sump: {
    twoway ///
	(line price lincome, yaxis(1) connect(stairstep) sort(lincome) lcolor(blue)) ///
	(histogram lincome, freq yaxis(2) color(gs10%50) lcolor(black)), ///
	yline(0.85, lcolor(red) lpattern(-)) ///
	xline(1200, lcolor(black) lpattern(-))  ///
	xline(4600, lcolor(black) lpattern(-))  ///
	ytitle("Giving Price", axis(1)) ///
	ytitle("Count", axis(2)) ///
	xtitle("Annual taxable income (10,000KRW)")  ///
	xlabel(1200 4600 8800 30000) ///
	legend(label(1 "Giving Price in 2013")) ///
	graphregion(fcolor(white))
}

frame drop sump