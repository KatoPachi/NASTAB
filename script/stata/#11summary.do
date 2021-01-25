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

keep if year >= 2012

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

** ---- SummaryCovariate
matrix sumcov = J(7, 7, .)

label variable gender "Female"
label variable age "Age"
label variable lincome "Annual taxable income"
label variable univ "University graduate"
label variable highschool "High School Graduate"
label variable pid "#.Respondents"
label variable hhid "#.Households"

local j = 0
foreach v in gender age lincome univ highschool {
    local k = 1
	local j = `++j'
	di "j = `j'"
	forvalues y = 2012(1)2018 {
	    di "k = `k'"
		summarize `v' if year == `y'
		matrix sumcov[`j',`k'] = r(mean)
		local k = `++k'
	}
}

local k = 1
forvalues y = 2012(1)2018 {
    di = "k = `k'"
	summarize pid if year == `y'
	matrix sumcov[6, `k'] = r(N)
	local k = `++k'
}

frame copy default temp
frame temp: keep year hhid
frame temp: duplicates drop
local k = 1
forvalues y = 2012(1)2018 {
    di = "k = `k'"
	frame temp: summarize hhid if year == `y'
	matrix sumcov[7, `k'] = r(N)
	local k = `++k'
}

matrix rownames sumcov = gender age lincome univ highschool pid hhid 
xsvmat sumcov, saving(test.dta) rownames(xvar)

frmttable using test.csv, statmat(sumcov) varlabels  ///
	sdec(2\2\2\2\2\0\0) ///
	ctitles("", "2012", "2013", "2014", "2015", "2016", "2017", "2018")

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
	xlabel(1200 4600 8800 30000) ///
	legend(label(1 "Giving Price in 2013")) ///
	graphregion(fcolor(white))
}

** ---- ClearEnv
frame change default
frame drop avgdt
frame drop temp
frame drop sump