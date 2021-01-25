cd "C:\Users\vge00\Desktop\nastab"  //root path

** ---- ReadData
use "data\shaped.dta", clear

gen price = .
replace price = 1 - mtr if year < 2014
replace price = 1 - 0.15 if year >= 2014
gen log_price = ln(price)
gen log_total_g = ln(i_total_giving + 1)
gen log_pinc_all = ln(lincome + 100000)
replace gender = gender - 1
gen univ = (educ == 3) if !missing(educ)
gen highschool = (educ == 2) if !missing(educ)
gen juniorhigh = (educ == 1) if !missing(educ)
gen sqage = age^2/100

** ---- LagOperation
tsset pid year

gen lag1_price = l.price
gen lag2_price = l2.price
gen lag3_price = l3.price
gen lag4_price = l4.price

gen lag1iv = ln(price/lag1_price)
gen lag2iv = ln(price/lag2_price)
gen lag3iv = ln(price/lag3_price)
gen lag4iv = ln(price/lag4_price)

keep if year >= 2012

** ---- EstimateTrustIndex
xtreg trust_politician i.year##i.living_area if year >= 2015, fe
predict orgtrustid, u

xtreg trust_politician i.year##i.living_area if year == 2015 | year == 2016, fe
predict orgparktrustid, u

xtreg trust_politician i.year##i.living_area if year == 2017 | year == 2018, fe
predict orgmoontrustid, u

bysort pid: egen trustid = mean(orgtrustid)
bysort pid: egen parktrustid = mean(orgparktrustid) 
bysort pid: egen moontrustid = mean(orgmoontrustid)
gen diff = moontrustid - parktrustid

** ---- HistogramTrustid
frame copy default plotdt
frame plotdt: keep pid trustid
frame plotdt: duplicates drop

frame plotdt: {
	twoway ///
	(histogram trustid, freq yaxis(2) color(gs10%50) lcolor(black)), ///
	xtitle("Trust index") ///
	graphregion(fcolor(white))
}

** ---- RegTrustidOnCovariate
reg trustid gender log_pinc_all age sqage i.educ ib3.political_pref if year == 2018

** ---- ScatterTrusidDonations
bysort pid: egen meandonate = mean(i_total_giving)
frame copy default scatter3dt
frame scatter3dt: keep pid trustid meandonate
frame scatter3dt: duplicates drop

frame scatter3dt: {
	twoway  ///
	(scatter meandonate trustid, color(gs10%50)),  ///
	xtitle("Trust index") ytitle("Individual average donations across time")  ///
	graphregion(fcolor(white))
}

** ---- PlotDiffDonationsbwTrustGroup
frame create coefplotdt
frame coefplotdt: {
	set obs 21
	gen effect = .
	gen se_effect = .
	gen cutoff = .
}

frame scatter3dt: gen high = .
local k = 1
forvalues i = 0(.1)2.1 {
	di "k = `k'"
	frame scatter3dt: replace high = 0 if trustid <= `i'
	frame scatter3dt: replace high = 1 if trustid > `i'
	frame scatter3dt: replace high = . if missing(trustid)
	frame scatter3dt: reg meandonate high 
	frame coefplotdt: replace effect = _b[high] if _n == `k'
	frame coefplotdt: replace se_effect = _se[high] if _n == `k'
	frame coefplotdt: replace cutoff = `i' if _n == `k'
	local k = `k' + 1
}

frame coefplotdt: gen lcoef = effect - 1.96*se_effect
frame coefplotdt: gen hcoef = effect + 1.96*se_effect

frame coefplotdt: {
	twoway ///
	(scatter effect cutoff, color(blue)) ///
	(line effect cutoff, color(blue))  ///
	(rcap hcoef lcoef cutoff, color(black)), ///
	yline(0, lcolor(red) lpattern(-))  ///
	xtitle("Threshold of trust index") ///
	ytitle("Difference in mean (+/- 1.96*se)") ///
	legend(off) ///
	graphregion(fcolor(white))
}

** ---- Scatter1Trustid
frame copy default scatter1dt
frame scatter1dt: keep pid parktrustid moontrustid
frame scatter1dt: duplicates drop

frame scatter1dt: {
	twoway ///
	(scatter moontrustid parktrustid, color(gs10%50)) ///
	(fpfit moontrustid parktrustid, color(red)), ///
	xtitle("Park's trust index") ytitle("Moon's trust index") ///
	legend(off) ///
	graphregion(fcolor(white))
}

** ---- TtestPresidentTrustid
frame scatter1dt: ttest moontrustid == parktrustid

** ---- Scatter2Trusid
frame copy default scatter2dt
frame scatter2dt: keep pid diff trustid
frame scatter2dt: duplicates drop

frame scatter2dt: {
	twoway ///
	(scatter trustid diff, color(gs10%50))  ///
	(fpfit trustid diff, color(red)), ///
	xtitle("Difference b/w president-specific trust index") ///
	ytitle("Trust index") ///
	legend(off)  ///
	graphregion(fcolor(white))
}

** ---- RegTrustidOnDiff2Trustid
frame scatter2dt: reg trustid diff
frame scatter2dt: reg trustid diff if abs(diff) < 2
frame scatter2dt: reg trustid diff if abs(diff) < 1
frame scatter2dt: reg trustid diff if abs(diff) < 0.5



** ---- ClearEnv
frame change default
frame drop plotdt
frame drop scatter1dt
frame drop scatter2dt
frame drop scatter3dt
frame drop coefplotdt















