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

** ---- EstimatePoliticalViewsIndex
xtreg political_pref i.year##i.living_area if year >= 2015, fe
predict orgpoliticid, u

xtreg political_pref i.year##i.living_area if year == 2015 | year == 2016, fe
predict orgparkpoliticid, u

xtreg political_pref i.year##i.living_area if year == 2017 | year == 2018, fe
predict orgmoonpoliticid, u



* make trustid dataset
frame copy default politicdt
frame politicdt: {
	bysort pid: egen politicid = mean(orgpoliticid)
	bysort pid: egen park_politicid = mean(orgparkpoliticid) 
	bysort pid: egen moon_politicid = mean(orgmoonpoliticid)
}
frame politicdt: keep pid politicid park_politicid moon_politicid
frame politicdt: duplicates drop
frame politicdt: gen diff_politicid = moon_politicid - park_politicid
frame politicdt: xtile politic5 = politicid, nq(5) 
frame politicdt: xtile park_politic5 = park_politicid, nq(5)
frame politicdt: {
	gen lessdiff1_politic = 0
	replace lessdiff1_politic = 1 if abs(diff_politicid) < 1
	replace lessdiff1_politic = . if missing(diff_politicid)
}
frame politicdt: {
	gen lessdiffhalf_politic = 0
	replace lessdiffhalf_politic = 1 if abs(diff_politicid) < 0.5
	replace lessdiffhalf_politic = . if missing(diff_politicid)
}
frame politicdt: save "data\shape\politicid.dta", replace

merge m:1 pid using "data\shape\politicid.dta"
drop _merge


** ---- HistogramTrustid
frame politicdt: {
	twoway ///
	(histogram politicid, freq yaxis(2) color(gs10%50) lcolor(black)), ///
	xtitle("Political view index") ///
	graphregion(fcolor(white))
}


** ---- Scatter1Trustid
frame politicdt: {
	twoway ///
	(scatter moon_politicid park_politicid, color(gs10%50)) ///
	(fpfit moon_politicid park_politicid, color(red)), ///
	xtitle("Park's political view index") ytitle("Moon's political view index") ///
	legend(off) ///
	graphregion(fcolor(white))
}

** ---- TtestPresidentTrustid
frame politicdt: ttest moon_politicid == park_politicid

** ---- Scatter2Trusid
frame politicdt: {
	twoway ///
	(scatter politicid diff_politicid, color(gs10%50))  ///
	(fpfit politicid diff, color(red)), ///
	xtitle("Difference b/w president-specific political view index") ///
	ytitle("Trust index") ///
	legend(off)  ///
	graphregion(fcolor(white))
}

** ---- RegTrustidOnDiff2Trustid
frame politicdt: reg politicid diff
frame politicdt: reg politicid diff if abs(diff) < 2
frame politicdt: reg politicid diff if abs(diff) < 1
frame politicdt: reg politicid diff if abs(diff) < 0.5


** ---- ScatterTrusidDonations
frame copy default scatdt
frame scatdt: bysort pid: egen avgdonate = mean(i_total_giving)
frame scatdt: keep pid politicid avgdonate
frame scatdt: duplicates drop

frame scatdt: {
	twoway  ///
	(scatter avgdonate politicid, color(gs10%50)),  ///
	xtitle("Political view index") ytitle("Individual average donations across time")  ///
	graphregion(fcolor(white))
}

** ---- RegTrustidOnCovariate
reg politicid gender log_pinc_all age sqage i.educ if year == 2018

** ---- EstimateElasticityByTrustGroup
forvalues i = 1(1)6 {
	xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
		if politic5 == `i', fe vce(cluster pid)
}

** ---- EstimateInteractionByTrustGroup
xtreg log_total_g c.log_price##ib3.politic5 log_pinc_all age i.living_area i.year##i.gender i.year##i.educ, ///
	fe vce(cluster pid)
	
** ---- Robust1EstimateInteractionByTrustGroup
xtreg log_total_g c.log_price##ib3.politic5 log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	if year == 2013|year == 2014, fe vce(cluster pid)
xtreg log_total_g c.log_price##ib3.park_politic5 log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	if year == 2013|year == 2014, fe vce(cluster pid)	


** ---- ClearEnv
frame change default
frame drop scatdt
frame drop politicdt