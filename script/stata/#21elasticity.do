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

** ---- EstimateElasticity
label variable log_price "ln(giving price)"

xtreg log_total_g log_price log_pinc_all i.year, fe vce(cluster pid)
xtreg log_total_g log_price log_pinc_all age i.year, fe vce(cluster pid)
xtreg log_total_g log_price log_pinc_all age i.year##i.educ, fe vce(cluster pid)
xtreg log_total_g log_price log_pinc_all age i.year##i.gender i.year##i.educ, fe vce(cluster pid)
xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ, fe vce(cluster pid)

** ---- Robust1EstimateElasticity
xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	(log_price = lag1iv), fe first vce(cluster pid)
xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	(log_price = lag2iv), fe first vce(cluster pid)
xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	(log_price = lag3iv), fe first vce(cluster pid)
xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	(log_price = lag4iv), fe first vce(cluster pid)

** ---- Robust2EstimateElasticity
xtreg log_total_g log_price log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	if year == 2013|year == 2014, fe vce(cluster pid)
xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	(log_price = lag1iv) if year == 2013|year == 2014, fe first vce(cluster pid)
xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	(log_price = lag2iv) if year == 2013|year == 2014, fe first vce(cluster pid)
xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	(log_price = lag3iv) if year == 2013|year == 2014, fe first vce(cluster pid)
xtivreg log_total_g log_pinc_all age i.living_area i.year##i.gender i.year##i.educ ///
	(log_price = lag4iv) if year == 2013|year == 2014, fe first vce(cluster pid)