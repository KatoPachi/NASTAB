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

** ---- ConstructTaxBalance
gen now_welfare = 1
replace now_welfare = 2 if avg_welfare_tax < 3 & avg_welfare_tax <= 6
replace now_welfare = 3 if avg_welfare_tax <= 3
replace now_welfare = . if missing(avg_welfare_tax)

gen now_tax = 1
replace now_tax = 2 if avg_welfare_tax == 2 | avg_welfare_tax == 5 | avg_welfare_tax == 8
replace now_tax = 3 if avg_welfare_tax == 3 | avg_welfare_tax == 6 | avg_welfare_tax == 9
replace now_tax = . if missing(avg_welfare_tax)

gen now_balance = 0
replace now_balance = 2 if avg_welfare_tax == 1
replace now_balance = 1 if avg_welfare_tax == 2 | avg_welfare_tax == 4
replace now_balance = -1 if avg_welfare_tax == 6 | avg_welfare_tax == 8
replace now_balance = -2 if avg_welfare_tax == 9
replace now_balance = . if missing(avg_welfare_tax)

gen now_balance3 = 0
replace now_balance3 = 1 if now_balance > 0
replace now_balance3 = -1 if now_balance < 0
replace now_balance3 = . if missing(now_balance)

** ---- EstimateTaxBalanceIndex
xtreg now_balance i.year##i.living_area if year >= 2015, fe
predict orgbalanceid, u

xtreg now_balance i.year##i.living_area if year == 2015 | year == 2016, fe
predict orgparkbalanceid, u

xtreg now_balance i.year##i.living_area if year == 2017 | year == 2018, fe
predict orgmoonbalanceid, u



* make trustid dataset
frame copy default balancedt
frame balancedt: {
	bysort pid: egen balanceid = mean(orgbalanceid)
	bysort pid: egen park_balanceid = mean(orgparkbalanceid) 
	bysort pid: egen moon_balanceid = mean(orgmoonbalanceid)
}
frame balancedt: keep pid balanceid park_balanceid moon_balanceid
frame balancedt: duplicates drop
frame balancedt: gen diff_balance = moon_balanceid - park_balanceid
frame balancedt: xtile balance5 = balanceid, nq(5) 
frame balancedt: xtile park_balance5 = park_balanceid, nq(5)
frame balancedt: {
	gen lessdiff1_balance = 0
	replace lessdiff1_balance = 1 if abs(diff_balance) < 1
	replace lessdiff1_balance = . if missing(diff_balance)
}
frame balancedt: {
	gen lessdiffhalf_balance = 0
	replace lessdiffhalf_balance = 1 if abs(diff_balance) < 0.5
	replace lessdiffhalf_balance = . if missing(diff_balance)
}
frame balancedt: save "data\shape\balanceid.dta", replace

merge m:1 pid using "data\shape\balanceid.dta"
drop _merge