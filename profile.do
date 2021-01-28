cd "C:\Users\gt510\Documents\Korea"  //root path

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
