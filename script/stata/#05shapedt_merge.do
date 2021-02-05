
cd "C:\Users\vge00\Desktop\NaSTaB"  //root path

do "script\stata\#01shapedt_ses.do"
do "script\stata\#02shapedt_giving.do"
do "script\stata\#03shapedt_tax.do"

* call giving.dta
use "data\shape\giving.dta", clear

* merge i_inctax.dta
merge 1:1 hhid pid year using "data\shape\i_inctax.dta"
drop _merge

* merge ses_attitude.dta 
merge 1:1 hhid pid year using "data\shape\ses_attitude.dta"
drop _merge

label variable year "年度（調査年度の前年）"

* fix covariate
replace gender = gender - 1
gen univ = (educ == 3) if !missing(educ)
gen highschool = (educ == 2) if !missing(educ)
gen juniorhigh = (educ == 1) if !missing(educ)
gen sqage = age^2/100

gen benefit_group = .
replace benefit_group = 1 if credit_benefit == 1
replace benefit_group = 2 if credit_neutral == 1
replace benefit_group = 3 if credit_loss == 1

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

label variable univ "[世帯員]大卒ダミー"
label variable highschool "[世帯員]高卒ダミー"
label variable juniorhigh "[世帯員]中卒ダミー"
label variable sqage "[世帯員]年齢の二乗/100"
label variable benefit_group "[世帯員]税制改正の利益"
label variable now_balance "[世帯員]現在の税負担と福祉水準のバランス"
label variable ideal_balance "[世帯員]理想的な税負担と福祉水準のバランス"

* first giving price
gen price = .
replace price = 1 - first_mtr if year < 2014
replace price = 1 - 0.15 if year >= 2014
label variable price "[世帯員]first giving price"

* last giving price
gen lprice = .
replace lprice = 1 - last_mtr if year < 2014 
replace lprice = 1 - 0.15 if year >= 2014
label variable lprice "[世帯員]last giving price"

* lagged giving price
forvalues i = 1(1)3 {
    gen lag`i'inc_price = .
	replace lag`i'inc_price = 1 - lag`i'inc_mtr
	label variable lag`i'inc_price "[世帯員]first giving price (lag`i' income, tax deduction)"
}

* iv giving price
tsset pid year 
gen nyear = year - 2014

forvalues i = 1(1)3 {
    gen lag`i'_nyear = l`i'.nyear
	gen iv`i'price = .
	replace iv`i'price = price/(1-0.15) if lag`i'_nyear >= 0
	replace iv`i'price = price/lag`i'inc_price if lag`i'_nyear < 0
	label variable iv`i'price "[世帯員]first price with lagged`i' income/lagged`i' first price"
	drop lag`i'_nyear
}

drop nyear

* save file
save "data\shaped.dta", replace
