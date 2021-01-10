
cd "C:\Users\vge00\Desktop\NaSTaB"  //root path

do "script\stata\#01shapedt_ses.do"
do "script\stata\#02shapedt_giving.do"
do "script\stata\#03shapedt_tax.do"
do "script\stata\#04shapedt_expense.do"

* call giving.dta
use "data\shape\giving.dta", clear

* merge i_inctax.dta
merge 1:1 hhid pid year using "data\shape\i_inctax.dta"
drop _merge

* merge ses_attitude.dta 
merge 1:1 hhid pid year using "data\shape\ses_attitude.dta"
drop _merge

label variable year "年度（調査年度の前年）"

* merge public_bdg.dta
merge m:1 year living_area using "data\shape\public_bdg.dta"
drop _merge

* save file
save "data\shaped.dta", replace
