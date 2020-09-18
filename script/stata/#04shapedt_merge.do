
cd "C:\Users\vge00\Desktop\NaSTaB"  //root path

* call giving.dta
use "data\shape\giving.dta", clear

* merge i_inctax.dta
merge 1:1 hhid pid year using "data\shape\i_inctax.dta"
drop _merge

* merge ses_attitude.dta 
merge 1:1 hhid pid year using "data\shape\ses_attitude.dta"
drop _merge

label variable year "年度（調査年度の前年）"

* save file
save "data\shaped.dta", replace