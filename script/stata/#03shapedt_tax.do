

cd "C:\Users\vge00\Desktop\NaSTaB"  //root path

* transform file extention csv -> dta
import delim using "data\origin\mtrdt.csv", clear
save "data\origin\mtrdt.dta", replace

* call and merge MTR data
use "data\shape\inc.dta", clear
joinby year using "data\origin\mtrdt.dta"

/*
 keep (i) observations we have information of lincome and 
 (ii) observations which lincome is greater than or equal to lower_income_10000won
 Note: lower_income_10000won is a lower bound of income bracket of specific tax rate
*/
keep if lincome != .
keep if lower_income_10000won <= lincome

bysort pid year: egen max_mtr = max(mtr)
drop lower_income_10000won mtr
rename max_mtr mtr 
duplicates drop

label variable mtr "[世帯員]年間労働所得ベースの限界所得税率"


* tax deduction -> tax credit benefit/loss variable
frame copy default treat
frame treat: keep if year == 2013
frame treat: keep hhid pid mtr
frame treat {
	gen credit_neutral = 0
	replace credit_neutral = 1 if mtr == float(.15)
	gen credit_benefit = 0
	replace credit_benefit = 1 if mtr < float(.15)
	gen credit_loss = 0
	replace credit_loss = 1 if mtr > float(.15)
	
	label variable credit_neutral "[世帯員]税制改正の利益：2013年MTR = 0.15"
	label variable credit_benefit "[世帯員]税制改正の利益：2013年MTR < 0.15"
	label variable credit_loss "[世帯員]税制改正の利益：2013年MTR > 0.15"
}
frame treat: save "data\shape\i_taxreform.dta", replace

* merge 
frame change default
frame drop treat

merge m:1 hhid pid using "data\shape\i_taxreform.dta"
drop _merge

* save file
save "data\shape\i_inctax.dta", replace
