

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

* save file
save "data\shape\i_inctax.dta", replace