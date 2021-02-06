

cd "C:\Users\vge00\Desktop\NaSTaB"  //root path

* transform file extention csv -> dta
import delim using "data\origin\mtrdt.csv", clear
save "data\origin\mtrdt.dta", replace

* call income data
use "data\shape\inc.dta", clear

/*
 keep (i) observations we have information of lincome and 
 (ii) observations which lincome is greater than or equal to lower_income_10000won
 Note: lower_income_10000won is a lower bound of income bracket of specific tax rate
*/

frame copy default firstdt
frame firstdt {
	joinby year using "data\origin\mtrdt.dta"
	
	keep if lincome != .
	keep if lower_income_10000won <= lincome

	bysort pid year: egen max_mtr = max(mtr)
	drop lower_income_10000won mtr
	rename max_mtr first_mtr 
	duplicates drop

	label variable first_mtr "[世帯員]年間労働所得ベースの限界所得税率"
}
frame firstdt: save "data\shape\i_firstmtr.dta", replace
frame drop firstdt

frame copy default lastdt
frame lastdt {
	joinby year using "data\origin\mtrdt.dta"
	joinby year hhid pid using "data\shape\giving.dta"
	
	keep if !missing(lincome) & !missing(i_total_giving)
	gen lincome_give = lincome - i_total_giving
	keep if lower_income_10000won <= lincome_give

	bysort pid year: egen max_mtr = max(mtr)
	drop lower_income_10000won mtr h_total_giving h_ext_giving i_total_giving i_ext_giving 
	rename max_mtr last_mtr 
	duplicates drop
	
	label variable lincome_give "[世帯員]年間労働所得（-寄付額）"
	label variable last_mtr "[世帯員]寄付を差し引いた年間労働所得ベースの限界所得税率"
}
frame lastdt: save "data\shape\i_lastmtr.dta", replace
frame drop lastdt

forvalues i=1(1)3 {
	frame copy default lag`i'dt
	frame lag`i'dt {
		tsset pid year 
		gen lincome_l`i' = l`i'.lincome
		
		joinby year using "data\origin\mtrdt.dta"
		
		keep if lincome_l`i' != .
		keep if lower_income_10000won <= lincome_l`i'

		bysort pid year: egen max_mtr = max(mtr)
		drop lower_income_10000won mtr
		rename max_mtr lag`i'inc_mtr 
		duplicates drop

		label variable lincome_l`i' "[世帯員]年間労働所得（lag = `i'）"
		label variable lag`i'inc_mtr "[世帯員]年間労働所得ベースの限界所得税率(lag = `i')"
	}
	frame lag`i'dt: save "data\shape\i_lag`i'mtr.dta", replace
	frame drop lag`i'dt
}

* tax deduction -> tax credit benefit/loss variable
frame create treat
frame treat {
	use "data\shape\i_firstmtr.dta"
	keep if year == 2013
	keep hhid pid first_mtr
	
	gen credit_neutral = 0
	replace credit_neutral = 1 if first_mtr == float(.15)
	gen credit_benefit = 0
	replace credit_benefit = 1 if first_mtr < float(.15)
	gen credit_loss = 0
	replace credit_loss = 1 if first_mtr > float(.15)
	
	label variable credit_neutral "[世帯員]税制改正の利益：2013年MTR = 0.15"
	label variable credit_benefit "[世帯員]税制改正の利益：2013年MTR < 0.15"
	label variable credit_loss "[世帯員]税制改正の利益：2013年MTR > 0.15"
}
frame treat: save "data\shape\i_taxreform.dta", replace
frame drop treat

* merge 
frame change default

merge 1:1 hhid pid year using "data\shape\i_firstmtr.dta"
drop _merge

merge 1:1 hhid pid year using "data\shape\i_lastmtr.dta"
drop _merge

forvalues i = 1(1)3 {
	merge 1:1 hhid pid year using "data\shape\i_lag`i'mtr.dta"
	drop _merge
}

merge m:1 hhid pid using "data\shape\i_taxreform.dta"
drop _merge

* save file
save "data\shape\i_inctax.dta", replace
