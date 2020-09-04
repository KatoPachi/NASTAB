
* call original data
cd "C:\Users\vge00\Desktop\NaSTaB"  //root path
use "data\merge\merge.dta", clear

* keeping variables: id, year, donation data 
keep hhid pid year h_exp_cr hcr001 hcr004-hcr020

* save individual giving data: purposes
frame copy default i_giving_purpose
frame i_giving_purpose: keep hhid pid year hcr004 hcr007 hcr010 hcr013 hcr016 hcr019
frame i_giving_purpose: reshape long hcr, i(hhid pid year) j(vars) string 
frame i_giving_purpose {
    rename hcr purpose
    rename vars key
	label variable key "目的別個人寄付額のマージキー"
	replace key = "005" if key == "004"
	replace key = "008" if key == "007"
	replace key = "011" if key == "010"
	replace key = "014" if key == "013"
	replace key = "017" if key == "016"
	replace key = "020" if key == "019"
}

frame i_giving_purpose: save "data\shape\i_giving_purpose.dta", replace

* save individual giving data: amount
frame copy default i_giving_amount
frame i_giving_amount: keep hhid pid year hcr005 hcr008 hcr011 hcr014 hcr017 hcr020
frame i_giving_amount: reshape long hcr, i(hhid pid year) j(key) string
frame i_giving_amount {
    rename hcr amount
	label variable key "目的別個人寄付額のマージキー"
}

frame i_giving_amount: save "data\shape\i_giving_amount.dta", replace

* save household giving data
frame copy default i_giving_others
frame i_giving_others: keep hhid pid year h_exp_cr hcr001
frame i_giving_others {
    rename h_exp_cr h_total_giving
	rename hcr001 h_ext_giving
}

frame i_giving_others: save "data\shape\i_giving_others.dta", replace

* clear memories
frame change default
frame drop i_giving_purpose
frame drop i_giving_amount
frame drop i_giving_others

* merge purpose and amount data
use "data\shape\i_giving_purpose.dta", clear
merge 1:1 hhid pid year key using "data\shape\i_giving_amount.dta", keepus(amount)
drop _merge key

* merge other giving data
merge m:1 hhid pid year using "data\shape\i_giving_others.dta", keepus(h_total_giving h_ext_giving)
drop _merge
elabel drop (h_ext_giving)   //install package "elabel"
replace h_ext_giving = 0 if h_ext_giving == 2

* calculate individual giving data
replace amount = 0 if amount == . | amount == -9
bysort hhid pid year: egen i_total_giving = sum(amount)

gen i_ext_giving = .
replace i_ext_giving = 1 if i_total_giving > 0
replace i_ext_giving = 0 if i_total_giving == 0

drop purpose amount
duplicates drop

* save giving data
save "data\shape\giving.dta"

