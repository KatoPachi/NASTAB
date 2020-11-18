
cd "C:\Users\vge00\Desktop\nastab"  //root path

* call data
use "data\shaped.dta", clear

* variables
gen price = .
replace price = 1 - mtr if year < 2014
replace price = 1 - 0.15 if year >= 2014

gen log_price = ln(price)
gen log_total_g = ln(i_total_giving + 1)
gen log_pinc_all = ln(lincome + 100000)
gen log_PPP_pubbdg = ln(pubbdg + 1)
gen sqlog_PPP_pubpdg = log_PPP_pubbdg^2
gen log_PPP_healthbdg = ln(healthbdg + 1)
gen welfarebdg = pubbdg + healthbdg
gen PPP_welfarebdg = welfarebdg/pop
gen log_PPP_welfarebdg = ln(PPP_welfarebdg + 1)

* panel data
xtset pid

