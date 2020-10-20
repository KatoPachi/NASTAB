
cd "C:\Users\vge00\Desktop\NaSTaB"  //root path

* call original data
use "data\origin\福祉分野歳出データ.dta"

* rename variables
rename h_b10 living_area
rename 人口 pop
rename 人口密度 popdens
rename 社会福祉予算の割合 prop_pubbdg
rename 社会福祉分野の予算額 pubbdg
rename 保健分野の予算額 healthbdg
rename 総予算額 bdg

* create variables
gen PPP_pubbdg = pubbdg/pop
gen PPP_healthbdg = healthbdg/pop

* create label
label variable pop "人口"
label variable popdens "人口密度"
label variable prop_pubbdg "社会福祉分野に対する予算の割合"
label variable pubbdg "社会福祉分野の予算額"
label variable healthbdg "保健分野の予算額"
label variable bdg "地方政府の予算"
label variable PPP_pubbdg "一人当たり社会福祉分野予算"
label variable PPP_healthbdg "一人当たり保健分野予算"

* save file
save "data\shape\public_bdg.dta", replace

* call shaped.dta 
use "data\shaped.dta", clear

* merge 
merge m:1 year living_area using "data\shape\public_bdg.dta"
drop _merge

* save file 
save "data\shaped.dta", replace