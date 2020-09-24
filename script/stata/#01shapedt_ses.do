
* call original data
cd "C:\Users\vge00\Desktop\NaSTaB"  //root path
use "data\merge\merge.dta", clear

* keep variables
keep hhid pid year wave p_page p_pedu p_pgen h_b10  ///
	pca201-pca228 pinc_all inc_bb1  ///
	pga001 pgb110 pgb120 pgb151-pgb159 pgb051-pgb058 pgb090 pgb020 ///
	pgc007 pgc008 pea002
	
* make variable
gen welfare_level = .
replace welfare_level = pea002 if year < 2015
replace welfare_level = pgb020 if 2015 <= year
	
* rename variables
rename p_page age     //年齢
rename p_pedu educ    //学歴
rename p_pgen gender  //性別
rename h_b10 living_area  //地域コード
rename pca201 ext_deduct_lincome  //労働所得控除合計 (abbr) ext: 該当したかどうか, krw: 控除額
rename pca202 krw_deduct_lincome
rename pca225 ext_deduct_giving  //寄付金所得控除
rename pca226 krw_deduct_giving
rename pca227 ext_credit_giving  //寄付金税額控除
rename pca228 krw_credit_giving
rename pinc_all tincome  //昨年1年間の総合所得
rename inc_bb1 lincome   //昨年1年間の労働所得
rename pga001 trust_politician //政治家への信頼
rename pgb110 avg_welfare_tax //平均的な税負担と福祉水準
rename pgb120 opt_welfare_tax //望ましい税負担と福祉水準

rename pgb151 opttax_tincome_1000  //所与の総合所得に対する適切な税率
rename pgb152 opttax_tincome_3000
rename pgb153 opttax_tincome_5000
rename pgb154 opttax_tincome_7000
rename pgb155 opttax_tincome_10000
rename pgb156 opttax_tincome_20000
rename pgb157 opttax_tincome_30000
rename pgb158 opttax_tincome_50000
rename pgb159 opttax_tincome_100000

rename pgb051 opttax_lincome_1500  //所与の勤労所得に対する所与の税額が望ましいかどうか (2016年限定)
rename pgb052 opttax_lincome_3500
rename pgb053 opttax_lincome_5500
rename pgb054 opttax_lincome_7000
rename pgb055 opttax_lincome_9000
rename pgb056 opttax_lincome_13000
rename pgb057 opttax_lincome_37000
rename pgb058 opttax_lincome_180000

rename pgb090 addtax //福祉拡充のための税の追加負担の意向

rename pgc007 comp_pg //政府の公共財供給で自身の納税行動が変化する
rename pgc008 comp_trust //政府の信頼で自身の納税行動が変化する

* edit value
elabel drop (ext_deduct_lincome)
elabel drop (ext_deduct_giving)

replace ext_deduct_lincome = 0 if ext_deduct_lincome == 2  //denote "No" as 0 
replace ext_deduct_giving = 0 if ext_deduct_giving == 2
replace ext_credit_giving = 0 if ext_credit_giving == 2
replace ext_deduct_lincome = . if ext_deduct_lincome == -9  //from not respond to NA
replace ext_deduct_giving = . if ext_deduct_giving == -9
replace ext_credit_giving = . if ext_credit_giving == -9

replace krw_deduct_lincome = 0 if ext_deduct_lincome == 0   //if "No", then replace KRW 0
replace krw_deduct_giving = 0 if ext_deduct_giving == 0
replace krw_credit_giving = 0 if ext_credit_giving == 0 
replace krw_deduct_lincome = . if krw_deduct_lincome == -9  //from not respond to NA
replace krw_deduct_giving = . if krw_deduct_giving == -9
replace krw_credit_giving = . if krw_credit_giving == -9

replace trust_politician = 6 - trust_politician   //5:high trust ~ 1:low trust

replace welfare_level =  . if welfare_level == -9
replace welfare_level = 6 - welfare_level   //5:very high ~ 1:very low

replace comp_pg = 6 - comp_pg
replace comp_trust = 6 - comp_trust

* extract income data (This uses tax calculation in shapedt_tax.do)
frame copy default inc
frame inc {
    keep hhid pid year tincome lincome
	
	label variable tincome "[世帯員]年間所得総額（単位：10,000KRW）"
	label variable lincome "[世帯員]年間労働所得（単位:10,000KRW）"
}

frame inc: save "data\shape\inc.dta", replace

* extract (time-varying) socio-economic data
frame copy default ses
frame ses {
    keep hhid pid year living_area age educ gender ///
		ext_deduct_lincome krw_deduct_lincome ///
		ext_deduct_giving krw_deduct_giving   ///
		ext_credit_giving krw_credit_giving   ///]
		trust_politician welfare_level
		
	label variable living_area "[世帯]地域コード"
	label variable ext_deduct_lincome "[世帯員]労働所得控除の有無"
	label variable krw_deduct_lincome "[世帯員]労働所得控除額"
	label variable ext_deduct_giving "[世帯員]寄付金所得控除の有無"
	label variable krw_deduct_giving "[世帯員]寄付金所得控除額"
	label variable ext_credit_giving "[世帯員]寄付金税額控除の有無"
	label variable krw_credit_giving "[世帯員]寄付金税額控除額"
	label variable trust_politician "[世帯員]政治家への信頼度"
	label variable welfare_level "[世帯員]納税レベルに対する福祉水準"
}

frame ses: save "data\shape\ses.dta", replace

* extract (time-invariant) tax-attitude data
frame copy default tax_attitude
frame tax_attitude: keep if year == 2018
frame tax_attitude {
    keep hhid pid trust_politician avg_welfare_tax opt_welfare_tax ///
		opttax_tincome_1000 opttax_tincome_3000 opttax_tincome_5000 opttax_tincome_7000 ///
		opttax_tincome_10000 opttax_tincome_20000 opttax_tincome_30000 ///
		opttax_tincome_50000 opttax_tincome_100000 ///
		addtax welfare_level
	
	label variable avg_welfare_tax "[世帯員]税負担と福祉水準の程度（2018年調査）"
	label variable opt_welfare_tax "[世帯員]望ましい税負担と福祉水準の程度（2018年調査）"
	label variable opttax_tincome_1000 "[世帯員]年間所得10mKRWの適切な税率（2018年調査）"
	label variable opttax_tincome_3000 "[世帯員]年間所得30mKRWの適切な税率（2018年調査）"
	label variable opttax_tincome_5000 "[世帯員]年間所得50mKRWの適切な税率（2018年調査）"
	label variable opttax_tincome_7000 "[世帯員]年間所得70mKRWの適切な税率（2018年調査）"
	label variable opttax_tincome_10000 "[世帯員]年間所得100mKRWの適切な税率（2018年調査）"
	label variable opttax_tincome_20000 "[世帯員]年間所得200mKRWの適切な税率（2018年調査）"
	label variable opttax_tincome_30000 "[世帯員]年間所得300mKRWの適切な税率（2018年調査）"
	label variable opttax_tincome_50000 "[世帯員]年間所得500mKRWの適切な税率（2018年調査）"
	label variable opttax_tincome_100000 "[世帯員]年間所得1000mKRWの適切な税率（2018年調査）"
	label variable addtax "[世帯員]福祉拡充の追加課税への選好（2018年調査）"
}

frame tax_attitude: save "data\shape\tax_attitude1.dta", replace

frame copy default tax_attitude2
frame tax_attitude2: keep if year == 2016
frame tax_attitude2 {
    keep hhid pid comp_pg comp_trust
	
	label variable comp_pg "[世帯員]政府公共財の申告行動への影響（2016年調査）"
	label variable comp_trust "[世帯員]政府に対する信頼の申告行動への影響（2016年調査）"
}

frame tax_attitude2: save "data\shape\tax_attitude2.dta", replace

* clear environment
frame change default
frame drop inc
frame drop ses
frame drop tax_attitude
frame drop tax_attitude2

* merge three datasets
use "data\shape\ses.dta", clear
merge m:1 hhid pid using "data\shape\tax_attitude1.dta"
drop _merge
merge m:1 hhid pid using "data\shape\tax_attitude2.dta"
drop _merge

save "data\shape\ses_attitude.dta", replace    //Dataset 1 for final merge





