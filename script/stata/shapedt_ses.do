
* call original data
cd "C:\Users\vge00\Desktop\NaSTaB"  //root path
use "data\merge\merge.dta", clear

* keep variables
keep hhid pid year p_page p_pedu p_pgen h_b10 pca301 pda301  ///
	pca201-pca228 pinc_all inc_bb1  ///
	pga020 pgb110 pgb120 pgb151-pgb159 pgb051-pgb058 pgb090 pgb020
	

* rename variables
rename p_page age     //年齢
rename p_pedu educ    //学歴
rename p_pgen gender  //性別
rename h_b10 living_area
rename pca301 tax_lincome  //昨年1年間の労働所得に対する納税額
rename pda301 tax_tincome  //昨年1年間の総合所得に対する納税額
rename pca201 ext_deduct_lincome  //労働所得控除合計 (abbr) ext: 該当したかどうか, krw: 控除額
rename pca202 krw_deduct_lincome
rename pca225 ext_deduct_giving  //寄付金所得控除
rename pca226 krw_deduct_giving
rename pca227 ext_credit_giving  //寄付金税額控除
rename pca228 krw_credit_giving
rename pinc_all tincome  //昨年1年間の総合所得
rename inc_bb1 lincome   //昨年1年間の労働所得
rename pga020 trust_politician //政治家への信頼
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

rename pgb051 opttax_lincome_1500  //所与の勤労所得に対する所与の税額が望ましいかどうか
rename pgb052 opttax_lincome_3500
rename pgb053 opttax_lincome_5500
rename pgb054 opttax_lincome_7000
rename pgb055 opttax_lincome_9000
rename pgb056 opttax_lincome_13000
rename pgb057 opttax_lincome_37000
rename pgb058 opttax_lincome_180000

rename pgb090 addtax //福祉拡充のための税の追加負担の意向
rename pgb020 welfare_level //現在の納税レベルに対して政府が提供する福祉水準は高いかどうか

