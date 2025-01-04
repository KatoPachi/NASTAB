*****************  Data setting -merge *************

*** 0_1. db smart_nastab_v2 code
*** 0_2. Income. From household member - To household
*** 0_3. household features.  From household - To household member
*** Append (2 & 3)
*** Merge (1 & 2 & 3)

*****************************************************

*** 0_1. db smart_nastab_v2 code

smart_nastab_v2 hgen hage hedu hjob hjpo hmar fnum hinc hexp ba001 ba002 ba007 ba003 ba700 hsize bb002 exp_bb exp_cb exp_cc exp_cd exp_ce exp_cf exp_cg exp_ch exp_co exp_cp exp_cr exp_ci exp_cj exp_ck exp_cl exp_cm exp_cn exp_cq exp_cs exp_ct exp_cu asset_fa b10 hcw hlw pgen page prel pedu a13 aa100 aa001 aa200 aa002 aa005 aa006 bc026 bc032 bc012 bc019 bc100 pinc inc_ba1 inc_ba2 inc_ba3 inc_bb1 inc_bb2 inc_bb3 inc_bb4 inc_bb5 pcw plw, wave(04 05 06 07 08 09 10 11) add_h() add_p() wd( NaSTaB/data/1.original) website( ) save( )

save "NaSTaB/data/2.save/save1.dta" ,replace

* hoseuhold
clear

smart_nastab_v2 hgen pgen , wave(04 05 06 07 08 09 10 11) add_h(wfnum wcnum wcnum_1 wcnum_2 wrgn01 wrgn02 wrgn03 wrgn04 wrgn05 wrgn06 wrgn07 wrgn08 wrgn09 wrgn10 wedu01 wedu02 wedu03 wedu04 wedu05 wedu06 wedu07 wedu08 wedu09 wedu10 wgrd01 wgrd02 wgrd03 wgrd04 wgrd05 wgrd06 wgrd07 wgrd08 wgrd09 wgrd10 wjob01 wjob02 wjob03 wjob04 wjob05 wjob06 wjob07 wjob08 wjob09 wbjo10 wjpo01 wjpo02 wjpo03 wjpo04 wjpo05 wjpo06 wjpo07 wjpo08 wjpo09 wjpo10 hca001 hca002 hca003 hca004 hcr001 hcr003 hcr004 hcr005 hcr006 hcr007 hcr008 hcr009 hcr010 hcr011 hcr012 hcr013 hcr014 hcr015 hcr016 hcr017 hcr018 hcr019 hcr020 hda001 hda002 hda003 hda004 hdh002 hdh003 hdh005 hdh006 hdh008 hdh009 hdh011 hdh012 hfa001 hfa002 hfb030 hfb031 hfc001 hka002 hka005 hka010 hka015 hka020 hka025 hka030 hka035 hka041 hka045 hka082 hcwt hlwt) add_p() wd( NaSTaB/data/1.original) website( ) save( )

save "NaSTaB/data/2.save/save2.dta", replace


* Household- transfer
clear

smart_nastab_v2 hgen pgen , wave(04 05 06 07 08 09 10 11) add_h(hdc001 hdc004 hdc005 hdc006 hdc007 hdc008 hdc010 hdc101 hdc011 hdc014 hdc015 hdc016 hdc017 hdc018 hdc020 hdc102 hdc021 hdc024 hdc025 hdc026 hdc027 hdc028 hdc030 hdc103 hdc031 hdc034 hdc035 hdc036 hdc037 hdc038 hdc040 hdc104 hdc041 hdc043 hdc044 hdc045 hdc046 hdc047 hdc049 hdc105 hdc050 hdc052 hdc053 hdc054 hdc055 hdc056 hdc058 hdc106 hdc059 hdc061 hdc062 hdc063 hdc064 hdc065 hdc067 hdc107 hdc068 hdd001 hdd003 hdd004 hdd005 hdd006 hdd007 hdd009 hdd101 hdd011 hdd013 hdd014 hdd015 hdd016 hdd017 hdd019 hdd102 hdd020 hdd021 hdd023 hdd024 hdd025 hdd026 hdd027 hdd029 hdd103 hdd031 hdd033 hdd034 hdd035 hdd036 hdd037 hdd039 hdd104 hdd041 hdd043 hdd044 hdd045 hdd046 hdd047 hdd049 hdd105 hdd051 hdd052 hdd053 hdd054 hdd055 hdd056 hdd058 hdd106 hdd060 hdd061 hdd062 hdd063 hdd064 hdd065 hdd067 hdd107 hdd069 hdd070 hdd071 hdd072 hdd073 hdd074 hdd076 hdd108 hdd078 hdd079 hdd080 hdd081 hdd082 hdd083 hdd085 hdd109 hdd087 hdd088 hdd089 hdd090 hdd091 hdd092 hdd094 hdd110) add_p() wd( NaSTaB/data/1.original) website( ) save( )

save "NaSTaB/data/2.save/save3.dta", replace


* household member
clear

smart_nastab_v2 hgen pgen , wave(04 05 06 07 08 09 10 11) add_h() add_p(psa13 psa18 psa22 paa001 paa101 paa102 paa008 paa009 paa002 paa005 paa006 paa103 paa115 paa116 paa117 paa118 paa016 pbb001 pbb002 pbb003 pbb004 pbb005 pbb006 pbb007 pbb008 pbc001 pbc002 pbc003 pbc012 pbc013 pbc014 pbc015 pbc016 pbc017 pbc018 pbc019 pbc020 pbc056 pbc057 pbc058 pbc059 pbc061 pbc062 pbc021 pbc023 pbc026 pbc027 pbc028 pbc029 pbc030 pbc032 pbc044 pbc045 pbc046 pbc047 pbc048 pbc049 pbc050 pbc051 pbc052 pbc053 pbc054 pbc055 pbd014 pbd008 pbd010 pbd012 pbd015 pbd016 pbd009 pbd011 pbd013 pbd017 pbd004 pbe003 pbe004 pca201 pca202 pca401 pca402 pca203 pca204 pca211 pca212 pca213 pca214 pca225 pca226 pca227 pca228 pca229 pca230 pca231 pca232 pca241 pca242  pca247 pca248 pca249 pca250 pca301 pca302 pca303 pda201 pda401 pda402 pda202 pda207 pda208 pda209 pda210 pda229 pda230 pda231 pda232 pda233 pda234 pda235 pda236 pda301 pda302 pda303 pcwt plwt) wd( NaSTaB/data/1.original) website( ) save( )

save "NaSTaB/data/2.save/save4.dta", replace

clear

smart_nastab_v2 hgen pgen , wave(04 05 06 07 08 09 10 11) add_h() add_p(pea001 pea002 pea003 pea004 pea005 pea006 pea007 pea008 pea009 pea010 pea011 peb001 peb002 peb003 peb004 peb005 peb006 peb007 peb008 pec001 pec002 pec003 ped001 ped002 pga001 pga003 pga004 pga005 pga006 pga007 pga008 pga009 pga010 pga011 pga020 pga030 pga040 pgb001 pgb020 pgb030 pgb151 pgb152 pgb153 pgb154 pgb155 pgb156 pgb157 pgb158 pgb159 pgb041 pgb042 pgb043 pgb044 pgb045 pgb051 pgb052 pgb053 pgb054 pgb055 pgb056 pgb057 pgb058 pgb050 pgb060 pgb070 pgb080 pgb081 pgb082 pgb083 pgb084 pgb085 pgb086 pgb087 pgb088 pgb090 pgb100 pgb110 pgb120 pgc001 pgc002 pgc009 pgc003 pgc004 pgc005 pgc006 pgc007 pgc008 pgc020 pgc021 pgc022 pgc023 pgc030 pgc025 pgc026 pgc027 pgc040 pgc041 pgc042 pgc043 pgc044 pgc045 pgc050 pgc060 pgd041 pgd042 pgd043 pgd044 pgd045 pgd046 pgd047 pgd048 pgf001 pge001 pge020 pge030 pge031 pge041 pge042 pgh001 pgh002 pgh003 pgh004 pgh005 pgh006 pgh007 pgh008 pgh009 pgh0010 pgi001 pgi002 pgi003 pgj001 pgj002 pgj003 pgj043 pgj004 pgj005 pgj006 pgj046 pgj007 pgj008 pgj009 pgj049 pgj010 pgj011 pgj012 pgj052 pgj013 pgj014 pgj015 pgj016 pgj017 pgj018 pgj019 pgj020 pgj021 pgj022 pgj023 pgj024 pgj025 pgj026 pgj027 pgj028 pgj029 pgj030 pgj031) wd( NaSTaB/data/1.original) website( ) save( )

save "NaSTaB/data/2.save/save5.dta", replace




*** 0_3. household features.  From household - To household member

* wave 4
clear
use "NaSTaB/data/1.original/NaSTaB04H.dta" ,clear
keep hid04 hpid01-hpid09 w04edu01-h04ba003
gen wave=4
reshape long hpid0 w04edu0 w04grd0 w04job0 w04jpo0 w04whm0 w04whw0 w04whe0 w04rgn0 , i(hid04) j(n)
rename hpid0 pid04
drop if mi(pid04)
rename pid04 pid
rename hid04 hhid
rename w04* *
rename h04* *

save "NaSTaB/data/2.save/H04.dta", replace

* wave 5
clear
use "NaSTaB/data/1.original/NaSTaB05H.dta" ,clear
keep hid05 hpid01-hpid09 w05edu01-h05ba003
gen wave=5
reshape long hpid0 w05edu0 w05grd0 w05job0 w05jpo0 w05whm0 w05whw0 w05whe0 w05rgn0 , i(hid05) j(n)
rename hpid0 pid05
drop if mi(pid05)
rename pid05 pid
rename hid05 hhid
rename w05* *
rename h05* *

save "NaSTaB/data/2.save/H05.dta", replace

* wave 6
clear
use "NaSTaB/data/1.original/NaSTaB06H.dta" ,clear
keep hid06 hpid01-hpid09 w06edu01-h06ba003
gen wave=6
reshape long hpid0 w06edu0 w06grd0 w06job0 w06jpo0 w06whm0 w06whw0 w06whe0 w06rgn0 , i(hid06) j(n)
rename hpid0 pid06
drop if mi(pid06)
rename pid06 pid
rename hid06 hhid
rename w06* *
rename h06* *

save "NaSTaB/data/2.save/H06.dta", replace

* wave 7
clear
use "NaSTaB/data/1.original/NaSTaB07H.dta" ,clear
keep hid07 hpid01-hpid09 w07edu01-h07ba003
gen wave=7
reshape long hpid0 w07edu0 w07grd0 w07job0 w07jpo0 w07whm0 w07whw0 w07whe0 w07rgn0 , i(hid07) j(n)
rename hpid0 pid07
drop if mi(pid07)
rename pid07 pid
rename hid07 hhid
rename w07* *
rename h07* *

save "NaSTaB/data/2.save/H07.dta", replace

* wave 8
clear
use "NaSTaB/data/1.original/NaSTaB08H.dta" ,clear
keep hid08 hpid01-hpid09 w08edu01-h08ba003
gen wave=8
reshape long hpid0 w08edu0 w08grd0 w08job0 w08jpo0 w08whm0 w08whw0 w08whe0 w08rgn0 , i(hid08) j(n)
rename hpid0 pid08
drop if mi(pid08)
rename pid08 pid
rename hid08 hhid
rename w08* *
rename h08* *

save "NaSTaB/data/2.save/H08.dta", replace

* wave 9
clear
use "NaSTaB/data/1.original/NaSTaB09H.dta" ,clear
keep hid09 hpid01-hpid09 w09edu01-h09ba003
gen wave=9
reshape long hpid0 w09edu0 w09grd0 w09job0 w09jpo0 w09whm0 w09whw0 w09whe0 w09rgn0 , i(hid09) j(n)
rename hpid0 pid09
drop if mi(pid09)
rename pid09 pid
rename hid09 hhid
rename w09* *
rename h09* *

save "NaSTaB/data/2.save/H09.dta" , replace

* wave 10
clear
use "NaSTaB/data/1.original/NaSTaB10H.dta" ,clear
keep hid10 hpid01-hpid09 w10edu01-h10ba003
gen wave=10
reshape long hpid0 w10edu0 w10grd0 w10job0 w10jpo0 w10whm0 w10whw0 w10whe0 w10rgn0 , i(hid10) j(n)
rename hpid0 pid10
drop if mi(pid10)
rename pid10 pid
rename hid10 hhid
rename w10* *
rename h10* *

save "NaSTaB/data/2.save/H10.dta", replace

* wave 11
clear
use "NaSTaB/data/1.original/NaSTaB11H.dta" ,clear
keep hid11 hpid01-hpid09 w11edu01-h11ba003
gen wave=11
reshape long hpid0 w11edu0 w11grd0 w11job0 w11jpo0 w11whm0 w11whw0 w11whe0 w11rgn0 , i(hid11) j(n)
rename hpid0 pid11
drop if mi(pid11)
rename pid11 pid
rename hid11 hhid
rename w11* *
rename h11* *

save "NaSTaB/data/2.save/H11.dta", replace


*** Append
clear
use "NaSTaB/data/2.save/H04.dta"
append using "NaSTaB/data/2.save/H05.dta" ///
  "NaSTaB/data/2.save/H06.dta" ///
  "NaSTaB/data/2.save/H07.dta" ///
  "NaSTaB/data/2.save/H08.dta" ///
  "NaSTaB/data/2.save/H09.dta" ///
  "NaSTaB/data/2.save/H10.dta" ///
  "NaSTaB/data/2.save/H11.dta"

drop jhd01 jhr01 jhn01 jhd02 jhr02 jhn02 jhd03 jhr03 jhn03 jhd04 jhr04 jhn04 jhd05 jhr05 jhn05 jhd06 jhr06 jhn06 jhd07 jhr07 jhn07 jhd08 jhr08 jhn08 jhd09 jhr09 jhn09 pps01 kpp01 rpy01 rpm01 rpa01 pps02 kpp02 rpy02 rpm02 rpa02 pps03 kpp03 rpy03 rpm03 rpa03 pps04 kpp04 rpy04 rpm04 rpa04 pps05 kpp05 rpy05 rpm05 rpa05 pps06 kpp06 rpy06 rpm06 rpa06 pps07 kpp07 rpy07 rpm07 rpa07 pps08 kpp08 rpy08 rpm08 rpa08 pps09 kpp09 rpy09 rpm09 rpa09 rpp01 rpp02 rpp03 rpp04 rpp05 rpp06 rpp07 rpp08 rpp09 edu10 grd10 job10 jpo10 pps10 kpp10 rpy10 rpm10 rpp10 rpa10 whm10 whw10 whe10 jhd10 jhr10 jhn10 rgn10

save "NaSTaB/data/2.save/H.dta", replace

clear


*** Merge
clear
use "NaSTaB/data/2.save/H.dta"
merge 1:1 hhid pid wave using "NaSTaB/data/2.save/save1.dta"
drop _merge
merge 1:1 hhid pid wave using "NaSTaB/data/2.save/save2.dta"
drop _merge
merge 1:1 hhid pid wave using "NaSTaB/data/2.save/save3.dta"
drop _merge
merge 1:1 hhid pid wave using "NaSTaB/data/2.save/save4.dta"
drop _merge
merge 1:1 hhid pid wave using "NaSTaB/data/2.save/save5.dta"
drop _merge
save "NaSTaB/data/2.save/Merge.dta", replace