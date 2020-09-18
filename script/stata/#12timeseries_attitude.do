
cd "C:\Users\vge00\Desktop\nastab"  //root path

* call data
use "data\shaped.dta", clear

* summary of factor
table welfare_level if year == 2013

/*
Factor: perceived welfare size
*/

* variables
gen bin_welfare_level = 0
replace bin_welfare_level = 1 if welfare_level > 3

egen factors_year = group(bin_welfare_level year)

/*
Total donations
*/

bysort factors_year: egen avg = mean(i_total_giving)
bysort bin_welfare_level: gen avg13 = avg if year == 2013
bysort bin_welfare_level (avg13): replace avg13 = avg13[1]
gen norm_avg = avg/avg13

graph twoway (scatter avg year if bin_welfare_level == 1, msymbol(o) connect(l) sort(year)) ///
			 (scatter avg year if bin_welfare_level == 0, msymbol(o) connect(l) sort(year)), ///
			 xline(2013.5)  ///
			 xlabel(2007(1)2018) xtitle("Year")  ///
			 ylabel(0(5)40) ytitle("Average Donations")  ///
			 legend(label(1 "High welfare level") label(2 "Low welfare level")) ///
			 title("Time seiries of total donations by perceived welfare level")  

graph export "_assets\stata\timeSeries_absAvg_byWelfare.png", replace			 

graph twoway (scatter norm_avg year if bin_welfare_level == 1, msymbol(o) connect(l) sort(year)) ///
			 (scatter norm_avg year if bin_welfare_level == 0, msymbol(o) connect(l) sort(year)), ///
			 xline(2013.5)  ///
			 xlabel(2007(1)2018) xtitle("Year")  ///
			 ytitle("Normalized Average Donations")  ///
			 legend(label(1 "High welfare level") label(2 "Low welfare level")) ///
			 title("Time seiries of total donations by perceived welfare level")  

graph export "_assets\stata\timeseries_normAvg_byWelfare.png", replace

/*
Income Level
*/

bysort factors_year: egen incavg = mean(lincome)
bysort bin_welfare_level: gen incavg13 = incavg if year == 2013
bysort bin_welfare_level (incavg13): replace incavg13 = incavg13[1]
gen norm_incavg = incavg/incavg13

graph twoway (scatter incavg year if bin_welfare_level == 1, msymbol(o) connect(l) sort(year)) ///
			 (scatter incavg year if bin_welfare_level == 0, msymbol(o) connect(l) sort(year)), ///
			 xline(2013.5)  ///
			 xlabel(2007(1)2018) xtitle("Year")  ///
			 ytitle("Average Donations")  ///
			 legend(label(1 "High welfare level") label(2 "Low welfare level")) ///
			 title("Time seiries of total donations by perceived welfare level")  