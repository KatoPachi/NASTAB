
cd "C:\Users\vge00\Desktop\nastab"  //root path

* call data
use "data\shaped.dta", clear

* variables
gen treat = .
replace treat = 1 if credit_benefit == 1
replace treat = 2 if credit_neutral == 1
replace treat = 3 if credit_loss == 1

egen treat_year = group(treat year)

gen report = .
replace report = ext_deduct_giving if year < 2014
replace report = ext_credit_giving if 2013 < year

/*
total donation
*/

bysort year: summarize i_total_giving if credit_neutral == 1
bysort year: summarize i_total_giving if credit_benefit == 1
bysort year: summarize i_total_giving if credit_loss == 1

bysort treat_year: egen avg = mean(i_total_giving)
bysort treat: gen avg13 = avg if year == 2013
bysort treat (avg13): replace avg13 = avg13[1]
gen norm_avg = avg/avg13

bysort year: summarize norm_avg if credit_neutral == 1
bysort year: summarize norm_avg if credit_benefit == 1
bysort year: summarize norm_avg if credit_loss == 1

graph twoway (scatter norm_avg year if credit_neutral == 1, msymbol(o) connect(l)) ///
			 (scatter norm_avg year if credit_benefit == 1, msymbol(o) connect(l)) ///
			 (scatter norm_avg year if credit_loss == 1, msymbol(o) connect(l)), ///
			 xline(2013.5)  ///
			 xlabel(2008(1)2017) xtitle("Year")  ///
			 ylabel(.6(.2)1.4) ytitle("Normalized Average Donations")  ///
			 legend(label(1 "t = z") label(2 "t < z") label(3 "t > z")) ///
			 title("Time seiries of total donations by treatment")  ///
			 note("t is the marginal tax rate at 2013 based on labor income"  ///
				  "z is the tax credit rate (= 0.15)")

graph export "_assets\stata\timeseries_average.png", replace
	 
/*
extentive and intensive margin
*/

bysort year: summarize i_ext_giving if credit_neutral == 1
bysort year: summarize i_ext_giving if credit_benefit == 1
bysort year: summarize i_ext_giving if credit_loss == 1

bysort treat_year: egen ext = mean(i_ext_giving)

graph twoway (scatter ext year if credit_neutral == 1, msymbol(o) connect(l)) ///
			 (scatter ext year if credit_benefit == 1, msymbol(o) connect(l)) ///
			 (scatter ext year if credit_loss == 1, msymbol(o) connect(l)), ///
			 xline(2013.5)  ///
			 xlabel(2008(1)2017) xtitle("Year")  ///
			 ylabel(0(.1).6) ytitle("Prob(Donations > 0)")  ///
			 legend(label(1 "t = z") label(2 "t < z") label(3 "t > z")) ///
			 title("Time seiries of extensive margin by treatment")  ///
			 note("t is the marginal tax rate at 2013 based on labor income"  ///
				  "z is the tax credit rate (= 0.15)")

graph export "_assets\stata\timeseries_extensive.png", replace

frame copy default intensive
frame intensive: drop if i_ext_giving == 0
frame intensive {
	bysort year: summarize i_total_giving if credit_neutral == 1
	bysort year: summarize i_total_giving if credit_benefit == 1
	bysort year: summarize i_total_giving if credit_loss == 1
	
	bysort treat_year: egen inte = mean(i_total_giving)
	bysort treat: gen inte13 = inte if year == 2013
	bysort treat (inte13): replace inte13 = inte13[1]
	gen norm_inte = inte/inte13
	
	bysort year: summarize norm_inte if credit_neutral == 1
	bysort year: summarize norm_inte if credit_benefit == 1
	bysort year: summarize norm_inte if credit_loss == 1
}

frame intensive {
    graph twoway (scatter norm_inte year if credit_neutral == 1, msymbol(o) connect(l)) ///
			     (scatter norm_inte year if credit_benefit == 1, msymbol(o) connect(l)) ///
				 (scatter norm_inte year if credit_loss == 1, msymbol(o) connect(l)), ///
				 xline(2013.5)  ///
				 xlabel(2008(1)2017) xtitle("Year")  ///
				 ylabel(.6(.2)1.4) ytitle("Normalized average donations cond. on donors", size(small))  ///
				 legend(label(1 "t = z") label(2 "t < z") label(3 "t > z")) ///
				 title("Time seiries of intensive margin by treatment")  ///
				 note("t is the marginal tax rate at 2013 based on labor income"  ///
					  "z is the tax credit rate (= 0.15)")
	
	graph export "_assets\stata\timeseries_intensive.png", replace
}


/*
Tax filing (reporting)
*/

bysort year: summarize report if credit_neutral == 1
bysort year: summarize report if credit_benefit == 1
bysort year: summarize report if credit_loss == 1

bysort treat_year: egen avg_report = mean(report)

graph twoway (scatter avg_report year if credit_neutral == 1 & year < 2018, msymbol(o) connect(l)) ///
			 (scatter avg_report year if credit_benefit == 1 & year < 2018, msymbol(o) connect(l)) ///
			 (scatter avg_report year if credit_loss == 1 & year < 2018, msymbol(o) connect(l)), ///
			 xline(2013.5)  ///
			 xlabel(2008(1)2017) xtitle("Year")  ///
			 ylabel(0(.1).7) ytitle("Prob(Receiving Tax Benefit)")  ///
			 legend(label(1 "t = z") label(2 "t < z") label(3 "t > z")) ///
			 title("Time seiries of extensive margin by treatment")  ///
			 note("t is the marginal tax rate at 2013 based on labor income"  ///
				  "z is the tax credit rate (= 0.15)")

graph export "_assets\stata\timeseries_report.png", replace
