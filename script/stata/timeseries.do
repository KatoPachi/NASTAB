
* directory
cd "C:\Users\vge00\Desktop\nastab"

* call shaped data.
* This data is made by R. I will replace latter.
use "data\shapedt.dta", clear

* variables
egen treat_year = group(treat year)

gen extensive = .
replace extensive = 1 if total_g > 0
replace extensive = 0 if total_g == 0

/*
total donation
*/

bysort year: summarize total_g if treat_neutral == 1
bysort year: summarize total_g if treat_higher == 1
bysort year: summarize total_g if treat_lower == 1

bysort treat_year: egen avg = mean(total_g)
bysort treat: gen avg13 = avg if year == 2013
bysort treat (avg13): replace avg13 = avg13[1]
gen norm_avg = avg/avg13

bysort year: summarize norm_avg if treat_neutral == 1
bysort year: summarize norm_avg if treat_higher == 1
bysort year: summarize norm_avg if treat_lower == 1

graph twoway (scatter norm_avg year if treat_neutral == 1, msymbol(o) connect(l)) ///
			 (scatter norm_avg year if treat_higher == 1, msymbol(o) connect(l)) ///
			 (scatter norm_avg year if treat_lower == 1, msymbol(o) connect(l)), ///
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

bysort year: summarize extensive if treat_neutral == 1
bysort year: summarize extensive if treat_higher == 1
bysort year: summarize extensive if treat_lower == 1

bysort treat_year: egen ext = mean(extensive)

line ext year if treat_neutral == 1 ///
|| line ext year if treat_higher == 1 ///
|| line ext year if treat_lower == 1  ///
|| , ///
xline(2014, lcolor("black") lpattern("dash")) ///
ylabel(0(0.2)1) ytitle("Extensive margin of donations")  ///
xtitle("Year")  ///
title("Time series of extensive margin of donations")  ///
legend(label(1 "t = z") label(2 "t < z") label(3 "t > z")) ///
note("t is the marginal tax rate at 2013 based on labor income"  ///
	 "z is the tax credit rate (= 0.15)")

graph export "_assets\stata\timeseries_extensive.png"

frame copy default intensive
frame intensive: drop if extensive == 0
frame intensive {
	bysort year: summarize total_g if treat_neutral == 1
	bysort year: summarize total_g if treat_higher == 1
	bysort year: summarize total_g if treat_lower == 1
	
	bysort treat_year: egen inte = mean(total_g)
	bysort treat: gen inte13 = inte if year == 2013
	bysort treat (inte13): replace inte13 = inte13[1]
	gen norm_inte = inte/inte13
	
	bysort year: summarize norm_inte if treat_neutral == 1
	bysort year: summarize norm_inte if treat_higher == 1
	bysort year: summarize norm_inte if treat_lower == 1
}

frame intensive {
    graph twoway (scatter norm_inte year if treat_neutral == 1, msymbol(o) connect(l)) ///
			     (scatter norm_inte year if treat_higher == 1, msymbol(o) connect(l)) ///
				 (scatter norm_inte year if treat_lower == 1, msymbol(o) connect(l)), ///
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

bysort year: summarize report if treat_neutral == 1
bysort year: summarize report if treat_higher == 1
bysort year: summarize report if treat_lower == 1

bysort treat_year: egen avg_report = mean(report)

line avg_report year if treat_neutral == 1 ///
|| line avg_report year if treat_higher == 1 ///
|| line avg_report year if treat_lower == 1  ///
|| , ///
xline(2014, lcolor("black") lpattern("dash")) ///
ylabel(0(0.2)1) ytitle("Proportion of tax report")  ///
xtitle("Year")  ///
title("Time series of tax report")  ///
legend(label(1 "t = z") label(2 "t < z") label(3 "t > z")) ///
note("Most observations are missing values. Observed units are around 1000." ///
	 "t is the marginal tax rate at 2013 based on labor income"  ///
	 "z is the tax credit rate (= 0.15)")

graph export "_assets\stata\timeseries_report.png"
