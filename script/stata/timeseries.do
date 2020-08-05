
* directory
cd "C:\Users\vge00\Desktop\nastab"

* call shaped data.
* This data is made by R. I will replace latter.
use "data\shapedt.dta", clear
egen treat_year = group(treat year)

/*
total donation
*/

bysort year: summarize avg if treat_neutral == 1
bysort year: summarize total_g if treat_higher == 1
bysort year: summarize total_g if treat_lower == 1

bysort treat_year: egen avg = mean(total_g)

line avg year if treat_neutral == 1 ///
|| line avg year if treat_higher == 1 ///
|| line avg year if treat_lower == 1  ///
|| , ///
xline(2014, lcolor("black") lpattern("dash")) ///
ylabel(0(50)200) ytitle("Average amount of donations at year")  ///
xtitle("Year")  ///
title("Time series of total donations by treatment status")  ///
legend(label(1 "t = z") label(2 "t < z") label(3 "t > z")) ///
note("t is the marginal tax rate at 2013 based on labor income"  ///
	 "z is the tax credit rate (= 0.15)")

graph export "_assets\stata\timeseries_average.png"
	 
/*
extentive and intensive margin
*/

gen extensive = .
replace extensive = 1 if total_g > 0
replace extensive = 0 if total_g == 0

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
	bysort year: summarize avg if treat_neutral == 1
	bysort year: summarize total_g if treat_higher == 1
	bysort year: summarize total_g if treat_lower == 1
	
	bysort treat_year: egen inte = mean(total_g)
}

frame intensive {
	line inte year if treat_neutral == 1 ///
	|| line inte year if treat_higher == 1 ///
	|| line inte year if treat_lower == 1  ///
	|| , ///
	xline(2014, lcolor("black") lpattern("dash")) ///
	ylabel(0(50)300) ytitle("Average amount of donations at year")  ///
	xtitle("Year")  ///
	title("Time series of intentive margin of donations")  ///
	legend(label(1 "t = z") label(2 "t < z") label(3 "t > z")) ///
	note("Drop observations if total donations is zero." ///
		 "t is the marginal tax rate at 2013 based on labor income"  ///
		 "z is the tax credit rate (= 0.15)")
	
	graph export "_assets\stata\timeseries_intensive.png"
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
