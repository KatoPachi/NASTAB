
* directory
cd "C:\Users\vge00\Desktop\nastab"

* call shaped data.
* This data is made by R. I will replace latter.
use "data\shapedt.dta", clear

/*
total donation
*/

bysort year: summarize avg if treat_neutral == 1
bysort year: summarize total_g if treat_higher == 1
bysort year: summarize total_g if treat_lower == 1

egen treat_year = group(treat year)
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
intensive margin
*/