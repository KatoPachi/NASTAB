
* directroy
cd "C:\Users\vge00\Desktop\nastab"

* call shaped data.
* This data is made by R. I will replace latter.
use "data\shapedt.dta", clear

* summary of donation
summarize total_g if year == 2013  //-> max = 3200, n = 13984
summarize total_g if year == 2014  //-> max = 7100, n = 13787


* density plot: total donations using all observations
kdensity total_g if year == 2013, ///
	addplot(kdensity total_g if year == 2014, ///
			legend(label(1 "2013") label(2 "2014")) range(0 7100) ///
			title("Use All Units"))

* summary of dontaion: units receiving tax banafit 
summarize total_g if year == 2013 & pca225 == 1  //-> max = 3200, n = 958
summarize total_g if year == 2014 & pca227 == 1  //-> max = 7100, n = 705			

* density plot: total donations using units receiving tax benefits
kdensity total_g if year == 2013 & pca225 == 1, ///
	addplot(kdensity total_g if year == 2014 & pca227 == 1, ///
			legend(label(1 "2013") label(2 "2014")) range(0 7100) ///
			title("Use Units Receiving Tax Benefit"))

/* Max value may be outlier -> Restrict 95% percentile */

* use all units 
summarize total_g if year == 2013, detail  //-> 95% = 400
summarize total_g if year == 2014, detail  //-> 95% = 360

kdensity total_g if year == 2013 & total_g <= 400, ///
	addplot(kdensity total_g if year == 2014 & total_g <= 360, ///
			legend(label(1 "2013") label(2 "2014")) ///
			title("Use All Units (95 percentile)"))

* use units receiving tax benefit
summarize total_g if year == 2013 & pca225 == 1, detail  //-> 95% = 725
summarize total_g if year == 2014 & pca227 == 1, detail  //-> 95% = 850

kdensity total_g if year == 2013 & pca225 == 1 & total_g <= 725, ///
	addplot(kdensity total_g if year == 2014 & pca227 == 1 & total_g <= 850, ///
			legend(label(1 "2013") label(2 "2014")) ///
			title("Use Units Receiving Tax Benefit (95 percentile)"))