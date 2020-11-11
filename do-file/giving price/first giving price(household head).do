
* 1.1.1 household giving price = h_price

gen h_price=.
replace h_price=0.94 if head_income<=1200  & year==2018
replace h_price=0.85 if 1200<head_income & head_income<=4600 & year==2018
replace h_price=0.76 if 4600<head_income & head_income<=8800 & year==2018
replace h_price=0.65 if 8800<head_income & head_income<=15000 & year==2018
replace h_price=0.62 if 15000<head_income & head_income<=30000 & year==2018
replace h_price=0.60 if 30000<head_income & head_income<=50000 & year==2018
replace h_price=0.58 if head_income>50000 & year==2018


replace h_price=0.94 if head_income<=1200  & year==2017
replace h_price=0.85 if 1200<head_income & head_income<=4600 & year==2017
replace h_price=0.76 if 4600<head_income & head_income<=8800 & year==2017
replace h_price=0.65 if 8800<head_income & head_income<=15000 & year==2017
replace h_price=0.62 if 15000<head_income & head_income<=50000 & year==2017
replace h_price=0.60 if head_income>50000 & year==2017


replace h_price=0.94 if head_income<=1200  & (year==2014 | year== 2015 | year==2016)
replace h_price=0.85 if 1200<head_income & head_income<=4600 & (year==2014 | year== 2015 | year==2016)
replace h_price=0.76 if 4600<head_income & head_income<=8800 & (year==2014 | year== 2015 | year==2016)
replace h_price=0.65 if 8800<head_income & head_income<=15000 & (year==2014 | year== 2015 | year==2016)
replace h_price=0.62 if head_income>15000 & (year==2014 | year== 2015 | year==2016)


replace h_price=0.94 if head_income<=1200  & (year==2012 | year== 2013) 
replace h_price=0.85 if 1200<head_income & head_income<=4600 & (year==2012 | year== 2013)
replace h_price=0.76 if 4600<head_income & head_income<=8800 & (year==2012 | year== 2013)
replace h_price=0.65 if 8800<head_income & head_income<=30000 & (year==2012 | year== 2013)
replace h_price=0.62 if head_income>30000 & (year==2012 | year== 2013)


replace h_price=0.94 if head_income<=1200 & (year==2010 | year== 2011)
replace h_price=0.85 if 1200<head_income & head_income<=4600 & (year==2010 | year== 2011)
replace h_price=0.76 if 4600<head_income & head_income<=8800 & (year==2010 | year== 2011)
replace h_price=0.65 if head_income>8800 & (year==2010 | year== 2011)


replace h_price=0.94 if head_income<=1200 & year==2009
replace h_price=0.84 if 1200<head_income & head_income<=4600 & year==2009 
replace h_price=0.75 if 4600<head_income & head_income<=8800 & year==2009
replace h_price=0.65 if head_income>8800 & year==2009


replace h_price=0.92 if head_income<=1200 & year==2008
replace h_price=0.83 if 1200<head_income & head_income<=4600 & year==2008 
replace h_price=0.74 if 4600<head_income & head_income<=8800 & year==2008
replace h_price=0.65 if head_income>8800 & year==2008


replace h_price=0.92 if head_income<=1000 & year==2007
replace h_price=0.83 if 1000<head_income & head_income<=4000 & year==2007 
replace h_price=0.74 if 4000<head_income & head_income<=8000 & year==2007
replace h_price=0.65 if head_income>8000 & year==2007