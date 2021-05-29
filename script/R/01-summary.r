#0summary

## ---- library
library(xfun)
xfun::pkg_attach2(c("readstata13", "tidyverse", "rlist"))
xfun::pkg_attach2(c("plm", "lmtest", "sandwich"))

## --- GGTemp
my_theme <- theme_minimal() +
  theme(
    # setting: background
    plot.background = element_rect(
      #fill="#87CEEB50", 
      color = "transparent"
    ),
    
    # setting: plot
    panel.border = element_rect(color = "white", fill = NA),
    panel.background = element_rect(fill = "white"),   
    panel.grid = element_line(color = "grey80"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # setting: text
    plot.title = element_text(hjust=0.5,size=20),       
    plot.caption = element_text(size=11),       
    
    # setting: axis
    axis.text = element_text(color="black",size=13),    
    axis.title = element_text(size=13),
    axis.ticks.length = unit(0.25, "cm"),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.line = element_line(),
    
    # setting: legend
    legend.title = element_text(size=12),               
    legend.text = element_text(size=12),                
    legend.key.size = unit(0.5,"cm"),
    #legend.background = element_rect(color = "black"), 
    legend.position = "bottom"
  )


## ---- ReadData
df <- read.dta13("data/shaped.dta") %>% 
	data.frame() %>% 
	mutate(
		log_price = log(price),
		log_lprice = log(lprice),
		log_iv1price = log(iv1price),
	    log_iv2price = log(iv2price),
    	log_iv3price = log(iv3price),
		log_total_g = log(i_total_giving + 1),
		log_pinc_all = log(lincome + 100000),
	) %>% 
	group_by(pid) %>% 
	mutate(
		lag1_log_total_g = dplyr::lag(log_total_g, order_by = year),
		lag2_log_total_g = dplyr::lag(log_total_g, order_by = year, n = 2),
		lag3_log_total_g = dplyr::lag(log_total_g, order_by = year, n = 3),
		lag1_log_pinc_all = dplyr::lag(log_pinc_all, order_by = year),
		lag2_log_pinc_all = dplyr::lag(log_pinc_all, order_by = year, n = 2),
		lag3_log_pinc_all = dplyr::lag(log_pinc_all, order_by = year, n = 3),
		lag1_age = dplyr::lag(age, order_by = year),
		lag2_age = dplyr::lag(age, order_by = year, n = 2),
		lag3_age = dplyr::lag(age, order_by = year, n = 3),
		lag1_sqage = dplyr::lag(sqage, order_by = year),
		lag2_sqage = dplyr::lag(sqage, order_by = year, n = 2),
		lag3_sqage = dplyr::lag(sqage, order_by = year, n = 3)
	) %>% 
	ungroup() %>% 
	mutate(
		log_diff1g = log_total_g - lag1_log_total_g,
		log_diff2g = log_total_g - lag2_log_total_g,
		log_diff3g = log_total_g - lag3_log_total_g,
		log_diff1I = log_pinc_all - lag1_log_pinc_all,
		log_diff2I = log_pinc_all - lag2_log_pinc_all,
		log_diff3I = log_pinc_all - lag3_log_pinc_all,
		diff1_age = age - lag1_age,
		diff2_age = age - lag2_age,
		diff3_age = age - lag3_age,
		diff1_sqage = sqage - lag1_sqage,
		diff2_sqage = sqage - lag2_sqage,
		diff3_sqage = sqage - lag3_sqage
	) %>% 
	filter(year >= 2012 & age >= 24) 

## ---- SummaryOutcome
avgext <- df %>% 
	group_by(year) %>% 
	summarize_at(vars(i_ext_giving), list(mu=~mean(., na.rm = TRUE))) 

avgint <- df %>% 
	filter(i_ext_giving == 1) %>% 
	group_by(year) %>% 
	summarize_at(vars(i_total_giving), list(mu=~mean(., na.rm = TRUE)))

ggplot(avgext, aes(x = year, y = mu)) +
	geom_bar(aes(fill = "Extensive margin"), stat = "identity", color = "black") +
	geom_point(data = avgint, aes(x = year, y = mu/1000, color = "Intensive margin"), size = 2) +
	geom_line(data = avgint, aes(x = year, y = mu/1000, color = "Intensive margin"), size = 1) +
	geom_hline(aes(yintercept = 0)) +
	geom_vline(aes(xintercept = 2013.5), color = "red", linetype = 2, size = 1) +
	scale_fill_manual(NULL, values = "grey80") +
	scale_color_manual(NULL, values = "blue") +
	scale_y_continuous(sec.axis = sec_axis(~.*1000, name = "Average Amout of Donations among Donors (Intensive margin)")) +
	scale_x_continuous(breaks = seq(2012, 2018, 1)) +
	labs(x = "Year", y = "Proportion of Donors (Extensive margin)") +
	my_theme

## ---- SummaryCovariate
sumcov <- df %>% 
	summarize_at(
		vars(lincome, price, i_total_giving, i_ext_giving, now_balance, ideal_balance, age, gender, univ, highschool, juniorhigh),
		list(
			N =~ sum(!is.na(.)),
			mean =~ mean(., na.rm = TRUE),
			sd =~ sd(., na.rm = TRUE),
			min =~ min(., na.rm = TRUE),
			q25 = ~quantile(., prob = .25, na.rm = TRUE),
			median = ~median(., na.rm = TRUE),
			q75 = ~quantile(., prob = .75, na.rm = TRUE),
			max =~ max(., na.rm = TRUE)
		)
	) %>% 
	pivot_longer(
		everything(),
		names_to = c("vars", ".value"),
		names_pattern = "(.*)_(mean|se|sd|min|q25|median|q75|max|sum|N)"
	)

## ---- SummaryPriceChange
df %>% 
	filter(year == 2013) %>% 
	dplyr::select(lincome, price) %>% 
	ggplot(aes(x = lincome)) +
		geom_histogram(aes(y = ..count../sum(..count..), fill = "Relative frequency"), color = "black") +
		geom_step(aes(y = price*0.5, color = "Giving Price in 2013"), size = 1) +
		geom_hline(aes(yintercept = (1 - 0.15)*0.5), color = "red", linetype = 2, size = 1) +
		scale_color_manual(NULL, values = "blue") +
		scale_fill_manual(NULL, values = "grey80") +
		scale_y_continuous(breaks = seq(0, 0.5, 0.125), sec.axis = sec_axis(~./0.5, name = "Giving price")) +
		scale_x_continuous(breaks = c(1200, 4600, 8800, 30000)) +
		labs(x = "Annual taxable income (10,000KRW)", y = "Relative frequency") +
		my_theme