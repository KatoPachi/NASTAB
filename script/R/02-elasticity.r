#1elasticity

## ---- library
library(xfun)
xfun::pkg_attach2(c("readstata13", "tidyverse", "rlist"))
xfun::pkg_attach2(c("plm", "lmtest", "sandwich", "lfe", "Formula"))
source("script/R/00-analysis_functions.r")

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

## ---- BaselineModel
# regression model
xlist <- list(
  quote(log_price + log_pinc_all),
  quote(log_price + log_pinc_all + age + sqage),
  quote(log_price + log_pinc_all + age + sqage + factor(year):factor(educ)),
  quote(log_price + log_pinc_all + age + sqage + factor(year):factor(educ) + factor(year):factor(gender)),
  quote(log_price + log_pinc_all + age + sqage + factor(year):factor(educ) + factor(year):factor(gender) + 
    factor(year):factor(living_area))
)
fixef <- list(quote(year + pid))
cluster <- list(quote(pid))

# tabulation
addline <- tribble(
  ~vars, ~stat, ~reg1, ~reg2, ~reg3, ~reg4, ~reg5,
  "Individual FE", "vars", "Y", "Y", "Y", "Y", "Y",
  "Time FE", "vars", "Y", "Y", "Y", "Y", "Y",
  "Age", "vars", "N", "Y", "Y", "Y", "Y",
  "Year x Education", "vars", "N", "N", "Y", "Y", "Y", 
  "Year x Gender", "vars", "N", "N", "N", "Y", "Y", 
  "Year x Resident Area", "vars", "N", "N", "N", "N", "Y" 
)

## ---- TotalElasticity
# regressions
elast <- est_felm(
  y = list(quote(log_total_g)), 
  x = xlist, 
  fixef = fixef, 
  cluster = cluster, 
  data = df
)

# tabulation
tab.elast <- fullset_tab(
  elast, 
  keep_coef = c("log_price", "log_pinc_all"),
  label_coef = list("log_price" = "ln(giving price)", "log_pinc_all" = "ln(annual taxable income)"), 
  keep_stat = c("N", "R-squared"), 
  addlines = addline
)

## ---- ExtElasticity
# regressions
ext_elast <- est_felm(
  y = list(quote(i_ext_giving)), 
  x = xlist, 
  fixef = fixef, 
  cluster = cluster, 
  data = df, 
  implied_e = TRUE
)

# tabulation
tab.ext_elast <- fullset_tab(
  ext_elast,
  keep_coef = c("log_price", "log_pinc_all"),
  label_coef = list("log_price" = "ln(giving price)", "log_pinc_all" = "ln(annual taxable income)"), 
  keep_stat = c("N", "R-squared"), 
  addlines = addline
)

## ---- IntElasticity
# regressions
int_elast <- est_felm(
  y = list(quote(log_total_g)), 
  x = xlist, 
  fixef = fixef, 
  cluster = cluster, 
  data = subset(df, i_ext_giving == 1)
)

# tabulation
tab.int_elast <- fullset_tab(
  int_elast,
  keep_coef = c("log_price", "log_pinc_all"),
  label_coef = list("log_price" = "ln(giving price)", "log_pinc_all" = "ln(annual taxable income)"), 
  keep_stat = c("N", "R-squared"), 
  addlines = addline
)
