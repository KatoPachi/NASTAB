#4TaxWelfare

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

balance_dt <- read.dta13("data/shape/balanceid.dta") %>% data.frame()

merge_df <- left_join(df, balance_dt, by = "pid") %>% 
  mutate(balance3 = ntile(balanceid, 3)) %>%
  mutate(
    log_price_int2 = log_price * (balance3 == 2),
    log_price_int3 = log_price * (balance3 == 3),
  )
  
## ---- HistogramTaxBalanceIndex
merge_df %>% 
  dplyr::select(pid, balanceid) %>% 
  distinct(.keep_all = TRUE) %>% 
  ggplot(aes(x = balanceid)) + 
    geom_histogram(aes(y = ..count..), fill = "grey80", color = "black") +
    labs(x = "Efficient index") +
    my_theme

## ---- DensityTaxBalanceIndex
merge_df %>% 
  dplyr::select(pid, balanceid, ideal_balanceid) %>% 
  distinct(.keep_all = TRUE) %>% 
  ggplot(aes(x = balanceid)) +
    stat_density(aes(linetype = "Full sample"), geom = "line", position = "identity") + 
    stat_density(
      data = ~subset(.x, ideal_balanceid > 0), 
      aes(linetype = "Ideal efficient index > 0"), geom = "line", position = "identity") +
    labs(x = "Efficient index", linetype = "") +
    my_theme

## ---- HeteroModel
xlist_hetero <- list(
  quote(log_price + log_price_int2 + log_price_int3 +  log_pinc_all + age + sqage + 
    factor(year):factor(educ) + factor(year):factor(gender) + factor(year):factor(living_area))
)
fixef_hetero <- list(quote(year + pid))
cluster_hetero <- list(quote(pid))

#tabulation
addline_hetero <- tibble(
  vars = c("Individual FE", "Time FE", "Other controls"),
  stat = "vars",
  reg1 = "Y"
)

## ---- HeteroElasticity
elast_hetero <- est_felm(
  y = list(quote(log_total_g)), x = xlist_hetero,
  fixef = fixef_hetero, cluster = cluster_hetero,
  data = merge_df
)

i_elast_hetero <- est_felm(
  y = list(quote(log_total_g)), x = xlist_hetero,
  fixef = fixef_hetero, cluster = cluster_hetero,
  data = subset(merge_df, i_ext_giving == 1)
)

e_elast_hetero <- est_felm(
  y = list(quote(i_ext_giving)), x = xlist_hetero,
  fixef = fixef_hetero, cluster = cluster_hetero,
  data = merge_df
)

# tabulation
tab.elast_hetero <- fullset_tab(
  elast_hetero, 
  keep_coef = c("log_price", "log_pinc_all"),
  label_coef = list(
    "log_price" = "ln(giving price)", 
    "log_price_int2" = "ln(giving price) x 2Q efficient group", 
    "log_price_int3" = "ln(giving price) x 3Q efficient group", 
    "log_pinc_all" = "ln(annual taxable income)"), 
  keep_stat = c("N", "R-squared"),
  addline = addline_hetero
)

tab.i_elast_hetero <- fullset_tab(
  i_elast_hetero, 
  keep_coef = c("log_price", "log_pinc_all"),
  label_coef = list(
    "log_price" = "ln(giving price)", 
    "log_price_int2" = "ln(giving price) x 2Q efficient group", 
    "log_price_int3" = "ln(giving price) x 3Q efficient group", 
    "log_pinc_all" = "ln(annual taxable income)"), 
  keep_stat = c("N", "R-squared"),
  addline = addline_hetero
)

tab.e_elast_hetero <- fullset_tab(
  e_elast_hetero, 
  keep_coef = c("log_price", "log_pinc_all"),
  label_coef = list(
    "log_price" = "ln(giving price)", 
    "log_price_int2" = "ln(giving price) x 2Q efficient group", 
    "log_price_int3" = "ln(giving price) x 3Q efficient group", 
    "log_pinc_all" = "ln(annual taxable income)"), 
  keep_stat = c("N", "R-squared"),
  addline = addline_hetero
)

newtab.elast_hetero <- full_join(tab.elast_hetero$set, tab.e_elast_hetero$set, by = c("vars", "stat")) %>% 
  full_join(tab.i_elast_hetero$set, by = c("vars", "stat")) %>% 
  dplyr::rename(reg1 = reg1.x, reg2 = reg1.y, reg3 = reg1)

## ---- IdealHeteroElasticity
elast_hetero2 <- est_felm(
  y = list(quote(log_total_g)), x = xlist_hetero,
  fixef = fixef_hetero, cluster = cluster_hetero,
  data = subset(merge_df, ideal_balanceid > 0)
)

i_elast_hetero2 <- est_felm(
  y = list(quote(log_total_g)), x = xlist_hetero,
  fixef = fixef_hetero, cluster = cluster_hetero,
  data = subset(merge_df, i_ext_giving == 1 & ideal_balanceid > 0)
)

e_elast_hetero2 <- est_felm(
  y = list(quote(i_ext_giving)), x = xlist_hetero,
  fixef = fixef_hetero, cluster = cluster_hetero,
  data = subset(merge_df, ideal_balanceid > 0)
)

# tabulation
tab.elast_hetero2 <- fullset_tab(
  elast_hetero2, 
  keep_coef = c("log_price", "log_pinc_all"),
  label_coef = list(
    "log_price" = "ln(giving price)", 
    "log_price_int2" = "ln(giving price) x 2Q efficient group", 
    "log_price_int3" = "ln(giving price) x 3Q efficient group", 
    "log_pinc_all" = "ln(annual taxable income)"), 
  keep_stat = c("N", "R-squared"),
  addline = addline_hetero
)

tab.i_elast_hetero2 <- fullset_tab(
  i_elast_hetero2, 
  keep_coef = c("log_price", "log_pinc_all"),
  label_coef = list(
    "log_price" = "ln(giving price)", 
    "log_price_int2" = "ln(giving price) x 2Q efficient group", 
    "log_price_int3" = "ln(giving price) x 3Q efficient group", 
    "log_pinc_all" = "ln(annual taxable income)"), 
  keep_stat = c("N", "R-squared"),
  addline = addline_hetero
)

tab.e_elast_hetero2 <- fullset_tab(
  e_elast_hetero2, 
  keep_coef = c("log_price", "log_pinc_all"),
  label_coef = list(
    "log_price" = "ln(giving price)", 
    "log_price_int2" = "ln(giving price) x 2Q efficient group", 
    "log_price_int3" = "ln(giving price) x 3Q efficient group", 
    "log_pinc_all" = "ln(annual taxable income)"), 
  keep_stat = c("N", "R-squared"),
  addline = addline_hetero
)

newtab.elast_hetero2 <- full_join(tab.elast_hetero2$set, tab.e_elast_hetero2$set, by = c("vars", "stat")) %>% 
  full_join(tab.i_elast_hetero2$set, by = c("vars", "stat")) %>% 
  dplyr::rename(reg1 = reg1.x, reg2 = reg1.y, reg3 = reg1)
