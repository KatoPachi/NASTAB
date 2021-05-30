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

# wald test 
wald <- list(
  "Implied price elasticity" = "imp * log_price",
  "Implied income elasticity" = "imp * log_pinc_all"
)

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
  y = list(quote(log_total_g)), x = xlist, 
  fixef = fixef, cluster = cluster, 
  data = df
)

# tabulation
tab.elast <- fullset_tab(
  elast$result, 
  keep_coef = c("log_price", "log_pinc_all"),
  label_coef = list("log_price" = "ln(giving price)", "log_pinc_all" = "ln(annual taxable income)"), 
  keep_stat = c("N", "R-squared"), 
  addlines = addline
)

## ---- ExtElasticity
# regressions
e_elast <- est_felm(
  y = list(quote(i_ext_giving)), x = xlist, 
  fixef = fixef, cluster = cluster, 
  wald_hypo = wald,
  data = df
)

# tabulation
tab.e_elast <- fullset_tab(
  e_elast$result, e_elast$test,
  keep_coef = c("log_price", "log_pinc_all"),
  label_coef = list("log_price" = "ln(giving price)", "log_pinc_all" = "ln(annual taxable income)"), 
  keep_stat = c("N", "R-squared"), 
  addlines = addline
)

## ---- IntElasticity
# regressions
i_elast <- est_felm(
  y = list(quote(log_total_g)), x = xlist, 
  fixef = fixef, cluster = cluster, 
  data = subset(df, i_ext_giving == 1)
)

# tabulation
tab.i_elast <- fullset_tab(
  i_elast$result,
  keep_coef = c("log_price", "log_pinc_all"),
  label_coef = list("log_price" = "ln(giving price)", "log_pinc_all" = "ln(annual taxable income)"), 
  keep_stat = c("N", "R-squared"), 
  addlines = addline
)

## ---- LastElasticityModel
# regeression model
xlist_rob1 <- list(
  quote(log_pinc_all),
  quote(log_pinc_all + age + sqage),
  quote(log_pinc_all + age + sqage + factor(year):factor(educ)),
  quote(log_pinc_all + age + sqage + factor(year):factor(educ) + factor(year):factor(gender)),
  quote(log_pinc_all + age + sqage + factor(year):factor(educ) + factor(year):factor(gender) + factor(year):factor(living_area)) 
)

z_rob1 <- list(quote((log_lprice ~ log_price)))
fixef_rob1 <- list(quote(year + pid))
cluster_rob1 <- list(quote(pid))

# wald test 
wald_rob1 <- list(
  "Implied price elasticity" = "imp * `log_lprice(fit)`",
  "Implied income elasticity" = "imp * log_pinc_all"
)

# tabulation
addline_rob1 <- tribble(
  ~vars, ~stat, ~reg1, ~reg2, ~reg3, ~reg4, ~reg5,
  "Individual FE", "vars", "Y", "Y", "Y", "Y", "Y",
  "Time FE", "vars", "Y", "Y", "Y", "Y", "Y",
  "Age", "vars", "N", "Y", "Y", "Y", "Y",
  "Year x Education", "vars", "N", "N", "Y", "Y", "Y", 
  "Year x Gender", "vars", "N", "N", "N", "Y", "Y", 
  "Year x Resident Area", "vars", "N", "N", "N", "N", "Y" 
)

## ---- LastElasticity
elast_rob1 <- est_felm(
  y = list(quote(log_total_g)), x = xlist_rob1, z = z_rob1,
  fixef = fixef_rob1, cluster = cluster_rob1,
  data = df
)

# tabulation
# f-stat (first stage)
fstat <- elast_rob1$result %>% purrr::map(~.$stage1$iv1fstat$log_lprice[["F"]]) %>% as_vector()
fstat_line <- c(vars = "F-statistics of IV", stat = "stat", fstat)

tab.elast_rob1 <- fullset_tab(
  elast_rob1$result, 
  keep_coef = c("log_lprice", "log_pinc_all"),
  label_coef = list("`log_lprice(fit)`" = "ln(giving price)", "log_pinc_all" = "ln(annual taxable income)"), 
  keep_stat = c("N"), 
  addlines = addline
)

newtab.elast_rob1 <- bind_rows(
  tab.elast_rob1$set[1:10,],
  fstat_line,
  tab.elast_rob1$set[11,]
)

## ---- LastExtElasticity
e_elast_rob1 <- est_felm(
  y = list(quote(i_ext_giving)), x = xlist_rob1, z = z_rob1,
  fixef = fixef, cluster = cluster,
  wald_hypo = wald_rob1,
  data = df
)

# tabulation
# f-stat (first stage)
e_fstat <- e_elast_rob1$est %>% purrr::map(~.$stage1$iv1fstat$log_lprice[["F"]]) %>% as_vector()
e_fstat_line <- c(vars = "F-statistics of IV", stat = "stat", fstat)

tab.e_elast_rob1 <- fullset_tab(
  e_elast_rob1$result, e_elast_rob1$test, 
  keep_coef = c("log_lprice", "log_pinc_all"),
  label_coef = list("`log_lprice(fit)`" = "ln(giving price)", "log_pinc_all" = "ln(annual taxable income)"), 
  keep_stat = c("N"), 
  addlines = addline
)

newtab.e_elast_rob1 <- bind_rows(
  tab.e_elast_rob1$set[1:10,],
  e_fstat_line,
  tab.e_elast_rob1$set[11,]
)

## ---- LastIntElasticity
i_elast_rob1 <- est_felm(
  y = list(quote(log_total_g)), x = xlist_rob1, z = z_rob1,
  fixef = fixef_rob1, cluster = cluster_rob1,
  data = subset(df, i_ext_giving == 1)
)

# tabulation
# f-stat (first stage)
i_fstat <- e_elast_rob1$est %>% purrr::map(~.$stage1$iv1fstat$log_lprice[["F"]]) %>% as_vector()
i_fstat_line <- c(vars = "F-statistics of IV", stat = "stat", fstat)

tab.i_elast_rob1 <- fullset_tab(
  i_elast_rob1$result, 
  keep_coef = c("log_lprice", "log_pinc_all"),
  label_coef = list("`log_lprice(fit)`" = "ln(giving price)", "log_pinc_all" = "ln(annual taxable income)"), 
  keep_stat = c("N"), 
  addlines = addline
)

newtab.i_elast_rob1 <- bind_rows(
  tab.i_elast_rob1$set[1:10,],
  i_fstat_line,
  tab.i_elast_rob1$set[11,]
)

## ---- ShortModel
# regression model
xlist_rob2 <- list(
  quote(log_price + log_pinc_all),
  quote(log_price + log_pinc_all + age + sqage + factor(year):factor(educ) + factor(year):factor(gender) + 
    factor(year):factor(living_area))
)

fixef_rob2 <- list(quote(year + pid))
cluster_rob2 <- list(quote(pid))

# wald test 
wald_rob2 <- list(
  "Implied price elasticity" = "imp * log_price",
  "Implied income elasticity" = "imp * log_pinc_all"
)

# tabulation
addline_rob2 <- tribble(
  ~vars, ~stat, ~reg1, ~reg2,
  "Individual FE", "vars", "Y", "Y",
  "Time FE", "vars", "Y", "Y", 
  "Other Controls", "vars", "N", "Y", 
)

## ---- ShortElasticity
elast_rob2_1 <- est_felm(
  y = list(quote(log_total_g)), x = xlist_rob2,
  fixef = fixef_rob2, cluster = cluster_rob2,
  data = subset(df, year >= 2013) 
)

elast_rob2_2 <- est_felm(
  y = list(quote(log_total_g)), x = xlist_rob2,
  fixef = fixef_rob2, cluster = cluster_rob2,
  data = subset(df, year == 2013 | year == 2014) 
)

# tabulation
tab.elast_rob2_1 <- fullset_tab(
  elast_rob2_1$result, 
  keep_coef = c("log_price", "log_pinc_all"),
  label_coef = list("log_price" = "ln(giving price)", "log_pinc_all" = "ln(annual taxable income)"), 
  keep_stat = c("N", "R-squared"),
  addline = addline_rob2
)

tab.elast_rob2_2 <- fullset_tab(
  elast_rob2_2$result, 
  keep_coef = c("log_price", "log_pinc_all"),
  label_coef = list("log_price" = "ln(giving price)", "log_pinc_all" = "ln(annual taxable income)"), 
  keep_stat = c("N", "R-squared"),
  addline = addline_rob2
)

tab.elast_rob2 <- full_join(tab.elast_rob2_1$set, tab.elast_rob2_2$set, by = c("vars", "stat")) %>% 
  dplyr::rename(reg1 = reg1.x, reg2 = reg2.x, reg3 = reg1.y, reg4 = reg2.y) 

## ---- ShortIntElasticity
i_elast_rob2_1 <- est_felm(
  y = list(quote(log_total_g)), x = xlist_rob2,
  fixef = fixef_rob2, cluster = cluster_rob2,
  data = subset(df, year >= 2013 & i_ext_giving == 1) 
)

i_elast_rob2_2 <- est_felm(
  y = list(quote(log_total_g)), x = xlist_rob2,
  fixef = fixef_rob2, cluster = cluster_rob2,
  data = subset(df, (year == 2013 | year == 2014) & i_ext_giving == 1) 
)

# tabulation
tab.i_elast_rob2_1 <- fullset_tab(
  i_elast_rob2_1$result, 
  keep_coef = c("log_price", "log_pinc_all"),
  label_coef = list("log_price" = "ln(giving price)", "log_pinc_all" = "ln(annual taxable income)"), 
  keep_stat = c("N", "R-squared"),
  addline = addline_rob2
)

tab.i_elast_rob2_2 <- fullset_tab(
  i_elast_rob2_2$result, 
  keep_coef = c("log_price", "log_pinc_all"),
  label_coef = list("log_price" = "ln(giving price)", "log_pinc_all" = "ln(annual taxable income)"), 
  keep_stat = c("N", "R-squared"),
  addline = addline_rob2
)

tab.i_elast_rob2 <- full_join(tab.i_elast_rob2_1$set, tab.i_elast_rob2_2$set, by = c("vars", "stat")) %>% 
  dplyr::rename(reg1 = reg1.x, reg2 = reg2.x, reg3 = reg1.y, reg4 = reg2.y) 

## ---- ShortExtElasticity
e_elast_rob2_1 <- est_felm(
  y = list(quote(i_ext_giving)), x = xlist_rob2,
  fixef = fixef_rob2, cluster = cluster_rob2,
  wald_hypo = wald_rob2,
  data = subset(df, year >= 2013) 
)

e_elast_rob2_2 <- est_felm(
  y = list(quote(i_ext_giving)), x = xlist_rob2,
  fixef = fixef_rob2, cluster = cluster_rob2,
  wald_hypo = wald_rob2,
  data = subset(df, year == 2013 | year == 2014) 
)

# tabulation
tab.e_elast_rob2_1 <- fullset_tab(
  e_elast_rob2_1$result, e_elast_rob2_1$test, 
  keep_coef = c("log_price", "log_pinc_all"),
  label_coef = list("log_price" = "ln(giving price)", "log_pinc_all" = "ln(annual taxable income)"), 
  keep_stat = c("N", "R-squared"),
  addline = addline_rob2
)

tab.e_elast_rob2_2 <- fullset_tab(
  e_elast_rob2_2$result, e_elast_rob2_2$test, 
  keep_coef = c("log_price", "log_pinc_all"),
  label_coef = list("log_price" = "ln(giving price)", "log_pinc_all" = "ln(annual taxable income)"), 
  keep_stat = c("N", "R-squared"),
  addline = addline_rob2
)

tab.e_elast_rob2 <- full_join(tab.e_elast_rob2_1$set, tab.e_elast_rob2_2$set, by = c("vars", "stat")) %>% 
  dplyr::rename(reg1 = reg1.x, reg2 = reg2.x, reg3 = reg1.y, reg4 = reg2.y) 

## ---- kdiffModel
ylist_rob3 <- list(quote(log_diff1g), quote(log_diff2g), quote(log_diff3g))
xlist_rob3 <- list(
  quote(log_iv1price +  log_diff1I + diff1_age + diff1_sqage + factor(year):factor(educ) + factor(year):factor(gender) + 
    factor(year):factor(living_area)),
  quote(log_iv2price +  log_diff2I + diff2_age + diff2_sqage + factor(year):factor(educ) + factor(year):factor(gender) + 
    factor(year):factor(living_area)),
  quote(log_iv3price +  log_diff3I + diff3_age + diff3_sqage + factor(year):factor(educ) + factor(year):factor(gender) + 
    factor(year):factor(living_area))
)
fixef_rob3 <- list(quote(year + pid))
cluster_rob3 <- list(quote(pid))

#tabulation
addline_rob3 <- tibble(
  vars = c("Individual FE", "Time FE", "Other controls"),
  stat = rep("vars", 3),
  reg1 = rep("Y", 3), reg2 = rep("Y", 3), reg3 = rep("Y", 3),
  reg4 = rep("Y", 3), reg5 = rep("Y", 3), reg6 = rep("Y", 3),
  reg7 = rep("Y", 3), reg8 = rep("Y", 3), reg9 = rep("Y", 3)
)

## ---- kDiffElasticity
elast_rob3 <- est_felm(
  y = ylist_rob3, x = xlist_rob3, 
  fixef = fixef_rob3, cluster = cluster_rob3,
  data = df
)

tab.elast_rob3 <- fullset_tab(
  elast_rob3$result,
  keep_coef = c("log_iv", "log_diff"),
  keep_stat = c("N", "R-squared"),
  addline = addline_rob3
)

newtab.elast_rob3 <- tab.elast_rob3$set %>% 
  dplyr::select(vars, stat, reg1, reg5, reg9) %>% 
  pivot_longer(reg1:reg9, names_to = "model", values_to = "val") %>% 
  mutate(
    vars = case_when(
      str_detect(vars, "log_iv") ~ "Lagged difference of first price (log)",
      str_detect(vars, "log_diff") ~ "Lagged difference of annual income (log)",
      TRUE ~ vars
    )
  ) %>% 
  dplyr::filter(!is.na(val)) %>% 
  pivot_wider(names_from = "model", values_from = "val")

## ---- kDiffIntElasticity
i_elast_rob3 <- est_felm(
  y = ylist_rob3, x = xlist_rob3, 
  fixef = fixef_rob3, cluster = cluster_rob3,
  data = subset(df, i_ext_giving == 1)
)

tab.i_elast_rob3 <- fullset_tab(
  i_elast_rob3$result,
  keep_coef = c("log_iv", "log_diff"),
  keep_stat = c("N", "R-squared"),
  addline = addline_rob3
)

newtab.i_elast_rob3 <- tab.i_elast_rob3$set %>% 
  dplyr::select(vars, stat, reg1, reg5, reg9) %>% 
  pivot_longer(reg1:reg9, names_to = "model", values_to = "val") %>% 
  mutate(
    vars = case_when(
      str_detect(vars, "log_iv") ~ "Lagged difference of first price (log)",
      str_detect(vars, "log_diff") ~ "Lagged difference of annual income (log)",
      TRUE ~ vars
    )
  ) %>% 
  dplyr::filter(!is.na(val)) %>% 
  pivot_wider(names_from = "model", values_from = "val")
