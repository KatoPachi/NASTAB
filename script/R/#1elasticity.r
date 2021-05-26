#1elasticity

## ---- library
library(xfun)
xfun::pkg_attach2(c("readstata13", "tidyverse", "rlist"))
xfun::pkg_attach2(c("plm", "lmtest", "sandwich", "lfe", "Formula"))

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

## ---- TotalElasticity
# regressions
reg <- Formula(log_total_g ~ log_price + log_pinc_all | pid + year)

setreg <- list(
  base = . ~ . | . | 0 | pid,
  age = . ~ . + age + sqage | . | 0 | pid,
  educ = . ~ .  + age + sqage + factor(year):factor(educ) | . | 0 | pid,
  gender = . ~ .  + age + sqage + factor(year):factor(educ) + factor(year):factor(gender) | . | 0 | pid,
  living = . ~ . + age + sqage + factor(year):factor(educ) + factor(year):factor(gender) + 
    factor(year):factor(living_area) | . | 0 | pid
)

elast <- setreg %>% 
  purrr::map(~as.formula(update(reg, .))) %>%
  purrr::map(~lfe::felm(as.formula(.), data = df))

# tabulation
basetab <- felm_regtab(elast, keep_coef = c("log_price", "log_pinc_all"))

addline <- tribble(
  ~vars, ~stat, ~reg1, ~reg2, ~reg3, ~reg4, ~reg5,
  "Individual FE", "vars", "Y", "Y", "Y", "Y", "Y",
  "Time FE", "vars", "Y", "Y", "Y", "Y", "Y",
  "Age", "vars", "N", "Y", "Y", "Y", "Y",
  "Year x Education", "vars", "N", "N", "Y", "Y", "Y", 
  "Year x Gender", "vars", "N", "N", "N", "Y", "Y", 
  "Year x Resident Area", "vars", "N", "N", "N", "N", "Y" 
)

tab.elast <- bind_rows(
  basetab$coef,
  addline,
  basetab$stat
)

## ---- ExtElasticity
#regressions
reg <- Formula(i_ext_giving ~ log_price + log_pinc_all | pid + year)

setreg <- list(
  base = . ~ . | . | 0 | pid,
  age = . ~ . + age + sqage | . | 0 | pid,
  educ = . ~ .  + age + sqage + factor(year):factor(educ) | . | 0 | pid,
  gender = . ~ .  + age + sqage + factor(year):factor(educ) + factor(year):factor(gender) | . | 0 | pid,
  living = . ~ . + age + sqage + factor(year):factor(educ) + factor(year):factor(gender) + 
    factor(year):factor(living_area) | . | 0 | pid
)

ext_elast <- setreg %>% 
  purrr::map(~as.formula(update(reg, .))) %>%
  purrr::map(~lfe::felm(as.formula(.), data = df))

# implied elasticity
dbar <- 1/mean(df$i_ext_giving, na.rm = TRUE)

imp_ext_elast_p <- ext_elast %>% 
  purrr::map(~list(model = ., rhs = matrix(c(dbar, numeric(length(coef(.)) - 1)), nrow = 1))) %>% 
  purrr::map(~list(coef =  .$rhs[1] * coef(.$model)["log_price"], test = lfe::waldtest(.$model, .$rhs))) %>% 
  purrr::map(function(x)
    tibble(
      vars = "Implied price elasticity",
      coef = x$coef,
      se = abs(x$coef)/sqrt(x$test["F"]),
      p = x$test["p.F"]
    ) %>% 
    mutate(
			coef = case_when(
				p <= .01 ~ sprintf("%1.3f***", coef),
				p <= .05 ~ sprintf("%1.3f**", coef),
				p <= .1 ~ sprintf("%1.3f*", coef),
				TRUE ~ sprintf("%1.3f", coef)
			),
			se = sprintf("(%1.3f)", se)
		) %>% 
		dplyr::select(-p) %>% 
		pivot_longer(-vars, names_to = "stat", values_to = "val")
	) %>% 
	reduce(full_join, by = c("vars", "stat")) %>%
	setNames(c("vars", "stat", paste("reg", 1:length(ext_elast), sep = "")))

imp_ext_elast_y <- ext_elast %>% 
  purrr::map(~list(model = ., rhs = matrix(c(0, dbar, numeric(length(coef(.)) - 2)), nrow = 1))) %>% 
  purrr::map(~list(coef =  .$rhs[2] * coef(.$model)["log_pinc_all"], test = lfe::waldtest(.$model, .$rhs))) %>% 
  purrr::map(function(x)
    tibble(
      vars = "Implied income elasticity",
      coef = x$coef,
      se = abs(x$coef)/sqrt(x$test["F"]),
      p = x$test["p.F"]
    ) %>% 
    mutate(
			coef = case_when(
				p <= .01 ~ sprintf("%1.3f***", coef),
				p <= .05 ~ sprintf("%1.3f**", coef),
				p <= .1 ~ sprintf("%1.3f*", coef),
				TRUE ~ sprintf("%1.3f", coef)
			),
			se = sprintf("(%1.3f)", se)
		) %>% 
		dplyr::select(-p) %>% 
		pivot_longer(-vars, names_to = "stat", values_to = "val")
	) %>% 
	reduce(full_join, by = c("vars", "stat")) %>%
	setNames(c("vars", "stat", paste("reg", 1:length(ext_elast), sep = "")))

# tabulation
basetab <- felm_regtab(ext_elast, keep_coef = c("log_price", "log_pinc_all"))

addline <- tribble(
  ~vars, ~stat, ~reg1, ~reg2, ~reg3, ~reg4, ~reg5,
  "Individual FE", "vars", "Y", "Y", "Y", "Y", "Y",
  "Time FE", "vars", "Y", "Y", "Y", "Y", "Y",
  "Age", "vars", "N", "Y", "Y", "Y", "Y",
  "Year x Education", "vars", "N", "N", "Y", "Y", "Y", 
  "Year x Gender", "vars", "N", "N", "N", "Y", "Y", 
  "Year x Resident Area", "vars", "N", "N", "N", "N", "Y" 
)

tab.ext_elast <- bind_rows(
  basetab$coef,
  imp_ext_elast_p,
  imp_ext_elast_y,
  addline,
  basetab$stat
)

## ---- IntElasticity
# regressions
reg <- Formula(log_total_g ~ log_price + log_pinc_all | pid + year)

setreg <- list(
  base = . ~ . | . | 0 | pid,
  age = . ~ . + age + sqage | . | 0 | pid,
  educ = . ~ .  + age + sqage + factor(year):factor(educ) | . | 0 | pid,
  gender = . ~ .  + age + sqage + factor(year):factor(educ) + factor(year):factor(gender) | . | 0 | pid,
  living = . ~ . + age + sqage + factor(year):factor(educ) + factor(year):factor(gender) + 
    factor(year):factor(living_area) | . | 0 | pid
)

int_elast <- setreg %>% 
  purrr::map(~as.formula(update(reg, .))) %>%
  purrr::map(~lfe::felm(as.formula(.), data = subset(df, i_ext_giving == 1)))

# tabulation
basetab <- felm_regtab(int_elast, keep_coef = c("log_price", "log_pinc_all"))

addline <- tribble(
  ~vars, ~stat, ~reg1, ~reg2, ~reg3, ~reg4, ~reg5,
  "Individual FE", "vars", "Y", "Y", "Y", "Y", "Y",
  "Time FE", "vars", "Y", "Y", "Y", "Y", "Y",
  "Age", "vars", "N", "Y", "Y", "Y", "Y",
  "Year x Education", "vars", "N", "N", "Y", "Y", "Y", 
  "Year x Gender", "vars", "N", "N", "N", "Y", "Y", 
  "Year x Resident Area", "vars", "N", "N", "N", "N", "Y" 
)

tab.int_elast <- bind_rows(
  basetab$coef,
  addline,
  basetab$stat
)
