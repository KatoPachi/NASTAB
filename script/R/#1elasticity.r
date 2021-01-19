#1elasticity

## ---- library
package_load <- function(pkg.name){
  if (!require(pkg.name, character.only=TRUE)){
    install.packages(pkg.name)
  } else {
    library(pkg.name, character.only = TRUE)
  }
}

package_load("readstata13")  
package_load("tidyverse")
package_load("rlist")
package_load("rlang")
package_load("plm")
package_load("lmtest")
package_load("sandwich")
package_load("lfe")
package_load("Formula")

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
    legend.background = element_rect(color = "black"), 
    legend.position = "bottom"
  )


## ---- ReadDataforElasticity
df <- read.dta13("data/shaped.dta")
df <- data.frame(df) %>% 
    mutate(
        price = case_when(year < 2014 ~ 1 - mtr, TRUE ~ 1 -0.15),
        log_price = log(price),
        log_total_g = log(i_total_giving + 1),
        log_pinc_all = log(lincome + 100000),
        log_PPP_pubbdg = log(PPP_pubbdg + 1),
        sqlog_PPP_pubbdg = log_PPP_pubbdg^2,
        log_PPP_healthbdg = log(PPP_healthbdg + 1),
        sqlog_PPP_healthbdg = log_PPP_healthbdg^2,
        political_pref = factor(political_pref, level = c(3, 1, 2, 4, 5))
    ) 
df <- df %>% 
  group_by(pid) %>% 
  mutate(
    lag1_price = dplyr::lag(price, n = 1, default = NA, order_by = year),
    lag2_price = dplyr::lag(price, n = 2, default = NA, order_by = year),
    lag3_price = dplyr::lag(price, n = 3, default = NA, order_by = year),
    lag4_price = dplyr::lag(price, n = 4, default = NA, order_by = year)
  ) %>% 
  ungroup()
df <- df %>% 
  mutate(
    lag1iv = log(price/lag1_price),
    lag2iv = log(price/lag2_price),
    lag3iv = log(price/lag3_price),
    lag4iv = log(price/lag4_price),
  )


## ---- EstimateElasticity
#regressions
reg <- Formula(log_total_g ~ log_price + log_pinc_all | pid + year)

setreg <- list(
    base = . ~ . | . | 0 | pid,
    age = . ~ . + age | . | 0 | pid,
    educ = . ~ .  + age + factor(year):factor(educ) | . | 0 | pid,
    gender = . ~ .  + age + factor(year):factor(educ) + factor(year):factor(gender) | . | 0 | pid,
    living = . ~ . +age+factor(year):factor(educ)+factor(year):factor(gender)+factor(living_area) | . | 0 | pid
)

basereg <- setreg %>% 
    purrr::map(~as.formula(update(reg, .))) %>%
    purrr::map(~lfe::felm(as.formula(.), data = subset(df, year >= 2012)))
n.basereg <- basereg %>% purrr::map(~sprintf("%1d", nobs(.))) %>% as_vector()

# make tabulation
keep <- c("log_price") %>% paste(collapse = "|")
varlist <- exprs(
  stat == "se" ~ "",
  vars == "log_price" ~ "ln(giving price)"
)

coef.basereg <- basereg %>% 
	purrr::map(function(x)
    data.frame(
      vars = rownames(summary(x)$coefficients),
      coef = apply(matrix(summary(x)$coefficients[,4], ncol = 1), MARGIN = 2,
        FUN = function(y) case_when(
          y <= .01 ~ sprintf("%1.3f***", summary(x)$coefficients[,1]),
          y <= .05 ~ sprintf("%1.3f**", summary(x)$coefficients[,1]),
          y <= .1 ~ sprintf("%1.3f*", summary(x)$coefficients[,1]),
          TRUE ~ sprintf("%1.3f", summary(x)$coefficients[,1])
        )
      ),
      se = sprintf("(%1.3f)", summary(x)$coefficients[,2]),
      stringsAsFactors = FALSE
    )
  ) %>% 
  purrr::map(function(x) x[str_detect(x$vars, keep),]) %>%
  purrr::map(function(x) pivot_longer(x, -vars, names_to = "stat", values_to = "val")) %>% 
	purrr::reduce(full_join, by = c("vars", "stat")) %>% 
	mutate(vars = case_when(!!!varlist)) %>% 
  select(-stat)

addline <- rbind(
  c("Logarithm of income", "Y", "Y", "Y", "Y", "Y"),
  c("Age", "N", "Y", "Y", "Y", "Y"),
  c("Year X Educ", "N", "N", "Y", "Y", "Y"),
  c("Year X Gender", "N", "N", "N", "Y", "Y"),
  c("Living Dummy", "N", "N", "N", "N", "Y"),
  c("Obs", n.basereg)
)

tab.basereg <- rbind(as.matrix(coef.basereg), addline) %>% data.frame()

## ---- Robust1EstimateElasticity
# regressions
reg <- Formula(
  log_total_g ~ log_pinc_all +age+factor(year):factor(educ)+factor(year):factor(gender)+factor(living_area)|
  pid + year
)

setreg <- list(
    lag1 = . ~ .  | . | (log_price ~ lag1iv) | pid,
    lag2 = . ~ .  | . | (log_price ~ lag2iv) | pid,
    lag3 = . ~ .  | . | (log_price ~ lag3iv) | pid,
    lag4 = . ~ .  | . | (log_price ~ lag4iv) | pid
)

pivreg <- setreg %>% 
    purrr::map(~as.formula(update(reg, .))) %>%
    purrr::map(~lfe::felm(as.formula(.), data = subset(df, year >= 2012)))

n.pivreg <- pivreg %>% purrr::map(~sprintf("%1d", nobs(.))) %>% as_vector()
f.pivreg <- pivreg %>% purrr::map(~sprintf("%1.3f", .$stage1$iv1fstat[[1]]["F"])) %>% as_vector()

# tabulation
keep <- c("log_price") %>% paste(collapse = "|")
varlist <- exprs(
  stat == "se" ~ "",
  str_detect(vars, "log_price") ~ "ln(giving price)"
)

coef.pivreg <- pivreg %>% 
	purrr::map(function(x)
    data.frame(
      vars = rownames(summary(x)$coefficients),
      coef = apply(matrix(summary(x)$coefficients[,4], ncol = 1), MARGIN = 2,
        FUN = function(y) case_when(
          y <= .01 ~ sprintf("%1.3f***", summary(x)$coefficients[,1]),
          y <= .05 ~ sprintf("%1.3f**", summary(x)$coefficients[,1]),
          y <= .1 ~ sprintf("%1.3f*", summary(x)$coefficients[,1]),
          TRUE ~ sprintf("%1.3f", summary(x)$coefficients[,1])
        )
      ),
      se = sprintf("(%1.3f)", summary(x)$coefficients[,2]),
      stringsAsFactors = FALSE
    )
  ) %>% 
  purrr::map(function(x) x[str_detect(x$vars, keep),]) %>%
  purrr::map(function(x) pivot_longer(x, -vars, names_to = "stat", values_to = "val")) %>% 
	purrr::reduce(full_join, by = c("vars", "stat")) %>% 
	mutate(vars = case_when(!!!varlist)) %>% 
  select(-stat)

addline <- rbind(
  c("F-stat of IV", f.pivreg),
  c("Obs", n.pivreg)
)

tab.pivreg <- rbind(as.matrix(coef.pivreg), addline) %>% data.frame()

## ---- Robust2EstimateElasticity
# regressions
reg <- Formula(
  log_total_g ~ log_pinc_all +age+factor(year):factor(educ)+factor(year):factor(gender)+factor(living_area)|
  pid + year
)

setreg <- list(
    base = . ~ . + log_price | . | 0 | pid,
    lag1 = . ~ .  | . | (log_price ~ lag1iv) | pid,
    lag2 = . ~ .  | . | (log_price ~ lag2iv) | pid,
    lag3 = . ~ .  | . | (log_price ~ lag3iv) | pid,
    lag4 = . ~ .  | . | (log_price ~ lag4iv) | pid
)

limitreg <- setreg %>% 
    purrr::map(~as.formula(update(reg, .))) %>%
    purrr::map(~lfe::felm(as.formula(.), data = subset(df, year == 2013 | year == 2014)))

n.limitreg <- limitreg %>% purrr::map(~sprintf("%1d", nobs(.))) %>% as_vector()
f.limitreg <- limitreg %>% purrr::map(~sprintf("%1.3f", .$stage1$iv1fstat[[1]]["F"])) %>% as_vector()

# tabulation
keep <- c("log_price") %>% paste(collapse = "|")
varlist <- exprs(
  stat == "se" ~ "",
  str_detect(vars, "log_price") ~ "ln(giving price)"
)

coef.limitreg <- limitreg %>% 
	purrr::map(function(x)
    data.frame(
      vars = rownames(summary(x)$coefficients),
      coef = apply(matrix(summary(x)$coefficients[,4], ncol = 1), MARGIN = 2,
        FUN = function(y) case_when(
          y <= .01 ~ sprintf("%1.3f***", summary(x)$coefficients[,1]),
          y <= .05 ~ sprintf("%1.3f**", summary(x)$coefficients[,1]),
          y <= .1 ~ sprintf("%1.3f*", summary(x)$coefficients[,1]),
          TRUE ~ sprintf("%1.3f", summary(x)$coefficients[,1])
        )
      ),
      se = sprintf("(%1.3f)", summary(x)$coefficients[,2]),
      stringsAsFactors = FALSE
    )
  ) %>% 
  purrr::map(function(x) x[str_detect(x$vars, keep),]) %>%
  purrr::map(function(x) x %>% mutate(vars = case_when(vars == "`log_price(fit)`" ~ "log_price", TRUE ~ vars))) %>% 
  purrr::map(function(x) pivot_longer(x, -vars, names_to = "stat", values_to = "val")) %>% 
	purrr::reduce(full_join, by = c("vars", "stat")) %>% 
	mutate(vars = case_when(!!!varlist)) %>% 
  select(-stat)

addline <- rbind(
  c("F-stat of IV", "",  f.limitreg),
  c("Obs", n.limitreg)
)

tab.limitreg <- rbind(as.matrix(coef.limitreg), addline) %>% data.frame()
