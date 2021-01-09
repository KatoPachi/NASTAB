#1baseline

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

## --- GGTemp
my_theme <- theme_minimal() +
  theme(
    # setting: background
    plot.background = element_rect(fill="#87CEEB50", color = "transparent"),
    
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


## ---- ReadData
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
        sqlog_PPP_healthbdg = log_PPP_healthbdg^2
    )

## ---- TrustIndex
reg <- trust_politician ~ factor(year)*factor(living_area) + factor(year)
indexreg <- plm(reg, data = subset(df, year >= 2015), model = "within", index = c("pid", "year"))
feval <- fixef(indexreg)

indexdf <- data.frame(
  pid = as.numeric(attr(feval, "names")),
  trustid = (c(feval) - min(feval))/(max(feval) - min(feval))
)

ggplot(indexdf, aes(x = trustid)) + 
  geom_histogram(color = "black", fill = "grey50") + 
  my_theme

## ---- TrustReg
indexreg <- trustid ~ gender + age + I((age/100)^2) + factor(educ) + factor(political_pref)

estdf <- df %>% left_join(indexdf, by = "pid") 
est.indexreg <- lm(indexreg, data = subset(estdf, year == 2018))

N <- nobs(est.indexreg)
r2 <- summary(est.indexreg)$adj.r.squared

## ---- TabTrustReg
keep <- c("gender", "age", "educ", "political") %>% paste(collapse = "|")

coef.indexreg <- data.frame(
  vars = rownames(summary(est.indexreg)$coefficients),
  coef = apply(matrix(summary(est.indexreg)$coefficients[,4], ncol = 1), MARGIN = 2,
    FUN = function(y) case_when(
      y <= .01 ~ sprintf("%1.3f***", summary(est.indexreg)$coefficients[,1]),
      y <= .05 ~ sprintf("%1.3f**", summary(est.indexreg)$coefficients[,1]),
      y <= .1 ~ sprintf("%1.3f*", summary(est.indexreg)$coefficients[,1]),
      TRUE ~ sprintf("%1.3f", summary(est.indexreg)$coefficients[,1])
    )
  ),
  se = sprintf("(%1.3f)", summary(est.indexreg)$coefficients[,2]),
  stringsAsFactors = FALSE
) %>% 
.[str_detect(.$vars, keep),]

tab.indexreg <- as.matrix(coef.indexreg) %>% 
  rbind(rbind(
    c("Obs", sprintf("%1d", N), ""), 
    c("Adjusted R-sq", sprintf("%1.4f", r2, ""), "")
  )) %>% 
  data.frame()

## ---- BaseReg
reg <- log_total_g ~ log_PPP_pubbdg + log_price + log_pinc_all + factor(year)

setreg <- list(
    base = . ~ .,
    age = . ~ . + age,
    educ = . ~ .  + age + factor(year)*factor(educ),
    gender = . ~ .  + age + factor(year)*factor(educ) + factor(year)*factor(gender),
    living = . ~ . +age+factor(year)*factor(educ)+factor(year)*factor(gender)+factor(living_area),
    sqlog = .~.+sqlog_PPP_pubbdg + age+factor(year)*factor(educ)+
        factor(year)*factor(gender)+factor(living_area)
)

basereg <- setreg %>% 
    purrr::map(~update(reg, .)) %>% 
    purrr::map(~plm(., data = subset(df, year >= 2012), model = "within", index = c("pid", "year")))

rob.basereg <- basereg %>% purrr::map(~coeftest(., vcov = vcovHC(., type = "HC0", cluster = "group")))
n.basereg <- basereg %>% purrr::map(~sprintf("%1d", nobs(.))) %>% as_vector()

## --- TabBaseReg
keep <- c("PPP_pubbdg", "log_price") %>% paste(collapse = "|")
varlist <- exprs(
  stat == "se" ~ "",
  vars == "log_PPP_pubbdg" ~ "ln(Social Welfare+1)",
  vars == "sqlog_PPP_pubbdg" ~ "ln(Social Welfare+1)^2",
  vars == "log_price" ~ "ln(giving price)"
)
varorder <- exprs(
  vars == "log_PPP_pubbdg" ~ 1,
  vars == "sqlog_PPP_pubbdg" ~ 2,
  vars == "log_price" ~ 3
)

coef.basereg <- list(rob.basereg$base, rob.basereg$living, rob.basereg$sqlog) %>% 
	purrr::map(function(x)
    data.frame(
      vars = rownames(x),
      coef = apply(matrix(x[,4], ncol = 1), MARGIN = 2,
        FUN = function(y) case_when(
          y <= .01 ~ sprintf("%1.3f***", x[,1]),
          y <= .05 ~ sprintf("%1.3f**", x[,1]),
          y <= .1 ~ sprintf("%1.3f*", x[,1]),
          TRUE ~ sprintf("%1.3f", x[,1])
        )
      ),
      se = sprintf("(%1.3f)", x[,2]),
      stringsAsFactors = FALSE
    )
  ) %>% 
  purrr::map(function(x) x[str_detect(x$vars, keep),]) %>%
  purrr::map(function(x) pivot_longer(x, -vars, names_to = "stat", values_to = "val")) %>% 
	purrr::reduce(full_join, by = c("vars", "stat")) %>% 
	mutate(order = case_when(!!!varorder), vars = case_when(!!!varlist)) %>% 
  .[with(., order(order)),] %>% 
  select(-stat, -order)

addline <- rbind(
  c("Logarithm of income", "Y", "Y", "Y"),
  c("Age", "N", "Y", "Y"),
  c("Year X Educ", "N", "Y", "Y"),
  c("Year X Gender", "N", "Y", "Y"),
  c("Living Dummy", "N", "Y", "Y"),
  c("Obs", n.basereg[c(1, 5, 6)])
)

tab.basereg <- rbind(as.matrix(coef.basereg), addline) %>% data.frame()
