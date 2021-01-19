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
        sqlog_PPP_healthbdg = log_PPP_healthbdg^2,
        political_pref = factor(political_pref, level = c(3, 1, 2, 4, 5))
    ) %>% 
    filter(year >= 2012)

## ---- EstimatePElast
#regressions
reg <- log_total_g ~ log_price + log_pinc_all + factor(year)

setreg <- list(
    base = . ~ .,
    age = . ~ . + age,
    educ = . ~ .  + age + factor(year)*factor(educ),
    gender = . ~ .  + age + factor(year)*factor(educ) + factor(year)*factor(gender),
    living = . ~ . +age+factor(year)*factor(educ)+factor(year)*factor(gender)+factor(living_area)
)

basereg <- setreg %>% 
    purrr::map(~update(reg, .)) %>% 
    purrr::map(~plm(., data = subset(df, year >= 2012), model = "within", index = c("pid", "year")))

rob.basereg <- basereg %>% purrr::map(~coeftest(., vcov = vcovHC(., type = "HC0", cluster = "group")))
n.basereg <- basereg %>% purrr::map(~sprintf("%1d", nobs(.))) %>% as_vector()

# make tabulation
keep <- c("log_price") %>% paste(collapse = "|")
varlist <- exprs(
  stat == "se" ~ "",
  vars == "log_price" ~ "ln(giving price)"
)

coef.basereg <- rob.basereg %>% 
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
