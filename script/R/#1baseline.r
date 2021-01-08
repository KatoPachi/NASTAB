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

feest <- setreg %>% 
    purrr::map(~update(reg, .)) %>% 
    purrr::map(~plm(., data = subset(df, year >= 2012), model = "within", index = c("pid", "year")))

rob.feest <- feest %>% purrr::map(~coeftest(., vcov = vcovHC(., type = "HC0", cluster = "group")))

n.feest <- feest %>% purrr::map(~sprintf("%1d", nobs(.))) %>% as_vector()

## --- TabBaseReg
keep <- c("PPP_pubbdg") %>% paste(collapse = "|")
varlist <- exprs(
    stat == "se" ~ "",
    vars == "log_PPP_pubbdg" ~ "log(Social Welfare)",
    vars == "sqlog_PPP_pubbdg" ~ "log(Social Welfare)^2"
)

coef.feest <- list(rob.feest$base, rob.feest$living, rob.feest$sqlog) %>% 
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
    c("Giving Price", "Y", "Y", "Y"),
    c("logarithm of income", "Y", "Y", "Y"),
    c("Age", "N", "Y", "Y"),
    c("Year X Educ", "N", "Y", "Y"),
    c("Year X Gender", "N", "Y", "Y"),
    c("Living Dummy", "N", "Y", "Y"),
    c("Obs", n.feest[c(1, 5, 6)])
)

tab.feest <- rbind(as.matrix(coef.feest), addline) %>% data.frame()
