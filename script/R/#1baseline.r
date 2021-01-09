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
	mutate(order = case_when(!!!varorder), vars = case_when(!!!varlist)) %>% 
  .[with(., order(order)),] %>% 
  select(-stat, -order)

addline <- rbind(
  c("Logarithm of income", "Y", "Y", "Y", "Y", "Y", "Y"),
  c("Age", "N", "Y", "Y", "Y", "Y", "Y"),
  c("Year X Educ", "N", "N", "Y", "Y", "Y", "Y"),
  c("Year X Gender", "N", "N", "N", "Y", "Y", "Y"),
  c("Living Dummy", "N", "N", "N", "N", "Y", "Y"),
  c("Obs", n.basereg)
)

tab.basereg <- rbind(as.matrix(coef.basereg), addline) %>% data.frame()

## ---- TrustGroupReg
reg <- log_total_g ~ log_PPP_pubbdg + log_price + log_pinc_all + 
  age + factor(year)*factor(educ) + factor(year)*factor(gender) + factor(living_area) + factor(year)

estdf <- df %>% 
  left_join(indexdf, by = "pid") %>% 
  mutate(
    trusted = case_when(
      trustid < quantile(indexdf$trustid, prob = .2) ~ 1,
      trustid < quantile(indexdf$trustid, prob = .4) ~ 2,
      trustid < quantile(indexdf$trustid, prob = .6) ~ 3,
      trustid < quantile(indexdf$trustid, prob = .8) ~ 4,
      TRUE ~ 5
    )
  )

trustreg <- 1:5 %>% 
  purrr::map(
    ~plm(reg, data = subset(estdf, year >= 2012 & trusted == .), model = "within", index = c("pid", "year")))

rob.trustreg <- trustreg %>% purrr::map(~coeftest(., vcov = vcovHC(., type = "HC0", cluster = "group")))
n.trustreg <- trustreg %>% purrr::map(~sprintf("%1d", nobs(.))) %>% as_vector()

## ---- TabTrustGroupReg
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

coef.trustreg <- rob.trustreg %>% 
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

tab.trustreg <- rbind(as.matrix(coef.trustreg), c("Obs", n.trustreg)) %>% data.frame()

## ---- TrustHeteroReg
reg <- log_total_g ~ log_PPP_pubbdg*trustid + log_price*trustid + log_pinc_all + 
  age + factor(year)*factor(educ) + factor(year)*factor(gender) + factor(living_area) + factor(year)

heteroreg <- plm(reg, data = subset(estdf, year >= 2012), model = "within", index = c("pid", "year"))
rob.heteroreg <- heteroreg %>% coeftest(., vcov = vcovHC(., type = "HC0", cluster = "group"))
n.heteroreg <- sprintf("%1d", nobs(heteroreg))

## ---- TabTrustHeteroReg
keep <- c("PPP_pubbdg", "log_price") %>% paste(collapse = "|")
varlist <- exprs(
  str_detect(vars, "trustid") ~ "X Trust index",
  vars == "log_PPP_pubbdg" ~ "ln(Social Welfare+1)",
  vars == "log_price" ~ "ln(giving price)"
)
varorder1 <- exprs(
  str_detect(vars, "PPP") ~ 1,
  str_detect(vars, "price") ~ 2
)
varorder2 <- exprs(
  str_detect(vars, "trustid") ~ 1,
  TRUE ~ 0
)

coef.heteroreg <- data.frame(
  vars = rownames(rob.heteroreg),
  coef = apply(matrix(rob.heteroreg[,4], ncol = 1), MARGIN = 2,
    FUN = function(y) case_when(
      y <= .01 ~ sprintf("%1.3f***", rob.heteroreg[,1]),
      y <= .05 ~ sprintf("%1.3f**", rob.heteroreg[,1]),
      y <= .1 ~ sprintf("%1.3f*", rob.heteroreg[,1]),
      TRUE ~ sprintf("%1.3f", rob.heteroreg[,1])
    )
  ),
  se = sprintf("(%1.3f)", rob.heteroreg[,2]),
  stringsAsFactors = FALSE
) %>% 
.[str_detect(.$vars, keep),] %>% 
mutate(order1 = case_when(!!!varorder1), order2 = case_when(!!!varorder2), vars = case_when(!!!varlist)) %>% 
.[with(., order(order1, order2)),] %>% 
select(-starts_with("order"))

tab.heteroreg <- as.matrix(coef.heteroreg) %>% 
  rbind(c("Obs", n.heteroreg, "")) %>% 
  data.frame()

## ---- TrustHetero2Reg
reg <- log_total_g ~ log_PPP_pubbdg*trustid + log_PPP_pubbdg*I(trustid^2) + 
  log_price*trustid + log_price*I(trustid^2) + 
  log_pinc_all + 
  age + factor(year)*factor(educ) + factor(year)*factor(gender) + factor(living_area) + factor(year)

hetero2reg <- plm(reg, data = subset(estdf, year >= 2012), model = "within", index = c("pid", "year"))
rob.hetero2reg <- hetero2reg %>% coeftest(., vcov = vcovHC(., type = "HC0", cluster = "group"))
n.hetero2reg <- sprintf("%1d", nobs(hetero2reg))

## ---- TabTrustHetero2Reg
keep <- c("PPP_pubbdg", "log_price") %>% paste(collapse = "|")
varlist <- exprs(
  str_detect(vars, "I[[:punct:]]trustid.2[[:punct:]]") ~ "X Squared trust index",
  str_detect(vars, "trustid") ~ "X Trust index",
  vars == "log_PPP_pubbdg" ~ "ln(Social Welfare+1)",
  vars == "log_price" ~ "ln(giving price)"
)
varorder1 <- exprs(
  str_detect(vars, "PPP") ~ 1,
  str_detect(vars, "price") ~ 2
)
varorder2 <- exprs(
  str_detect(vars, "I[[:punct:]]trustid.2[[:punct:]]") ~ 2,
  str_detect(vars, "trustid") ~ 1,
  TRUE ~ 0
)

coef.hetero2reg <- data.frame(
  vars = rownames(rob.hetero2reg),
  coef = apply(matrix(rob.hetero2reg[,4], ncol = 1), MARGIN = 2,
    FUN = function(y) case_when(
      y <= .01 ~ sprintf("%1.3f***", rob.hetero2reg[,1]),
      y <= .05 ~ sprintf("%1.3f**", rob.hetero2reg[,1]),
      y <= .1 ~ sprintf("%1.3f*", rob.hetero2reg[,1]),
      TRUE ~ sprintf("%1.3f", rob.hetero2reg[,1])
    )
  ),
  se = sprintf("(%1.3f)", rob.hetero2reg[,2]),
  stringsAsFactors = FALSE
) %>% 
.[str_detect(.$vars, keep),] %>% 
mutate(order1 = case_when(!!!varorder1), order2 = case_when(!!!varorder2), vars = case_when(!!!varlist)) %>% 
.[with(., order(order1, order2)),] %>% 
select(-starts_with("order"))

tab.hetero2reg <- as.matrix(coef.hetero2reg) %>% 
  rbind(c("Obs", n.hetero2reg, "")) %>% 
  data.frame()

## ---- PlotPredictedElast
b_bdg <- rob.hetero2reg %>% .[str_detect(rownames(.), "PPP"),1]
b_price <- rob.heteroreg %>% .[str_detect(rownames(.), "price"),1]

vcov_bdg <- vcov(hetero2reg) %>% .[str_detect(rownames(.), "PPP"), str_detect(colnames(.), "PPP")]
vcov_price <- vcov(heteroreg) %>% .[str_detect(rownames(.), "price"), str_detect(colnames(.), "price")]

newdf <- data.frame(
  int = 1,
  trustid = unique(indexdf$trustid),
  trustid2 = unique(indexdf$trustid)^2
) %>% as.matrix()

se_bdg <- sqrt(diag(newdf %*% vcov_bdg %*% t(newdf)))
se_price <- sqrt(diag(newdf[,1:2] %*% vcov_price %*% t(newdf[,1:2])))

plotdt1 <- data.frame(
  x = newdf[,2],
  y = newdf %*% matrix(b_bdg, ncol = 1),
  se = se_bdg,
  var = "Budget for Social Welfare"
)

plotdt2 <- data.frame(
  x = newdf[,2],
  y = newdf[,1:2] %*% matrix(b_price, ncol = 1),
  se = se_price,
  var = "Giving Price"
)

plotdt <- rbind(plotdt1, plotdt2) %>% drop_na()

ggplot(plotdt, aes(x = x, y = y)) +
  geom_ribbon(
    aes(ymin = y - 1.96*se, ymax = y + 1.96*se, group = var, color = var), 
    fill = "white", alpha = 0, linetype = 2
  ) +
  geom_line(aes(group = var, color = var), size = 1) +
  geom_hline(aes(yintercept = 0), size = 1, color = "black", linetype = 2) +
  scale_y_continuous(breaks = seq(-5, 2, 1)) +
  labs(
    x = "Trust Index", y = "Estimated Elasticity",
    caption = "Dashed lines represent 95% CI of linear combination of parameter estimates"
  ) +
  my_theme
