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
        sqlog_PPP_healthbdg = log_PPP_healthbdg^2,
        political_pref = factor(political_pref, level = c(3, 1, 2, 4, 5))
    )

## ---- TrustIndex
reg <- trust_politician ~ factor(year)*factor(living_area) + factor(year)
indexreg <- plm(reg, data = subset(df, year >= 2015), model = "within", index = c("pid", "year"))
feval <- fixef(indexreg)
indexdf <- data.frame(
  pid = as.numeric(attr(feval, "names")),
  trustid = scale(c(feval))
)

ggplot(indexdf, aes(x = trustid)) + 
  geom_histogram(color = "black", fill = "grey50") + 
  my_theme

## ---- RobustTrustIndex
rob.indexreg1 <- df %>% 
  filter(year == 2015 | year == 2016) %>% 
  plm(reg, data = ., model = "within", index = c("pid", "year")) %>% 
  fixef() %>% 
  data.frame(pid = as.numeric(attr(., "names")), parktrustid = scale(c(.))) %>% 
  select(pid, parktrustid)

rob.indexreg2 <- df %>% 
  filter(year == 2017 | year == 2018) %>% 
  plm(reg, data = ., model = "within", index = c("pid", "year")) %>% 
  fixef() %>% 
  data.frame(pid = as.numeric(attr(., "names")), moontrustid = scale(c(.))) %>% 
  select(pid, moontrustid)

robindexdf <- indexdf %>% 
  full_join(rob.indexreg1, by = "pid") %>% 
  full_join(rob.indexreg2, by = "pid") %>% 
  drop_na()

ggplot(robindexdf, aes(x = parktrustid, y = moontrustid)) +
  geom_point(size = 2, alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  my_theme

## ---- TrustReg
indexreg <- trustid ~ gender + log_pinc_all + age + I(age^2/100) + factor(educ) + political_pref

estdf <- df %>% left_join(indexdf, by = "pid") 
est.indexreg <- lm(indexreg, data = subset(estdf, year == 2018))

N <- nobs(est.indexreg)
r2 <- summary(est.indexreg)$adj.r.squared

## ---- TabTrustReg
keep <- c("gender", "pinc", "age", "educ", "political") %>% paste(collapse = "|")

varlist <- exprs(
  vars == "gender" ~ "female",
  vars == "log_pinc_all" ~ "Logarithm of income",
  vars == "age" ~ "age",
  vars == "I(age^2/100)" ~ "squared age/100",
  vars == "factor(educ)2" ~ "High school graduate",
  vars == "factor(educ)3" ~ "University graduate",
  vars == "political_pref1" ~ "Extreme right wing",
  vars == "political_pref2" ~ "Right wing",
  vars == "political_pref4" ~ "Left wing",
  vars == "political_pref5" ~ "Extreme left wing"
)

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
.[str_detect(.$vars, keep),] %>% 
mutate(vars = case_when(!!!varlist))

tab.indexreg <- as.matrix(coef.indexreg) %>% 
  rbind(rbind(
    c("Obs", sprintf("%1d", N), "")
  )) %>% 
  data.frame()

## ---- CorrDonationsTrust
avgdonate <- estdf %>% 
	group_by(pid) %>% 
	summarize_at(vars(i_total_giving), list(~mean(., na.rm = TRUE)))

plotdt <- left_join(avgdonate, indexdf, by = "pid")

ggplot(plotdt, aes(x = trustid, y = i_total_giving)) + 
	geom_point(size = 1.5, alpha = 0.5) +
  labs(x = "Trust Index", y = "Individual Average Donations across Time") +
	my_theme

## ---- CorrTaxBenefitTrust
avgbenefit <- estdf %>% 
	mutate(receive_benefit = if_else(year < 2014, ext_deduct_giving, ext_credit_giving)) %>%
	group_by(pid) %>% 
	summarize_at(vars(receive_benefit), list(~sum(., na.rm = TRUE)))

plotdt <- left_join(avgbenefit, indexdf, by = "pid") 

ggplot(plotdt, aes(x = factor(receive_benefit), y = trustid)) + 
	geom_boxplot(fill = "grey90") +
	stat_summary(fun = "mean", geom = "point", shape = 21, size = 2., fill = "white") + 
  labs(x = "#. Receving Tax Benefit", y = "Trust Index") +
  scale_y_continuous(breaks = seq(-3, 5, 1)) +
  coord_flip() +
	my_theme +
  theme(panel.grid.major.x = element_line(), panel.grid.major.y = element_blank())

## ---- BaseReg
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

## --- TabBaseReg
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

## ---- TrustGroupReg
reg <- log_total_g ~ log_price + log_pinc_all + 
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
keep <- c("log_price") %>% paste(collapse = "|")
varlist <- exprs(
  stat == "se" ~ "",
  vars == "log_price" ~ "ln(giving price)"
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
	mutate(vars = case_when(!!!varlist)) %>%
  select(-stat)

tab.trustreg <- rbind(as.matrix(coef.trustreg), c("Obs", n.trustreg)) %>% data.frame()

## ---- TrustHeteroReg
reg <- log_total_g ~ log_price*trustid + log_pinc_all + 
  age + factor(year)*factor(educ) + factor(year)*factor(gender) + factor(living_area) + factor(year)

setreg <- list(
  base = . ~ .,
  squared = . ~ . + log_price*I(trustid^2)
)

heteroreg <- setreg %>% 
  purrr::map(~plm(update(reg, .), data = subset(estdf, year >= 2012), model = "within", index = c("pid", "year")))
rob.heteroreg <- heteroreg %>% 
  purrr::map(~coeftest(., vcov = vcovHC(., type = "HC0", cluster = "group")))
n.heteroreg <- heteroreg %>% purrr::map(~sprintf("%1d", nobs(.))) %>% as_vector()
r2.heteroreg <- heteroreg %>% purrr::map(~sprintf("%1.4f", plm::r.squared(.))) %>% as_vector()

## ---- TabTrustHeteroReg
keep <- c("log_price") %>% paste(collapse = "|")
varlist <- exprs(
  stat == "se" ~ "",
  str_detect(vars, "I[[:punct:]]trustid.2[[:punct:]]") ~ "X Squared trust index",
  str_detect(vars, "trustid") ~ "X Trust index",
  vars == "log_price" ~ "ln(giving price)"
)
varorder <- exprs(
  str_detect(vars, "I[[:punct:]]trustid.2[[:punct:]]") ~ 2,
  str_detect(vars, "trustid") ~ 1,
  TRUE ~ 0
)

coef.heteroreg <- rob.heteroreg %>% 
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

tab.heteroreg <- as.matrix(coef.heteroreg) %>% 
  rbind(rbind(c("Obs", n.heteroreg), c("R-aq", r2.heteroreg))) %>% 
  data.frame()

## ---- PlotPredictedElast
b_price <- rob.heteroreg[[1]] %>% .[str_detect(rownames(.), "price"),1]
b_price2 <- rob.heteroreg[[2]] %>% .[str_detect(rownames(.), "price"),1]
vcov_price <- vcov(heteroreg[[1]]) %>% .[str_detect(rownames(.), "price"), str_detect(colnames(.), "price")]
vcov_price2 <- vcov(heteroreg[[2]]) %>% .[str_detect(rownames(.), "price"), str_detect(colnames(.), "price")]

newdf <- data.frame(
  int = 1,
  trustid = unique(indexdf$trustid),
  trustid2 = unique(indexdf$trustid)^2
) %>% as.matrix()

se_price <- sqrt(diag(newdf[,1:2] %*% vcov_price %*% t(newdf[,1:2])))
se_price2 <- sqrt(diag(newdf %*% vcov_price2 %*% t(newdf)))

plotdt1 <- data.frame(
  x = newdf[,2],
  y = newdf[,1:2] %*% matrix(b_price, ncol = 1),
  se = se_price,
  label = "exclude squared trust index"
) %>% drop_na()

plotdt2 <- data.frame(
  x = newdf[,2],
  y = newdf %*% matrix(b_price2, ncol = 1),
  se = se_price2,
  label = "include squared trust index"
) %>% drop_na()

plotdt <- rbind(plotdt1, plotdt2)

ggplot(plotdt, aes(x = x, y = y)) +
  geom_ribbon(aes(ymin = y - 1.96*se, ymax = y + 1.96*se), color = "black", fill = "white", alpha = 0, linetype = 2) +
  geom_line(size = 1, color = "blue") +
  geom_hline(aes(yintercept = 0), size = 1, color = "red", linetype = 2) +
  scale_y_continuous(breaks = seq(-5, 6, 1)) +
  scale_x_continuous(breaks = seq(-5, 6, 1)) +
  labs(
    x = "Standarized trust Index", y = "Estimated Elasticity",
    caption = "Dashed lines represent 95% CI of linear combination of parameter estimates"
  ) +
  facet_wrap(~label) +
  my_theme