#2trust

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
df <- df %>% 
  group_by(pid) %>% 
  mutate(
    lag1_price = dplyr::lag(price, n = 1, default = NA, order_by = year),
    lag2_price = dplyr::lag(price, n = 2, default = NA, order_by = year),
    lag3_price = dplyr::lag(price, n = 3, default = NA, order_by = year),
    lag4_price = dplyr::lag(price, n = 4, default = NA, order_by = year)
  ) %>% 
    ) %>% 
  ) %>% 
  ungroup()
df <- df %>% 
  mutate(
    lag1iv = log(price/lag1_price),
    lag2iv = log(price/lag2_price),
    lag3iv = log(price/lag3_price),
    lag4iv = log(price/lag4_price),
  )

## ---- EstimateTrustIndex
reg <- trust_politician ~ factor(year)*factor(living_area) | pid | 0 | 0

dfset <- list(
	base = df %>% filter(year >= 2015),
	park = df %>% filter(year == 2015 | year == 2016),
	moon = df %>% filter(year == 2017 | year == 2018)
)

est.trustid <- dfset %>% 
	purrr::map(~lfe::felm(reg, data = .)) %>%
	purrr::map(~lfe::getfe(.)) %>% 
	purrr::map(~data.frame(trustid = c(.$effect), pid = as.numeric(as.character(.$idx))))


## ---- MergedWithTrustid

# index data
indexdf <- est.trustid$base %>% 
  left_join(est.trustid$park %>% rename(parktrustid = trustid), by = "pid") %>% 
  left_join(est.trustid$moon %>% rename(moontrustid = trustid), by = "pid") %>% 
	mutate(diff = moontrustid - parktrustid)

# scaled data
scaledf <- indexdf %>% 
  mutate_at(vars(starts_with("trustid")), list(~scale(.)))

# merged data
estdf <- df %>% 
  left_join(indexdf, by = "pid") %>% 
	mutate(
		lessdiffhalf = if_else(abs(diff) <= 0.5, 1 ,0),
		lessdiff1 = if_else(abs(diff) <= 1, 1 ,0),
	) %>% 
  mutate(
    original5 = case_when(
      trustid < quantile(indexdf$trustid, prob = .2) ~ 1,
      trustid < quantile(indexdf$trustid, prob = .4) ~ 2,
      trustid < quantile(indexdf$trustid, prob = .6) ~ 3,
      trustid < quantile(indexdf$trustid, prob = .8) ~ 4,
      trustid < quantile(indexdf$trustid, prob = 1) ~ 5
    ),
		park5 = case_when(
      parktrustid < quantile(indexdf$parktrustid, prob = .2, na.rm = TRUE) ~ 1,
      parktrustid < quantile(indexdf$parktrustid, prob = .4, na.rm = TRUE) ~ 2,
      parktrustid < quantile(indexdf$parktrustid, prob = .6, na.rm = TRUE) ~ 3,
      parktrustid < quantile(indexdf$parktrustid, prob = .8, na.rm = TRUE) ~ 4,
      parktrustid < quantile(indexdf$parktrustid, prob = 1, na.rm = TRUE) ~ 5
    )
  )

## ---- HistogramTrustid
ggplot(indexdf, aes(x = trustid)) + 
  geom_histogram(color = "black", fill = "grey50") + 
  my_theme

## ---- Scatter1Trustid
stats <- indexdf %>% 
	summarize_at(vars(parktrustid, moontrustid), list(mu = ~mean(., na.rm =TRUE), sd = ~sd(., na.rm =TRUE)))
difftest <- t.test(indexdf$moontrustid, indexdf$parktrustid, var.equal = TRUE)$p.value

annotation1 <- sprintf("Park's Trust Index: Mean = %1.3f, Std.Dev. = %1.3f", unlist(stats)[1], unlist(stats)[3])
annotation2 <- sprintf("Moon's Trust Index: Mean = %1.3f, Std.Dev. = %1.3f", unlist(stats)[2], unlist(stats)[4])
annotation3 <- sprintf("t-test of difference in mean: p-value = %1.3f", difftest)
annotation <- str_c(c(annotation1, annotation2, annotation3), collapse = "\n")

ggplot(indexdf, aes(x = parktrustid, y = moontrustid)) +
  geom_point(size = 2, alpha = 0.5) + 
  geom_smooth(se = FALSE, color = "red") +
  annotate("text", x = Inf, y = Inf, label = annotation, vjust = "top", hjust = "right") +
  ylim(c(0.5, 5.5)) +
  labs(x = "Park's Trust Index", y = "Moon's Trust Index") +
  my_theme + 
  theme(
    panel.grid.major.x = element_line(linetype = 2),
    panel.grid.major.y = element_line(linetype = 2)
  )

## ---- Scatter2Trustid
ggplot(indexdf, aes(x = diff, y = trustid)) + 
  geom_point(size = 2, alpha = 0.5) + 
  geom_smooth(se = FALSE, color = "red") + 
  labs(x = "Difference b/w Moon's and Park's Trust Index", y = "Trust Index") + 
  my_theme + 
  theme(
    panel.grid.major.x = element_line(linetype = 2),
    panel.grid.major.y = element_line(linetype = 2)
  )

## ---- RegTrustidOnDiff2Trustid
dfset <- list(
  full = indexdf,
  limit1 = indexdf %>% filter(abs(diff) <= 2),
  limit2 = indexdf %>% filter(abs(diff) <= 1),
  limit3 = indexdf %>% filter(abs(diff) <= 0.5)
)

est.dfset <- dfset %>% purrr::map(~lm(trustid ~ diff, data = .))
n.dfset <- est.dfset %>% purrr::map(~sprintf("%1d", nobs(.))) %>% as_vector()
r2.dfset <- est.dfset %>% purrr::map(~sprintf("%1.3f", summary(.)$adj.r.squared)) %>% as_vector()

# make tabulation
keep <- "diff"
varlist <- exprs(
  stat == "se" ~ "",
  vars == "diff" ~ "Moon's trust - Park's trust"
)

coef.dfset <- est.dfset %>% 
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

tab.dfset <- as.matrix(coef.dfset) %>% 
  rbind(rbind(c("Obs", n.dfset), c("Adjusted R-sq", r2.dfset)))

## ---- RegTrusidonSepTrustid
regset <- list(
  reg1 = trustid ~ moontrustid,
  reg2 = trustid ~ parktrustid,
  reg3 = trustid ~ moontrustid + parktrustid
)

est.regset <- regset %>% purrr::map(~lm(., data = indexdf))
n.regset <- est.regset %>% purrr::map(~sprintf("%1d", nobs(.))) %>% as_vector()
r2.regset <- est.regset %>% purrr::map(~sprintf("%1.3f", summary(.)$adj.r.squared)) %>% as_vector()

keep <- "trustid"
varlist <- exprs(
  stat == "se" ~ "",
  str_detect(vars, "park") ~ "Trust ID (Park Geun-hye)",
  str_detect(vars, "moon") ~ "Trust ID (Moon Jae-in)"
)

coef.regset <- est.regset %>% 
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

tab.regset <- as.matrix(coef.regset) %>% 
  rbind(rbind(c("Obs", n.regset), c("Adjusted R-sq", r2.regset)))

## ---- PredictTrustid
indexdf$predtrustid <- predict(est.regset$reg3, newdata = indexdf)

ggplot(indexdf, aes(x = predtrustid, y = trustid)) +
  geom_point(size = 2, alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Predicted value of trust index", y = "Trust Index") +
  my_theme + 
  theme(
    panel.grid.major.x = element_line(linetype = 2),
    panel.grid.major.y = element_line(linetype = 2)
  )

## ---- RegTrustidOnCovariate
indexreg <- trustid ~ gender + log_pinc_all + age + I(age^2/100) + factor(educ) + political_pref

est.indexreg <- lm(indexreg, data = subset(estdf, year == 2018))
N <- nobs(est.indexreg)
r2 <- summary(est.indexreg)$adj.r.squared

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

## ---- ScatterTrusidDonations
avgdonate <- estdf %>% 
	group_by(pid) %>% 
	summarize_at(vars(i_total_giving), list(~mean(., na.rm = TRUE)))

plotdt <- left_join(avgdonate, indexdf, by = "pid")

ggplot(plotdt, aes(x = trustid, y = i_total_giving)) + 
	geom_point(size = 1.5, alpha = 0.5) +
  labs(x = "Trust Index", y = "Individual Average Donations across Time") +
	my_theme

## ---- EstimateElasticityByTrustGroup
reg <- log_total_g ~ log_price + log_pinc_all + 
  age + factor(year):factor(educ) + factor(year):factor(gender) + factor(living_area) | pid + year | 0 | pid

trustreg <- 1:5 %>% 
  purrr::map(~lfe::felm(reg, data = subset(estdf, year >= 2012 & original5 == .)))
n.trustreg <- trustreg %>% purrr::map(~sprintf("%1d", nobs(.))) %>% as_vector()

# make tabulation
keep <- c("log_price") %>% paste(collapse = "|")
varlist <- exprs(
  stat == "se" ~ "",
  vars == "log_price" ~ "ln(giving price)"
)

coef.trustreg <- trustreg %>% 
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

tab.trustreg <- rbind(as.matrix(coef.trustreg), c("Obs", n.trustreg)) %>% data.frame()

## ---- EstimateInteractionByTrustGroup
intreg <- log_total_g ~ log_price + log_price:original5 + log_pinc_all + 
  age + factor(year):factor(educ) + factor(year):factor(gender) + factor(living_area) | pid + year | 0 | pid

est.intreg <- estdf %>% 
	filter(year >= 2012) %>% 
	mutate(original5 = factor(original5, levels = c(3, 1, 2, 4, 5))) %>%
	lfe::felm(intreg, data = .)
n.intreg <- sprintf("%1d", nobs(est.intreg))

# make tabulation
keep <- c("log_price") %>% paste(collapse = "|")
varlist <- exprs(
  str_detect(vars, "original51") ~ "X Lowest Trust",
  str_detect(vars, "original52") ~ "X Lower Trust",
  str_detect(vars, "original54") ~ "X Higher Trust",
  str_detect(vars, "original55") ~ "X Highest Trust",
  vars == "log_price" ~ "ln(giving price)"
)
varorder <- exprs(
  str_detect(vars, "original51") ~ 1,
  str_detect(vars, "original52") ~ 2,
  str_detect(vars, "original54") ~ 3,
  str_detect(vars, "original55") ~ 4,
  TRUE ~ 0
)

coef.intreg <- data.frame(
		vars = rownames(summary(est.intreg)$coefficients),
		coef = apply(matrix(summary(est.intreg)$coefficients[,4], ncol = 1), MARGIN = 2,
			FUN = function(y) case_when(
				y <= .01 ~ sprintf("%1.3f***", summary(est.intreg)$coefficients[,1]),
				y <= .05 ~ sprintf("%1.3f**", summary(est.intreg)$coefficients[,1]),
				y <= .1 ~ sprintf("%1.3f*", summary(est.intreg)$coefficients[,1]),
				TRUE ~ sprintf("%1.3f", summary(est.intreg)$coefficients[,1])
			)
		),
		se = sprintf("(%1.3f)", summary(est.intreg)$coefficients[,2]),
		stringsAsFactors = FALSE
	) %>% 
	.[str_detect(.$vars, keep),] %>% 
	mutate(order = case_when(!!!varorder), vars = case_when(!!!varlist)) %>%
  .[with(., order(order)),] %>% 
  select(-order)

tab.intreg <- as.matrix(coef.intreg) %>% 
  rbind(c("Obs", n.intreg, "")) %>% 
  data.frame()

## ---- Robust1EstimateInteractionByTrustGroup
intreg <- Formula(log_total_g ~ log_price + log_pinc_all + 
  age + factor(year):factor(educ) + factor(year):factor(gender) + factor(living_area) | pid + year)

factordf <- estdf %>% 
	mutate(
		park5 = factor(park5, levels = c(3, 1, 2, 4, 5)),
		original5 = factor(original5, levels = c(3, 1, 2, 4, 5))
	)

argset <- list(
	rob1 = list(
		reg = update(intreg, . ~ . + log_price:original5 | . | 0 | pid),
		data = factordf %>% filter(year == 2013 | year == 2014)
	),
	rob2 = list(
		reg = update(intreg, . ~ . + log_price:park5 | . | 0 | pid),
		data = factordf %>% filter(year == 2013 | year == 2014)
	),
	rob3 = list(
		reg = update(intreg, . ~ . + log_price:original5 | . | 0 | pid),
		data = factordf %>% filter(year >= 2012, lessdiff1 == 1)
	),
	rob4 = list(
		reg = update(intreg, . ~ . + log_price:original5 | . | 0 | pid),
		data = factordf %>% filter(year >= 2012, lessdiffhalf == 1)
	)
)

est.robintreg <- argset %>% purrr::map(~lfe::felm(.$reg, data = .$data))
n.robintreg <- est.robintreg %>% purrr::map(~sprintf("%1d", nobs(.))) %>% as_vector()

# make tabulation
keep <- c("log_price") %>% paste(collapse = "|")
varlist <- exprs(
  str_detect(vars, "51") ~ "X Lowest Trust",
  str_detect(vars, "52") ~ "X Lower Trust",
  str_detect(vars, "54") ~ "X Higher Trust",
  str_detect(vars, "55") ~ "X Highest Trust",
  vars == "log_price" ~ "ln(giving price)"
)
varorder <- exprs(
  str_detect(vars, "Lowest") ~ 1,
  str_detect(vars, "Lower") ~ 2,
  str_detect(vars, "Higher") ~ 3,
  str_detect(vars, "Highest") ~ 4,
  TRUE ~ 0
)

coef.robintreg <- est.robintreg %>% 
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
	purrr::map(function(x) x %>% mutate(vars = case_when(!!!varlist))) %>% 
  purrr::map(function(x) pivot_longer(x, -vars, names_to = "stat", values_to = "val")) %>% 
	purrr::reduce(full_join, by = c("vars", "stat")) %>% 
	mutate(order = case_when(!!!varorder)) %>% 
	.[with(., order(order)),] %>% 
	mutate(vars = case_when(stat == "se" ~ "", TRUE ~ vars)) %>% 
	select(-stat, -order)

tab.robintreg <- as.matrix(coef.robintreg) %>% 
  rbind(c("Obs", n.robintreg)) %>% 
  data.frame()

## ---- EstimateHeteroPEstByTrusid
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

# make tabulation
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

## ---- PlotHeteroPElast
b_price <- rob.heteroreg[[1]] %>% .[str_detect(rownames(.), "price"),1]
b_price2 <- rob.heteroreg[[2]] %>% .[str_detect(rownames(.), "price"),1]
vcov_price <- vcov(heteroreg[[1]]) %>% .[str_detect(rownames(.), "price"), str_detect(colnames(.), "price")]
vcov_price2 <- vcov(heteroreg[[2]]) %>% .[str_detect(rownames(.), "price"), str_detect(colnames(.), "price")]

newdf <- data.frame(
  int = 1,
  trustid = unique(scaledf$trustid),
  trustid2 = unique(scaledf$trustid)^2
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