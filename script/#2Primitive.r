#2Summary

## ---- library
package_load <- function(pkg.name){
  if(!require(pkg.name, character.only=TRUE)){
    install.packages(pkg.name)
    library(pkg.name, character.only = TRUE)
  } else {
    library(pkg.name, character.only = TRUE)
  }
}

package_load("tidyverse")
package_load("stargazer")

## ---- function
se <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

mtrcal <- function(y, t) {
  cutdt <- subset(mtr, year == t)
  cutdt$ind <- 1*(cutdt[,2] <= y)
  t <- with(subset(cutdt, ind == 1), max(MTR))
  return(t)    
}

my_theme <- theme_minimal() +
  theme(
    panel.border = element_blank(), 
    panel.grid = element_line(color = "grey80"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line = element_line(),
    plot.background = element_rect(fill="#87CEEB50"),
    panel.background = element_rect(),   
    plot.title = element_text(hjust=0.5,size=20),       
    plot.caption = element_text(size=11),       
    axis.text = element_text(color="black",size=13),    
    axis.title = element_text(size=13),                 
    legend.title = element_text(size=12),               
    legend.text = element_text(size=12),                
    legend.key.size = unit(0.5,"cm"),
    legend.background = element_rect(color = "black"), 
    legend.position = "bottom"
  )

## ---- data
nastab <- read_rds("./data/shapedt.rds")
mtr <- read.csv("./data/mtrdt.csv")

## ---- dedcution_rate
deduction <- nastab %>% 
  group_by(year) %>%
  summarize_at(c("pca225", "pca227"), list(~mean(., na.rm = TRUE))) %>% 
  gather(key = "system", value = "mean", -year) %>% 
  mutate(system = recode(system, "pca225" = "Tax Deduction", "pca227" = "Tax Credit"))

ggplot(deduction, aes(x = factor(year), y = mean)) +
  geom_bar(aes(fill = system), stat = "identity", position = "dodge", color = "black") + 
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Year", y = "Frequency of Deduction") +
  my_theme

## ---- MTR
incvec <- seq(0, 50000, by = 100)
mtrdt <- data.frame(
  income = rep(incvec, 2),
  year = c(rep(2013, length(incvec)), rep(2014, length(incvec)))
)
mtrdt$mtr <- with(mtrdt, mapply(mtrcal, income, year))
mtrdt$year <- factor(mtrdt$year)

ggplot(mtrdt, aes(x = income, y = mtr)) +
  geom_step(aes(color = year), size = 1) +
  labs(x = "Labor income", y = "Marginal Tax Rate", caption = "Black dashed line is tax credit rate") +
  geom_hline(aes(yintercept = 0.15), color = "black", linetype = 2, size = 1) +
  my_theme

## ---- deductive_credit_amount
bounds <- seq(-1057.5, 127.5, 5)
bounds.median <- NULL
for (i in 1:length(bounds) - 1) {
  bounds.median[i] <- bounds[i] + (bounds[i+1] - bounds[i])/2
}

benefit13 <- nastab %>% 
  filter(year == 2013 & pca226 > 0) %>%
  select(pid, pca226)

benefit14 <- nastab %>% 
  filter(year == 2014 & !is.na(pca228)) %>%
  select(pid, pca228)

benefitdt <- benefit13 %>% inner_join(benefit14, by = "pid") %>% 
  mutate(diff = pca228 - pca226) %>% 
  with(table(cut(diff, breaks = bounds, labels = bounds.median))) %>% 
  data.frame() %>% 
  mutate(Var1 = as.numeric(as.character(Var1)))

ggplot(filter(benefitdt, Var1 > -500), aes(x = Var1, y = Freq)) +
  geom_point() +
  geom_line(group = 1) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  labs(
    x = "Tax Refund in 2014 - Tax Refund in 2013",
    y = "Count", 
    caption = sprintf("N = %3.0f. Bin width is 10.", sum(benefitdt$Freq))) +
  my_theme

## ---- deductive_credit_amount_imputed
impute.benefit13 <- nastab %>% 
  filter(year == 2013  & total.g > 0 & 1 - deductive.price != 0.15) %>% 
  mutate(
    impute_refund13 = total.g*(1 - deductive.price),
    refund13 = pca225
  ) %>% 
  select(pid, impute_refund13, refund13, deductive.price)

impute.benefit14 <- nastab %>% 
  filter(year == 2014) %>% 
  mutate(
    impute_refund14 = total.g*0.15,
    refund14 = pca227
  ) %>%
  select(pid, impute_refund14, refund14)

impute.benefitdt <- inner_join(impute.benefit13, impute.benefit14, by = "pid") %>% 
  mutate(diff = impute_refund14 - impute_refund13)

bounds <- seq(-627.5, 917.5, 1)
bounds.median <- NULL
for (i in 1:length(bounds) - 1) {
  bounds.median[i] <- bounds[i] + (bounds[i+1] - bounds[i])/2
}

impute.benefit.y <- impute.benefitdt %>% 
  filter(refund13 == 1) %>% 
  with(table(cut(diff, breaks = bounds, labels = bounds.median))) %>% 
  data.frame() %>% 
  mutate(
    Var1 = as.numeric(as.character(Var1)),
    refund13 = 1
  )

impute.benefit.n <- impute.benefitdt %>% 
  filter(refund13 == 0) %>% 
  with(table(cut(diff, breaks = bounds, labels = bounds.median))) %>% 
  data.frame() %>% 
  mutate(
    Var1 = as.numeric(as.character(Var1)),
    refund13 = 0
  )

impute.benefit.count <- bind_rows(impute.benefit.y, impute.benefit.n)

ggplot(
  filter(impute.benefit.count, 75 >= Var1 & Var1 >= -75), 
  aes(x = Var1, y = Freq, color = factor(refund13))) +
  geom_point(aes(color = factor(refund13))) +
  geom_line(aes(group = factor(refund13))) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  labs(
    x = "Imputed Tax Refund in 2014 - Imputed Tax Refund in 2013",
    y = "Count", 
    caption = "A unit of x-axis is 10000. Bin width is 10,000 won.") +
  scale_color_manual(
    values = c("grey50", "red"),
    labels = c("No", "Yes"),
    name = "Refund in 2013"
  ) +
  my_theme

## ---- propentisity_deduction
prop.reg <- pca225 ~ inc_bb1 + p_page + I(p_page^2) + factor(p_pedu) + 
  p_pgen + factor(h_b10)

est.prop.reg <- nastab %>% 
  filter(year == 2013) %>% 
  glm(prop.reg, family = binomial("logit"), data = .)

data13 <- nastab %>% 
  filter(year == 2013) %>% 
  mutate(
    prop = predict(est.prop.reg, newdata = filter(nastab, year == 2013), "response")
  )

ggplot(data13, aes(x = prop, fill = factor(pca225))) + 
  geom_histogram(position = "dodge") + 
  my_theme

## ---- reg_giverate
giving.reg <- giving.ratio ~ loss*prop + gain*prop + 
  p_page + I(p_page^2) + I(log(inc_bb1 + 1))+ I(log(inc13 + 1)) + p_pgen
    
cut.data13 <- data13 %>% 
  filter(total.g > 0) %>%
  mutate(
    give13 = log(total.g + 1),
    mtr = 1 - deductive.price,
    inc13 = inc_bb1
  ) %>% 
  select(pid, prop, give13, mtr, inc13)

nastab %>% 
  filter(year == 2014) %>% 
  left_join(cut.data13, by = "pid") %>%
  mutate(
    giving.ratio = log(total.g + 1)/give13,
    loss = if_else(mtr > 0.151, 1, 0),
    gain = if_else(mtr < 0.15, 1, 0)
  ) %>% 
  lm(giving.reg, data = .) %>% 
  summary()
