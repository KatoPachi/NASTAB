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
  labs(x = "Year", y = "Frequency of Deduction")
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