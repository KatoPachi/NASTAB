#0summary

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
    ) %>% 
    filter(year >= 2012)

## ---- SummaryOutcome
avgext <- df %>% 
	group_by(year) %>% 
	summarize_at(vars(i_ext_giving), list(mu=~mean(., na.rm = TRUE))) 

avgint <- df %>% 
	filter(i_ext_giving == 1) %>% 
	group_by(year) %>% 
	summarize_at(vars(i_total_giving), list(mu=~mean(., na.rm = TRUE)))

ggplot(avgext, aes(x = year, y = mu)) +
	geom_bar(stat = "identity", fill = "grey80", color = "black") +
	geom_point(data = avgint, aes(x = year, y = mu/1000), size = 2, color = "blue") +
	geom_line(data = avgint, aes(x = year, y = mu/1000), size = 1, color = "blue") +
	geom_vline(aes(xintercept = 2013.5), color = "red", linetype = 2, size = 1) +
	scale_y_continuous(sec.axis = sec_axis(~.*1000, name = "Average Donations (Intensive Margin)")) +
	scale_x_continuous(breaks = seq(2012, 2018, 1)) +
	labs(x = "Year", y = "Proportion of Donors") +
	my_theme

## ---- SummaryCovariate
sumcov <- df %>% 
	mutate(gender = gender - 1) %>% 
	group_by(year) %>% 
	summarize_at(vars(gender, age, lincome), list(~mean(., na.rm = TRUE))) %>% 
	mutate(year = sprintf("Y%1d", year)) %>%
	pivot_longer(-year, names_to = "vars", values_to = "value") %>% 
	pivot_wider(names_from = "year", values_from = "value")

sumedu <- df %>% 
	filter(!is.na(educ)) %>%
	group_by(year, educ) %>% 
	summarize(N = n()) %>% 
	group_by(year) %>% 
	mutate(prop = N/sum(N)) %>% 
	select(-N) %>% 
	mutate(
		educ = case_when(
			educ == 1 ~ "Junior High School Graduate", 
			educ == 2 ~ "High School Graduate", 
			TRUE ~ "University Graduate"),
		year = sprintf("Y%1d", year)
	) %>% 
	pivot_wider(names_from = "year", values_from = "prop") %>% 
	rename(vars = educ)

sumN <- df %>% 
	group_by(year) %>% 
	summarize_at(vars(pid, hhid), list(~length(unique(.)))) %>% 
	mutate(year = sprintf("Y%1d", year)) %>%
	pivot_longer(-year, names_to = "vars", values_to = "N") %>% 
	pivot_wider(names_from = "year", values_from = "N")

tabsum <- rbind(sumcov, sumedu) %>% rbind(sumN)

## ---- TrustIndex
reg <- trust_politician ~ factor(year)*factor(living_area) + factor(year)
indexreg <- plm(reg, data = subset(df, year >= 2015), model = "within", index = c("pid", "year"))
feval <- fixef(indexreg)

indexdf <- data.frame(
  pid = as.numeric(attr(feval, "names")),
  trustid = scale(c(feval))
)

estdf <- df %>% left_join(indexdf, by = "pid")

## ---- CorrDonationsTrust
avgdonate <- estdf %>% 
	group_by(pid) %>% 
	summarize_at(vars(i_total_giving), list(~mean(., na.rm = TRUE)))

plotdt <- left_join(avgdonate, indexdf, by = "pid")

ggplot(plotdt, aes(x = trustid, y = i_total_giving)) + 
	geom_point(size = 1.5, alpha = 0.5) +
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
