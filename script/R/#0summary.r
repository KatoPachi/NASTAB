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

