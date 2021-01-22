#4TaxWelfare

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
  ungroup()
df <- df %>% 
  mutate(
    lag1iv = log(price/lag1_price),
    lag2iv = log(price/lag2_price),
    lag3iv = log(price/lag3_price),
    lag4iv = log(price/lag4_price),
  )

## ---- ConstructBalanceTaxWelfare
estdf <- df %>% 
	mutate(
		ideal_welfare = case_when(
			opt_welfare_tax <= 3 ~ 3,
			opt_welfare_tax <= 6 ~ 2,
			opt_welfare_tax <= 9 ~ 1
		),
		now_welfare = case_when(
			avg_welfare_tax <= 3 ~ 3,
			avg_welfare_tax <= 6 ~ 2,
			avg_welfare_tax <= 9 ~ 1
		),
		ideal_tax = case_when(
			opt_welfare_tax %in% c(3, 6, 9) ~ 3,
			opt_welfare_tax %in% c(2, 5, 8) ~ 2,
			opt_welfare_tax %in% c(1, 4, 7) ~ 1,
		),
		now_tax = case_when(
			avg_welfare_tax %in% c(3, 6, 9) ~ 3,
			avg_welfare_tax %in% c(2, 5, 8) ~ 2,
			avg_welfare_tax %in% c(1, 4, 7) ~ 1,
		),
		ideal_balance = case_when(
			opt_welfare_tax == 1 ~ 2,
			opt_welfare_tax %in% c(2, 4) ~ 1,
			opt_welfare_tax %in% c(3, 5, 7) ~ 0,
			opt_welfare_tax %in% c(6, 8) ~ -1,
			opt_welfare_tax == 9 ~ -2,
		),
		now_balance = case_when(
			avg_welfare_tax == 1 ~ 2,
			avg_welfare_tax %in% c(2, 4) ~ 1,
			avg_welfare_tax %in% c(3, 5, 7) ~ 0,
			avg_welfare_tax %in% c(6, 8) ~ -1,
			avg_welfare_tax == 9 ~ -2,
		),
		
	) %>% 
	mutate(
		diff_welfare = now_welfare - ideal_welfare,
		diff_tax = now_tax - ideal_tax,
		diff_balance = now_balance - ideal_balance
	) %>% 
	mutate(
		ideal_balance3 = case_when(
			ideal_balance < 0 ~ -1,
			ideal_balance == 0 ~ 0,
			ideal_balance >0 ~ 1
		),
		now_balance3 = case_when(
			now_balance < 0 ~ -1,
			now_balance == 0 ~ 0,
			now_balance >0 ~ 1
		),
		diff_balance3 = case_when(
			diff_balance < 0 ~ 1,
			diff_balance == 0 ~ 0,
			diff_balance > 0 ~ -1
		)
	)

