#3PoliticalViews

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

## ---- ConstructPoliticalViewID
sdpid <- df %>% 
  mutate(political_pref = as.numeric(as.character(political_pref))) %>% 
  group_by(pid) %>% 
  summarize_at(vars(political_pref), list(~sd(., na.rm = TRUE))) %>% 
  with(summary(political_pref))

politicid <- df %>% 
  select(pid, political_pref) %>% 
  distinct(.keep_all = TRUE) %>% 
  rename(politicid = political_pref) %>% 
  drop_na()

estdf <- df %>% left_join(politicid, by = "pid")

## ---- HistogramPoliticalViewID
ggplot(politicid, aes(x = as.numeric(as.character(politicid)))) + 
  geom_histogram(stat = "count", fill = "grey80", color = "black") +
  labs(x = "Political View Index", y = "Count") +
  my_theme

## ---- ScatterDonationsPoliticalViewID
idavgdt <- estdf %>% 
  group_by(pid) %>% 
  summarize_at(vars(i_total_giving), list(~mean(., na.rm = TRUE)))

plotdt <- idavgdt %>% 
  left_join(politicid, by = "pid") %>% 
  mutate(politicid = as.numeric(as.character(politicid)))

ggplot(plotdt, aes(x = politicid, y = i_total_giving)) + 
  geom_jitter(size = 1, alpha = 0.5) +
  labs(x = "Political View Index", y = "Individual Average Total Giving") +
  my_theme

## ---- EstimateElasticityByPoliticalView
reg <- log_total_g ~ log_price + log_pinc_all + 
  age + factor(year):factor(educ) + factor(year):factor(gender) + factor(living_area) | pid + year | 0 | pid

politicreg <- 1:5 %>% 
  purrr::map(~lfe::felm(reg, data = subset(estdf, year >= 2012 & as.numeric(as.character(politicid)) == .)))
n.politicreg <- politicreg %>% purrr::map(~sprintf("%1d", nobs(.))) %>% as_vector()

#tabulation
keep <- c("log_price") %>% paste(collapse = "|")
varlist <- exprs(
  stat == "se" ~ "",
  vars == "log_price" ~ "ln(giving price)"
)

coef.politicreg <- politicreg %>% 
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

tab.politicreg <- rbind(as.matrix(coef.politicreg), c("Obs", n.politicreg)) %>% data.frame()

## ---- EstimateInteractionByPoliticView
intreg <- log_total_g ~ log_price + log_price:politicid + log_pinc_all + 
  age + factor(year):factor(educ) + factor(year):factor(gender) + factor(living_area) | pid + year | 0 | pid

dfset <- list(
  base = estdf %>% filter(year >= 2012),
  limit = estdf %>% filter(year == 2012 | year == 2013)
)

est.intreg <- dfset %>% purrr::map(~lfe::felm(intreg, data = .))
n.intreg <- est.intreg %>% purrr::map(~sprintf("%1d", nobs(.))) %>% as_vector()

#tabulation
keep <- c("log_price") %>% paste(collapse = "|")
varlist <- exprs(
  str_detect(vars, "politicid1") ~ "X Extreme Right",
  str_detect(vars, "politicid2") ~ "X Right",
  str_detect(vars, "politicid4") ~ "X Left",
  str_detect(vars, "politicid5") ~ "X Extreme Left",
  vars == "log_price" ~ "ln(giving price)"
)
varorder <- exprs(
  str_detect(vars, "politicid1") ~ 1,
  str_detect(vars, "politicid2") ~ 2,
  str_detect(vars, "politicid4") ~ 3,
  str_detect(vars, "politicid5") ~ 4,
  TRUE ~ 0
)

coef.intreg <- est.intreg %>% 
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
	mutate(order = case_when(!!!varorder), vars = case_when(!!!varlist)) %>%
  .[with(., order(order)),] %>% 
  select(-stat, -order)

tab.intreg <- rbind(as.matrix(coef.intreg), c("Obs", n.intreg)) %>% data.frame()
