
## ---- library
library(xfun)
xfun::pkg_attach2(c("readstata13", "tidyverse", "rlist", "patchwork"))
xfun::pkg_attach2(c("plm", "lmtest", "sandwich", "lfe", "Formula"))
source("script/R/00-analysis_functions.r")

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
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.caption = element_text(size = 11),

    # setting: axis
    axis.text = element_text(color = "black", size = 13),
    axis.title = element_text(size = 13),
    axis.ticks.length = unit(0.25, "cm"),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.line = element_line(),

    # setting: legend
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.5, "cm"),
    #legend.background = element_rect(color = "black"),
    legend.position = "bottom"
  )


## ---- ReadData
mtrdt <- read_csv("data/origin/mtrdt.csv") %>%
  mutate(price = 1 - MTR) %>%
  arrange(year, lower_income_10000won) %>%
  group_by(year) %>%
  mutate(to_next_price = dplyr::lead(lower_income_10000won))

df <- read.dta13("data/shaped.dta") %>%
  data.frame() %>%
  mutate(
    log_price = log(price),
    log_lprice = log(lprice),
    log_iv1price = log(iv1price),
    log_iv2price = log(iv2price),
    log_iv3price = log(iv3price),
    log_total_g = log(i_total_giving + 1),
    log_pinc_all = log(lincome + 100000),
  ) %>%
  filter(year >= 2012 & age >= 24) %>%
  mutate(price = round(price, 2)) %>%
  left_join(mtrdt, by = c("year", "price")) %>%
  mutate(dist_to_next_price = to_next_price - lincome)

## ---- DensityInc
df %>%
  dplyr::filter(year < 2014 & lincome - i_total_giving > 0) %>%
  mutate(segment = round(lincome / 100, 0) * 100) %>%
  dplyr::filter(segment <= 12000) %>%
  ggplot() +
    # geom_histogram(binwidth = 100, color = "black", fill = "grey80") +
    geom_density(
      aes(
        x = (lincome - i_total_giving),
        color = "Annual taxable income - Annual donations"),
      size = 1) +
    geom_density(aes(x = lincome, color = "Annual taxable income"), size = 1) +
    geom_vline(aes(xintercept = 1200), linetype = 2, size = 1) +
    geom_vline(aes(xintercept = 4600), linetype = 2, size = 1) +
    geom_vline(aes(xintercept = 8800), linetype = 2, size = 1) +
    labs(x = "Income (income < 12000)") +
    my_theme

## ---- ScatterbwIncomeDonation
df %>%
  dplyr::filter(year < 2014) %>%
  mutate(segment = round(lincome / 100, 0) * 100) %>%
  group_by(segment) %>%
  summarize(mean = mean(i_total_giving, na.rm = TRUE)) %>%
  dplyr::filter(segment <= 12000) %>%
  ggplot(aes(x = segment, y = mean)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_vline(aes(xintercept = 1200), linetype = 2, size = 1) +
    geom_vline(aes(xintercept = 4600), linetype = 2, size = 1) +
    geom_vline(aes(xintercept = 8800), linetype = 2, size = 1) +
    labs(
      x = "Segment of annual taxable income",
      y = "Mean donation levels in 2013 and 2014") +
    my_theme

## ---- ScatterbwDistanceDonation
full <- df %>%
  dplyr::filter(0.62 < price & year < 2014) %>%
  mutate(segment = round(dist_to_next_price / 100, 0) * 100) %>%
  group_by(segment) %>%
  summarize(mean = mean(i_total_giving, na.rm = TRUE)) %>%
  ggplot(aes(x = segment, y = mean)) +
    geom_point(size = 2, alpha = 0.8) +
    my_theme
  
sub <- df %>%
  dplyr::filter(0.65 < price & year < 2014) %>%
  mutate(segment = round(dist_to_next_price / 100, 0) * 100) %>%
  group_by(price, segment) %>%
  summarize(mean = mean(i_total_giving, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(price = factor(
    price, labels = sprintf("giving price = %1.2f", unique(price))
  )) %>%
  ggplot(aes(x = segment, y = mean)) +
    geom_point(size = 2, alpha = 0.8) +
    facet_wrap(~price, ncol = 1) +
    my_theme

(full + sub) &
  labs(
    x = "Segment of distance to next lower giving price",
    y = "Mean donation levels") &
  plot_annotation(tag_levels = "A")
