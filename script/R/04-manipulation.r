
#+ library
library(xfun)
xfun::pkg_attach2(c("readstata13", "tidyverse", "rlist", "patchwork"))
xfun::pkg_attach2(c("plm", "lmtest", "sandwich", "lfe", "Formula"))
source("script/R/00-analysis_functions.r")

#+ GGTemp
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


#+ ReadData
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
  mutate(
    blacket2 = if_else(lincome >= 1200, 1, 0),
    run2 = lincome - 1200,
    blacket3 = if_else(lincome >= 4600, 1, 0),
    run3 = lincome - 4600,
    blacket4 = if_else(lincome >= 8800, 1, 0),
    run4 = lincome - 8800,
  ) %>%
  filter(year >= 2012 & age >= 24) %>%
  mutate(price = round(price, 2)) %>%
  left_join(mtrdt, by = c("year", "price")) %>%
  mutate(dist_to_next_price = to_next_price - lincome)

#+ DensityInc
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

#+ ScatterbwIncomeDonation
df %>%
  dplyr::filter(year < 2014) %>%
  mutate(segment = round(lincome / 100, 0) * 100) %>%
  group_by(segment) %>%
  summarize(mean = mean(log_total_g, na.rm = TRUE)) %>%
  dplyr::filter(segment <= 12000) %>%
  ggplot(aes(x = segment, y = mean)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_vline(aes(xintercept = 1200), linetype = 2, size = 1) +
    geom_vline(aes(xintercept = 4600), linetype = 2, size = 1) +
    geom_vline(aes(xintercept = 8800), linetype = 2, size = 1) +
    labs(
      x = "Segment of annual taxable income",
      y = "Mean logged donation levels in 2012 and 2013") +
    my_theme

#+ ScatterbwDistanceDonation
full <- df %>%
  dplyr::filter(0.62 < price & year < 2014) %>%
  mutate(segment = round(dist_to_next_price / 100, 0) * 100) %>%
  group_by(segment) %>%
  summarize(mean = mean(log_total_g, na.rm = TRUE)) %>%
  ggplot(aes(x = segment, y = mean)) +
    geom_point(size = 2, alpha = 0.8) +
    my_theme
  
sub <- df %>%
  dplyr::filter(0.65 < price & year < 2014) %>%
  mutate(segment = round(dist_to_next_price / 100, 0) * 100) %>%
  group_by(price, segment) %>%
  summarize(mean = mean(log_total_g, na.rm = TRUE)) %>%
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
    y = "Mean logged donation levels in 2012 and 2013") &
  plot_annotation(tag_levels = "A")

#+ distregs
distregs <- list(
  reg1 = log_total_g ~ dist_to_next_price + log_pinc_all | 0 | 0 | pid,
  reg2 = log_total_g ~ dist_to_next_price + log_pinc_all | year | 0 | pid,
  reg3 = log_total_g ~ dist_to_next_price + log_pinc_all | year + pid | 0 | pid,
  reg4 = log_total_g ~ log(dist_to_next_price) + log_pinc_all +
    age + sqage + factor(year):factor(educ) + factor(year):factor(gender) +
    factor(year):factor(living_area) | year + pid | 0 | pid
)

est_distregs <- distregs %>%
  purrr::map(~felm(., data = df %>% dplyr::filter(year < 2014)))

est_distregs_int <- distregs %>%
  purrr::map(
    ~felm(., data = df %>% dplyr::filter(year < 2014 & i_ext_giving == 1))
  )

est_distregs_ext <- distregs %>%
  purrr::map(~update(as.Formula(.), i_ext_giving ~ .)) %>%
  purrr::map(~felm(., data = df %>% dplyr::filter(year < 2014)))

#+ lpriceIV
lpregs <- list(
  reg1 = log_total_g ~ log_pinc_all |
    year + pid | (log_lprice ~ log_price) | pid,
  reg2 = log_total_g ~ log_pinc_all + age + sqage |
    year + pid | (log_lprice ~ log_price) | pid,
  reg3 = log_total_g ~ log_pinc_all + age + sqage + factor(year):factor(educ) |
    year + pid | (log_lprice ~ log_price) | pid,
  reg4 = log_total_g ~ log_pinc_all + age + sqage + factor(year):factor(educ) +
    factor(year):factor(gender) |
    year + pid | (log_lprice ~ log_price) | pid,
  reg5 = log_total_g ~ log_pinc_all + age + sqage + factor(year):factor(educ) +
    factor(year):factor(gender) + factor(year):factor(living_area) |
    year + pid | (log_lprice ~ log_price) | pid
)

est_lpregs <- lpregs %>% purrr::map(~felm(., data = df))

est_lpregs_int <- lpregs %>%
  purrr::map(
    ~felm(., data = df %>% dplyr::filter(i_ext_giving == 1))
  )

est_lpregs_ext <- lpregs %>%
  purrr::map(~update(as.Formula(.), i_ext_giving ~ .)) %>%
  purrr::map(~felm(., data = df))

#+ lpriceIV2
df2 <- df %>%
  mutate(dist_to_next_price = if_else(year >= 2014, 0, dist_to_next_price))

est_lpregs2 <- lpregs %>%
  purrr::map(~update(as.Formula(.), . ~ . + dist_to_next_price)) %>%
  purrr::map(~felm(., data = df2))

est_lpregs2_int <- lpregs %>%
  purrr::map(~update(as.Formula(.), . ~ . + dist_to_next_price)) %>%
  purrr::map(
    ~felm(., data = df %>% dplyr::filter(i_ext_giving == 1))
  )

est_lpregs2_ext <- lpregs %>%
  purrr::map(~update(as.Formula(.), i_ext_giving ~ . + dist_to_next_price)) %>%
  purrr::map(~felm(., data = df))

#+ lprice3
lpregs3 <- list(
  reg1 = log_total_g ~ log_pinc_all |
    year + pid | (log_lprice ~ log_price * dist_to_next_price) | pid,
  reg2 = log_total_g ~ log_pinc_all + age + sqage |
    year + pid | (log_lprice ~ log_price * dist_to_next_price) | pid,
  reg3 = log_total_g ~ log_pinc_all + age + sqage + factor(year):factor(educ) |
    year + pid | (log_lprice ~ log_price * dist_to_next_price) | pid,
  reg4 = log_total_g ~ log_pinc_all + age + sqage + factor(year):factor(educ) +
    factor(year):factor(gender) |
    year + pid | (log_lprice ~ log_price * dist_to_next_price) | pid,
  reg5 = log_total_g ~ log_pinc_all + age + sqage + factor(year):factor(educ) +
    factor(year):factor(gender) + factor(year):factor(living_area) |
    year + pid | (log_lprice ~ log_price * dist_to_next_price) | pid
)

df3 <- df %>%
  mutate(dist_to_next_price = if_else(year >= 2014, 1, dist_to_next_price))

est_lpregs3 <- lpregs3 %>%
  purrr::map(~felm(., data = df3))

est_lpregs3_int <- lpregs3 %>%
  purrr::map(~felm(., data = df3 %>% dplyr::filter(i_ext_giving == 1)))

est_lpregs3_ext <- lpregs3 %>%
  purrr::map(~update(Formula(.), i_ext_giving ~ .)) %>%
  purrr::map(~felm(., data = df3))