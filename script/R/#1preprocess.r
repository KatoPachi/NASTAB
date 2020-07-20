
#'
#+ library
package_load <- function(pkg.name){
  if(!require(pkg.name, character.only=TRUE)){
    install.packages(pkg.name)
    library(pkg.name, character.only = TRUE)
  } else {
    library(pkg.name, character.only = TRUE)
  }
}

package_load("tidyverse")
package_load("foreign")
package_load("readstata13")
package_load("stargazer")


#' 
#+ read dataset
nastab <- read.dta13("./data/origin.dta", convert.factors = FALSE) %>%
  data.frame()

#'
#+ function
giving_price <- function(
  data.name = "nastab", income.var = "p_pinc"){
        
    price.dt <- read.csv("./data/mtrdt.csv")
    
    income.dt <- get(data.name) %>%
      dplyr::filter(!is.na(get(income.var))) %>%
      dplyr::select(pid, year, income.var)

    cal <- function(y, t) {
        cutdt <- subset(price.dt, year == t)
        
        cutdt$ind <- 1*(cutdt[,2] <= y)
        t <- with(subset(cutdt, ind == 1), max(MTR))
        p <- 1 - t

        return(p)    
    }
    
    income.dt$price <- with(income.dt, mapply(cal, get(income.var), year))
    return(income.dt)
}

total_giving <- function(f, data.name = "nastab") {
  fielddt <- get(data.name) %>% 
    dplyr::select(
      pid, year, hcr004, hcr007, hcr010, hcr013, hcr016, hcr019) %>%
    tidyr::gather(key = num, value = field, -pid, -year)
    
  amountdt <- get(data.name) %>%
    dplyr::select(
      pid, year, hcr005, hcr008, hcr011, hcr014, hcr017, hcr020) %>%
    tidyr::gather(key = num, value = amount, -pid, -year) %>%
    dplyr::mutate(
      num = dplyr::recode(
        num,
        hcr005 = "hcr004",
        hcr008 = "hcr007",
        hcr011 = "hcr010",
        hcr014 = "hcr013",
        hcr017 = "hcr016",
        hcr020 = "hcr019"
      )
    ) %>% 
    dplyr::mutate_at(c("amount"), list(~ifelse(. == -9, NA, .)))
      
  givingdt <- fielddt %>%
    dplyr::left_join(amountdt, by = c("pid", "year", "num")) %>%
    dplyr::select(-num)

  aggregate.dt <- givingdt %>%
    dplyr::filter(field %in% f) %>%
    dplyr::group_by(pid, year) %>%
    dplyr::summarize(total_giving = sum(amount, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(pid, year, total_giving)
  
  return(aggregate.dt)
  
}

#'
#+ shape dataset
shape.nastab <- nastab %>% 
  dplyr::filter(year > 2007) %>% 
  dplyr::mutate(id = str_sub(pid, start = -2, end = -1)) %>% 
  dplyr::mutate(religious = get(paste("wrgn", id, sep = ""))) %>% 
  dplyr::select(-starts_with("wrgn"), -id) %>% 
  dplyr::mutate_at(c("religious"), list(~ifelse(. == -9, NA, .))) %>% 
  dplyr::select(
    hhid, pid, wave, year, h_b10, p_aa005, p_aa006, p_aa200,
    p_page, p_pedu, p_pgen, pca225, pca226, pca227, pca228, hcr001,
    pinc_all, inc_bb1, religious, p_prel, psa05) %>%
  dplyr::mutate_at(
    c("pca225", "pca227"), list(~ifelse(. == -9, NA, .))) %>%
  dplyr::mutate_at(
    c("pca225", "pca227", "hcr001"), list(~ifelse(. == 1, 1, 0))) %>%
  dplyr::mutate(
    pca226 = case_when(pca225 == 0 ~ 0,
                       pca226 != -9 ~ pca226),
    pca228 = case_when(pca227 == 0 ~ 0,
                       pca228 != -9 ~ pca228)
  )

give.price <- giving_price(i = "inc_bb1") %>% 
  rename(deductive.price = price)

total.donate <- total_giving(c(1, 2, 3, 4, 5, 6, 7)) %>% 
  rename(total.g = total_giving)

gen.donate <- total_giving(c(1, 2, 3, 4, 7)) %>%
  mutate_at(c("total_giving"), list(~ifelse(is.na(.), 0, .))) %>% 
  rename(general.g = total_giving)

rel.donate <- total_giving(c(5, 6)) %>% 
  mutate_at(c("total_giving"), list(~ifelse(is.na(.), 0, .))) %>% 
  rename(religious.g = total_giving)

shape.nastab <- shape.nastab %>% 
  dplyr::left_join(total.donate, by = c("pid", "year")) %>%
  dplyr::left_join(gen.donate, by = c("pid", "year")) %>%
  dplyr::left_join(rel.donate, by = c("pid", "year")) %>%
  dplyr::left_join(give.price, by = c("pid", "year", "inc_bb1")) %>%
  dplyr::mutate_at(
    c("total.g", "general.g", "religious.g"), 
    list(~ifelse(hcr001 == 0, 0, .))) %>% 
  dplyr::filter(hcr001 == 1*(total.g > 0))

#'
#+ save dataset
write_rds(shape.nastab, "./data/shapedt.rds")
write.dta(shape.nastab, "./data/shapedt.dta")