
library(tidyverse)
library(flextable)
library(officer)

outreg2flex <- function(file_path, cnames = NULL, vnames = NULL, nrow_foot = 2) {
  
  dt <- read.csv(file_path, sep = "\t", stringsAsFactors = FALSE)

  old_col <- colnames(dt)
  
  if (is.null(cnames)) {
    new_col <- unlist(dt[1,])
  } else {
    new_col <- cnames
  }

  attr(new_col, "names") <- NULL

  col <- vector("list", length(old_col))
  names(col) <- old_col
  for (i in 1:length(old_col)) {col[[i]] <- new_col[i]}

  if (!is.null(vnames)) {
    for (i in 1:length(varnames)) {
      dt[str_detect(dt$X, varnames[[i]][1]), "X"] <- varnames[[i]][2]
    }
  }
  
  if (nrow_foot > 0) {
    foot <- tail(dt, n=nrow_foot)[,1]
    start_foot <- nrow(dt) - (nrow_foot - 1)
    end_foot <- nrow(dt)
    body <- dt[-c(1, 2, start_foot:end_foot),]
  } else {
    body <- dt[-c(1, 2),]
  }

  flex <- flextable(body) %>% set_header_labels(values = col)
  
  if (nrow_foot > 0) {
    flex <- flex %>% add_footer_row(values = foot[1], colwidth = ncol(body))
  }

  if (nrow_foot > 1) {
    for (i in 2:length(foot)) {
      flex <- flex %>% add_footer_row(values = foot[i], top = FALSE, colwidth = ncol(body))
    }
  }

  flex <- flex %>% 
    merge_h(part = "header") %>% 
    align(j = 1, align = "left", part = "all") %>% 
    align(j = 2:ncol(body), align = "center", part = "all") %>% 
    autofit() %>% 
    border_remove() %>%
    hline_top(part = "header", border = fp_border()) %>%
    hline_bottom(part = "head", border = fp_border()) %>%
    hline_bottom(part = "body", border = fp_border())

  return(flex)
}

#'
#' how to set vnames
#' varnames <- list(
#'  c("log_PPP_healthbdg", "Health Expenses Per Capita (Log)"),
#'  c("log_price", "Price of Monetary Donations (Log)")
#' )



outreg2flex("_assets/stata/main_pubbdg.txt")
outreg2flex("_assets/stata/main_healthbdg.txt")

outreg2flex("_assets/stata/totalef_5scale.txt", nrow_foot = 0)
outreg2flex("_assets/stata/totalef_3scale.txt", nrow_foot = 0)
