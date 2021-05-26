
## ---- Regression Tables
ols_regtab <- function(lmobj, keep_coef = NULL, rm_coef = NULL, label_coef = NULL) {

  coeftab <- lmobj %>%
	purrr::map(~summary(.)$coefficients) %>%
    purrr::map(function(x)
      	data.frame(
			vars = rownames(x),
			coef = x[,1],
			se = x[,2],
			p = x[,4],
			stringsAsFactors = FALSE
		) %>% 
      	mutate(
			coef = case_when(
				p <= .01 ~ sprintf("%1.3f***", coef),
				p <= .05 ~ sprintf("%1.3f**", coef),
				p <= .1 ~ sprintf("%1.3f*", coef),
				TRUE ~ sprintf("%1.3f", coef)
			),
			se = sprintf("(%1.3f)", se)
		) %>% 
		dplyr::select(-p) %>% 
		pivot_longer(-vars, names_to = "stat", values_to = "val")
	) %>% 
	reduce(full_join, by = c("vars", "stat")) %>%
	setNames(c("vars", "stat", paste("reg", 1:length(lmobj), sep = "")))

  	if (!is.null(keep_coef)) coeftab <- coeftab[str_detect(coeftab$vars, keep_coef %>% paste(collapse = "|")),]
  	if (!is.null(rm_coef)) coeftab <- coeftab[!str_detect(coeftab$vars, rm_coef %>% paste(collapse = "|")),]  
	if (!is.null(label_coef)) coeftab <- mutate(coeftab, vars = recode(vars, !!!label_coef, .default = vars))

	stattab <- lmobj %>% 
  		purrr::map(~data.frame(
    		vars = c("N", "R-squared"),
    		stat = c("stat", "stat"),
    		val = c(sprintf("%3d", nobs(.)), sprintf("%1.3f", summary(.)$r.squared))
  		)) %>% 
  		reduce(full_join, by = c("vars", "stat")) %>% 
		setNames(c("vars", "stat", paste("reg", 1:length(lmobj), sep = "")))
  
  	return(list(coef = coeftab, stat = stattab))

}
