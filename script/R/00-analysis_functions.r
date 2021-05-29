
## ---- Analysis Functions

#' Using felm function (lfe package), we estimate fixed effect model with individual cluster standard errors.
#' outcome: character vector which is an outcome variable
#' data: dataset
#' implied: estimate implied elasticity and implement F-test for zero implied elasticity using waldtest function (in lfe package)
#' evaluate_at: numric value which estimate implied elasticity. defualt is average value of outcome.

est_felm <- function(y, x, z = list(0), fixef = list(0), cluster = list(0), data, 
					 implied_e = FALSE, price_var = NULL, income_var = "log_pinc_all") {

	estimate <- implied_p <- implied_y <- NULL

	replace_list <- list(y = y, x = x, z = z, fixef = fixef, cluster = cluster) %>% expand.grid()
	regset <- 1:nrow(replace_list) %>% 
		purrr::map(~substitute(y ~ x | fixef | z | cluster, unlist(replace_list[.,]))) %>% 
		setNames(paste("reg", 1:nrow(replace_list), sep = ""))

	estimate <- regset %>% purrr::map(~lfe::felm(as.formula(.), data = data))
	
	if (implied_e) {
		
		implied_p <- estimate %>%
			purrr::map(~list(model = ., dbar = 1/mean(.$response))) %>%  
			purrr::map(~list(model = .$model, rhs = matrix(c(.$dbar, numeric(length(coef(.$model)) - 1)), nrow = 1))) %>% 
			purrr::map(~list(
				coef =  .$rhs[1] * coef(.$model)[str_detect(names(coef(.$model)), price_var)], 
				test = lfe::waldtest(.$model, .$rhs)
			)) %>% 
			purrr::map(function(x)
				tibble(
					vars = "Implied price elasticity",
					coef = x$coef,
					se = abs(x$coef)/sqrt(x$test["F"]),
					f = x$test["F"],
					p = x$test["p.F"]
				)
			)

		implied_y <- estimate %>% 
			purrr::map(~list(model = ., dbar = 1/mean(.$response))) %>%  
	  		purrr::map(~list(model = .$model, rhs = matrix(c(0, .$dbar, numeric(length(coef(.$model)) - 2)), nrow = 1))) %>% 
  			purrr::map(~list(
				coef =  .$rhs[2] * coef(.$model)[str_detect(names(coef(.$model)), income_var)], 
				test = lfe::waldtest(.$model, .$rhs)
			)) %>% 
  			purrr::map(function(x)
    			tibble(
					vars = "Implied income elasticity",
					coef = x$coef,
		      		se = abs(x$coef)/sqrt(x$test["F"]),
					f = x$test["F"],
      				p = x$test["p.F"]
    			)
			)	 

	}

	return(list(model = regset, est = estimate, imp_elast = list(price = implied_p, inc = implied_y)))
	
}

## ---- Regression Tables
felm_regtab_nonchar <- function(
	felmobj, keep_coef = NULL, rm_coef = NULL, label_coef = NULL,
	keep_stat = c("N", "R-squared", "Adjusted R-squared")
) {

	coeftab <- felmobj %>%
		purrr::map(~summary(.)$coefficients) %>%
		purrr::map(function(x)
			tibble(
				vars = rownames(x),
				coef = x[,1],
				se = x[,2],
				p = x[,4]
			)
		)

	if (!is.null(keep_coef)) {
		coeftab <- coeftab %>% purrr::map(function(x) x[str_detect(x$vars, keep_coef %>% paste(collapse = "|")),])
	}
	
	if (!is.null(rm_coef)) {
		coeftab <- coeftab %>% purrr::map(function(x) x[!str_detect(x$vars, keep_coef %>% paste(collapse = "|")),])
	}
	
	if (!is.null(label_coef)) {
		coeftab <- coeftab %>% purrr::map(function(x) mutate(x, vars = recode(vars, !!!label_coef, .default = vars)))
	}
	
	stattab <- felmobj %>% 
		purrr::map(~tribble(
			~vars, ~stat, ~val,
			"N", "stat", nobs(.),
			"R-squared", "stat", summary(.)$r.squared,
			"Adjusted R-squared", "stat", summary(.)$adj.r.squared
		)) %>% 
		purrr::map(function(x) x %>% dplyr::filter(vars %in% keep_stat))
	
	return(list(coef = coeftab, stat = stattab))
	
}

regtab_char <- function(listobj) {

  tab <- listobj %>%
		purrr::map(function(x)
			mutate(
				x,
				coef = case_when(
					p <= .01 ~ sprintf("%1.3f***", coef),
					p <= .05 ~ sprintf("%1.3f**", coef),
					p <= .1 ~ sprintf("%1.3f*", coef),
					TRUE ~ sprintf("%1.3f", coef)
				),
				se = sprintf("(%1.3f)", se)
			) %>% 
			dplyr::select(vars, coef, se) %>% 
			pivot_longer(-vars, names_to = "stat", values_to = "val")
		) %>% 
		reduce(full_join, by = c("vars", "stat")) %>%
		setNames(c("vars", "stat", paste("reg", 1:length(listobj), sep = "")))
	
		return(tab)

}

fullset_tab <- function(
	estelaobj, combined = TRUE,
	keep_coef = NULL, rm_coef = NULL, label_coef = NULL,
	keep_stat = c("N", "R-squared", "Adjusted R-squared"),
	addlines = NULL
) {

	tab_coef_b <- tab_coef_c <- NULL
	
	# coefficient and stats tabulation
	tab_a <- felm_regtab_nonchar(
		estelaobj$est, keep_coef = keep_coef, rm_coef = rm_coef, label_coef = label_coef, keep_stat = keep_stat
	)
	tab_coef_a <- regtab_char(tab_a$coef)

	# implied elasticity tabulation
	if (!is.null(estelaobj$imp_elast$price)) {
		tab_coef_b <- regtab_char(estelaobj$imp_elast$price)
	}
	if (!is.null(estelaobj$imp_elast$inc)) {
		tab_coef_c <- regtab_char(estelaobj$imp_elast$inc)
	}

	# characteristic tabulation of regression stats
	tab_stat <- tab_a$stat %>% 
		purrr::map(function(x)
			mutate(x,
				val = case_when(
					vars == "N" ~ sprintf("%1d", as.integer(val)),
					TRUE ~ sprintf("%1.3f", val)
				)
			)
		) %>% 
		reduce(full_join, by = c("vars", "stat")) %>%
		setNames(c("vars", "stat", paste("reg", 1:length(tab_a$stat), sep = "")))
	
	# combined table
	if (combined) {
		tab_comb <- bind_rows(
			tab_coef_a,
			tab_coef_b,
			tab_coef_c,
			addlines,
			tab_stat
		)
	}

	return(list(
		set = tab_comb,
		parts = list(coef_a = tab_coef_a, coef_b = tab_coef_b, coef_c = tab_coef_c, stats = tab_stat)
	))

}
