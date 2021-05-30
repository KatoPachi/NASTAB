
## ---- Analysis Functions

#' Using felm function (lfe package), we estimate fixed effect model with individual cluster standard errors.
#' outcome: character vector which is an outcome variable
#' data: dataset
#' implied: estimate implied elasticity and implement F-test for zero implied elasticity using waldtest function (in lfe package)
#' evaluate_at: numric value which estimate implied elasticity. defualt is average value of outcome.

felm_waldtest <- function(felmobj, hypo) {

	# understanding hypothesis structure
	partial_split <- str_split(str_replace_all(hypo, pattern = " ", replacement = ""), pattern = "\\+")
	perfect_split <- partial_split %>% purrr::map(~str_split(., pattern = "\\*"))

	# calculate sample mean of outcome
	dbar <- mean(felmobj$response)

	# make matrix of right hand side
	rhs <- coef(felmobj); rhs[1:length(rhs)] <- 0
	set_rhs <- lapply(1:length(hypo), function(x) return(rhs))

	for (i in 1:length(hypo)) {
	   for (j in 1:length(perfect_split[[i]])) {
		   if (perfect_split[[i]][[j]][[1]] == "imp") {
		      set_rhs[[i]][perfect_split[[i]][[j]][2]] <- 1/dbar
		   } else {
		   	  set_rhs[[i]][perfect_split[[i]][[j]][2]] <- as.numeric(perfect_split[[i]][[j]][[1]])
		   }
	   }
	}

	set_rhs <- set_rhs %>% purrr::map(~matrix(., nrow = 1))
	
	wald <- set_rhs %>% 
		purrr::map(~lfe::waldtest(felmobj, .)) %>% 
		purrr::map(function(x) tibble(f = x["F"], p = x["p.F"])) %>% 
		reduce(bind_rows) %>% 
		mutate(vars = names(hypo))
	
	# calculate coefficients
	eval_text <- perfect_split

	for (i in 1:length(hypo)) {
	   for (j in 1:length(eval_text[[i]])) {
		   eval_text[[i]][[j]][2] <- paste("coef(felmobj)['", eval_text[[i]][[j]][2], "']", sep = "")
	   }
	}

	eval_text <- eval_text %>% 
		purrr::map_depth(2, ~str_replace_all(., pattern = "imp", replacement = "(1/dbar)")) %>% 
		purrr::map_depth(2, ~str_c(., collapse = "*")) %>% 
		purrr::map(~str_c(unlist(.), collapse = "+")) 
	
	coef <- eval_text %>% purrr::map(~eval(parse(text = .))) %>% as_vector()
	coef_df <- tibble(vars = names(hypo), coef = coef)

	# show result
	result_df <- left_join(coef_df, wald, by = "vars") %>% 
		mutate(se = abs(coef)/sqrt(f))
	
	return(result_df)

}


est_felm <- function(y, x, z = list(0), fixef = list(0), cluster = list(0), data, wald_hypo = NULL) {

	estimate <- wald <- NULL

	# make regression models
	replace_list <- list(y = y, x = x, z = z, fixef = fixef, cluster = cluster) %>% expand.grid()
	regset <- 1:nrow(replace_list) %>% 
		purrr::map(~substitute(y ~ x | fixef | z | cluster, unlist(replace_list[.,]))) %>% 
		setNames(paste("reg", 1:nrow(replace_list), sep = ""))

	# estimate regression models using felm function
	estimate <- regset %>% purrr::map(~lfe::felm(as.formula(.), data = data))

	# implement wald-test if wald_hypo is specified
	if (!is.null(wald_hypo)) {wald <- estimate %>% purrr::map(~felm_waldtest(., wald_hypo))}

	return(list(model = regset, result = estimate, test = wald))
	
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
	felm_result_list, felm_test_list = NULL, combined = TRUE,
	keep_coef = NULL, rm_coef = NULL, label_coef = NULL,
	keep_stat = c("N", "R-squared", "Adjusted R-squared"),
	addlines = NULL
) {

	# list of tabulation parts
	parts <- vector(mode = "list", length = 4)
	parts[[3]] <- addlines
	
	# coefficient and stats tabulation
	tab_result <- felm_regtab_nonchar(
		felm_result_list, keep_coef = keep_coef, rm_coef = rm_coef, label_coef = label_coef, keep_stat = keep_stat
	)

	tab_result_coef <- regtab_char(tab_result$coef)

	tab_result_stat <- tab_result$stat %>% 
		purrr::map(function(x)
			mutate(x,
				val = case_when(
					vars == "N" ~ sprintf("%1d", as.integer(val)),
					TRUE ~ sprintf("%1.3f", val)
				)
			)
		) %>% 
		reduce(full_join, by = c("vars", "stat")) %>%
		setNames(c("vars", "stat", paste("reg", 1:length(felm_result_list), sep = "")))
	
	parts[[1]] <- tab_result_coef; parts[[4]] <- tab_result_stat

	# wald test tabulation if specified
	if (!is.null(felm_test_list)) {
		tab_wald <- regtab_char(felm_test_list)
		parts[[2]] <- tab_wald
	} else {
		parts[[2]] <- NULL
	}
	
	# combined table
	if (combined) {tab_comb <- parts %>% reduce(bind_rows)}

	return(list(set = tab_comb, parts = parts))

}
