
## ---- Analysis Functions

#' Using felm function (lfe package), we estimate fixed effect model with individual cluster standard errors.
#' outcome: character vector which is an outcome variable
#' data: dataset
#' implied: estimate implied elasticity and implement F-test for zero implied elasticity using waldtest function (in lfe package)
#' evaluate_at: numric value which estimate implied elasticity. defualt is average value of outcome.

estimate_elast <- function(outcome, data, implied = FALSE, evaluate_at = NULL) {

	estimate <- implied_p <- implied_y <- NULL

	reg <- as.formula(paste(outcome, "~ log_price + log_pinc_all | pid + year", sep = "")) %>% Formula()

	setreg <- list(
		base = . ~ . | . | 0 | pid,
		age = . ~ . + age + sqage | . | 0 | pid,
		educ = . ~ .  + age + sqage + factor(year):factor(educ) | . | 0 | pid,
		gender = . ~ .  + age + sqage + factor(year):factor(educ) + factor(year):factor(gender) | . | 0 | pid,
		living = . ~ . + age + sqage + factor(year):factor(educ) + factor(year):factor(gender) + factor(year):factor(living_area) |
			. | 0 | pid
	)

	estimate <- setreg %>% 
		purrr::map(~as.formula(update(reg, .))) %>%
		purrr::map(~lfe::felm(as.formula(.), data = data))
	
	if (implied) {
		
		if (!is.null(evaluate_at)) {
			dbar <- evaluate_at
		} else {
			dbar <- 1/mean(as_vector(df[outcome]), na.rm = TRUE)
		}

		implied_p <- estimate %>% 
			purrr::map(~list(model = ., rhs = matrix(c(dbar, numeric(length(coef(.)) - 1)), nrow = 1))) %>% 
			purrr::map(~list(coef =  .$rhs[1] * coef(.$model)["log_price"], test = lfe::waldtest(.$model, .$rhs))) %>% 
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
  		purrr::map(~list(model = ., rhs = matrix(c(0, dbar, numeric(length(coef(.)) - 2)), nrow = 1))) %>% 
  		purrr::map(~list(coef =  .$rhs[2] * coef(.$model)["log_pinc_all"], test = lfe::waldtest(.$model, .$rhs))) %>% 
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

	return(list(est = estimate, imp_elast = list(price = implied_p, inc = implied_y)))
	
}

## ---- Regression Tables
felm_regtab_nonchar <- function(felmobj, keep_coef = NULL, rm_coef = NULL, label_coef = NULL) {

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
		))
	
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

fullset_regtab <- function(felmobj)