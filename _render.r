#' from R script to Rmd file
#+
knitr::spin(
    "./script/R/2-elasticity.r",
    knit = FALSE
)

#'
#+ Rmd File to Output

# HTML file
rmarkdown::render(
    "./Rmarkdown/6-self_selection.rmd",
    output_file = "issue76.html",
    output_dir = "report"
)

# knit draft.rmd
rmarkdown::render(
    "draft.Rmd",
    output_file = "draft.pdf",
    output_dir = "paper"
)

# knit slide.rmd
rmarkdown::render(
    "slide.Rmd",
    output_file = "slide.pdf",
    output_dir = "paper"
)
