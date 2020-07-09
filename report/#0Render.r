#Render

#'
#+ library and wd
library(rmarkdown)
Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/pandoc")
options(repo = "https://cran.rstudio.com/")

#'
#+ Rmd File to Output
rmarkdown::render(
    input = "./report/powerpoint.Rmd",
    output_file = "200710Meeting.pptx",
    output_dir = "./Report",
    clean = TRUE,
    encoding = "utf8"
)
