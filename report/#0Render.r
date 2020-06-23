#Render

#'
#+ library and wd
library(rmarkdown)
Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/pandoc")
options(repo = "https://cran.rstudio.com/")

#'
#+ Rmd File to Output
rmarkdown::render(
    input = "./Report/Powerpoint.Rmd",
    output_file = "200705骨髄バンク中間報告.pptx",
    output_dir = "./Report",
    clean = TRUE,
    encoding = "utf8"
)
