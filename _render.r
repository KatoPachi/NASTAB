
#'
#+ library and wd
library(rmarkdown)
library(Statamarkdown)
#Sys.setenv(RSTUDIO_PANDOC = "C:/Users/vge00/AppData/Local/Pandoc")
Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/Pandoc")
options(repo = "https://cran.rstudio.com/")

#'
#+ Rmd File to Output
rmarkdown::render(
    input = "slides.Rmd",
    output_file = "slides.pdf",
    output_dir = "./report",
    clean = TRUE,
    encoding = "utf8"
)
