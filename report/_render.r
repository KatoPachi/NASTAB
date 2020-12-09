
#'
#+ library and wd
library(rmarkdown)
library(Statamarkdown)
Sys.setenv(RSTUDIO_PANDOC = "C:/Users/vge00/AppData/Local/Pandoc")
options(repo = "https://cran.rstudio.com/")

#'
#+ Rmd File to Output
rmarkdown::render(
    input = "./report/report.Rmd",
    output_file = "report.html",
    output_dir = "./report",
    clean = TRUE,
    encoding = "utf8"
)
