#Render

#'
#+ library and wd
library(rmarkdown)
Sys.setenv(RSTUDIO_PANDOC = "C:/Users/katoo/AppData/Local/Pandoc")
options(repo = "https://cran.rstudio.com/")

#' from Rmd to Powerpoint
rmarkdown::render(
  input = "./report/script/powerpoint.rmd",
  output_file = "200710Meeting.pptx",
  output_dir = "./report", #いじらない
  clean = TRUE, #いじらない
  encoding = "utf8" #いじらない
)
