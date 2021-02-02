#' 
#+ pandoc version (optional)
#Sys.setenv(RSTUDIO_PANDOC = "C:/Users/vge00/AppData/Local/Pandoc")
Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/Pandoc")

#'
#+ library and wd
library(rmarkdown)
library(bookdown)
library(Statamarkdown)
options(repo = "https://cran.rstudio.com/")
options(bookdown.render.file_scope = FALSE)

#'
#+ Rmd File to Output

# pdf
bookdown::render_book(
    input = "index.rmd",
    output_format = "bookdown::pdf_book",
    output_file = "slides.pdf",
    output_dir = "report",
    clean = TRUE,
    encoding = "utf8"
)
