#' 
#+ pandoc version (optional)
#Sys.setenv(RSTUDIO_PANDOC = "C:/Users/vge00/AppData/Local/Pandoc")
#Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/Pandoc")

#'
#+ library and wd
library(rmarkdown)
library(bookdown)
library(Statamarkdown)
options(repo = "https://cran.rstudio.com/")
options(bookdown.render.file_scope = FALSE)

#'
#+ Rmd File to Output

# preview chapter
bookdown::preview_chapter(
    input = "manuscript/1_intro.Rmd", 
    output_format = "bookdown::pdf_document2",
    output_dir = "paper",
    output_file = "chapter1.pdf"
)

# pdf (slides)
bookdown::render_book(
    input = "index.rmd",
    output_format = "bookdown::pdf_book",
    output_file = "slides.pdf",
    output_dir = "report",
    clean = TRUE,
    encoding = "utf8"
)

# pdf (paper)
bookdown::render_book(
    input = "index.rmd",
    output_format = "bookdown::pdf_document2",
    output_file = "draft.pdf",
    output_dir = "paper",
    clean = TRUE,
    encoding = "utf8"
)
