#' from R script to Rmd file
#+
knitr::spin(
    "./script/R/04-manipulation.r",
    knit = FALSE
)

#'
#+ Rmd File to Output

# HTML file
rmarkdown::render(
    "./manuscript/issue72.rmd",
    output_file = "issue72.html",
    output_dir = "report"
)

# preview chapter
bookdown::preview_chapter(
    input = "manuscript/6_heteroElasticity.rmd", 
    output_format = "bookdown::pdf_book",
    output_dir = "report",
    output_file = "chapter5.pdf"
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
