#'
#+ Rmd File to Output

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
    output_dir = "docs",
    clean = TRUE,
    encoding = "utf8"
)

# pdf (paper)
bookdown::render_book(
    input = "index.rmd",
    output_format = "bookdown::pdf_document2",
    output_file = "draft.pdf",
    output_dir = "docs",
    clean = TRUE,
    encoding = "utf8"
)
