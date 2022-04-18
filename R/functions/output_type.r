output_type <- function() {
  if (params$preview) {
    "preview"
  } else {
    if (!params$appendix & !params$slide) {
      "body"
    } else if (params$appendix) {
      "appx"
    } else if (params$slide) {
      "slide"
    }
  }
}