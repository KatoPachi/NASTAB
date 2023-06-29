#' ### {ggplot2}のテンプレート
#'
#' ggplot2のテンプレート関数
#'
#+
ggtemp <- function(flip = FALSE,
                   family = NULL,
                   size = list(
                     axis_title = 13,
                     axis_text = 9,
                     title = 20,
                     caption = 11
                   ),
                   legend_key_size = 1) {
  my_theme <- theme_minimal(base_family = family) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text = element_text(
        color = "black", size = size$axis_text, family = family
      ),
      axis.title = element_text(size = size$axis_title, family = family),
      axis.ticks.length = unit(0.25, "cm"),
      axis.ticks.x = element_line(),
      axis.ticks.y = element_line(),
      axis.line = element_line(),
      legend.text = element_text(size = size$text, family = family),
      legend.key.size = unit(legend_key_size, "cm"),
      legend.title = ggplot2::element_text(size = size$title),
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(size = size$caption),
      plot.title = element_text(size = size$title)
    )

  if (flip) {
    my_theme <- my_theme +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line()
      )
  }

  return(my_theme)
}
