#' ### {ggplot2}のテンプレート
#'
#' ggplot2のテンプレート関数
#'
#' @param flip `coord_flip()`をするかどうかのT/F変数。TRUEなら描画する軸を逆転させる
#'
#' @return `gg`と`theme`クラスのオブジェクトを返す
#+
ggtemp <- function(flip = FALSE) {

  my_theme <- theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text = element_text(color = "black", size = 13),
      axis.title = element_text(size = 13),
      axis.ticks.length = unit(0.25, "cm"),
      axis.ticks.x = element_line(),
      axis.ticks.y = element_line(),
      axis.line = element_line(),
      legend.text = element_text(size = 13),
      legend.key.size = unit(1, "cm"),
      legend.position = "bottom"
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