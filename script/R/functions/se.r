#' ### 標準誤差の計算
#'
#' 標準誤差を計算する
#'
#' @param x 変数ベクトル
#' @param na.rm 欠損値を無視するかどうかのT/F変数
#'
#' @return 標準誤差の数値を返す
#+
se <- function(x, na.rm = FALSE) {

  # na.rm = TRUE, we remove NA using na.omit()
  if (na.rm) x <- na.omit(x)

  # calculate and return se
  sqrt(var(x) / length(x))

}