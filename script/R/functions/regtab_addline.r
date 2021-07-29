#' ### 追加的な表を加えて回帰分析表を生成する
#'
#' 係数表と回帰統計量表の間に自分で作成した追加的なデータフレームを挟んで一つのデータフレームにする
#'
#' @param regtab 回帰分析のデータフレームのリスト
#' @param addtab 係数表と統計量を含むデータフレームの間に入れる表のリスト
#'
#' @details `regtab`に渡すリストは回帰係数表を`coef`でラベルして、モデル統計量のデータを`stat`でラベルする
#'
#' @return 追加的な表を加えたデータフレームを返す
#'
#+
regtab_addline <- function(regtab, addtab) {

  fulltab <- regtab$coef

  for (i in seq_len(length(addtab))) {
    fulltab <- dplyr::bind_rows(fulltab, addtab[[i]])
  }

  fulltab <- dplyr::bind_rows(fulltab, regtab$stat)

  return(fulltab)

}
