#' ### Fixed effect modelの推定
#'
#' {lfe}の`felm`関数を用いて固定効果モデルやパネルIVモデルの推定を行う。
#' また、統計的推論ではクラスター標準誤差を用いることができる
#'
#' @param y アウトカム変数を含めたformulaオブジェクト (e.g. `y ~ .`)
#' @param x 説明変数を含めたformulaオブジェクト（e.g. ` ~ x1 + x2`）
#' @param z 内生変数と操作変数を含めたformulaオブジェクト（e.g. `x3 ~ z`）
#' @param fixef 固定効果を含めたformulaオブジェクト（e.g. `~ fix1 + fix2`）
#' @param cluster クラスター標準誤差のクラスター変数を含めたformulaオブジェクト（e.g. `~ clust1`）
#' @param data 推定に用いるデータ
#'
#' @details `y`と`x`は必ず指定する必要がある。
#' `z`、`fixed`、`cluster`は指定がない限り0で処理してformulaを作成する
#'
#' @return 推定結果のオブジェクト（`felm`クラス）を返す
#+
est_felm <- function(y, x, z = 0, fixef = 0, cluster = 0, data) {

  # make regression model
  yvar <- all.vars(y)[1]
  xeq <- as.character(x)[-1]
  yxeq <- paste(yvar, "~", xeq)

  fixeq <- zeq <- clusteq <- "0"

  if (fixef != 0) {
    fixeq <- all.vars(fixef) %>% paste(collapse = " + ")
  }

  if (z != 0) {
    zeq <- as.character(z)
    zeq <- paste(zeq[2], "~", zeq[3]) %>% paste("(", ., ")")
  }

  if (cluster != 0) {
    clusteq <- all.vars(cluster) %>% paste(collapse = " + ")
  }

  model <- paste(yxeq, "|", fixeq, "|", zeq, "|", clusteq)

  # estimate fixed effect model using felm function provided by {lfe}
  est_model <- lfe::felm(as.formula(model), data = data)

  return(est_model)

}
