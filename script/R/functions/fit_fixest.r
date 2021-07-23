#' ### Fixed effect modelの推定
#'
#' {fixest}の`feols`関数を用いて固定効果モデルやパネルIVモデルの推定を行う。
#' また、統計的推論ではクラスター標準誤差を用いることができる
#'
#' @param y アウトカム変数を含めたformulaオブジェクト (e.g. `y ~ .`)
#' @param x 説明変数を含めたformulaオブジェクト（e.g. ` ~ x1 + x2`）
#' @param z 内生変数と操作変数を含めたformulaオブジェクト（e.g. `x3 ~ z`）
#' @param fixef 固定効果を含めたformulaオブジェクト（e.g. `~ fix1 + fix2`）
#' @param cluster クラスター標準誤差のクラスター変数を含めたformulaオブジェクト（e.g. `~ clust1`）
#' @param se Character scalar: “standard”, “hetero”,
#' “cluster”, “twoway”, “threeway” or “fourway”?
#' By default if there are clusters in the estimation: se = "cluster",
#' otherwise se = "standard"
#' @param data 推定に用いるデータ
#'
#' @details `y`と`x`は必ず指定する必要がある。
#'
#' @return 推定結果のオブジェクト（`felm`クラス）を返す
#+
fit_fixest <- function(y, x, z = 0, fixef = 0, cluster = 0, data, se = NULL) {

  # make regression model
  yvar <- all.vars(y)[1]
  xeq <- as.character(x)[-1]

  model <- paste(yvar, "~", xeq)

  if (fixef != 0) {
    fixeq <- all.vars(fixef) %>% paste(collapse = " + ")
    model <- paste(model, "|", fixeq)
  }

  if (z != 0) {
    zeq <- as.character(z); zeq <- paste(zeq[2], "~", zeq[3])
    model <- paste(model, "|", zeq)
  }

  model <- as.formula(model)

  # estimate fixed effect model using felm function provided by {fixest}
  if (cluster == 0) {
    if (is.null(se)) se <- "standard"
    est_model <- fixest::feols(model, data = data, se = se)
  } else {
    if (is.null(se)) se <- "cluster"
    est_model <- fixest::feols(model, data = data, cluster = cluster, se = se)
  }

  return(est_model)

}
