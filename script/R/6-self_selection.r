#' Issue #76: ITTの問題を克服するために、PSM-DIDのストラテジーを利用した推定
#'
#' パッケージのロード
#+
library(xfun)
xfun::pkg_attach2(c("readstata13", "tidyverse", "rlist"))
xfun::pkg_attach2(c("plm", "lmtest", "sandwich", "lfe", "Formula", "censReg"))

lapply(Sys.glob(file.path("script/R/functions", "*.r")), source)
