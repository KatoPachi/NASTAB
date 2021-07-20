#' ### 変数の記述統計量
#'
#' 変数の記述統計量の計算
#'
#' @param data 使用するデータフレーム
#' @param vars 対象とする変数の文字列ベクトル
#' @param stat 記述統計量の文字列ベクトル。
#' `"mean"`は平均。`"se"`は標準誤差。`"sd"`は標準偏差。`"min"`は最小値。`"q25"`は第一四分位数。
#' `"median"`は中央値。`"q75"`は第三四分位数。`"max"`は最大値。`"sum"`は合計。`"N"`はサンプルサイズである。
#'
#' @return 指定した記述統計量のdataframeを返す
#'
#'

sumvars <- function(
    data, vars,
    stat = c(
        "mean", "se", "sd", "min", "q25", "median", "q75", "max", "sum", "N"
    )
) {

  tab <- data %>%
    dplyr::summarize_at(
      vars,
      list(
        mean = ~mean(., na.rm = TRUE),
        se = ~se(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        q25 = ~quantile(., prob = .25, na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        q75 = ~quantile(., prob = .75, na.rm = TRUE),
        max = ~max(., na.rm = TRUE),
        sum = ~sum(., na.rm = TRUE),
        N = ~sum(!is.na(.))
      )
    )

    if (length(vars) == 1) {

        tab <- tab %>%
          mutate(vars = vars) %>%
          dplyr::select(c("vars", stat))

    } else {

        pattern <- "(.*)_(mean|se|sd|min|q25|median|q75|max|sum|N)"

        tab <- tab %>%
          pivot_longer(
            everything(),
            names_to = c("vars", ".value"),
            names_pattern = pattern
          ) %>%
          dplyr::select(c("vars", stat))

    }

    return(tab)

}