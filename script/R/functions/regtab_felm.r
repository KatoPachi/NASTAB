#' ### felmの回帰分析表の作成
#'
#' パネルデータモデルを実行する`felm`の結果を`kable`に渡しやすいように文字列のdataframeに変換する
#'
#' @param felmobj `felm`クラスのオブジェクトで構成されているリスト
#' @param keep_coef 結果に残す変数名ベクトル(`|`で一つの文字列にしたものを`str_detect`に渡す)
#' @param rm_coef 結果から落とす変数名ベクトル(`|`で一つの文字列にしたものを`str_detect`に渡す)
#' @param label_coef `dplyr::recode`で変数ラベルを変えるために旧変数名=新変数名で構築されたリストを指定する
#' @param bindtab 二つのデータフレーム（回帰分析の統計量と係数表）を合わせた形で返すかどうかのT/F変数
#'
#' @return `bindtab = FALSE`ならば、二つのdataframeを含んだリストで返す
#' - `coef`: 係数表
#' - `stat`: 回帰分析の統計量（R-squaredとN）
#' `bindtab = TRUE`ならば、以上のデータフレームを`bind_rows`して返す
#'
#+
regtab_felm <- function(
  felmobj, keep_coef = NULL, rm_coef = NULL, label_coef = NULL,
  bindtab = FALSE) {

  # make coeftab: coefficient, star, and standard errors in parenthesis
  coeftab <- felmobj %>%
    purrr::map(~ summary(.)$coefficients) %>%
    purrr::map(function(x)
      tibble(
        vars = rownames(x),
        coef = x[, 1],
        se = x[, 2],
        p = x[, 4]
      ) %>%
      mutate(
        coef = case_when(
          p <= .01 ~ sprintf("%1.3f***", coef),
          p <= .05 ~ sprintf("%1.3f**", coef),
          p <= .1 ~ sprintf("%1.3f*", coef),
          TRUE ~ sprintf("%1.3f", coef)
        ),
        se = sprintf("(%1.3f)", se)
      ) %>%
      dplyr::select(-p) %>%
      pivot_longer(-vars, names_to = "stat", values_to = "val")
    ) %>%
    reduce(full_join, by = c("vars", "stat")) %>%
    setNames(c("vars", "stat", paste0("reg", seq_len(length(felmobj)))))

  # keep variables in coeftab
  if (!is.null(keep_coef)) {
    strings <- keep_coef %>% paste(collapse = "|")
    coeftab <- coeftab[stringr::str_detect(coeftab$vars, strings), ]
  }

  # drop variables from coeftab
  if (!is.null(rm_coef)) {
    strings <- rm_coef %>% paste(collapse = "|")
    coeftab <- coeftab[!stringr::str_detect(coeftab$vars, strings), ]
  }

  # rename variables
  if (!is.null(label_coef)) {
    coeftab <- coeftab %>%
      mutate(vars = dplyr::recode(vars, !!!label_coef, .default = vars))
  }

  # make tabulation of regression stats (R2 and N)
  stattab <- felmobj %>%
    purrr::map(~tibble(
      vars = c("N", "R-squared"),
      stat = c("stat", "stat"),
      val = c(sprintf("%3d", nobs(.)), sprintf("%1.3f", summary(.)$r.squared))
    )) %>%
    reduce(full_join, by = c("vars", "stat")) %>%
    setNames(c("vars", "stat", paste0("reg", seq_len(length(felmobj)))))

  # if bindtab = FALSE, return coeftab and stattab separately
  # if bindtab = TRUE, return coeftab combined with stattab
  if (bindtab) {
    return(dplyr::bind_rows(coeftab, stattab))
  } else {
    return(list(coef = coeftab, stat = stattab))
  }

}
