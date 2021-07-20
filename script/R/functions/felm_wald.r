#' ### felmオブジェクトのwald検定
#'
#' {lfe}の`felm`で推定したモデルのwald検定（線型結合検定）を実施する
#'
#' @param felmobj felmクラスのオブジェクト
#' @param hypo 帰無仮説を示した文字列のリスト
#' @param args 帰無仮説の式に代替する数値のリスト
#' @param tab 回帰係数表の形のデータフレームで返すかどうかの論理ベクトル
#'
#' @details 帰無仮説の式は文字列で表現する。
#' たとえば、`a * x1 + b * x2`ならば、帰無仮説は`a * x1 + b * x2 = 0`である。
#' `a`や`b`は数値が入り、数値が1の場合でも1として書く必要がある。
#' また、`args`を使えば、代入が可能である。
#' たとえば、`args = list(a = 2, b = 3)`ならば、
#' `a`に2を、`b`に3を代入して帰無仮説を構築する。
#'
#' @return `tab = FALSE`ならば、f値を含めたデータフレームで返す。
#' `tab = TRUE`ならば、回帰係数表の形のデータフレームで返す。
#+
felm_wald <- function(felmobj, hypo, args = NULL, tab = TRUE) {

  # understanding hypothesis structure
  perfect_split <- hypo %>%
    stringr::str_replace_all(pattern = " ", replacement = "") %>%
    stringr::str_split(pattern = "\\+") %>%
    purrr::map(~ stringr::str_split(., pattern = "\\*"))

  # make matrix of left-hand side
  lhs <- coef(felmobj); lhs[seq_len(length(lhs))] <- 0
  set_lhs <- lapply(seq_len(length(hypo)), function(x) return(lhs))

  for (i in seq_len(length(hypo))) {
    for (j in seq_len(length(perfect_split[[i]]))) {
      if (perfect_split[[i]][[j]][[1]] %in% names(args)) {
        key <- perfect_split[[i]][[j]][[1]]
        set_lhs[[i]][perfect_split[[i]][[j]][2]] <- args[[key]]
      } else {
        val <- as.numeric(perfect_split[[i]][[j]][[1]])
        set_lhs[[i]][perfect_split[[i]][[j]][2]] <- val
      }
    }
  }

  set_lhs <- set_lhs %>% purrr::map(~ matrix(., nrow = 1))

  # implement wald test via waldtest fucntion
  wald <- set_lhs %>%
    purrr::map(~ lfe::waldtest(felmobj, .)) %>%
    purrr::map(function(x) tibble(f = x["F"], p = x["p.F"])) %>%
    reduce(bind_rows) %>%
    mutate(vars = names(hypo))

  # calculate coefficient
  tf <- !is.nan(coef(felmobj))

  estimate <- set_lhs %>%
    purrr::map(~ .[tf] %*% coef(felmobj)[tf]) %>%
    as_vector() %>%
    tibble(coef = ., vars = names(hypo))

  # calculate se
  result <- dplyr::left_join(estimate, wald, by = "vars") %>%
    mutate(se = abs(coef) / sqrt(f))

  # make tabulation if TRUE
  if (tab) {

    result <- result %>%
      mutate(
        coef = case_when(
          p <= .01 ~ sprintf("%1.3f***", coef),
          p <= .05 ~ sprintf("%1.3f**", coef),
          p <= .1 ~ sprintf("%1.3f*", coef),
          TRUE ~ sprintf("%1.3f", coef)
        ),
        se = sprintf("(%1.3f)", se)
      ) %>%
      dplyr::select(-p, -f) %>%
      pivot_longer(-vars, names_to = "stat", values_to = "val")

  }

  return(result)

}
