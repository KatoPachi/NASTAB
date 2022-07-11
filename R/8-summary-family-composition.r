#' ---
#' title: 家族構成に関するデータの記述統計
#' author: Hiroki Kato
#' output:
#'   bookdown::html_document2:
#'     toc: true
#'     toc_float: true
#'     number_sections: false
#' params:
#'   preview: true
#'   appendix: true
#'   slide: true
#' ---
#'
#+ include = FALSE, eval = params$preview
library(here)
source(here("R", "_html_header.r"))

#+ include = FALSE
source(here("R", "_library.r"))

#'
#+ include = FALSE
df <- readr::read_csv(here("data/shaped2.csv"))

#'
#+
df %>%
  mutate(
    family_position = factor(
      family_position,
      levels = 1:13,
      label = c(
        "世帯主",
        "配偶者",
        "世帯主の子供",
        "世帯主の子供の配偶者",
        "世帯主の親",
        "配偶者の親",
        "孫とその配偶者",
        "ひ孫とその配偶者",
        "世帯主の祖父母",
        "世帯主の兄弟姉妹",
        "世帯主の兄弟姉妹の子供とその配偶者",
        "世帯主の親の兄弟姉妹とその配偶者",
        "その他"
      )
    ),
    work = factor(
      work,
      levels = 1:7,
      labels = c(
        "就業（常勤）",
        "就業（臨時）",
        "就業（自営業）",
        "就業（家族従事者・無給）",
        "専業主婦",
        "無職",
        "学生"
      )
    )
  ) %>%
  datasummary_crosstab(
    family_position ~ work,
    data = .
  )

#'
#' - 給与所得者ダミーは「就業（常勤）」ならば1を取るダミー変数である
#' - 今回は世帯主との関係に関わらず、
#' 「就業（家族従事者・無給）」・「専業主婦」・「学生」を除いた分析を行った。
#'
#' 他にアイデアはありますか？
#'
# /*
#+
rmarkdown::render(
  here("R", "8-summary-family-composition.r"),
  output_dir = here("docs", "html-preview")
)
# */