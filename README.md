# Tax-Price Elasticites of Charitable Giving and Selection of Reporting: Panel Study of South Korea

Authors

- Hiroki Kato (Repository owner)
- Tsuyoshi Goto
- Youngrok Kim

This repository only contains replication files (computer programs). The data used in this paper are available from the [National Survey of Tax and Benefit repository](https://www.kipf.re.kr/panel/).

## How to Reproduce

**Step 1**: Prepare raw data (wave 4--11). You can easily obtain the data from the [National Survey of Tax and Benefit repository](https://www.kipf.re.kr/panel/).

**Step 2**: Data preprocessing. First, run `preprocess.do` in the `Stata` folder. Second, run `preprocess.r` in the `R` folder.

**Step 3**: Reproduce our results. Running `Output.rmd` (Rmarkdown file) will give you all the results published in the paper and the online appendix. Code for statistical analysis such as regression analysis and visualization is included below.

- `R/R6_FirstPrice.r`
- `R/R6_LastPrice.r`
- `R/R6_PolicyEffect.r`
- `R/R6_StartAnalysis.r`
- `R/R6_SummaryData.r`
- `Stata/poisson.do`
- `Stata/tobit.do`