
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FunOnFun

<!-- badges: start -->
<!-- badges: end -->

## Purpose

This package does multivariate functional Principal Component Analysis
(MFPCA) and then conducts function on function regression the MFPCA
scores.

- The URL to the [GitHub](https://github.com/bpar67/FunOnFun)
- The URL to the [Website](https://bpark67.github.io/FunOnFun/)

## Installation

You can install the development version of FunOnFun from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bpark67/FunOnFun")
```

## Dependencies

This package depends on `magrittr`, `dplyr`, `purrr`, `tsibble`,
`tibble`, `brolgar`.

## Session Info

This tool was developed using the following R session:

``` r
devtools::session_info()
#> ─ Session info ───────────────────────────────────────────────────────────────
#>  setting  value
#>  version  R version 4.3.3 (2024-02-29)
#>  os       macOS Sonoma 14.4.1
#>  system   x86_64, darwin20
#>  ui       X11
#>  language (EN)
#>  collate  en_US.UTF-8
#>  ctype    en_US.UTF-8
#>  tz       America/Los_Angeles
#>  date     2024-05-23
#>  pandoc   3.1.11 @ /Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/x86_64/ (via rmarkdown)
#> 
#> ─ Packages ───────────────────────────────────────────────────────────────────
#>  package     * version date (UTC) lib source
#>  cachem        1.1.0   2024-05-16 [1] CRAN (R 4.3.3)
#>  cli           3.6.2   2023-12-11 [1] CRAN (R 4.3.0)
#>  devtools      2.4.5   2022-10-11 [1] CRAN (R 4.3.0)
#>  digest        0.6.35  2024-03-11 [1] CRAN (R 4.3.2)
#>  ellipsis      0.3.2   2021-04-29 [1] CRAN (R 4.3.0)
#>  evaluate      0.23    2023-11-01 [1] CRAN (R 4.3.0)
#>  fastmap       1.2.0   2024-05-15 [1] CRAN (R 4.3.3)
#>  fs            1.6.4   2024-04-25 [1] CRAN (R 4.3.2)
#>  glue          1.7.0   2024-01-09 [1] CRAN (R 4.3.0)
#>  htmltools     0.5.8.1 2024-04-04 [1] CRAN (R 4.3.2)
#>  htmlwidgets   1.6.4   2023-12-06 [1] CRAN (R 4.3.0)
#>  httpuv        1.6.15  2024-03-26 [1] CRAN (R 4.3.2)
#>  knitr         1.45    2023-10-30 [1] CRAN (R 4.3.0)
#>  later         1.3.2   2023-12-06 [1] CRAN (R 4.3.0)
#>  lifecycle     1.0.4   2023-11-07 [1] CRAN (R 4.3.0)
#>  magrittr      2.0.3   2022-03-30 [1] CRAN (R 4.3.0)
#>  memoise       2.0.1   2021-11-26 [1] CRAN (R 4.3.0)
#>  mime          0.12    2021-09-28 [1] CRAN (R 4.3.0)
#>  miniUI        0.1.1.1 2018-05-18 [1] CRAN (R 4.3.0)
#>  pkgbuild      1.4.4   2024-03-17 [1] CRAN (R 4.3.2)
#>  pkgload       1.3.4   2024-01-16 [1] CRAN (R 4.3.0)
#>  profvis       0.3.8   2023-05-02 [1] CRAN (R 4.3.0)
#>  promises      1.3.0   2024-04-05 [1] CRAN (R 4.3.2)
#>  purrr         1.0.2   2023-08-10 [1] CRAN (R 4.3.0)
#>  R6            2.5.1   2021-08-19 [1] CRAN (R 4.3.0)
#>  Rcpp          1.0.12  2024-01-09 [1] CRAN (R 4.3.0)
#>  remotes       2.5.0   2024-03-17 [1] CRAN (R 4.3.2)
#>  rlang         1.1.3   2024-01-10 [1] CRAN (R 4.3.0)
#>  rmarkdown     2.27    2024-05-17 [1] CRAN (R 4.3.3)
#>  rstudioapi    0.16.0  2024-03-24 [1] CRAN (R 4.3.2)
#>  sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.3.0)
#>  shiny         1.8.1.1 2024-04-02 [1] CRAN (R 4.3.2)
#>  stringi       1.8.4   2024-05-06 [1] CRAN (R 4.3.2)
#>  stringr       1.5.1   2023-11-14 [1] CRAN (R 4.3.0)
#>  urlchecker    1.0.1   2021-11-30 [1] CRAN (R 4.3.0)
#>  usethis       2.2.3   2024-02-19 [1] CRAN (R 4.3.2)
#>  vctrs         0.6.5   2023-12-01 [1] CRAN (R 4.3.0)
#>  xfun          0.44    2024-05-15 [1] CRAN (R 4.3.3)
#>  xtable        1.8-4   2019-04-21 [1] CRAN (R 4.3.0)
#>  yaml          2.3.8   2023-12-11 [1] CRAN (R 4.3.0)
#> 
#>  [1] /Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/library
#> 
#> ──────────────────────────────────────────────────────────────────────────────
```
