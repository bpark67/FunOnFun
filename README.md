
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FunOnFun

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version of FunOnFun from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bpark67/FunOnFun")
```

## Simulate Data

### Covariates

``` r
library(FunOnFun)

t = seq(0, 1, length.out = 100)

n = 199

components = 3

mean_funs = list(
  function(t) -2*(t-0.5)^2 + 5,
  function(t) 3*(t-0.75)^3 - 5
  )

eigen_funs_list = list(
  list(
    function(t) sin(2*pi*t),
    function(t) sin(4*pi*t),
    function(t) sin(6*pi*t)
  ),
  list(
    function(t) cos(3*pi*t),
    function(t) cos(pi*t),
    function(t) cos(5*pi*t)
  )
)

lambdas = c(5, 3, 1)

X = FunOnFun::simMFPCA(16, t, n, 3, mean_funs, eigen_funs_list, lambdas, response = FALSE)
```

### Response

``` r
mean_funs = list(
  function(t) 6*exp(-(t-1)^2),
  function(t) -2*14^(t-0.5)
)

eigen_funs_list = list(
  list(
    function(t) cos(9*pi*t),
    function(t) cos(5*pi*t),
    function(t) cos(2*pi*t)
  ),
  list(
    function(t) sin(3*pi*t),
    function(t) sin(5*pi*t),
    function(t) sin(7*pi*t)
  )
)

B = matrix(c(-1, 2, 3, -1, -9, 3, 5, 5, -3),
           nrow = components,
           ncol = components)

Y = FunOnFun::simMFPCA(16, t, n, 3, mean_funs, eigen_funs_list, lambdas, response = TRUE, B = B)
sigma = 0.001
E = matrix(rnorm(2*length(t)*n, mean = 0, sd = sigma), n, 2*length(t))

Y$X = Y$X + E
```

## Plot Simulated Data

``` r
matplot(t(X$X), 
        type='l', 
        ylab='X(t)', 
        xlab='time', 
        main='Plot of predictor curves', 
        col=rgb(0,0,0,alpha=0.4))
matlines(apply(t(X$X), 1, mean),
         type='l',
         lwd=3,
         lty=1,
         col="red")
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

``` r

matplot(t(Y$X), 
        type='l', 
        ylab='Y(t)', 
        xlab='time', 
        main='Plot of response curves', 
        col=rgb(0,0,0,alpha=0.6))
matlines(apply(t(Y$X), 1, mean),
         type='l',
         lwd=3,
         lty=1,
         col = "red")
```

<img src="man/figures/README-unnamed-chunk-12-2.png" width="100%" />
