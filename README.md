
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
library(fdapace)
library(ggplot2)

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

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

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

<img src="man/figures/README-unnamed-chunk-7-2.png" width="100%" />

## Simulate Missingness

``` r
X_miss = FunOnFun::simMiss(99, X$X, t, seed = 51)
Y_miss = FunOnFun::simMiss(99, Y$X, t, seed = 51)
```

## FPCA

``` r
df = X_miss %>% FunOnFun::tibbleFormat(t) %>% FunOnFun::fpcaFormat(id_col = "id")
df_Y = Y_miss %>% FunOnFun::tibbleFormat(t) %>% FunOnFun::fpcaFormat(id_col = "id")
```

``` r
res_X1 = fdapace::FPCA(df$Variable1,
                       df$Time,
                       list(dataType = "Sparse",
                            error = F,
                            kernel = "epan",
                            verbose = F,
                            nRegGrid = length(t)))

res_X2 = fdapace::FPCA(df$Variable2,
                       df$Time,
                       list(dataType = "Sparse",
                            error = F,
                            kernel = "epan",
                            verbose = F,
                            nRegGrid = length(t)))

res_Y1 = fdapace::FPCA(df_Y$Variable1,
                       df_Y$Time,
                       list(dataType = "Sparse",
                            error = T,
                            kernel = "epan",
                            verbose = F,
                            nRegGrid = length(t)))
res_Y2 = fdapace::FPCA(df_Y$Variable2,
                       df_Y$Time,
                       list(dataType = "Sparse",
                            error = T,
                            kernel = "epan",
                            verbose = F,
                            nRegGrid = length(t),
                            methodSelectK = 3)) # TODO: CHECK THIS
```

## Visualize FPCA

``` r
act = data.frame(act1 = Y$mu[1:100],
                 act2 = Y$mu[101:200])
hat = data.frame(hat1 = res_Y1$mu,
                 hat2 = res_Y2$mu)

hat %>%
  ggplot() +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = hat1)) +
  geom_line(data = act, aes(x = seq(0, 1, length.out = 100), y = act1), linetype = "dashed") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

``` r

hat %>%
  ggplot() +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = hat2)) +
  geom_line(data = act, aes(x = seq(0, 1, length.out = 100), y = act2), linetype = "dashed") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-11-2.png" width="100%" />

``` r


phi_X2_df = Y$phi[101:200,] %>% as.data.frame()

res_Y2$phi[, 1:3] %>%
  as.data.frame() %>%
  ggplot() +
  geom_line(aes(x = t, y = V1), col = "red") +
  geom_line(data = phi_X2_df, aes(x = t, y = V1), col = "red", linetype = "dashed") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-11-3.png" width="100%" />

``` r

res_Y2$phi[, 1:3] %>%
  as.data.frame() %>%
  ggplot() +
  geom_line(aes(x = t, y = V2), col = "red") +
  geom_line(data = phi_X2_df, aes(x = t, y = V2), col = "red", linetype = "dashed") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-11-4.png" width="100%" />

``` r

res_Y2$phi[, 1:3] %>%
  as.data.frame() %>%
  ggplot() +
  geom_line(aes(x = t, y = V3), col = "red") +
  geom_line(data = phi_X2_df, aes(x = t, y = V3), col = "red", linetype = "dashed") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-11-5.png" width="100%" />

## Irregular MFPCA

``` r
res = FunOnFun::irregMFPCA(components = 3,
                           split = T,
                           res_X1,
                           res_X2)

res_Y = FunOnFun::irregMFPCA(components = 3,
                             split = T,
                             res_Y1,
                             res_Y2)
```

### Check results

``` r
eigenf = res$unstacked_phi
colnames(eigenf) = c("var_1_1", "var_1_2", "var_1_3", "var_2_1", "var_2_2", "var_2_3")
eigens = res$xi
colnames(eigens) = c("comp_1", "comp_2", "comp_3")
```

``` r
eigenf %>%
  as.data.frame() %>%
  ggplot() +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = var_1_1), col = "red") +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = X$phi[1:100, 1]), col = "black", linetype = "dotted") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="100%" />

``` r

eigenf %>%
  as.data.frame() %>%
  ggplot() +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = -var_1_2), col = "red") +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = X$phi[1:100, 2]), col = "black", linetype = "dotted") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-14-2.png" width="100%" />

``` r

eigenf %>%
  as.data.frame() %>%
  ggplot() +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = -var_1_3), col = "red") +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = X$phi[1:100, 3]), col = "black", linetype = "dotted") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-14-3.png" width="100%" />

``` r

eigenf %>%
  as.data.frame() %>%
  ggplot() +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = var_2_1), col = "red") +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = X$phi[101:200, 1]), col = "black", linetype = "dotted") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-14-4.png" width="100%" />

``` r

eigenf %>%
  as.data.frame() %>%
  ggplot() +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = -var_2_2), col = "red") +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = X$phi[101:200, 2]), col = "black", linetype = "dotted") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-14-5.png" width="100%" />

``` r

eigenf %>%
  as.data.frame() %>%
  ggplot() +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = -var_2_3), col = "red") +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = X$phi[101:200, 3]), col = "black", linetype = "dotted") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-14-6.png" width="100%" />

``` r

eigens = data.frame(est1 = eigens[,1]/sqrt(res$Dhat[1,1]),
                    est2 = eigens[,2]/sqrt(res$Dhat[2,2]),
                    est3 = eigens[,3]/sqrt(res$Dhat[3,3]),
                    act1 = X$xi[, 1],
                    act2 = X$xi[, 2],
                    act3 = X$xi[, 3])

eigens %>%
  ggplot(aes(x = est1, y = act1)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  geom_smooth(method = "lm", se = F) +
  theme_bw()
#> `geom_smooth()` using formula = 'y ~ x'
```

<img src="man/figures/README-unnamed-chunk-14-7.png" width="100%" />

``` r

eigens %>%
  ggplot(aes(x = -est2, y = act2)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  geom_smooth(method = "lm", se = F) +
  theme_bw()
#> `geom_smooth()` using formula = 'y ~ x'
```

<img src="man/figures/README-unnamed-chunk-14-8.png" width="100%" />

``` r

eigens %>%
  ggplot(aes(x = -est3, y = act3)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  geom_smooth(method = "lm", se = F) +
  theme_bw()
#> `geom_smooth()` using formula = 'y ~ x'
```

<img src="man/figures/README-unnamed-chunk-14-9.png" width="100%" />

``` r
eigenf = res_Y$unstacked_phi
colnames(eigenf) = c("var_1_1", "var_1_2", "var_1_3", "var_2_1", "var_2_2", "var_2_3")
eigens = res_Y$xi
colnames(eigens) = c("comp_1", "comp_2", "comp_3")
```

``` r
eigenf %>%
  as.data.frame() %>%
  ggplot() +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = var_1_1), col = "red") +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = Y$phi[1:100, 1]), col = "black", linetype = "dotted") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-16-1.png" width="100%" />

``` r

eigenf %>%
  as.data.frame() %>%
  ggplot() +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = -var_1_2), col = "red") +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = Y$phi[1:100, 2]), col = "black", linetype = "dotted") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-16-2.png" width="100%" />

``` r

eigenf %>%
  as.data.frame() %>%
  ggplot() +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = -var_1_3), col = "red") +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = Y$phi[1:100, 3]), col = "black", linetype = "dotted") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-16-3.png" width="100%" />

``` r

eigenf %>%
  as.data.frame() %>%
  ggplot() +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = var_2_1), col = "red") +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = Y$phi[101:200, 1]), col = "black", linetype = "dotted") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-16-4.png" width="100%" />

``` r

eigenf %>%
  as.data.frame() %>%
  ggplot() +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = -var_2_2), col = "red") +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = Y$phi[101:200, 2]), col = "black", linetype = "dotted") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-16-5.png" width="100%" />

``` r

eigenf %>%
  as.data.frame() %>%
  ggplot() +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = -var_2_3), col = "red") +
  geom_line(aes(x = seq(0, 1, length.out = 100), y = Y$phi[101:200, 3]), col = "black", linetype = "dotted") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-16-6.png" width="100%" />

``` r

eigens = data.frame(est1 = eigens[,1]/sqrt(res_Y$Dhat[1,1]),
                    est2 = eigens[,2]/sqrt(res_Y$Dhat[2,2]),
                    est3 = eigens[,3]/sqrt(res_Y$Dhat[3,3]),
                    act1 = Y$xi[, 1],
                    act2 = Y$xi[, 2],
                    act3 = Y$xi[, 3])

eigens %>%
  ggplot(aes(x = est1, y = act1)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  geom_smooth(method = "lm", se = F) +
  theme_bw()
#> `geom_smooth()` using formula = 'y ~ x'
```

<img src="man/figures/README-unnamed-chunk-16-7.png" width="100%" />

``` r

eigens %>%
  ggplot(aes(x = -est2, y = act2)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  geom_smooth(method = "lm", se = F) +
  theme_bw()
#> `geom_smooth()` using formula = 'y ~ x'
```

<img src="man/figures/README-unnamed-chunk-16-8.png" width="100%" />

``` r

eigens %>%
  ggplot(aes(x = -est3, y = act3)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  geom_smooth(method = "lm", se = F) +
  theme_bw()
#> `geom_smooth()` using formula = 'y ~ x'
```

<img src="man/figures/README-unnamed-chunk-16-9.png" width="100%" />

## Regression

``` r
response = res_Y$xi
predictor = res$xi

mod = lm(response ~ -1 + predictor)

B; mod$coefficients
#>      [,1] [,2] [,3]
#> [1,]   -1   -1    5
#> [2,]    2   -9    5
#> [3,]    3    3   -3
#>                 [,1]       [,2]       [,3]
#> predictor1  1.547973  0.8665041 -0.6766258
#> predictor2 -9.643038  1.5481153 -0.2327695
#> predictor3  6.048158 16.2177891  1.1154167
```
