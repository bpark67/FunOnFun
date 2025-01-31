Simulate-CV
================

``` r
library(FunOnFun)
library(fdapace)
library(tidyverse)
```

``` r
rm(list = ls())
```

``` r
.closest_t = function(actual_time, hat_time){
  closest = rep(NA, length(actual_time))
  for(i in 1:length(actual_time)){
    closest[i] = which.min(abs(actual_time[i] - hat_time))
  }

  return(closest)
}
```

# Generate Data

## Predictor

``` r
# Define Hyperparameters
t = seq(0, 1, length.out = 200) # Length of Time
n = 50 # Number of Subjects
components = 3 # Number of "True" Components
lambdas = c(5, 3, 1) # Eigenvalues (Variances)
sigma = 10 # Variance of Measurement Error to be added to Y
missing_rate = 0.9
```

``` r
# Choose Mean functions of X
mean_funs = list(
  function(t) -2*(t-0.5)^2 + 5,
  function(t) 3*(t-0.75)^3 - 5
)

# Choose Eigenfunctions of X
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

X = FunOnFun::simMFPCA(16, t, n, 3, mean_funs, eigen_funs_list, lambdas, response = FALSE)
```

## Response

``` r
# Choose Mean Functions of Y
mean_funs = list(
  function(t) 6*exp(-(t-1)^2),
  function(t) -2*14^(t-0.5)
)

# Choose Eigenfunctions of Y
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

# Define Matrix that relates X Eigenscores with Y Eigenscores
B = diag((rep(1, 3)))

# Uses EigX %*% B as its eigenscore
Y = FunOnFun::simMFPCA(16, t, n, 3, mean_funs, eigen_funs_list, lambdas, response = TRUE, B = B)

set.seed(0)
E = matrix(rnorm(2*length(t)*n, mean = 0, sd = sigma), n, 2*length(t))

Y$X = Y$X + E
```

## Orthogonalize Response

``` r
popEig = FunOnFun::populationEigen(Y, B, t, sigma)

par(mfrow=c(1, 2))
image(popEig$pop_covY)
image(cov(Y$X))
```

![](Simulate-CV_files/figure-gfm/unnamed-chunk-81-1.png)<!-- -->

# Plot Simulated Data

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

![](Simulate-CV_files/figure-gfm/plot-data-1.png)<!-- -->

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

![](Simulate-CV_files/figure-gfm/plot-data-2.png)<!-- -->

# Simulate Missingness

``` r
observed = round(length(t) * (1 - missing_rate)) 
X_miss = FunOnFun::simMiss(observed, X$X, t, seed = 51)
Y_miss = FunOnFun::simMiss(observed, Y$X, t, seed = 51)
```

# FPCA/MFPCA

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
                            nRegGrid = length(t),
                            methodSelectK = 3))
res_Y2 = fdapace::FPCA(df_Y$Variable2,
                       df_Y$Time,
                       list(dataType = "Sparse",
                            error = T,
                            kernel = "epan",
                            verbose = F,
                            nRegGrid = length(t),
                            methodSelectK = 3))
```

## Irregular MFPCA

``` r
res = FunOnFun::irregMFPCA(components = 6,
                           split = T,
                           res_X1,
                           res_X2)

res_Y = FunOnFun::irregMFPCA(components = 6,
                             split = T,
                             res_Y1,
                             res_Y2)
```

# Inspect CV

``` r
time_points_x = nrow(res$unstacked_phi) # 100
time_points_y = nrow(res_Y$unstacked_phi) # 100
nvars = nrow(res$stacked_phi) / time_points_x # 2

t1 = ncol(res$xi) # 5
t2 = ncol(res$xi) # 5

# HOW MANY COMPONENTS DO WE WANT TO TEST?
i = 6
j = 6

# Scores
U = res$xi[, 1:i, drop = F]
V = res_Y$xi[, 1:j, drop = F]

# Eigenfunctions
PhiU = res$stacked_phi[, 1:i, drop = F]
PhiV = res_Y$stacked_phi[, 1:j, drop = F]

# Score by Score Regression
mod = lm(V ~ -1+U)
B = mod$coefficients

# In Bag Estimation
Vhat = U %*% B

# How well are scores estimated?
# ISSUE 3: Poor fit in estimating scores
vis = data.frame(
  val = c(c(V), c(Vhat)),
  type = c(rep("Actual", n*i), rep("Hat", n*i)),
  order = c(rep(1:i, each = n), rep(1:i, each = n)),
  index = rep(1:n, i)
)
vis %>%
  ggplot(aes(x = index, y = val, color = type))+
  geom_point()+
  geom_line()+
  facet_wrap(~order, scales = "free_y")+
  theme_bw()
```

![](Simulate-CV_files/figure-gfm/check-1.png)<!-- -->

``` r

# Estimate original functional profile
orighat = PhiV %*% t(Vhat) %>% as.data.frame()

# Name for convenience
colnames(orighat) = as.character(1:n)

# Append Time column
# Add slight lag for visualization purpose
orighat$t = rep(seq(0, 1, length.out = time_points_y), nvars)
orighat$t[(time_points_x+1):(time_points_x+time_points_y)] = orighat$t[(time_points_x+1):(time_points_x+time_points_y)] + 1.01

# CHOOSE CASE HERE
case = 1

# Visualize
closest = .closest_t(df_Y$Time[[case]], seq(0, 1, length.out = time_points_y))
increment = rep(0:(nvars-1) * time_points_y, each = length(closest))
final = rep(closest, nvars) + increment

orighat_case = cbind(orighat[final, case], orighat[final, "t"]) %>%
  as.data.frame() %>%
  rename(val = V1, t = V2)

orig_case = data.frame(
  case = df_Y[case, 3:(3+nvars-1)] %>% unlist() %>% unname() - c(res_Y1$mu, res_Y2$mu)[final],
  t = orighat_case$t
)

orighat_case %>%
  ggplot() +
  geom_point(data = orighat_case, aes(x = t, y = val)) +
  geom_line(data = orighat_case, aes(x = t, y = val)) +
  geom_point(data = orig_case, aes(x = t, y = case), color = "red") +
  geom_line(data = orig_case, aes(x = t, y = case), color = "red") +
  theme_bw() +
  labs(title = "Red: O, Black: O hat")
```

![](Simulate-CV_files/figure-gfm/check-2.png)<!-- -->

``` r

SST = sum(orig_case^2)
SSR = sum((orig_case-orighat_case)^2)
(R2 = 1-SSR/SST)
#> [1] 0.03578578
```

# Inspect Scores

## Check Scores

``` r
df1 = as.data.frame(U[,1:2])
df2 = as.data.frame(V[,1:2])

df_combined = rbind(
  data.frame(id = row.names(df1), x = df1$V1, y = df1$V2, group = "TP1"),
  data.frame(id = row.names(df1), x = df2$V1, y = df2$V2, group = "TP2")
)
df_arrows = merge(df1, df2, by = "row.names", suffixes = c("_1", "_2"))

df_combined %>%
  ggplot()+
  geom_point(aes(x = x, y = y, color = group, size = abs(x-y)), alpha = 0.5) +
  scale_color_manual(values = c("purple", "orange")) +
  geom_segment(data = df_arrows, aes(x = V1_1, y = V2_1, xend = V1_2, yend = V2_2), 
               arrow = arrow(length = unit(0.2, "cm")), color = "gray") +
  theme_bw()
```

![](Simulate-CV_files/figure-gfm/unnamed-chunk-82-1.png)<!-- -->

# Run CV

## Get Folds

``` r
# Folds
set.seed(16)
fld = caret::createFolds(1:nrow(res$xi), k = 10, list = F)
```

``` r
# Time Points and Predictors
fmatrix = matrix(nrow = 6, ncol = 6)
for(i in 1:6){
  for(j in 1:6){
    # Subset Data and EigenFunctions
    U = res$xi[, 1:i, drop = F]
    V = res_Y$xi[, 1:j, drop = F]

    PhiU = res$stacked_phi[, 1:i, drop = F]
    PhiV = res_Y$stacked_phi[, 1:j, drop = F]
    kfoldR2s = rep(NA, 10)
    
    # Loop through all folds
    for(k in 1:10){
      fld_case = which(fld == k, fld)

      # Regression on remaining 9 folds
      mod = stats::lm(V[fld != k, ] ~ -1 + U[fld != k, ])
      
      # Extract coefficients
      B = mod %>% stats::coef()
      
      # Get out of bag fitted values
      # One outlier in there
      Vhat = U[fld == k, , drop = F] %*% B
      
      # Estimate original profile
      orighat = PhiV %*% t(Vhat) %>% as.data.frame()
      
      # Step to name orighat
      nameCases = c(1:n)[(1:n %in% fld_case)]
      colnames(orighat) = nameCases
      
      # Append Time column
      orighat$t = rep(seq(0, 1, length.out = time_points_y), nvars)
      
      # Now for each case in the bag, we must match each function value to its closest estimate
      # For all cases in the fold, we are taking the average of SSTs and SSRs
      SSTs = rep(NA, sum(fld == k))
      SSRs = rep(NA, sum(fld == k))
      for (case in nameCases){
            # Find closest function values from our estimated functions
            closest = .closest_t(df_Y$Time[[case]], seq(0, 1, length.out = time_points_y))
            increment = rep(0:(nvars-1) * time_points_y, each = length(closest))

            final = rep(closest, nvars) + increment

            orighat_case = orighat[final, which(case == nameCases)]
            orig_case = df_Y[case, 3:(3+nvars-1)] %>% unlist() %>% unname() - c(res_Y1$mu, res_Y2$mu)[final]

            # The mean function is a 0 function.
            SSTs[which(case == nameCases)] = sum(orig_case^2)
            SSRs[which(case == nameCases)] = sum((orig_case-orighat_case)^2)

      }
      kfoldR2s[k] = 1 - mean(SSRs)/mean(SSTs)
    }
    fmatrix[i, j] = mean(kfoldR2s)
  }
}

which(fmatrix == max(fmatrix), arr.ind = T)
#>      row col
#> [1,]   5   2
image(fmatrix)
```

![](Simulate-CV_files/figure-gfm/unnamed-chunk-84-1.png)<!-- -->

# Real Data

``` r
rm(list = ls())

load("res_pred.RData")
load("res_resp.RData")
load("tp2.RData")
load("res_Y1.RData")
load("res_Y2.RData")
```

## In Bag Testing

``` r
time_points_x = nrow(res_pred$unstacked_phi) # 51
time_points_y = nrow(res_resp$unstacked_phi) # 52
nvars = nrow(res_pred$stacked_phi) / time_points_x # 2

t1 = ncol(res_pred$xi) # 10
t2 = ncol(res_resp$xi) # 10

# Full Model
i = j = 10

# Scores
X = res_pred$xi[, 1:i, drop = F]
Y = res_resp$xi[, 1:j, drop = F]


# Eigenfunctions
PhiX = res_pred$stacked_phi[, 1:i, drop = F]
PhiY = res_resp$stacked_phi[, 1:j, drop = F]

# Score by Score Regression
mod = lm(Y ~ -1+X)
B = mod$coefficients

# In Bag Estimation
Yhat = X %*% B

# ISSUE 3: Poor fit in estimating scores
# Even when I decrease the number of components, fit gets very bad with many components
vis = data.frame(
  val = c(c(Y), c(Yhat)),
  type = c(rep("Actual", 32*i), rep("Hat", 32*i)),
  order = c(rep(1:i, each = 32), rep(1:i, each = 32)),
  index = rep(1:32, i)
)
vis %>%
  ggplot(aes(x = index, y = val, color = type))+
  geom_point()+
  geom_line()+
  facet_wrap(~order, scales = "free_y")+
  theme_bw()
```

![](Simulate-CV_files/figure-gfm/unnamed-chunk-86-1.png)<!-- -->

``` r

# Estimate original functional profile
orighat = PhiY %*% t(Yhat) %>% as.data.frame()

# Name for convenience
colnames(orighat) = as.character(1:32)

# Append Time column
# Add slight lag for visualization purpose
orighat$t = rep(seq(0, 1, length.out = time_points_y), nvars)
orighat$t[53:104] = orighat$t[53:104] + 1.01

# CHOOSE CASE HERE
# CASE 6 is VERY PATHOLOGICAL
case = 1

# Visualize
closest = .closest_t(tp2$Time[[case]], seq(0, 1, length.out = time_points_y))
increment = rep(0:(nvars-1) * time_points_y, each = length(closest))
final = rep(closest, nvars) + increment

orighat_case = cbind(orighat[final, case], orighat[final, "t"]) %>%
  as.data.frame() %>%
  rename(val = V1, t = V2)

orig_case = data.frame(
  case = tp2[case, 3:(3+nvars-1)] %>% unlist() %>% unname() - c(res_Y1$mu, res_Y2$mu)[final],
  t = orighat_case$t
)

orighat_case %>%
  ggplot() +
  geom_point(data = orighat_case, aes(x = t, y = val)) +
  geom_line(data = orighat_case, aes(x = t, y = val)) +
  geom_point(data = orig_case, aes(x = t, y = case), color = "red") +
  geom_line(data = orig_case, aes(x = t, y = case), color = "red") +
  theme_bw() +
  labs(title = "Red: O, Black: O hat")
```

![](Simulate-CV_files/figure-gfm/unnamed-chunk-86-2.png)<!-- -->

``` r

SST = sum(orig_case^2)
SSR = sum((orig_case-orighat_case)^2)
(R2 = 1-SSR/SST)
#> [1] -0.1106634
```

## Check Scores

``` r
df1 = as.data.frame(X[,1:2])
df2 = as.data.frame(Y[,1:2])

df_combined = rbind(
  data.frame(id = row.names(df1), x = df1$V1, y = df1$V2, group = "TP1"),
  data.frame(id = row.names(df1), x = df2$V1, y = df2$V2, group = "TP2")
)
df_arrows = merge(df1, df2, by = "row.names", suffixes = c("_1", "_2"))

df_combined %>%
  ggplot()+
  geom_point(aes(x = x, y = y, color = group, size = abs(x-y)), alpha = 0.5) +
  scale_color_manual(values = c("purple", "orange")) +
  geom_segment(data = df_arrows, aes(x = V1_1, y = V2_1, xend = V1_2, yend = V2_2), 
               arrow = arrow(length = unit(0.2, "cm")), color = "gray") +
  theme_bw()
```

![](Simulate-CV_files/figure-gfm/unnamed-chunk-87-1.png)<!-- -->

# Run CV

## Get Folds

``` r
# Folds
set.seed(16)
fld = caret::createFolds(1:nrow(res_pred$xi), k = 10, list = F)
```

``` r
# Time Points and Predictors
fmatrix = matrix(nrow = 10, ncol = 10)
for(i in 1:10){
  for(j in 1:10){
    # Subset Data and EigenFunctions
    U = res_pred$xi[, 1:i, drop = F]
    V = res_resp$xi[, 1:j, drop = F]

    PhiU = res_pred$stacked_phi[, 1:i, drop = F]
    PhiV = res_resp$stacked_phi[, 1:j, drop = F]
    kfoldR2s = rep(NA, 10)
    
    # Loop through all folds
    for(k in 1:10){
      fld_case = which(fld == k, fld)

      # Regression on remaining 9 folds
      mod = stats::lm(V[fld != k, ] ~ -1 + U[fld != k, ])
      
      # Extract coefficients
      B = mod %>% stats::coef()
      
      # Get out of bag fitted values
      # One outlier in there
      Vhat = U[fld == k, , drop = F] %*% B
      
      # Estimate original profile
      orighat = PhiV %*% t(Vhat) %>% as.data.frame()
      
      # Step to name orighat
      nameCases = c(1:32)[(1:32 %in% fld_case)]
      colnames(orighat) = nameCases
      
      # Append Time column
      orighat$t = rep(seq(0, 1, length.out = time_points_y), nvars)
      
      # Now for each case in the bag, we must match each function value to its closest estimate
      # For all cases in the fold, we are taking the average of SSTs and SSRs
      SSTs = rep(NA, sum(fld == k))
      SSRs = rep(NA, sum(fld == k))
      for (case in nameCases){
            # Find closest function values from our estimated functions
            closest = .closest_t(tp2$Time[[case]], seq(0, 1, length.out = time_points_y))
            increment = rep(0:(nvars-1) * time_points_y, each = length(closest))

            final = rep(closest, nvars) + increment

            orighat_case = orighat[final, which(case == nameCases)]
            orig_case = tp2[case, 3:(3+nvars-1)] %>% unlist() %>% unname() - c(res_Y1$mu, res_Y2$mu)[final]

            # The mean function is a 0 function.
            SSTs[which(case == nameCases)] = sum(orig_case^2)
            SSRs[which(case == nameCases)] = sum((orig_case-orighat_case)^2)

      }
      kfoldR2s[k] = 1 - mean(SSRs)/mean(SSTs)
    }
    fmatrix[i, j] = mean(kfoldR2s)
  }
}

which(fmatrix == max(fmatrix), arr.ind = T)
#>      row col
#> [1,]   1   1
image(fmatrix)
```

![](Simulate-CV_files/figure-gfm/unnamed-chunk-89-1.png)<!-- -->
