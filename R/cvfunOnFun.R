#' Internal function for matching to closest time point
#'
#' @param actualhat Time points of estimated functions
#' @param actual Time points of actual functions
#'
#' @return A vector of indices of the closest time points
#'
.closest_t = function(actual_time, hat_time){
  closest = rep(NA, length(actual_time))
  for(i in 1:length(actual_time)){
    closest[i] = which.min(abs(actual_time[i] - hat_time))
  }

  return(closest)
}


#' A function to choose optimal number of eigenfunctions via Cross-Validation
#'
#' @param seed Seed for reproducibility
#' @param predictor An object of class irregMFPCA corresponding to the predictor
#' @param response An object of class irregMFPCA corresponding to the response
#' @param actual A data frame with the actual values of the response
#' @param folds Number of folds for cross-validation. Default is 10
#'
#' @return A matrix of mean squared errors for each combination of number of eigenfunctions
#' @export
#'
cvfunOnFun = function(seed, predictor, response, actual, folds = 10){
  stopifnot(class(predictor) == "irregMFPCA")
  stopifnot(class(response) == "irregMFPCA")
  set.seed(seed)

  # Get Length of time points
  time_points_x = nrow(predictor$unstacked_phi)
  time_points_y = nrow(response$unstacked_phi)
  nvars = nrow(predictor$stacked_phi) / time_points_x

  # Create folds
  fld = caret::createFolds(1:nrow(predictor$xi), k = folds, list = F)

  t1 = ncol(predictor$xi)
  t2 = ncol(response$xi)

  fmatrix = matrix(nrow = t1, ncol = t2)

  for(i in 1:t1){
    for(j in 1:t2){
      X = predictor$xi[, 1:i, drop = F]
      Y = response$xi[, 1:j, drop = F]
      PhiX = predictor$stacked_phi[, 1:i, drop = F]
      PhiY = response$stacked_phi[, 1:j, drop = F]
      kfoldMSEs = rep(NA, folds)


      # Loop Through Folds
      # LOO
      for(k in 1:folds){
        fld_case = which(fld == k, fld)

        # First do multivariate multiple regression to
        # Regress Eigenscores in TP2 on Eigenscores in TP1
        B = stats::lm(Y[fld != k, ] ~ -1 + X[fld != k, ]) %>% stats::coef()

        # Find estimates of eigenscores in TP2
        Yhat = X[fld == k, , drop = F] %*% B

        # Find estimates of original NW and WR data
        # Estimates of eigen scores X eigen functions
        orighat = PhiY %*% t(Yhat) %>% as.data.frame()

        # Change column names
        # Append a time variable
        colnames(orighat) = fld_case
        orighat$t = rep(seq(0, 1, length.out = time_points_y), nvars)

        # Loop over all cases
        MSEs = rep(NA, sum(fld == k))

        for (case in fld_case){
          # Find closest function values from our estimated functions
          closest = .closest_t(actual$Time[[case]], seq(0, 1, length.out = time_points_y))
          increment = rep(0:(nvars-1) * time_points_y, each = length(closest))

          final = rep(closest, nvars) + increment
          # Pull out corresponding estimates of the original
          # These come from demeaned values; Is the demeaning done per case?
          # Or is it done by the global mean?
          orighat_case = orighat[final, which(case == fld_case)]
          orig_case = actual[case, 3:(3+nvars-1)] %>% unlist() %>% unname()

          # Find Mean sum of squares error for this case
          MSEs[which(case == fld_case)] = mean((orig_case-orighat_case)^2)
          if(is.nan(mean((orig_case-orighat_case)^2))){
            print(orig_case)
            print(orighat_case)
          }
        }
        kfoldMSEs[k] = mean(MSEs)
      }
      fmatrix[i, j] = mean(kfoldMSEs)
    }
  }
  return(fmatrix)
}
