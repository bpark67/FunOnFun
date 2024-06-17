#' Function to conduct Function on Function regression based on eigenscores
#'
#' @param resp A response `irregMFPCA` object
#' @param pred A predictor `irregMFPCA` object
#'
#' @return The regression matrix
#' @export
#'
funOnFun = function(resp, pred){
  stopifnot(class(resp) == "irregMFPCA")
  stopifnot(class(pred) == "irregMFPCA")

  mod = stats::lm(resp$xi ~ 0 + pred$xi)

  return(mod$coefficients)
}

#' Function to return estimand of `funOnFun`
#'
#' @param X A predictor `simMFPCA` object
#' @param popXiY The population orthogonalized eigenscores of the response
#'
#' @return The estimand of the regression matrix
#' @export
#'
populationReg = function(X, popXiY){
  stopifnot(class(X) == "simMFPCA")

  xiX = X$xi %*% X$D
  mod = stats::lm(popXiY ~ 0 + xiX)

  return(mod$coefficients)
}
