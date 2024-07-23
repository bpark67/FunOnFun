#' A function to reconstruct the beta matrix from the response and predictor `irregMFPCA` objects
#'
#' @param resp A response `irregMFPCA` object
#' @param pred A predictor `irregMFPCA` object
#' @param Bhat The estimated B matrix from the `funOnFun` function
#'
#' @return The reconstructed estimated beta matrix
#' @export
#'
reconBeta = function(resp, pred, Bhat){
  stopifnot(class(resp) == "irregMFPCA")
  stopifnot(class(pred) == "irregMFPCA")

  return(resp$stacked_phi %*% t(Bhat) %*% t(pred$stacked_phi))
}

#' A function to reconstruct the estimand, the population beta matrix, from the predictor `simMFPCA` object
#'
#' @param popCovY The population covariance matrix of the response
#' @param popB The population B matrix
#' @param t The time points
#' @param X The predictor `simMFPCA` object
#'
#' @return The reconstructed population beta matrix
#' @export
#'
populationBeta = function(popCovY, popB, t, X){
  stopifnot(class(X) == "simMFPCA")
  stopifnot(length(t)*X$n_variables == dim(X$phi)[1])
  stopifnot(dim(popB)[1] == dim(X$phi)[2])

  components = dim(popB)[2]
  eigenVectors = eigen(popCovY)$vectors[, 1:components]

  return((eigenVectors * sqrt(X$n_variables * length(t))) %*% popB %*% t(X$phi))
}
