#' Compute Inner Product (Covariance) of Matrices
#'
#' @param mat An n x p matrix
#'
#' @return A p x p matrix of the inner product of the columns of mat
#' @export
#'
mip = function(mat){
  stopifnot(is.matrix(mat))

  return(t(mat) %*% mat)
}
