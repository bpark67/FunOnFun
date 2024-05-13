#' A function to normalize a matrix
#'
#' @param mat A matrix
#' @param normal A scalar to normalize the matrix. Default is the number of rows of the matrix
#'
#' @return A matrix normalized so that its inner product is \eqn{normal*I_n}
#'
.normalizer = function(mat, normal = NULL){
  if(is.null(normal)){normal = nrow(mat)}

  constants = sqrt(normal/apply(mat, 2, function(x) sum(x^2)))

  return(mat %*% diag(constants))
}

#' A function to generate mean functions for each variable
#'
#' @param t A vector of time points
#' @param func_list A list of functions to generate mean functions
#'
#' @return A t by num.var matrix of mean functions
.generate_mu = function(t, func_list){
  stopifnot(is.numeric(t))
  stopifnot(all(sapply(func_list, is.function)))

  res = lapply(func_list, function(f) f(t))
  return(unlist(res))
}

#' A function to generate orthonormal eigenscores
#'
#' @param n Number of subjects
#' @param components Number of components
#'
#' @return A n by length(t) matrix of orthogonal eigenscores such that the inner product is \eqn{n \times I_n}
#'
.generate_xi = function(n, components){
  stopifnot(is.numeric(n))
  stopifnot(is.numeric(components))

  scores = matrix(stats::rnorm(n * components), nrow = n)
  scores = scores %>% apply(2, scale) %>% .normalizer()
  return(scores)
}

#' A function to generate orthogonal eigenfunctions
#'
#' @param t A vector of time points
#' @param eigen_funs A list of functions to generate eigenfunctions; Length should match number of components
#'
#' @return A t by number of components matrix of eigenfunctions such that the inner product is length of t * \eqn{I_n}
#'
.generate_phi = function(t, eigen_funs){
  stopifnot(is.numeric(t))
  stopifnot(all(sapply(eigen_funs, is.function)))

  eigenfs = lapply(eigen_funs, function(f) f(t)) %>% do.call(cbind, .) %>% .normalizer()
  return(eigenfs)
}

#' A function to stack multiple eigenfunctions if there are multiple variables
#'
#' @param t A vector of time points
#' @param eigen_funs_list A list of lists of functions to generate eigenfunctions for each variable
#'
#' @return A stacked matrix of eigenfunctions
#'
.stack_phi = function(t, eigen_funs_list){
  stopifnot(is.list(eigen_funs_list))
  stopifnot(all(sapply(eigen_funs_list, is.list)))
  stopifnot(all(sapply(eigen_funs_list, function(x) all(sapply(x, is.function)))))
  # Check if all function lists are of equal length
  stopifnot(all(sapply(eigen_funs_list, function(x) length(x) == length(eigen_funs_list[[1]]))))

  eigenfs = lapply(eigen_funs_list, function(eigen_funs) .generate_phi(t, eigen_funs))
  return(do.call(rbind, eigenfs))
}



#' A function to generate randomly simulated data for multivariate functional principal component analysis
#'
#' @param seed A seed for random number generation
#' @param t A vector of time points
#' @param n Number of subjects
#' @param components Number of principal components
#' @param mean_funs A list of functions to generate mean functions for each variable
#' @param eigen_funs_list A list of lists of functions to generate eigenfunctions for each variable
#' @param lambdas A vector of magnitudes of eigenvalues
#' @param response A logical indicating whether the data is a response or a covariate
#' @param B A matrix of regression coefficients for the response data
#'
#' @return A list of simulated data
#' @export
#'
simMFPCA = function(seed, t, n, components, mean_funs, eigen_funs_list, lambdas, response = FALSE, B = NULL){
  stopifnot(is.numeric(t))
  stopifnot(is.numeric(n))
  stopifnot(is.numeric(components))
  stopifnot(length(eigen_funs_list) == length(mean_funs))
  stopifnot(is.numeric(lambdas))
  if(response){
    stopifnot(!is.null(B))
    stopifnot(all(dim(B) == c(components, components)))
    }

  set.seed(seed)
  mu = .generate_mu(t, mean_funs)
  xi = .generate_xi(n, components)
  phi = .stack_phi(t, eigen_funs_list)
  D = diag(lambdas)

  # Generate the data
  if(response){
    X = xi %*% B %*% D %*% t(phi) + rep(1, n) %*% t(mu)
  }else{
    X = xi %*% D %*% t(phi) + rep(1, n) %*% t(mu)
  }

  return(
    list(
      X = X,
      mu = mu,
      xi = xi,
      phi = phi,
      D = D,
      type = ifelse(response, "response", "covariate")
      )
    )
}
