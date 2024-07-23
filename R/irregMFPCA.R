#' A function to perform irregular MFPCA
#'
#' @param components Number of principal components to consider
#' @param split Whether to split the data into separate variables
#' @param ... A list of FPCA objects
#'
#' @return A list containing the unstacked (wide) eigenfunctions, stacked (long) eigenfunctions, eigenscores, Dhat, and Xhat
#' @export
irregMFPCA = function(components = NULL, split = F, ...){
  stopifnot(length(list(...)) > 1)

  J = length(list(...))

  eigs = lapply(list(...), function(x) x$phi)
  xis = lapply(list(...), function(x) x$xiEst)
  eigs_all = do.call(cbind, eigs)
  XiEst = do.call(cbind, xis)

  ms = sapply(eigs, function(x) ncol(x))
  Mplus = sum(ms)

  N = nrow(XiEst)

  Zhat = (N-1)^(-1)*t(XiEst) %*% (XiEst)
  eigen_analysis = eigen(Zhat)

  cm = eigen_analysis$vectors

  cmat = matrix(0, Mplus, J*Mplus)

  for(j in 1:J){
    if(j == 1){
      cmat[1:ms[j], 1:Mplus] = cm[1:ms[j], ]
    } else{
      cmat[(ms[(j-1)]+1):(ms[(j-1)] + ms[j]), ((j-1)*Mplus+1):(j*Mplus)] = cm[(ms[(j-1)]+1):(ms[(j-1)] + ms[j]), ]
    }
  }

  psi = eigs_all %*% cmat
  psi = sqrt(J)*psi

  spl = split(1:ncol(psi), ceiling(1:ncol(psi)/Mplus))
  list_psi = lapply(spl, function(c) psi[,c])
  stack = do.call(rbind, list_psi) %>% as.matrix()

  rho = XiEst %*% cm
  rho = sqrt(J)^(-1)*rho

  Xhat = rho %*% t(stack)

  if(!is.null(components)){
    cols = 1:components
    for(j in 2:J){cols = append(cols, 1:components + Mplus)}
    psi = psi[, cols]
    stack = stack[, 1:components, drop = F]
    rho = rho[, 1:components]
    Xhat = rho %*% t(stack)
  }

  if(split == T){
    spl = split(1:ncol(Xhat), ceiling(1:ncol(Xhat)/(nrow(eigs_all)/J)))
    list_Xhat = lapply(spl, function(c) Xhat[,c])
    return(structure(list(
      "unstacked_phi" = psi,
      "stacked_phi" = stack,
      "xi" = rho,
      "Dhat" = t(rho) %*% rho / N,
      "Xhat" = list_Xhat
    ),
    class = "irregMFPCA"
    )
    )
  }else{
    return(structure(
      list(
      "unstacked_phi" = psi,
      "stacked_phi" = stack,
      "xi" = rho,
      "Dhat" = t(rho) %*% rho / N,
      "Xhat" = Xhat
    ),
      class = "irregMFPCA"
    )
  )
}



  if(is.null(components)){
    Xhat = rho %*% t(stack)
    if(split == T){
      spl = split(1:ncol(Xhat), ceiling(1:ncol(Xhat)/51))
      list_Xhat = lapply(spl, function(c) Xhat[,c])
      return(structure(list("unstacked_phi" = psi,
                  "stacked_phi" = stack,
                  "rho" = rho,
                  "Dhat" = t(rho) %*% rho / N,
                  "Xhat" = list_Xhat),
                  class = "irregMFPCA")
             )
    }else{
      return(structure(list("unstacked_phi" = psi,
                  "stacked_phi" = stack,
                  "rho" = rho,
                  "Dhat" = t(rho) %*% rho / N,
                  "Xhat" = Xhat),
                  class = "irregMFPCA"))
    }
  }
}
