#' A function to simulate missing data
#'
#' @param n_obs Number of observations to keep
#' @param dat A matrix of data
#' @param t A vector of time points
#' @param seed A seed for random number generation
#'
#' @return A matrix of data with missing values
#' @export
#'
simMiss = function(n_obs,
                                  dat,
                                  t = seq(0, 1, length.out = 51),
                                  seed = 51){
  stopifnot(is.numeric(n_obs))
  stopifnot(is.matrix(dat))
  stopifnot(is.numeric(t))
  stopifnot(n_obs < length(t))
  stopifnot(ncol(dat) %% length(t) == 0)

  set.seed(seed)

  N = nrow(dat); l = length(t); c = ncol(dat)/l

  for(i in 1:N){
    s = sample(1:l, l - n_obs)
    S = s
    if(c > 1){
      for(j in 1:(c-1)){
        S = c(S, s + j*l)
      }
    }
    dat[i, ][S] = NA
  }

  # Add time row
  dat = rbind(dat, rep(t, c))

  # Transpose and change to data frame
  # dat = dat %>% t() %>% as.data.frame()

  return(dat)
}
