#' A function to regularize time for each id in a dataset
#'
#' @param dat A data frame with clustered data
#' @param id_col The name of the column with the id
#'
#' @return A data frame with a new column t that goes from 0 to 1 for each id
#' @export
#'
regularizeTime = function(dat, id_col){
  nobs = dat %>%
    dplyr::group_by(get(id_col)) %>%
    dplyr::summarize(n = n())

  time_indices = list()

  for(i in 1:nrow(nobs)){
    time_indices[[i]] = seq(from = 0, to = 1, length.out = nobs$n[i])
  }

  dat$t = unlist(time_indices)

  return(dat)
}
