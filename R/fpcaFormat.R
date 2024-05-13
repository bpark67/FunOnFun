#' A function to fill an empty vector with values from a data frame
#'
#' @param v A vector to fill
#' @param target The target column to fill
#' @param source The source data frame
#' @param id_col The column that contains the unique identifier for each subject
#' @param uid A unique identifier for each subject
#'
#' @return A filled vector
.vector_filler = function(v, target, source, id_col, uid){
  stopifnot(is.vector(v))
  stopifnot(is.character(target))
  stopifnot(is.data.frame(source))

  id_col = grep(id_col, names(source), value = TRUE)
  N = length(v)

  for(k in 1:N){
    v[k] = source %>%
      dplyr::filter(!!dplyr::sym(id_col) == uid[k]) %>%
      dplyr::select(dplyr::all_of(target)) %>%
      dplyr::pull() %>%
      list()
  }
  return(v)
}


#' A function to format data for FPCA
#'
#' @param dat A data frame of data
#' @param id_col Designates the column that contains the unique identifier for each subject
#'
#' @return A tibble of data in FPCA format
#' @export
fpcaFormat = function(dat, id_col){
  # Grab the number of components
  C = ncol(dat)-3

  # Convert to FPCA friendly format
  df_tib = dat %>% dplyr::as_tibble()

  # Define variables
  uid = unique(df_tib$id)

  N = length(uid)
  Time = rep(0,N)

  for(c in 1:C){
    assign(paste0("Variable", c), rep(0, N))
  }

  # Fill list with vectors of observation for each person
  Time = .vector_filler(Time, target = "t", source = df_tib, id_col = id_col, uid = uid)

  for(c in 1:C){
    assign(paste0("Variable", c),
           .vector_filler(get(paste0("Variable", c)),
                         target = paste0("variable", c),
                         source = df_tib,
                         id_col = id_col,
                         uid = uid))
  }

  vars = dplyr::syms(c("uid", "Time", paste0("Variable", 1:C)))

  return(tibble::tibble(!!!vars))
}
