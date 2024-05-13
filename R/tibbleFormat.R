#' A function to convert a data frame into a long format
#'
#' @param dat A data frame of data
#'
#' @return A data frame in long format
.longFormat = function(dat){
  # Grab number of cases; last column is the time index
  # Grab length of time index as well; by default should be 51
  N = ncol(dat) - 1; l = length(t)

  # Pull out first column, which correponds to first case
  long_data = dat[, c(1, N + 1)] %>%
    stats::na.omit() %>%
    dplyr::mutate("id" = 1)

  colnames(long_data) = c("value", "t", "id")
  long_data = long_data %>%
    dplyr::select("id", "t", "value")

  # Iterate over remaining columns
  for(i in 2:N){
    append_dat = dat[, c(i, N + 1)] %>%
      stats::na.omit() %>%
      dplyr::mutate("id" = i)
    colnames(append_dat) = c("value", "t", "id")
    append_dat = append_dat %>%
      dplyr::select("id", "t", "value")

    long_data = long_data %>%
      rbind(append_dat)

    rm(append_dat)
  }

  long_data$id = as.numeric(long_data$id)
  long_data = long_data %>% dplyr::arrange("id")
  return(long_data)
}

#' A function to convert a matrix into a tibble
#'
#' @param dat A matrix of data
#' @param t A vector of time points
#'
#' @return A tibble of data
#' @export
tibbleFormat = function(dat, t){
  stopifnot(is.matrix(dat))
  stopifnot(ncol(dat) %% length(t) == 0)
  stopifnot(dat[nrow(dat), 1:length(t)] == t)


  dat = dat %>% t() %>% as.data.frame()
  # Grab number of cases, length of time index, and number of components
  N = ncol(dat) - 1; l = length(t); c = nrow(dat)/l

  # Split data into each component's dataframe
  chunks = split(1:nrow(dat), ceiling(1:nrow(dat)/l))
  list_dat = lapply(chunks, function(c) dat[c,])

  # Format into long format (Similar to reading "Walli" data)
  list_long_dat = lapply(list_dat, .longFormat)

  long_data = list_long_dat %>%
    purrr::reduce(dplyr::left_join, by = c("id", "t"))

  colnames(long_data) = c("id", "t", paste0("variable", 1:c))

  df = tsibble::as_tsibble(
    long_data,
    key = "id",
    index = "t",
    regular = F
    )

  df = df %>%
    brolgar::add_n_obs()

  return(df)
}
