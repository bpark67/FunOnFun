% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fpcaFormat.R
\name{fpcaFormat}
\alias{fpcaFormat}
\title{A function to format data for FPCA}
\usage{
fpcaFormat(dat, id_col, var_cols = NULL)
}
\arguments{
\item{dat}{A data frame of data}

\item{id_col}{Designates the column that contains the unique identifier for each subject}

\item{var_cols}{A vector of column names that contain the variables of interest}
}
\value{
A tibble of data in FPCA format
}
\description{
A function to format data for FPCA
}
