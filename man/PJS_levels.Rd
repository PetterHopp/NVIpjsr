% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{PJS_levels}
\alias{PJS_levels}
\title{Data: Variables per PJS-level.}
\format{
A data frame with 9 variables:
\describe{
  \item{variable}{column name for variables read from PJS and standardized using
    \ifelse{html}{\code{\link[NVIdb:standardize_columns]{NVIdb::standardize_columns}}}{\code{NVIdb::standardize_columns}}}
  \item{sak}{columns at sak-level are given value 1}
  \item{prove}{columns at prove-level are given value 1}
  \item{delprove}{columns at delprove-level are given value 1}
  \item{undersokelse}{columns at undersokelse-level are given value 1}
  \item{resultat}{columns at resultat-level are given value 1}
  \item{konklusjon}{columns at konklusjon-level are given value 1}
  \item{subundersokelse}{columns at subundersokelse-level are given value 1}
  \item{subresultat}{columns at subresultat-level are given value 1}
}
}
\source{
"./data-raw/PJS_levels.xlsx" in package \code{NVIpjsr}
}
\usage{
PJS_levels
}
\description{
A data frame with the variable names (column names) in PJS and their corresponding PJS-level. The column names
   are the standardized column names, i.e. after running
    \ifelse{html}{\code{\link[NVIdb:standardize_columns]{NVIdb::standardize_columns}}}{\code{NVIdb::standardize_columns}}.
   The raw data can be edited in the "./data-raw/PJS_levels.xlsx" and the the code for preparing of the data frame is written in
  "./data-raw/generate_PJS_levels.R". The \code{PJS_levels} is used as input for \code{\link{choose_PJS_levels}}.
}
\details{
The variables included into a specific level is given the value 1, if not included they are given the value 0. To
   ensure that information on a specific level an be traced to the correct sak, all index variables are given value 1.
}
\keyword{datasets}
