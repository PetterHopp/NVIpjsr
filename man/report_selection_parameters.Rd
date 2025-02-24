% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_selection_parameters.R
\name{report_selection_parameters}
\alias{report_selection_parameters}
\title{Report selection parameters}
\usage{
report_selection_parameters(
  year = NULL,
  selection_parameters = NULL,
  additional_parameters = NULL,
  translation_table = PJS_codes_2_text
)
}
\arguments{
\item{year}{[\code{numeric}]\cr
One year or a vector giving the first and last years that should be selected.
    Defaults to \code{NULL}.}

\item{selection_parameters}{[\code{character(1)}]\cr
Either the path and file name for an R script that can be sourced and that
    sets the selection parameters or a named list with the selection parameters
    (i.e. of the same format as the output of
    \code{\link{set_disease_parameters}}).
    Defaults to \code{NULL}.}

\item{additional_parameters}{[\code{character(1)}]\cr
A named list with additional selection parameters not included in the standard
    selection parameters, for example sample type. Defaults to \code{NULL}.}

\item{translation_table}{[\code{data.frame}] \cr
Table with the code and the description for PJS variables. Defaults to
"PJS_codes_2_text".}
}
\value{
[\code{data.frame}] with the selection parameters prepared for reporting.
}
\description{
Reports the selection parameters to a data frame.
}
\details{
The selection parameters can be set by using
    \code{\link{set_disease_parameters}}.
    or by giving a list of similar format for input to
    \code{selection_parameters}, see the build_query-functions for necessary
    input.

When using the argument additional_parameters the input should be a named list.
    The name of each element should be constructed by the "variable type", "2"
    and thereafter either "select" or "delete", i.e. "metode2delete". The
    variable type must be one of the variable types for which the code can be
    automatically translated to description text, see
    \code{\link{add_PJS_code_description}}.
}
\examples{
\dontrun{
# Reads translation table for PJS codes
PJS_codes_2_text <- read_PJS_codes_2_text()

selection_parameters <- set_disease_parameters(
  hensikt2select = c("0100108018", "0100109003", "0100111003", "0800109"),
  analytt2select = c("01220104\%", "1502010235"),
  metode2select = c("070070", "070231", "010057", "060265"),
  FUN = build_query_one_disease)

selection <- report_selection_parameters(year = 2024,
                                         selection_parameters = selection_parameters,
                                         translation_table = PJS_codes_2_text)
}

}
\author{
Petter Hopp Petter.Hopp@vetinst.no
}
