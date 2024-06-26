% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_query_hensikt.R
\name{build_query_hensikt}
\alias{build_query_hensikt}
\title{Builds query for selecting data for hensikt from PJS}
\usage{
build_query_hensikt(year, hensikt, db = "PJS")
}
\arguments{
\item{year}{[\code{numeric}]\cr
One year or a vector giving the first and last years that should 
be selected.}

\item{hensikt}{[\code{character}]\cr
Vector with one or more specific hensiktkoder. If sub-hensikter
should be included, end the code with \%.}

\item{db}{[\code{character(1)}]\cr
The database for which the query is built. Defaults to "PJS" that currently
is the only valid value.}
}
\value{
A list with select statements for "v2_sak_m_res" and "v_sakskonklusjon",
    respectively.
}
\description{
Builds the query for selecting all data for one or
    more hensikt within one year from PJS. The query is written
    in T-SQL as used by MS-SQL.
}
\details{
The function builds the SQL syntax to select all
    PJS-journals concerning the hensiktkoder from PJS. The
    select statements can thereafter be used to query
    journal_rapp/PJS using
    \ifelse{html}{\code{\link[DBI:dbGetQuery]{DBI::dbGetQuery}}}{\code{DBI::dbGetQuery}}
    when using \code{odbc} or
    \ifelse{html}{\code{\link[RODBC:sqlQuery]{RODBC::sqlQuery}}}{\code{RODBC::sqlQuery}}
    when using \code{RODBC}.
}
\examples{
# SQL-select query for Pancreatic disease (PD)
build_query_hensikt(year = 2020,
                    hensikt = c("0200102"))
}
\author{
Petter Hopp Petter.Hopp@vetinst.no
}
