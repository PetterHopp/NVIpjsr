#' @title Builds query to select all data for a single disease from PJS
#' @description Builds the query for selecting all data for one infectious
#'     agent/disease for the chosen period from PJS. The necessary input is the
#'     period (given by year(s) or dates) and analytter. In addition one may
#'     input hensiktskoder, utbruddsID-er and metodekoder specific for the
#'     infection and/or disease. The the query is written in T-SQL as used by
#'     MS-SQL.
#'
#' @details The function builds select statements with SQL syntax to select all
#'     PJS-saker regarding a single disease from PJS. The select statements can
#'     thereafter be used to query journal_rapp/PJS either by giving it as input
#'     to the wrapper function
#'     \ifelse{html}{\code{\link{retrieve_PJSdata}}}{\code{retrieve_PJSdata}} or
#'     by using it in the statement argument in
#'     \ifelse{html}{\code{\link[DBI:dbGetQuery]{DBI::dbGetQuery}}}{\code{DBI::dbGetQuery}}
#'     when using \code{odbc} or by using it in the query argument in
#'     \ifelse{html}{\code{\link[RODBC:sqlQuery]{RODBC::sqlQuery}}}{\code{RODBC::sqlQuery}}
#'     when using \code{RODBC}.
#'
#'     The select statements are build to select all cases for a single
#'     infectious agent and disease. For the input analytt, all analyttkoder
#'     relevant for the infectious agent should be included, i.e. the
#'     infectious agent code, the disease code. For some agents also analytt
#'     codes for agent properties should be included. This should ensure that
#'     all cases with a result, konklusjon or sakskonklusjon for the analytt(er)
#'     are included. Thereby, all journals were the examination have been
#'     performed and a result or conclusion has been entered should be selected.
#'
#'     One or more specific utbruddsid may be given as input. The utbruddsid is
#'     the internal id of the utbrudd in the utbrudds-register in PJS. With
#'     specific utbruddsid is meant a utbruddsid that imply that the sample
#'     should be examined for the infectious agent or disease. Thereby, the
#'     selection will also include samples that are part of an outbreak but
#'     haven't been set up for examination yet, samples that were
#'     rejected and samples for which wrong result analytt or conclusion
#'     analytt have been entered.
#'
#'     One or more specific hensikter may be input to the selection statement.
#'     With specific hensikt is meant a hensikt that will imply that the sample
#'     should be examined for the infectious agent or disease. Thereby, the
#'     selection will also include samples that have a relevant hensikt, but
#'     haven't been set up for examination yet, samples that were unfit for
#'     examination and samples for which wrong result analytt or conclusion
#'     analytt have been entered.
#'
#'     One or more specific metoder may be input to the selection statement.
#'     With specific metode is meant a metode that implies an examination that
#'     will give one of the input analytter as a result. Thereby, the query
#'     will include samples that have been set up for examination, but haven't
#'     been examined yet, samples that were unfit for examination and samples
#'     for which wrong results have been entered.
#'
#' @param period [\code{numeric}]\cr
#'     Time period given as either one year or a vector giving the first
#'     and last years or as a vector giving the first and last dates.
#' @param analytt [\code{character}]\cr
#'     Analyttkoder that should be selected. If sub-analytter should be included,
#'     end the code with \%.
#' @param hensikt [\code{character}]\cr
#'     Specific hensiktkoder. If sub-hensikter should be included,
#'     end the code with \%. Defaults to \code{NULL}.
#' @param metode [\code{character}]\cr
#'     Specific metodekoder. Defaults to \code{NULL}.
#' @param utbrudd  [\code{character}]\cr
#'     Specific utbruddsid(er) that should be selected. Defaults to \code{NULL}.
#' @template build_query_db
#'
#' @return A list with select-statements for "v_sak_prove_konkl",
#'     "v_sak_prove_res", and "v_sakskonklusjon".
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' # SQL-select query for Pancreatic disease (PD)
#' build_query_single_disease(
#'   period = 2020,
#'   analytt = c("01220104%", "1502010235"),
#'   hensikt = c("0100108018", "0100109003", "0100111003", "0800109"),
#'   metode = c("070070", "070231", "010057", "060265")
#'   )
#'
build_query_single_disease <- function(period,
                                       analytt = NULL,
                                       hensikt = NULL,
                                       metode = NULL,
                                       utbrudd = NULL,
                                       db = "PJS") {

  # ARGUMENT CHECKKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  checkmate::assert(checkmate::check_integerish(period,
                                                lower = 1990,
                                                upper = as.numeric(format(Sys.Date(), "%Y")),
                                                any.missing = FALSE,
                                                min.len = 1),
                    checkmate::check_date(period,
                                          lower = as.Date("1990-01-01"), upper = Sys.Date(),
                                          any.missing = FALSE,
                                          min.len = 1, max.len = 2),
                    add = checks)
  checkmate::assert_character(analytt, min.chars = 2, any.missing = FALSE, add = checks)
  checkmate::assert_character(hensikt, min.chars = 2, null.ok = TRUE, any.missing = FALSE, add = checks)
  checkmate::assert_character(metode, min.chars = 2, null.ok = TRUE, any.missing = FALSE, add = checks)
  checkmate::assert_character(utbrudd, min.chars = 1, null.ok = TRUE, any.missing = FALSE, add = checks)
  checkmate::assert_choice(db, choices = c("PJS"), add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)

  # PREPARE INPUT BEFORE BUILDING QUERIES ----
  if (inherits(period, what = "Date")) {
    year <- as.numeric(format(period, "%Y"))
  } else {
    year <- period
    }


  # BUILD QUERY FOR v_sak_prove_konkl ----
  # Extract all samples
  #  1 with relevant konkl_analytt and
  #  2 with relevant hensikt or utbrudd and missing konkl_analytt. Thereby,
  #    irrelevant konkl_analytt should be avoided for the hensikt and utbrudd.

  # Build modules for the select statement
  # Build sql code snippet for extracting year, always present
  sql_snippet_year <- build_sql_select_year(year = year, varname = "aar")

  # Build sql code snippet for extracting konkl_analyttkode, always present
  sql_snippet_konkl_analytt <- build_sql_select_code(values = analytt, varname = "konkl_analyttkode")

  # Build extra part if hensikt or utbrudd are present
  if (!is.null(hensikt) | !is.null(utbrudd)) {
    # Build sql code snippet for extracting hensikt
    if (!is.null(hensikt)) {
      sql_snippet_hensikt <- build_sql_select_code(values = hensikt, varname = "hensiktkode")
      if (!is.null(utbrudd)) {
        sql_snippet_hensikt <- paste(sql_snippet_hensikt, "OR")
      }
    } else {sql_snippet_hensikt <- ""}

    # Build sql code snippet for extracting utbrudd
    if (!is.null(utbrudd)) {
      sql_snippet_utbrudd <- build_sql_select_code(values = utbrudd, varname = "utbrudd_id")
    } else {sql_snippet_utbrudd <- ""}

    # Combining sql_snippet_hensikt and sql_snippet_utbrudd with missing(konkl_analytt)
    sql_snippet_missing_konkl_analytt <-
      paste("OR", "((", sql_snippet_hensikt, sql_snippet_utbrudd, ")",
            "AND konkl_analyttkode IS NULL)")
  } else {sql_snippet_missing_konkl_analytt <- ""}

  # Combine code snippets into query for v_sak_prove_konkl
  query_v_sak_prove_konkl <- paste("SELECT * FROM v_sak_prove_konkl",
                                   "WHERE", sql_snippet_year,
                                   "AND",
                                   "(",
                                   sql_snippet_konkl_analytt,
                                   sql_snippet_missing_konkl_analytt,
                                   ")")

  # BUILD QUERY FOR v_sak_prove_res ----
  # Ensures that all samples with relevant metode or res_analytt are included
  # Build modules for the select statement
  # Use already created modules for year, hensikt, and utbrudd

  # Build sql code snippet for extracting res_analyttkode, always present
  sql_snippet_res_analytt <- build_sql_select_code(values = analytt, varname = "analyttkode_funn")

  # Build sql code snippet for extracting metode
  if (!is.null(metode)) {
    sql_snippet_metode <- build_sql_select_code(values = metode, varname = "metodekode")
    sql_snippet_metode <- paste(sql_snippet_metode, "OR")
  } else {sql_snippet_metode <- ""}

  # Build extra part if hensikt or utbrudd are present
  if (!is.null(hensikt) | !is.null(utbrudd)) {
    # Combining sql_snippet_hensikt and sql_snippet_utbrudd with missing(res_analytt)
    sql_snippet_missing_metode <-
      paste("OR", "((", sql_snippet_hensikt, sql_snippet_utbrudd, ")",
            "AND metodekode IS NULL)")
  } else {sql_snippet_missing_metode <- ""}

  # Combine modules into query for v_sak_prove_konkl
  query_v_sak_prove_res <- paste("SELECT * FROM v_sak_prove_res",
                                 "WHERE", sql_snippet_year, "AND",
                                 "(",
                                 sql_snippet_metode,
                                 sql_snippet_res_analytt,
                                 sql_snippet_missing_metode,
                                 ")")


  # BUILD QUERY FOR THE SELECT STATEMENT FOR v_sakskonklusjon ----
  # Build sql code snippet for extracting saks_year
  sql_snippet_sak_year <- build_sql_select_year(year = year, varname = "sak.aar")

  # Build sql code snippet for extracting sakskonkl_analyttkode
  sql_snippet_sakskonkl_analytt <- build_sql_select_code(values = analytt, varname = "analyttkode")

  # Combine modules into query for v_sakskonklusjon
  query_sakskonklusjon <- paste("SELECT v_sakskonklusjon.*,",
                                "sak.mottatt_dato, sak.uttaksdato, sak.sak_avsluttet, sak.hensiktkode,",
                                "sak.eier_lokalitetstype, sak.eier_lokalitetnr",
                                "FROM v_innsendelse AS sak",
                                "INNER JOIN v_sakskonklusjon",
                                "ON (v_sakskonklusjon.aar = sak.aar AND",
                                "v_sakskonklusjon.ansvarlig_seksjon = sak.ansvarlig_seksjon AND",
                                "v_sakskonklusjon.innsendelsesnummer = sak.innsendelsesnummer)",
                                "WHERE", sql_snippet_sak_year, "AND",
                                paste0("(", sql_snippet_sakskonkl_analytt, ")"))

  # REMOVE EXTRA SPACES FROM SELECT QUERIES
  for (query in c("query_v_sak_prove_konkl", "query_v_sak_prove_res", "query_sakskonklusjon")) {
    query <- gsub("\\s+", " ", query) # multiple spaces -> one
    query <- gsub("\\( ", "(", query) # remove space after (
    query <- gsub(" \\)", ")", query) # remove space before )
  }

  # RETURN SELECT QUERIES
  # Combine queries into list
  # Each select statment is given name after the main table for the selection query
  select_statement <- list("v_sak_prove_konkl" = query_v_sak_prove_konkl,
                           "v_sak_prove_res" = query_v_sak_prove_res,
                           "v_sakskonklusjon" = query_sakskonklusjon)


  # return list
  return(select_statement)
}
