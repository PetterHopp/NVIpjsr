#' @title Builds query to select data for a single disease from PJS
#' @description Builds the query for selecting all data for one infectious
#'     agent/disease for selected years from PJS. The necessary input is the
#'     year(s) and analytter. In addition one may input specific utbruddsid,
#'     hensiktskoder and metodekoder specific for the infection and/or disease.
#'     The the query is written in T-SQL as used by MS-SQL.
#'
#' @details The function builds select statements with SQL syntax to
#'     select all PJS-saker regarding a single disease from PJS.
#'     The select statements can thereafter be used to query
#'     journal_rapp/PJS using
#'     \ifelse{html}{\code{\link[DBI:dbGetQuery]{DBI::dbGetQuery}}}{\code{DBI::dbGetQuery}}
#'     when using \code{odbc} or
#'     \ifelse{html}{\code{\link[RODBC:sqlQuery]{RODBC::sqlQuery}}}{\code{RODBC::sqlQuery}}
#'     when using \code{RODBC}.
#'
#'     The select statements are build to select all cases for a single
#'     infectious agent and disease. For the input analytt, all analyttkoder
#'     relevant for the infectious agent should be included, i.e. the
#'     infectious agent code, the disease code and for some agents also analytt
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
#' @template build_query_year
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
#'   year = 2020,
#'   analytt = c("01220104%", "1502010235"),
#'   hensikt = c("0100108018", "0100109003", "0100111003", "0800109"),
#'   metode = c("070070", "070231", "010057", "060265")
#'   )
#'
build_query_single_disease <- function(year,
                                       analytt = NULL,
                                       hensikt = NULL,
                                       metode = NULL,
                                       utbrudd = NULL,
                                       db = "PJS") {

  # ARGUMENT CHECKKING ----

  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # Perform checks
  checkmate::assert_integerish(year, lower = 1990, upper = as.numeric(format(Sys.Date(), "%Y")), min.len = 1, add = checks)
  checkmate::assert_character(analytt, min.chars = 2, any.missing = FALSE, add = checks)
  checkmate::assert_character(hensikt, min.chars = 2, null.ok = TRUE, any.missing = FALSE, add = checks)
  checkmate::assert_character(metode, min.chars = 2, null.ok = TRUE, any.missing = FALSE, add = checks)
  checkmate::assert_character(utbrudd, min.chars = 1, null.ok = TRUE, any.missing = FALSE, add = checks)
  checkmate::assert_choice(db, choices = c("PJS"), add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)

  # BUILD QUERY FOR v_sak_prove_konkl ----
  # Build modules for the select statement
  # Build select module for year
  select_year <- build_sql_select_year(year = year, varname = "aar")

  # Build select module for hensikt
  select_hensikt <- build_sql_select_code(values = hensikt, varname = "hensiktkode")
  if (nchar(select_hensikt) > 0) {select_hensikt <- paste(select_hensikt, "OR")}

  # Build select module for utbrudd
  select_utbrudd <- build_sql_select_code(values = utbrudd, varname = "utbrudd_id")
  if (nchar(select_utbrudd) > 0) {select_utbrudd <- paste(select_utbrudd, "OR")}

  # Build select module for konkl_analyttkode
  select_konkl_analytt <- build_sql_select_code(values = analytt, varname = "konkl_analyttkode")

  # Combine modules into query for v_sak_prove_konkl
  selection_v_sak_prove_konkl <- paste("SELECT * FROM v_sak_prove_konkl",
                                       "WHERE", select_year, "AND",
                                       paste0("(", select_hensikt),
                                       select_utbrudd,
                                       paste0(select_konkl_analytt, ")"))

  # # Remove double spaces from string
  selection_v_sak_prove_konkl <- gsub(' +', ' ', selection_v_sak_prove_konkl)

  "SELECT *
  FROM v_sak_prove_konkl
  WHERE aar = 2020 AND
     ( hensiktkode IN ('0100108018', '0100109003', '0100111003', '0800109') OR
     konkl_analyttkode = '1502010235' OR konkl_analyttkode LIKE '01220104%' )"

  # BUILD QUERY FOR v_sak_prove_res ----
  # Build modules for the select statement
  # Use already created modules for year, hensikt, and utbrudd

  # Build select module for metode
  select_metode <- build_sql_select_code(values = metode, varname = "metodekode")
  if (nchar(select_metode) > 0) {select_metode <- paste(select_metode, "OR")}

  # Build select module for res_analyttkode
  select_res_analytt <- build_sql_select_code(values = analytt, varname = "analyttkode_funn")

  # Combine modules into query for v_sak_prove_konkl
  selection_v_sak_prove_res <- paste("SELECT * FROM v_sak_prove_res",
                                     "WHERE", select_year, "AND",
                                     paste0("(", select_hensikt),
                                     select_utbrudd,
                                     select_metode,
                                     paste0(select_res_analytt, ")"))

  # Remove double spaces from string
  selection_v_sak_prove_res <- gsub(' +', ' ', selection_v_sak_prove_res)


  # BUILD QUERY FOR THE SELECT STATEMENT FOR v_sakskonklusjon ----
  # Build select module for saks_year
  select_sak_year <- build_sql_select_year(year = year, varname = "sak.aar")

  # Build select module for sakskonkl_analyttkode
  select_sakskonkl_analytt <- build_sql_select_code(values = analytt, varname = "analyttkode")

  # Combine modules into query for v_sakskonklusjon
  selection_sakskonklusjon <- paste("SELECT v_sakskonklusjon.*,",
                                    "sak.mottatt_dato, sak.uttaksdato, sak.sak_avsluttet, sak.hensiktkode,",
                                    "sak.eier_lokalitetstype, sak.eier_lokalitetnr",
                                    "FROM v_innsendelse AS sak",
                                    "INNER JOIN v_sakskonklusjon",
                                    "ON (v_sakskonklusjon.aar = sak.aar AND",
                                    "v_sakskonklusjon.ansvarlig_seksjon = sak.ansvarlig_seksjon AND",
                                    "v_sakskonklusjon.innsendelsesnummer = sak.innsendelsesnummer)",
                                    "WHERE", select_sak_year, "AND",
                                    paste0("(", select_sakskonkl_analytt, ")"))

  # RETURN SELECT QUERIES
  # Combine queries into list
  # Each select statment is given name after the main table for the selection query
  select_statement <- list("v_sak_prove_konkl" = selection_v_sak_prove_konkl,
                           "v_sak_prove_res" = selection_v_sak_prove_res,
                           "v_sakskonklusjon" = selection_sakskonklusjon)


  # return list
  return(select_statement)
}
