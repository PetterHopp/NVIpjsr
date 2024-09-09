#' @title Report selection parameters
#' @description Reports the selection parameters to a data frame.
#' @details The selection parameters can be set by using
#'     \code{\link{set_disease_parameters}}.
#'     or by giving a list of similar format for input to
#'     \code{selection_parameters}, see the build_query-functions for necessary
#'     input.
#'
#' When using the argument additional_parameters the input should be a named list.
#'     The name of each element should be constructed by the "variable type", "2"
#'     and thereafter either "select" or "delete", i.e. "metode2delete". The
#'     variable type must be one of the variable types for which the code can be
#'     automatically translated to description text, see
#'     \code{\link{add_PJS_code_description}}.
#'
#' @param year [\code{numeric}]\cr
#' One year or a vector giving the first and last years that should be selected.
#'     Defaults to \code{NULL}.
#' @param selection_parameters [\code{character(1)}]\cr
#' Either the path and file name for an R script that can be sourced and that
#'     sets the selection parameters or a named list with the selection parameters
#'     (i.e. of the same format as the output of
#'     \code{\link{set_disease_parameters}}).
#'     Defaults to \code{NULL}.
#' @param additional_parameters [\code{character(1)}]\cr
#' A named list with additional selection parameters not included in the standard
#'     selection parameters, for example sample type. Defaults to \code{NULL}.
#' @param translation_table [\code{data.frame}] \cr
#'     Table with the code and the description for PJS variables. Defaults to
#'     "PJS_codes_2_text".
#' @return [\code{data.frame}] with the selection parameters prepared for reporting.
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Reads translation table for PJS codes
#' PJS_codes_2_text <- read_PJS_codes_2_text()
#'
#' selection_parameters <- set_disease_parameters(
#'   hensikt2select = c("0100108018", "0100109003", "0100111003", "0800109"),
#'   analytt2select = c("01220104%", "1502010235"),
#'   metode2select = c("070070", "070231", "010057", "060265"),
#'   FUN = build_query_one_disease)
#'
#' selection <- report_selection_parameters(year = 2024,
#'                                          selection_parameters = selection_parameters,
#'                                          translation_table = PJS_codes_2_text)
#' }
#'
report_selection_parameters <- function(year = NULL,
                                        selection_parameters = NULL,
                                        additional_parameters = NULL,
                                        translation_table = PJS_codes_2_text) {
  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # Perform checks
  checkmate::assert_integerish(year,
                               lower = 1990, upper = as.numeric(format(Sys.Date(), "%Y")),
                               min.len = 1,
                               any.missing = FALSE, all.missing = FALSE,
                               add = checks)
  NVIcheckmate::assert(checkmate::check_file_exists(x = selection_parameters, access = "r"),
                       checkmate::check_list(x = selection_parameters, min.len = 1,
                               all.missing = FALSE),
                       combine = "or",
                       comment = "The argument selection_parameter must either be a file with selection parameters or a list with selection parameters",
                       add = checks)
  # additional_parameters
  checkmate::assert_list(additional_parameters, null.ok = TRUE, add = checks)
  # translation_table
  checkmate::assert_data_frame(translation_table, add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)

  # READ ARGUMENTS
  selection_parameters <- set_disease_parameters(selection_parameters = selection_parameters)

  # ARGUMENT CHECKING OF selection_parameters ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  checkmate::assert_function(unlist(selection_parameters$FUN), null.ok = TRUE, add = checks)
  checkmate::assert(checkmate::check_list(x = selection_parameters$select_statement, null.ok = TRUE),
                    checkmate::check_string(x = selection_parameters$select_statement),
                    combine = "or",
                    add = checks)
  NVIcheckmate::assert_non_null(list(selection_parameters$hensikt2select,
                                     selection_parameters$analytt2select,
                                     selection_parameters$utbrudd2select,
                                     selection_parameters$select_statement, add = checks))
  # NVIcheckmate::assert_non_null(list(unlist(selection_parameters$FUN), selection_parameters$select_statement), add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)


  report <- as.data.frame(list("Status" = "Selektert",
                               "Variable" = "\u00C5r",
                               "Kode" = paste(unique(c(min(year), max(year))), collapse = "-"),
                               "Beskrivelse" = "Fra \u00e5r - til \u00e5r"))

  # hensikt2select
  parameters <- list_parameters(variables = selection_parameters$hensikt2select,
                                PJS_variable_type = "Hensikt",
                                translation_table = translation_table)
  report <- rbind(report, parameters)

  # utbrudd2select
  parameters <- list_parameters(variables = selection_parameters$utbrudd2select,
                                PJS_variable_type = "Utbrudd",
                                translation_table = translation_table)
  report <- rbind(report, parameters)

  # analyttt2select
  parameters <- list_parameters(variables = selection_parameters$analytt2select,
                                PJS_variable_type = "Analytt",
                                translation_table = translation_table)
  report <- rbind(report, parameters)

  # metode2select
  parameters <- list_parameters(variables = selection_parameters$metode2select,
                                PJS_variable_type = "Metode",
                                translation_table = translation_table)
  report <- rbind(report, parameters)

  # art2select
  parameters <- list_parameters(variables = selection_parameters$art2select,
                                PJS_variable_type = "Art",
                                translation_table = translation_table)
  report <- rbind(report, parameters)

  # hensikt2delete
  parameters <- list_parameters(variables = selection_parameters$hensikt2delete,
                                PJS_variable_type = "Hensikt",
                                translation_table = translation_table)
  report <- rbind(report, parameters)

  # analytt2delete
  parameters <- list_parameters(variables = selection_parameters$analytt2delete,
                                PJS_variable_type = "Analytt",
                                translation_table = translation_table)
  report <- rbind(report, parameters)

  # Additional parameters
  if (!is.null(additional_parameters) && length(additional_parameters) > 0) {
    for (i in c(1:length(additional_parameters))) {
      # i <- 1
      variables <- additional_parameters[[i]]
      varname <- names(additional_parameters[i])
      PJS_variable_type <- gsub(pattern = "2delete|2select",
                                replacement = "",
                                x = varname)
      PJS_variable_type <- paste0(toupper(substr(PJS_variable_type, 1, 1)),
                                  substr(PJS_variable_type, 2, nchar(PJS_variable_type)))
      parameters <- list_parameters(varname = varname,
                                    variables = variables,
                                    PJS_variable_type = PJS_variable_type,
                                    translation_table = translation_table)
      report <- rbind(report, parameters)
    }
  }

  # Clean table for empty rows
  report <- subset(report, !report$Kode %in% c("", "X") & !is.na(report$Kode))

  return(report)
}


# Data for reporting selection criteria

list_parameters <- function(varname = NULL, variables, PJS_variable_type, translation_table) {
  if (is.null(varname)) {
    varname <- deparse(substitute(variables))
  }
  parameters <- as.data.frame(list("Status" = NA, "Variable" = NA, "Kode" = NA, "Beskrivelse" = NA))

  if (!is.null(variables)) {
    Kode <- trimws(variables)
    parameters <- as.data.frame(Kode)
    parameters$kode_stripped <- gsub("%", "", parameters$Kode)
    if (grepl(pattern = "select", x = varname, ignore.case = TRUE)) {
      parameters$Status <- "Selektert"
    }
    if (grepl(pattern = "delete", x = varname, ignore.case = TRUE)) {
      parameters$Status <- "Ekskludert"
    }
    parameters$Variable <- PJS_variable_type
    parameters <- NVIpjsr::add_PJS_code_description(parameters,
                                                  translation_table = translation_table,
                                                  PJS_variable_type = tolower(PJS_variable_type),
                                                  code_colname = "kode_stripped",
                                                  new_column = "Beskrivelse")
    parameters <- parameters[, c("Status", "Variable", "Kode", "Beskrivelse")]

    # parameters <- as.data.frame(matrix(c(variables),
    #                                    nrow = length(c(variables)),
    #                                    dimnames = list(NULL, "Kode"))) %>%
    #   dplyr::mutate(kode_stripped = gsub("%", "", Kode)) %>%
    #   dplyr::mutate(Status = dplyr::case_when(grepl(pattern = "select", x = varname, ignore.case = TRUE) ~ "Selektert",
    #                                           grepl(pattern = "delete", x = varname, ignore.case = TRUE) ~ "Ekskludert")) %>%
    #   dplyr::mutate(Variable = PJS_variable_type) %>%
    #   add_PJS_code_description(PJS_variable_type = tolower(PJS_variable_type),
    #                            code_colname = "kode_stripped",
    #                            new_column = "Beskrivelse") %>%
    #   dplyr::select(Status, Variable, Kode, Beskrivelse)
  }

  return(parameters)
}
