#' @export
#' @rdname add_PJS_code_description

read_PJS_codes_2_text <- function(filename = "PJS_codes_2_text.csv",
                                  from_path = file.path(NVIdb::set_dir_NVI("Provedata_Rapportering", slash = FALSE),
                                                        "FormaterteData"),
                                  ...) {

  # Removing ending "/" and "\\" from pathnames
  from_path <- sub("/+$|\\\\+$", "", from_path)

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # # Perform checks
  # checks <- assert_read_functions(filename = filename, from_path = from_path, add = checks)
  ## filename
  checkmate::assert(checkmate::check_character(filename,
                                               min.chars = 1, len = 1,
                                               any.missing = FALSE),
                    checkmate::check_list(filename,
                                          min.len = 1,
                                          any.missing = FALSE),
                    combine = "or",
                    add = checks)
  ## from_path / filename
    checkmate::assert_file_exists(file.path(from_path, filename), access = "r", add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)

  # READ DATA ----
  # PJS_codes_2_text <- read_csv_file(filename = filename,
  #                                   from_path = from_path,
  #                                   options = list(colClasses = "character", fileEncoding = "UTF-8"))

  PJS_codes_2_text <- utils::read.csv2(file = file.path(from_path, filename),
                                       colClasses = "character",
                                       fileEncoding = "UTF-8",
                                       ...)

  # Remove double "" that have replaced single when saving as csv-file
  PJS_codes_2_text$navn <- gsub('\"\"', "\"", PJS_codes_2_text$navn)

  return(PJS_codes_2_text)

}
