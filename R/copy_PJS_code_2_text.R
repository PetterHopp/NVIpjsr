#' @export
#' @rdname add_PJS_code_description

copy_PJS_codes_2_text <- function(filename = "PJS_codes_2_text.csv",
                                  from_path = file.path(NVIdb::set_dir_NVI("Provedata_Rapportering", slash = FALSE),
                                                        "FormaterteData"),
                                  to_path = NULL) {

  # PREPARE ARGUMENT ----
  # Removing ending "/" and "\\" from pathnames
  from_path <- sub("/+$|\\\\+$", "", from_path)
  to_path <- sub("/+$|\\\\+$", "", to_path)

  # # ARGUMENT CHECKING ----
  # assert_copy_function(filename = filename,
  #                      from_path = from_path,
  #                      to_path = to_path)
  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  ## filename
  checkmate::assert(checkmate::check_character(filename,
                                               min.chars = 1,
                                               len = 1),
                    checkmate::check_list(filename, min.len = 1),
                    combine = "or",
                    add = checks)
  ## from_path / filename
    checkmate::assert_file_exists(file.path(from_path, filename), access = "r", add = checks)
  ## to_path
  checkmate::assert_directory_exists(to_path, access = "r", add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)

  # COPY FILE ----
  copy_file_if_updated(filename = filename, from_path = from_path, to_path = to_path)

}
