# assert_arguments
#
# assert collections that developed to fit functions with standardized input arguments.
# - assert_copy_function
#
#' @title Collection of assertions for copy_functions
#' @description Collection of assertions used in standard copy_functions.
#'
#' @details All copy functions have the same arguments and the
#'     assertion can be standardized. The assertion of \code{filname} is
#'     constructed to handle both character and list input.
#'
#'     The assertion is based on removing ending "\\" and "/" from
#'     \code{from_path} and \code{to_path} before the assertion is performed.
#'
#' @param filename Argument to the copy-function to be asserted.
#' @param from_path Argument to the copy-function to be asserted. Ending
#'     "\\" and "/" should have been removed before the assertion is performed.
#' @param to_path Argument to the copy-function to be asserted. Ending
#'     "\\" and "/" should have been removed before the assertion is performed.
#'
#' @return \code{TRUE} if none of the assertions failed. If any of the assertions
#'     failed, one or more error messages are returned.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @keywords internal
#'
#'
assert_copy_function <- function(filename,
                                 from_path,
                                 to_path) {

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

  # checkmate::assert_list(filename, len = 2, add = checks)
  # # from_path
  # checkmate::assert_character(from_path, len = 1, min.chars = 1, add = checks)
  # if (endsWith(from_path, "/")) {
  #   checkmate::assert_directory_exists(substr(from_path, 1, nchar(from_path) - 1), access = "r", add = checks)
  # } else {
  #   checkmate::assert_directory_exists(from_path, access = "r", add = checks)
  # }

  ## from_path / filename
  for (i in c(1:length(filename))) {
    checkmate::assert_file_exists(file.path(from_path, filename[[i]]), access = "r", add = checks)
  }
  ## to_path
  checkmate::assert_directory_exists(to_path, access = "r", add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)
}
