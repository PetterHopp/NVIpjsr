#' @export
#' @rdname login_PJS

login_by_credentials_PJS <- function(dbinterface = NULL, ...) {

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # Identify if NVIconfig are installed.
  NVIcheckmate::assert_package(x = "NVIconfig", add = checks)
  # credentials
  NVIcheckmate::assert_credentials(x = "PJS", add = checks)
  # dbinterface
  checkmate::assert_choice(dbinterface, choices = c("odbc", "RPostgreSQL", "RODBC"), null.ok = TRUE, add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)


  connection <- NVIdb::login_by_credentials(dbservice = "PJS", dbinterface = dbinterface, ...)

  return(connection)
}
