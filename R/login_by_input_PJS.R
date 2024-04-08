#' @export
#' @rdname login_PJS

login_by_input_PJS <- function(dbinterface = NULL, ...) {

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # dbinterface
  checkmate::assert_choice(dbinterface, choices = c("odbc", "RPostgreSQL", "RODBC"), null.ok = TRUE, add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)

  # Oppretterknytning mot journal_rapp
  odbcConnection <- NVIdb::login_by_input(dbservice = "PJS", dbinterface = dbinterface, ...)

  return(odbcConnection)
}
