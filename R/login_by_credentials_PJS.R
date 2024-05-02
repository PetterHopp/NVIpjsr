#' @title Log in to PJS /journal_rapp
#' @description Log in to NVI's data base service: journal_rapp/PJS.
#' @details \code{login_by_credentials_PJS} is wrapper for
#'     \ifelse{html}{\code{\link[NVIdb:login]{NVIdb::login_by_credentials}}}{\code{NVIdb::login_by_credentials}}
#'     and is the same as
#'     \ifelse{html}{\code{\link[NVIdb:login]{NVIdb::login_by_credentials("PJS")}}}{\code{NVIdb::login_by_credentials("PJS")}}.
#'     It uses predefined connection parameters for the PJS database and
#'     it is therefore dependent on \code{NVIconfig} being installed. The user
#'     is never asked for username and password, and the function can
#'     only be used when the credentials previously have been set in
#'     the user's profile at the current computer by
#'     \ifelse{html}{\code{\link[NVIdb:set_credentials]{NVIdb::set_credentials("PJS")}}}{\code{NVIdb::set_credentials("PJS")}}.
#'     If you would like to be asked for username and password, you
#'     should use
#'     \ifelse{html}{\code{\link[NVIdb:login]{NVIdb::login("PJS")}}}{\code{NVIdb::login("PJS")}}.
#'
#' \code{login_by_credentials_PJS} returns an open ODBC-channel to journal_rapp/PJS.
#'     The database can then be queried by using functions in the package
#'     used for data base interface. The data base interface must be one
#'     of \code{odbc} or \code{RODBC}. The default
#'     for journal_rapp/PJS is \code{RODBC} to obtain backward compatibility. However,
#'     it is recommended to use \code{odbc} which is faster.
#'
#' When the session is finished, the script shall close the ODBC-channel by
#'     \ifelse{html}{\code{\link[DBI:dbDisconnect]{DBI::dbDisconnect("myodbcchannel")}}}{\code{DBI::dbDisconnect("myodbcchannel")}}
#'     when using \code{odbc} or
#'     \ifelse{html}{\code{\link[RODBC:odbcClose]{RODBC::odbcClose("myodbcchannel")}}}{\code{RODBC::odbcClose("myodbcchannel")}}
#'     or
#'     \ifelse{html}{\code{\link[RODBC:odbcCloseAll]{RODBC::odbcCloseAll)}}}{\code{RODBC::odbcCloseAll}}
#'     when using \code{RODBC}.
#' @param dbinterface [\code{character(1)}]\cr
#' The R-package that is used for interface towards the data
#'     base. Defaults to \code{NULL} which implies using \code{RODBC}.
#' @param \dots Other arguments to be passed to
#'     \code{login_by_credentials}.
#' @return An open ODBC-channel to journal_rapp/PJS.
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' library(odbc)
#' library(DBI)
#' journal_rapp <- login_by_credentials_PJS(dbinterface = "odbc")
#' # Reads hensiktregistret from PJS
#' hensikter <- DBI::dbGetQuery(con = journal_rapp,
#'                              statement = "select * from v_hensikt")
#' DBI::dbDisconnect(journal_rapp)
#' }
#'
login_by_credentials_PJS <- function(dbinterface = NULL, ...) {

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # Identify if NVIconfig are installed.
  NVIcheckmate::assert_package(x = "NVIconfig", add = checks)
  # credentials
  NVIcheckmate::assert_credentials(x = "PJS", add = checks)
  # dbinterface
  checkmate::assert_choice(dbinterface, choices = c("odbc", "RODBC"), null.ok = TRUE, add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)


  connection <- NVIdb::login_by_credentials(dbservice = "PJS", dbinterface = dbinterface, ...)

  return(connection)
}
