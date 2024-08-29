#' @title Give a startup message when \code{NVIdb} is loaded after \code{NVIpjsr}
#' @description Creates a hook that gives a checks if startup message when
#'     \code{NVIdb} is loaded after \code{NVIpjsr}.
#'
#' @return If \code{NVIpjsr} is attached before \code{NVIdb}, a startup message
#'    is created when \code{NVIdb} is attached.

.onAttach <- function(libname, pkgname) {
  setHook(packageEvent("NVIdb", "attach"), function(...) {
    packageStartupMessage(
      "You have loaded 'NVIdb' after 'NVIpjsr' - this is likely ",
      "to cause problems.\nIf you need functions from both 'NVIdb' and 'NVIpjsr', ",
      "please load 'NVIdb' first, then 'NVIpjsr':\nlibrary(NVIdb); library(NVIpjsr)"
    )
  })
}
