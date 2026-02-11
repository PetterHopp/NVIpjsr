#' @title Deprecated Functions and Arguments in Package NVIpjsr
#' @description These functions are provided for compatibility with older
#'     versions of NVIpjsr only, and may be defunct as soon as the next release.
#'     When possible, alternative functions are mentioned. Help pages for
#'     deprecated functions are available at \code{help("<function>-deprecated")}.
#' @details The arguments \code{missing_art} and \code{file} in
#'     \code{set_disease_parameters} was deprecated from NVIdb v0.11.0 released
#'     2024-01-24. These arguments are replaced by the more meaningful
#'     \code{include_missing_art} and \code{selection_parameters}, respectively.
#'     If using the old arguments, the input will be transferred to the new
#'     arguments.
#'
#' The arguments \code{FUN} and \code{selection_statement} in
#'     \code{retrieve_PJSdata} was deprecated from NVIdb v0.11.2 released
#'     2024-04-05. These arguments should instead be included in the list input
#'     to \code{selection_parameters}. If using the old arguments, the input
#'     will be transferred to \code{selection_parameters}.
#'
#' @param \dots (arguments)
#' @return (results)
#' @name NVIpjsr-deprecated
#' @keywords internal
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#'
#' @examples
#' \dontrun{
#' retrieve_PJSdata(...) ###
#' set_disease_parameters(...) ###
#' }
NULL
