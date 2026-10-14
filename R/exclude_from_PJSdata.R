#' @title exclude rows from PJS-data
#' @description Performs common cleaning of PJS-data by excluding rows
#'
#' @details Performs common cleaning of PJSdata by removing samples that usually
#'     should not be included when analysing PJSdata. The cleaning is dependent
#'     on having the following columns: eier_lokalitettype, eier_lokalitetnr and
#'     hensiktkode.
#'
#'     \code{abroad = "exclude"} will exclude samples that have eier_lokalitet
#'     of type "LAND" and eier_lokalitetnr being different from "NO". Samples
#'     from abroad registered on other types than "LAND" are not excluded.
#'
#'     \code{quality = "exclude"} will exclude all samples registered as quality
#'     assurance and ring trials, i.e. hensiktkode starting with "09".
#'
#' @param PJSdata [\code{data.frame}] \cr
#'     Data retrieved from PJS.
#' @param abroad [\code{character(1)}]\cr
#'     If equal to "exclude", samples from abroad are excluded. Allowed
#'     values are c("exclude", "include"). Defaults to "exclude",
#' @param quality [\code{character(1)}]\cr
#'     If equal to "exclude", samples registered as quality assurance and
#'     ring trials are excluded. Allowed values are c("exclude", "include").
#'     Defaults to "exclude",
#'
#' @return data frame without excluded PJS-data.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # cleaning v2_sak_m_res
#' v2_sak_m_res <- exclude_from_PJSdata(PJSdata = v2_sak_m_res,
#'                                      abroad = "exclude",
#'                                      quality = "exclude")
#' }
#'
exclude_from_PJSdata <- function(PJSdata, abroad = "exclude", quality = "exclude") {

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  # pjsDATA
  checkmate::assert_data_frame(PJSdata, add = checks)
  # abroad
  checkmate::assert_choice(abroad, choices = c("exclude", "include"), add = checks)
  # quality
  checkmate::assert_choice(quality, choices = c("exclude", "include"), add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)

  # PERFORM EXCLUSION ----
  # Remove samples from abroad
  # Removes eier_lokalitet registered as LAND and
  #   with eier_lokalitetnr different from Norway (NO)
  if (abroad == "exclude") {
    # PJSdata <- subset(PJSdata,
    #                   PJSdata$eier_lokalitettype != "LAND" |
    #                     is.na(PJSdata$eier_lokalitettype) |
    #                     (PJSdata$eier_lokalitettype == "LAND" & PJSdata$eier_lokalitetnr == "NO"))
    PJSdata <- PJSdata[which(PJSdata$eier_lokalitettype != "LAND" |
                               is.na(PJSdata$eier_lokalitettype) |
                               (PJSdata$eier_lokalitettype == "LAND" & PJSdata$eier_lokalitetnr == "NO")), ]
  }

  # Remove samples with hensiktkode == "09" (ring trials and quality assessments)
  if (quality == "exclude") {
    # PJSdata <- subset(PJSdata,
    #                   substr(PJSdata$hensiktkode, 1, 2) != "09" |
    #                     is.na(PJSdata$hensiktkode))
    PJSdata <- PJSdata[which(substr(PJSdata$hensiktkode, 1, 2) != "09" |
                               is.na(PJSdata$hensiktkode)), ]
  }

  # RETURN DATA ----
  return(PJSdata)
}
