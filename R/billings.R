#' billings
#'
#' MBS (medicare benefits schedule) item numbers for CDM
#'
#' @name billings
#'
#' @include dMeasure.R
#' @include appointments.R
#' @include fomantic_definitions_tags.R
NULL

#' Billings done on same day as appointments
#'
#' filter to billings which are done on the same day as displayed appointments
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from (default dMeasure_obj$date_a) start date
#' @param date_to (default dMeasure_obj$date_b) end date (inclusive)
#' @param clinicians (default dMeasure_obj$clinicians) list of clinicians to view
#' @param lazy if TRUE, then do not recalculate appointment list. otherwise, re-calculate
#' @param screentag (default FALSE) optionally add a fomantic/semantic HTML description of 'action'
#' @param screentag_print (default TRUE) optionally add a 'printable' description of 'action'
#'
#' @return list of appointments (with patient details)
#' @export
appointments_billings_sameday <- function(dMeasure_obj, date_from, date_to, clinicians,
                                          lazy, screentag, screentag_print) {
  dMeasure_obj$appointments_billlings_sameday(date_from, date_to, clinicians,
                                              lazy, screentag, screentag_print)
}
.public(dMeasure, "appointments_billings_sameday", function(date_from = NA,
                                                            date_to = NA,
                                                            clinicians = NA,
                                                            lazy = FALSE,
                                                            screentag = FALSE,
                                                            screentag_print = TRUE) {

  if (is.na(date_from)) {
    date_from <- self$date_a
  }
  if (is.na(date_to)) {
    date_to <- self$date_b
  }
  if (all(is.na(clinicians))) {
    clinicians <- self$clinicians
  }
  # no additional clinician filtering based on privileges or user restrictions

  if (!lazy) {
    self$billed_appointments(date_from, date_to, clinicians, lazy = FALSE)
    # if not 'lazy' evaluation, then re-calculate self$appointments_billings
    # (that is automatically done by calling the $billed_appointments method)
  }

  billings_sameday <- self$appointments_billings %>>%
    dplyr::filter(ServiceDate == AppointmentDate) %>>%
    # billings done on the same day as displayed appointments
    dplyr::select(Patient, InternalID, AppointmentDate, AppointmentTime,
                  Provider, MBSItem, Description)
  # need to preserve ApppointmentTime and Provider
  # in the case where there are multiple apppointments
  # for the patient in the same time period/day and providers

  if (screentag) {
    # change MBSITEMS into fomantic/semantic tags
    billings_sameday <- billings_sameday %>>%
      dplyr::mutate(billingtag =
                      dMeasure::semantic_button(MBSItem,
                                                colour = 'green',
                                                popuphtml = paste0('<h4>', AppointmentDate,
                                                                   "</h3><p><font size=\'+0\'>",
                                                                   Description, '</p>')))
  }

  if (screentag_print) {
    billings_sameday <- billings_sameday %>>%
      dplyr::mutate(billingtag_print = MBSItem)
  }

  return(billings_sameday)
})

#' Billings done on same day as appointments
#'
#' filter to billings which are done on the same day as displayed appointments
#' billings are aggregated/group to patient/day
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from (default dMeasure_obj$date_a) start date
#' @param date_to (default dMeasure_obj$date_b) end date (inclusive)
#' @param clinicians (default dMeasure_obj$clinicians) list of clinicians to view
#' @param lazy if TRUE, then do not recalculate appointment list. otherwise, re-calculate
#' @param screentag (default FALSE) optionally add a fomantic/semantic HTML description of 'action'
#' @param screentag_print (default TRUE) optionally add a 'printable' description of 'action'
#'
#' @return list of appointments (with patient details)
#' @export
billings_list <- function(dMeasure_obj, date_from, date_to, clinicians,
                          lazy, screentag, screentag_print) {
  dMeasure_obj$billings_list(date_from, date_to, clinicians,
                             lazy, screentag, screentag_print)
}
.public(dMeasure, "billings_list", function(date_from = NA,
                                            date_to = NA,
                                            clinicians = NA,
                                            lazy = FALSE,
                                            screentag = FALSE,
                                            screentag_print = TRUE) {

  if (is.na(date_from)) {
    date_from <- self$date_a
  }
  if (is.na(date_to)) {
    date_to <- self$date_b
  }
  if (all(is.na(clinicians))) {
    clinicians <- self$clinicians
  }
  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    stop("Choose at least one clinicians\'s appointment to view")
  }

  billingslist <- NULL
  billingslist <- rbind(billingslist,
                        self$appointments_billings_sameday(date_from, date_to,
                                                           clinicians, lazy,
                                                           screentag, screentag_print))

  if (is.null(billingslist)) {
    # no valid appointments
  } else {
    billingslist <- billingslist %>>%
      dplyr::group_by(Patient, InternalID, AppointmentDate, AppointmentTime, Provider) %>>%
      # gathers vaccination notifications on the same appointment into a single row
      {if (screentag) {
        dplyr::summarise(., billingtag = paste(billingtag, collapse = ""))
      } else {.} } %>>%
      {if (screentag_print) {
        dplyr::summarise(., billingtag_print = paste(billingtag_print, collapse = ", "))
      } else {.} } %>>%
      dplyr::ungroup()
  }
  return(billingslist)
})
