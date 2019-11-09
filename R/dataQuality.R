##### Data Quality ###########################################
#' Data Quality
#'
#' @name dataquality
#' @include dMeasure.R
#' @include appointments.R
#' needs access to dMeasure and appointments functions and variables
#'
#' @include calculation_definitions.R
#' need function 'interval'
#'
#' @include fomantic_definitions_tags.R
#' 'tags only' fomantic/semantic definitions
NULL

#' List of patients with allergy (or adverse reaction) recordings
#'
#' Optionally added a HTML ('qualitytag') or printable ('qualitytag_print')
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from from date range (default $date_a)
#' @param date_to to date range (default $date_b)
#' @param clinicians list of clinicians (default $clinicians)
#' @param appointments_list provide an appointment list (default $appointments_list)
#' @param lazy = FALSE recalculate an appointment list
#' @param qualitytag = FALSE
#' @param qualitytag_print = TRUE
#'
#' @return dataframe list of patients with allergy recording status
#' @export
list_allergy <- function(dMeasure_obj,
                         date_from = NA, date_to = NA, clinicians = NA,
                         appointments_list = NULL,
                         lazy = FALSE,
                         qualitytag = FALSE, qualitytag_print = TRUE) {
  dMeasure_obj$list_zostavax(date_from, date_to , clinicians,
                             appointments_list,
                             lazy,
                             qualitytag, qualitytag_print)
}

.public(dMeasure, "list_allergy", function(date_from = NA, date_to = NA, clinicians = NA,
                                           appointments_list = NULL,
                                           lazy = FALSE,
                                           qualitytag = FALSE, qualitytag_print = TRUE) {
  # return datatable of appointments with allergy (or other adverse reactioin) recording status

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

  if (is.null(appointments_list) & !lazy) {
    self$list_appointments(date_from, date_to, clinicians, lazy = FALSE)
    # if not 'lazy' evaluation, then re-calculate self$appointments_billings
    # (that is automatically done by calling the $billed_appointments method)
  }

  if (is.null(appointments_list)) {
    appointments_list <- self$appointments_list
  }

  intID_Date <- appointments_list %>>%
    dplyr::select(InternalID, AppointmentDate) %>>%
    dplyr::rename(Date = AppointmentDate)
  # just the InternalID and AppointmentDate of the appointment list
  intID <- c(dplyr::pull(intID_Date, InternalID), -1)
  # add -1 dummy because cannot search %in% empty vector
  # just the InternalID

  allergy_list <- appointments_list %>>%
    dplyr::left_join(self$db$clinical %>>%
                       dplyr::filter(InternalID %in% intID) %>>%
                       dplyr::select(InternalID, KnownAllergies, Created, Updated),
                     by = "InternalID",
                     copy = TRUE) %>>%
    dplyr::left_join(self$db$reactions %>>%
                       # those who have been removed from the reminder system for Zostavax
                       dplyr::filter(InternalID %in% intID), by = "InternalID",
                     copy = TRUE) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(Created = as.Date(substr(Created, 1, 10))) %>>%
    dplyr::mutate(Created = dplyr::if_else(Created <= AppointmentDate, Created, as.Date(NA))) %>>%
    # only include recording if done up to date of appointment,
    # if there are is a recording of allergy (or 'none') at all
    # unfortunately, the date recording could be quite inaccurate, since creation date might be part of social history etc.
    # note that 'if_else' vectorizes,
    # demanding same datatype for TRUE/FALSE alternatives
    # 'ifelse' does not preserve date type in this circumstance
    {if (qualitytag)
      {dplyr::mutate(., reaction_string = paste0("<b>", ItemName , "</b> : ", Reaction))} else {.}} %>>%
    {if (qualitytag_print)
    {dplyr::mutate(., reaction_string = paste2(ItemName,
                                               paste2(Reaction, Severity, Comment, sep = " - ", na.rm = TRUE),
                                               sep = " : ", na.rm = TRUE))} else {.}}

  if (qualitytag) {
    allergy_list <- allergy_list %>>%
      dplyr::mutate(qualitytag =
                      dMeasure::semantic_tag(
                        paste0(' Allergy '),
                        colour =
                          dplyr::if_else(is.na(Created) | KnownAllergies == 0,
                                         # if no entry in 'clinical' table at all (at this date)
                                         # or if KnownAllergies is 'none recorded'
                                         c('red'),
                                         c('green')),
                        # red if not given, purple if removed from herpes zoster vax reminders
                        # and green if has had the vax
                        popuphtml =
                          paste0("<h4>",
                                 dplyr::if_else(is.na(Created) | KnownAllergies == 0,
                                                "No recording",
                                                dplyr::if_else(KnownAllergies == 1,
                                                        "Nil known",
                                                        reaction_string)),
                                 "</h4>")))
  }

  if (qualitytag_print) {
    allergy_list <- allergy_list %>>%
      dplyr::mutate(qualitytag_print =
                      paste0("Allergy", " ", # printable version of information
                             dplyr::if_else(is.na(Created) | KnownAllergies == 0,
                                            "(No recording)",
                                            dplyr::if_else(KnownAllergies == 1,
                                                    "(Nil known)",
                                                    paste0("(", reaction_string, ")")))
                      ))
  }

  allergy_list <- allergy_list %>>%
    dplyr::select(intersect(names(allergy_list),
                            c("Patient", "InternalID", "AppointmentDate", "AppointmentTime",
                              "Provider", "DOB", "Age", "qualitytag", "qualitytag_print")))

  return(allergy_list)
})

dataQuality_names <- c("Adverse Reactions")

#' List of patients for data quality check
#'
#' Optionally added a HTML ('qualitytag') or printable ('qualitytag_print')
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from from date range (default $date_a)
#' @param date_to to date range (default $date_b)
#' @param clinicians list of clinicians (default $clinicians)
#' @param appointments_list provide an appointment list (default $appointments_list)
#' @param lazy (default FALSE) recalculate an appointment list
#' @param qualitytag (default FALSE) HTML/browser version of tags
#' @param qualitytag_print (default TRUE) printable version of tags
#' @param chosen list of data quality names (default is all)
#'
#' @return dataframe list of influenza eligible patients
#' @export
list_dataQuality <- function(dMeasure_obj, date_from = NA, date_to = NA, clinicians = NA,
                             appointments_list = NULL,
                             lazy = FALSE,
                             qualitytag = FALSE, qualitytag_print = TRUE,
                             chosen = vax_names) {
  dMeasure_obj$list_dataQuality(date_from, date_to, clinicians,
                                appointments_list,
                                lazy,
                                qualitytag, qualitytag_print,
                                chosen)
}

.public(dMeasure, "list_dataQuality", function(date_from = NA, date_to = NA, clinicians = NA,
                                               appointments_list = NULL,
                                               lazy = FALSE,
                                               qualitytag = FALSE, qualitytag_print = TRUE,
                                               chosen = dataQuality_names) {

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

  if (is.null(appointments_list) & !lazy) {
    self$list_appointments(date_from, date_to, clinicians, lazy = FALSE)
    # if not 'lazy' evaluation, then re-calculate self$appointments_billings
    # (that is automatically done by calling the $billed_appointments method)
  }

  if (is.null(appointments_list)) {
    appointments_list <- self$appointments_list
  }

  vlist <- NULL
  if ("Adverse Reactions" %in% chosen) {
    vlist <- rbind(vlist, self$list_allergy(date_from, date_to, clinicians,
                                            appointments_list,
                                            lazy,
                                            qualitytag, qualitytag_print))
  }

  if (!is.null(vlist)) {
    vlist <- vlist %>>%
      dplyr::group_by(Patient, InternalID, AppointmentDate, AppointmentTime, Provider,
                      DOB, Age) %>>%
      # gathers vaccination notifications on the same appointment into a single row
      {if (qualitytag)
      {dplyr::summarise(., qualitytag = paste(qualitytag, collapse = ""))}
        else {.}} %>>%
      {if (qualitytag_print)
      {dplyr::summarise(., qualitytag_print = paste(qualitytag_print, collapse = ", "))}
        else {.}} %>>%
      dplyr::ungroup()
  }

  return(vlist)
})

