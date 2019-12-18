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
    {dplyr::mutate(., reaction_string = paste0("<b>", ItemName , "</b> : ", Reaction)) %>>%
        dplyr::group_by(Patient, InternalID, AppointmentDate, AppointmentTime, Provider, DOB, Age, Created, KnownAllergies) %>>%
        dplyr::summarise(reaction_string = paste(reaction_string, collapse = "<br>")) %>>%
        dplyr::ungroup()} else {.}} %>>%
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

#' List of patients with social history recordings
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
#' @return dataframe list of patients with social history recording status
#' @export
list_socialHx <- function(dMeasure_obj,
                          date_from = NA, date_to = NA, clinicians = NA,
                          appointments_list = NULL,
                          lazy = FALSE,
                          qualitytag = FALSE, qualitytag_print = TRUE) {
  dMeasure_obj$list_socialHx(date_from, date_to , clinicians,
                             appointments_list,
                             lazy,
                             qualitytag, qualitytag_print)
}

.public(dMeasure, "list_socialHx", function(date_from = NA, date_to = NA, clinicians = NA,
                                            appointments_list = NULL,
                                            lazy = FALSE,
                                            qualitytag = FALSE, qualitytag_print = TRUE) {
  # return datatable of appointments with socialHx status

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

  socialHx_list <- appointments_list %>>%
    dplyr::left_join(self$db$clinical %>>%
                       dplyr::filter(InternalID %in% intID) %>>%
                       dplyr::select(InternalID, SocialHx, Accomodation, LivesWith,
                                     HasCarer, IsCarer, Recreation,
                                     Created, Updated) %>>%
                       dplyr::left_join(self$db$accomodation,
                                        by = c("Accomodation" = "AccomodationCode")) %>>%
                       dplyr::left_join(self$db$liveswith,
                                        by = c("LivesWith" = "LivesWithCode")) %>>%
                       dplyr::select(-c(Accomodation, LivesWith)) %>>%
                       # remove the 'index column', and replace with the text
                       dplyr::rename(Accomodation = AccomodationText, LivesWith = LivesWithText),
                     by = "InternalID",
                     copy = TRUE) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(Created = as.Date(substr(Created, 1, 10))) %>>%
    dplyr::mutate(Created = dplyr::if_else(Created <= AppointmentDate, Created, as.Date(NA))) %>>%
    # only include recording if done up to date of appointment,
    # if there are is a recording of social history at all
    # unfortunately, the date recording could be quite inaccurate?,
    # since creation date might be part of smoking history etc.
    # note that 'if_else' vectorizes,
    # demanding same datatype for TRUE/FALSE alternatives
    # 'ifelse' does not preserve date type in this circumstance
    dplyr::mutate(LivesWith = dplyr::if_else(paste2(LivesWith, na.rm = TRUE) == "",
                                             # force to empty string if NULL/NA
                                             "",
                                             paste0("Lives with ", LivesWith))) %>>%
    {if (qualitytag)
    {dplyr::mutate(., socialHx_string = paste2(SocialHx, Accomodation, Recreation,
                                               sep = ", ", na.rm = TRUE))}
      else {.}} %>>%
    # paste2 changes NULL to empty string
    # there are other potential social history elements which could be included
    # from fields RETIRED, ACCOMODATION, LIVESWITH, HASCARER, ISCARER, RECREATION
    {if (qualitytag_print)
    {dplyr::mutate(., socialHx_string = paste2(SocialHx, Accomodation, Recreation,
                                               sep = ", ", na.rm = TRUE))}
      else {.}}

  if (qualitytag) {
    socialHx_list <- socialHx_list %>>%
      dplyr::mutate(qualitytag =
                      dMeasure::semantic_tag(
                        paste0(' Social History '),
                        colour =
                          dplyr::if_else(is.na(Created) | socialHx_string == "",
                                         # if no entry in 'clinical' table at all (at this date)
                                         # or string is empty
                                         c('red'),
                                         c('green')),
                        # red if not recorded
                        # and green if social history recording
                        popuphtml =
                          paste0("<h4>",
                                 dplyr::if_else(is.na(Created) | socialHx_string == "",
                                                "No recording",
                                                socialHx_string),
                                 "</h4>")))
  }

  if (qualitytag_print) {
    socialHx_list <- socialHx_list %>>%
      dplyr::mutate(qualitytag_print =
                      paste0("Social History", " ", # printable version of information
                             dplyr::if_else(is.na(Created) | socialHx_string == "",
                                            "(No recording)",
                                            paste0("(", socialHx_string, ")"))
                      ))
  }

  socialHx_list <- socialHx_list %>>%
    dplyr::select(intersect(names(socialHx_list),
                            c("Patient", "InternalID", "AppointmentDate", "AppointmentTime",
                              "Provider", "DOB", "Age", "qualitytag", "qualitytag_print")))

  return(socialHx_list)
})

#' List of patients with family history recordings
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
#' @return dataframe list of patients with family history recording status
#' @export
list_familyHx <- function(dMeasure_obj,
                          date_from = NA, date_to = NA, clinicians = NA,
                          appointments_list = NULL,
                          lazy = FALSE,
                          qualitytag = FALSE, qualitytag_print = TRUE) {
  dMeasure_obj$list_familyHx(date_from, date_to , clinicians,
                             appointments_list,
                             lazy,
                             qualitytag, qualitytag_print)
}

.public(dMeasure, "list_familyHx", function(date_from = NA, date_to = NA, clinicians = NA,
                                            appointments_list = NULL,
                                            lazy = FALSE,
                                            qualitytag = FALSE, qualitytag_print = TRUE) {
  # return datatable of appointments with familyHx status

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

  familyHx_list <- appointments_list %>>%
    dplyr::left_join(self$db$familyhistory %>>%
                       dplyr::filter(InternalID %in% intID) %>>%
                       dplyr::select(InternalID, Unknown,
                                     FatherAlive, MotherAlive,
                                     FatherCauseOfDeath, MotherCauseOfDeath,
                                     FatherAgeAtDeath, MotherAgeAtDeath,
                                     Comment,
                                     Relation, Condition, DiseaseComment),
                     # Relation, Condition, DiseaseComment are
                     # relation-specific information
                     # so there can be multiple rows per InternalID
                     by = "InternalID",
                     copy = TRUE) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(EmptyFamilyHistory = # TRUE if no recording
                    (FatherAlive == 0 & MotherAlive == 0 & Unknown == 0 &
                       (is.null(Comment) | Comment == "") &
                       is.null(Relation)),
                  Unknown = dplyr::if_else(Unknown == 0, "", "Unknown"),
                  FatherAgeAtDeath = dplyr::if_else(paste2(FatherAgeAtDeath, na.rm = TRUE) == 0,
                                                    "",
                                                    paste0("(", FatherAgeAtDeath, ")")),
                  MotherAgeAtDeath = dplyr::if_else(paste2(MotherAgeAtDeath, na.rm = TRUE) == 0,
                                                    "",
                                                    paste0("(", MotherAgeAtDeath, ")")),
                  FatherCauseOfDeath = paste2(FatherCauseOfDeath, na.rm = TRUE),
                  MotherCauseOfDeath = paste2(MotherCauseOfDeath, na.rm = TRUE),
                  Father = dplyr::if_else(FatherAlive == 0,
                                          "",
                                          dplyr::if_else(FatherAlive == 2,
                                                         "Father alive",
                                                         paste0("Father deceased",
                                                                FatherCauseOfDeath,
                                                                FatherAgeAtDeath,
                                                                sep = " "))),
                  Mother = dplyr::if_else(MotherAlive == 0,
                                          "",
                                          dplyr::if_else(MotherAlive == 2,
                                                         "Mother alive",
                                                         paste0("Mother deceased",
                                                                MotherCauseOfDeath,
                                                                MotherAgeAtDeath,
                                                                sep = " "))),
                  DiseaseComment = dplyr::if_else(paste2(DiseaseComment, na.rm = TRUE) == "",
                                                  # force to empty string if NULL/NA
                                                  "",
                                                  paste0("(", DiseaseComment, ")")),
                  RelativeDetail = dplyr::if_else(is.na(Relation),
                                                  "",
                                                  paste2(Relation, ":", Condition, DiseaseComment,
                                                         sep = ""))) %>>%
    dplyr::group_by(Patient, InternalID, AppointmentDate, AppointmentTime, Status, Provider,
                    DOB, Age, EmptyFamilyHistory, Unknown, Father, Mother, Comment) %>>%
    dplyr::summarise(RelativeDetails = paste(RelativeDetail, collapse = ", ")) %>>%
    dplyr::ungroup() %>>%
    dplyr::mutate(familyHx_string = paste2(Unknown, Father, Mother, Comment,
                                           RelativeDetails,
                                           sep = ", ", na.rm = TRUE))

  if (qualitytag) {
    familyHx_list <- familyHx_list %>>%
      dplyr::mutate(qualitytag =
                      dMeasure::semantic_tag(
                        paste0(' Family History '),
                        colour =
                          dplyr::if_else(EmptyFamilyHistory | familyHx_string == "",
                                         # if no details in family history at all
                                         # or string is empty
                                         c('red'),
                                         c('green')),
                        # red if no recordings,
                        # and green if there is a recording
                        popuphtml =
                          paste0("<h4>",
                                 dplyr::if_else(EmptyFamilyHistory | familyHx_string == "",
                                                "No recording",
                                                familyHx_string),
                                 "</h4>")))
  }

  if (qualitytag_print) {
    familyHx_list <- familyHx_list %>>%
      dplyr::mutate(qualitytag_print =
                      paste0("Family History", " ", # printable version of information
                             dplyr::if_else(EmptyFamilyHistory | familyHx_string == "",
                                            "(No recording)",
                                            paste0("(", familyHx_string, ")"))
                      ))
  }

  familyHx_list <- familyHx_list %>>%
    dplyr::select(intersect(names(familyHx_list),
                            c("Patient", "InternalID", "AppointmentDate", "AppointmentTime",
                              "Provider", "DOB", "Age", "qualitytag", "qualitytag_print")))

  return(familyHx_list)
})

.active(dMeasure, "dataQuality_choices", function(value) {
  if (!missing(value)) {
    warning("$dataQuality_choices is read-only.")
  } else {
    return(c("Allergies", "Social History", "Family History"))
    # vector of valid dataQuality choices
  }
})

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
                                               chosen = self$dataQuality_choices) {

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

  if (is.null(appointments_list) & !lazy) {
    self$list_appointments(date_from, date_to, clinicians, lazy = FALSE)
    # if not 'lazy' evaluation, then re-calculate self$appointments_billings
    # (that is automatically done by calling the $billed_appointments method)
  }

  if (is.null(appointments_list)) {
    appointments_list <- self$appointments_list
  }

  vlist <- data.frame(Patient = character(), InternalID = integer(),
                      AppointmentDate = as.Date(integer(), origin = "1970-01-01"),
                      AppointmentTime = character(), Provider = character(),
                      DOB = as.Date(integer(), origin = "1970-01-01"), Age = double())

  if (qualitytag) {
    vlist <- cbind(vlist, qualitytag = character())
  }
  if (qualitytag_print) {
    vlist <- cbind(vlist, qualitytag_print = character())
  }

  if ("Allergies" %in% chosen) {
    vlist <- rbind(vlist, self$list_allergy(date_from, date_to, clinicians,
                                            appointments_list,
                                            lazy,
                                            qualitytag, qualitytag_print))
  }

  if ("Social History" %in% chosen) {
    vlist <- rbind(vlist, self$list_socialHx(date_from, date_to, clinicians,
                                             appointments_list,
                                             lazy,
                                             qualitytag, qualitytag_print))
  }

  if ("Family History" %in% chosen) {
    vlist <- rbind(vlist, self$list_familyHx(date_from, date_to, clinicians,
                                             appointments_list,
                                             lazy,
                                             qualitytag, qualitytag_print))
  }

  if (nrow(vlist) > 0) {
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

