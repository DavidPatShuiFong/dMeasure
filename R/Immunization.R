##### Immunization ###########################################
#' Immunisation
#'
#' @name immunization
#' @include dMeasure.R
#' @include appointments.R
#' needs access to dMeasure and appointments functions and variables
#'
#' @include conditions.R
#'
#' @include calculation_definitions.R
#' need function 'interval'
#'
#' @include fomantic_definitions_tags.R
#' 'tags only' fomantic/semantic definitions
NULL

#' List of patients potentially eligible for Zostavax
#'
#' Includes patients who may have already had Zostavax,
#' date of previous Zostavax is included.
#'
#' Optionally added a HTML ('vaxtag') or printable ('vaxtag_print')
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from from date range (default $date_a)
#' @param date_to to date range (default $date_b)
#' @param clinicians list of clinicians (default $clinicians)
#' @param appointments_list provide an appointment list (default $appointments_list)
#' @param lazy = FALSE recalculate an appointment list
#' @param vaxtag = FALSE
#' @param vaxtag_print = TRUE
#'
#' @return dataframe list of Zostavax eligible patients
#' @export
list_zostavax <- function(dMeasure_obj,
                          date_from = NA, date_to = NA, clinicians = NA,
                          appointments_list = NULL,
                          lazy = FALSE,
                          vaxtag = FALSE, vaxtag_print = TRUE) {
  dMeasure_obj$list_zostavax(date_from, date_to , clinicians,
                             appointments_list,
                             lazy,
                             vaxtag, vaxtag_print)
}

.public(dMeasure, "list_zostavax", function(date_from = NA, date_to = NA, clinicians = NA,
                                            appointments_list = NULL,
                                            lazy = FALSE,
                                            vaxtag = FALSE, vaxtag_print = TRUE) {
  # return datatable of appointments where Zostavax is recommended (might already be given)

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

  zostavax_list <- appointments_list %>>%
    dplyr::filter(Age >= 70 & Age <= 80) %>>% # from age 70 to 80 years inclusive
    dplyr::left_join(self$db$immunizations %>>%
                       dplyr::filter((InternalID %in% intID) &&
                                       # those who have had the zostavax vaccine
                                       ((VaccineName %LIKE% "%zostavax%") ||
                                          (VaccineID == 103))),
                     by = "InternalID",
                     copy = TRUE) %>>%
    dplyr::left_join(self$db$preventive_health %>>%
                       # those who have been removed from the reminder system for Zostavax
                       dplyr::filter(ITEMID == 15), by = "InternalID",
                     copy = TRUE) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(GivenDate = as.Date(substr(GivenDate, 1, 10))) %>>%
    dplyr::mutate(GivenDate = dplyr::if_else(GivenDate <= AppointmentDate, GivenDate, as.Date(NA)))
  # only include immunizations given up to date of appointment,
  # if there are any immunizations at all
  # note that 'if_else' vectorizes,
  # demanding same datatype for TRUE/FALSE alternatives
  # 'ifelse' does not preserve date type in this circumstance

  if (vaxtag) {
    zostavax_list <- zostavax_list %>>%
      dplyr::mutate(vaxtag =
                      dMeasure::semantic_tag(
                        paste0(' Zostavax '),
                        colour =
                          dplyr::if_else(is.na(GivenDate),
                                         dplyr::if_else(is.na(ITEMID), c('red'), c('purple')),
                                         c('green')),
                        # red if not given, purple if removed from herpes zoster vax reminders
                        # and green if has had the vax
                        popuphtml =
                          paste0("<h4>",
                                 dplyr::if_else(is.na(ITEMID),
                                                dplyr::if_else(
                                                  is.na(GivenDate),
                                                  "Age 70 to 79 years",
                                                  paste0('Date : ', format(GivenDate))),
                                                'Removed from herpes zoster immunization reminders'),
                                 "</h4>")))
  }

  if (vaxtag_print) {
    zostavax_list <- zostavax_list %>>%
      dplyr::mutate(vaxtag_print =
                      paste0("Zostavax", " ", # printable version of information
                             dplyr::if_else(
                               is.na(GivenDate),
                               dplyr::if_else(
                                 is.na(ITEMID),
                                 "(DUE) (Age 70 to 79 years)",
                                 "(Removed from herpes zoster immunization reminders)"),
                               paste0("(Given : ", format(GivenDate), ")"))
                      ))
  }

  zostavax_list <- zostavax_list %>>%
    dplyr::select(intersect(names(zostavax_list),
                            c("Patient", "InternalID", "AppointmentDate", "AppointmentTime",
                              "Provider", "DOB", "Age", "vaxtag", "vaxtag_print")))

  return(zostavax_list)
})

#' List of patients potentially eligible for influenza vaccine
#'
#' Includes patients who may have already had influenza vaccine,
#' date of previous influenza vaccine is included.
#'
#' Optionally added a HTML ('vaxtag') or printable ('vaxtag_print')
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from from date range (default $date_a)
#' @param date_to to date range (default $date_b)
#' @param clinicians list of clinicians (default $clinicians)
#' @param appointments_list provide an appointment list (default $appointments_list)
#' @param lazy = FALSE recalculate an appointment list
#' @param vaxtag = FALSE
#' @param vaxtag_print = TRUE
#'
#' @return dataframe list of influenza eligible patients
#' @export
list_influenza <- function(dMeasure_obj, date_from = NA, date_to = NA, clinicians = NA,
                           appointments_list = NULL,
                           lazy = FALSE,
                           vaxtag = FALSE, vaxtag_print = TRUE) {
  dMeasure_obj$list_influenza(date_from, date_to , clinicians,
                              appointments_list,
                              lazy,
                              vaxtag, vaxtag_print)
}

.public(dMeasure, "list_influenza", function(date_from = NA, date_to = NA, clinicians = NA,
                                             appointments_list = NULL,
                                             lazy = FALSE,
                                             vaxtag = FALSE, vaxtag_print = TRUE) {
  # return datatable of appointments where influenza is recommended (might already be given)

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
  # just the InternalID. add 'dummy' because cannot search %in% empty vector
  fluvaxID <- unlist(self$db$vaccine_disease %>>%
                       dplyr::filter(DISEASECODE %in% c(7,30)) %>>%
                       dplyr::select(VACCINEID) %>>%
                       dplyr::collect(), use.names = FALSE)
  # there are many, many influenza vaccine IDs, but these can be found
  # via the db$vaccine_disease database

  lprevious <- appointments_list %>>%
    # those who have had influenza vaccines in the past
    dplyr::left_join(self$db$immunizations %>>%
                       dplyr::filter(InternalID %in% intID &&
                                       VaccineID %in% fluvaxID),
                     by = "InternalID",
                     copy = TRUE) %>>%
    dplyr::mutate(GivenDate = as.Date(substr(GivenDate, 1, 10))) %>>%
    dplyr::filter(GivenDate <= AppointmentDate) %>>%
    # only include immunizations given up to date of appointment,
    # if there are any immunizations at all
    # note that 'if_else' is vectorize,
    # demanding same datatype for TRUE/FALSE alternatives
    # 'ifelse' does not preserve date type in this circumstance
    dplyr::group_by(InternalID, AppointmentDate, AppointmentTime, Provider) %>>%
    # group by appointment
    dplyr::slice(which.max(GivenDate)) %>>%
    dplyr::ungroup() %>>%
    # (one) item with latest vaccinedate (prior to appointmentdate)
    dplyr::mutate(Reason = paste0("Given : ", GivenDate)) %>>%
    dplyr::select(c("Patient", "InternalID",
                    "AppointmentDate", "AppointmentTime", "Status", "Provider",
                    "DOB", "Age",
                    "GivenDate", "Reason"))

  l65 <- appointments_list %>>%
    dplyr::filter(Age>=65) %>>%
    dplyr::mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
                  Reason = "Age 65 years or greater")

  l5 <- appointments_list %>>%
    dplyr::mutate(AgeInMonths = dMeasure::calc_age_months(DOB, AppointmentDate)) %>>%
    dplyr::filter(AgeInMonths >= 6 & AgeInMonths < 60) %>>%
    dplyr::mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
                  Reason = "Age 6 months to 4 years inclusive") %>>%
    dplyr::select(-AgeInMonths)

  lprematurity <- appointments_list %>>%
    # pre-term infants
    dplyr::mutate(AgeInMonths = dMeasure::calc_age_months(DOB, AppointmentDate)) %>>%
    dplyr::filter(AgeInMonths >= 6 & AgeInMonths < 24) %>>%
    dplyr::filter(InternalID %in%
                    (self$db$history %>>% dplyr::filter(ConditionID == 2973) %>>%
                       dplyr::pull(InternalID))) %>>%
    dplyr::mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
                  Reason = "Premature infant (if <37 weeks gestation)") %>>%
    dplyr::select(-AgeInMonths)

  ldiabetes <- appointments_list %>>%
    dplyr::filter(InternalID %in% self$diabetes_list(intID_Date)) %>>%
    dplyr::mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
                  Reason = "Diabetes")

  latsi <- appointments_list %>>%
    dplyr::filter(InternalID %in% self$atsi_list(intID_Date)) %>>%
    dplyr::mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
                  Reason = "Aboriginal or Torres Strait Islander")

  lasthma <- appointments_list %>>%
    dplyr::filter(InternalID %in% self$asthma_list(intID_Date)) %>>%
    dplyr::mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
                  Reason = "Asthma")

  lmalignancy <- appointments_list %>>%
    dplyr::filter(InternalID %in% self$malignancy_list(intID_Date)) %>>%
    dplyr::mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
                  Reason = "Malignancy")

  lhiv <- appointments_list %>>%
    dplyr::filter(InternalID %in% self$hiv_list(intID_Date)) %>>%
    dplyr::mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
                  Reason = "HIV")

  lhaemoglobinopathy <- appointments_list %>>%
    dplyr::filter(InternalID %in% self$haemoglobinopathy_list(intID_Date)) %>>%
    dplyr::mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
                  Reason = "Haemoglobinopathy")

  lasplenic <- appointments_list %>>%
    dplyr::filter(InternalID %in% self$asplenic_list(intID_Date)) %>>%
    dplyr::mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
                  Reason = "Asplenia")

  ltransplant <- appointments_list %>>%
    dplyr::filter(InternalID %in% self$transplant_list(intID_Date)) %>>%
    dplyr::mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
                  Reason = "Transplant recipient")

  lcardiac <- appointments_list %>>%
    dplyr::filter(InternalID %in% self$cardiacdisease_list(intID_Date)) %>>%
    dplyr::mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
                  Reason = "Heart disease")

  lbmi30 <- appointments_list %>>%
    dplyr::filter(InternalID %in% self$bmi30_list(intID_Date)) %>>%
    dplyr::mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
                  Reason = "BMI>30")

  lchroniclung <- appointments_list %>>%
    dplyr::filter(InternalID %in% self$chroniclungdisease_list(intID_Date)) %>>%
    dplyr::mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
                  Reason = "Chronic lung disease")

  lneurology <- appointments_list %>>%
    dplyr::filter(InternalID %in% self$neurologic_list(intID_Date)) %>>%
    dplyr::mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
                  Reason = "Neurological disease")

  lchronicliver <- appointments_list %>>%
    dplyr::filter(InternalID %in% self$chronicliverdisease_list(intID_Date)) %>>%
    dplyr::mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
                  Reason = "Chronic liver disease")

  lrenaldisease <- appointments_list %>>%
    dplyr::filter(InternalID %in% self$chronicrenaldisease_list(intID_Date)) %>>%
    dplyr::mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
                  Reason = "BMI>30")

  lchildaspirin <- appointments_list %>>%
    # children aged 6 months to 10 years on long-term aspirin
    # risk of Reye's syndrome after influenza infection
    dplyr::mutate(AgeInMonths = dMeasure::calc_age_months(DOB, AppointmentDate)) %>>%
    dplyr::filter(AgeInMonths >= 6 & AgeInMonths <= 131) %>>%
    dplyr::filter(InternalID %in%
                    (self$db$currentrx %>>%
                       dplyr::filter(RXSTATUS == 1 & PRODUCTID %in%
                                       c(99,8489,222,522,534,12254,545,546,547,549,
                                         548,550,554,551,552,553,555,
                                         8726,11362,540,8060,8062,8061,8063,8064,541,
                                         8304,560,559,558,562,563,8071,
                                         710,13262,1131,1148,11361,11327,1612,1613,1614,
                                         1619,11360,1917,16891,2328,
                                         2340,2341,2342,2345,2344,11326,14681,2523,3531,
                                         16877,6827,6918,12519,
                                         7651,7704)) %>>%
                       # RXSTATUS == 1 (long-term medication), many aspirin productIDs!
                       dplyr::pull(InternalID))
    ) %>>%
    dplyr::mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
                  Reason = "Child aged 6 months to 10 years on long-term aspirin") %>>%
    dplyr::select(c("Patient", "InternalID",
                    "AppointmentDate", "AppointmentTime", "Status", "Provider",
                    "DOB", "Age",
                    "GivenDate", "Reason"))

  lpregnant <- appointments_list %>>%
    dplyr::filter(InternalID %in% self$pregnant_list(intID_Date)) %>>%
    dplyr::mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
                  Reason = "Pregnancy")

  lhomeless <- appointments_list %>>%
    # homeless infants
    dplyr::filter(InternalID %in%
                    (self$db$history %>>% dplyr::filter(ConditionID == 3017 & Status == "Active") %>>%
                       dplyr::pull(InternalID))) %>>%
    dplyr::mutate(GivenDate = as.Date(-Inf, origin = '1970-01-01'),
                  Reason = "Homeless")

  l <- rbind(lprevious, l65, l5, lprematurity, latsi, ldiabetes, lasthma,
             lmalignancy, lhiv, lhaemoglobinopathy, lasplenic, ltransplant,
             lcardiac, lbmi30, lchroniclung, lneurology, lrenaldisease, lchronicliver,
             lchildaspirin, lpregnant, lhomeless) %>>%
    dplyr::group_by(Patient, InternalID, AppointmentDate, AppointmentTime, Provider, DOB, Age) %>>%
    dplyr::summarise(GivenDate = max(GivenDate),
                     Reason = paste0(Reason, collapse = ", ")) %>>% # join unique Reasons together
    dplyr::ungroup() %>>%
    dplyr::left_join(self$db$preventive_health %>>%
                       # those who have been removed from the reminder system for influenza
                       dplyr::filter(ITEMID == 1), by = "InternalID",
                     copy = TRUE) %>>%
    dplyr::collect()

  if (vaxtag) {
    l <- l %>>%
      dplyr::mutate(
        vaxtag =
          dMeasure::semantic_tag(
            paste0(' Influenza '),
            colour =
              dplyr::if_else(is.na(GivenDate) |
                               (GivenDate == as.Date(-Inf, origin = '1970-01-01')),
                             dplyr::if_else(is.na(ITEMID),
                                            c('red'), c('purple')),
                             dplyr::if_else(
                               format(GivenDate, "%Y") == format(AppointmentDate, "%Y"),
                               # this compares dates
                               # https://stackoverflow.com/questions/36568070/extract-year-from-date
                               c('green'), c("yellow"))),
            # red if not given, purple if removed from flu vax reminders
            # and green if has had the vax this year. yellow if 'old' vax
            popuphtml =
              paste0("<h4>",
                     dplyr::if_else(is.na(ITEMID),
                                    as.character(Reason), # co-erce to character (it could be empty)
                                    'Removed from influenza immunization reminders'),
                     "</h4>")))
  }

  if (vaxtag_print) {
    l <- l %>>%
      dplyr::mutate(
        vaxtag_print =
          paste0("Influenza", # printable version of information
                 dplyr::if_else(
                   is.na(GivenDate),
                   dplyr::if_else(is.na(ITEMID),
                                  paste0(" (", as.character(Reason), ")"),
                                  " (Removed from influenza immunization reminders)"),
                   paste0(dplyr::if_else(
                     is.na(GivenDate) | (GivenDate == -Inf), # no previous vax
                     " (DUE)",
                     dplyr::if_else(
                       format(GivenDate, "%Y") == format(AppointmentDate, "%Y"),
                       # this compares dates
                       # https://stackoverflow.com/questions/36568070/extract-year-from-date
                       " ", " (DUE)")),
                     " (", Reason, ")"
                   )) # ?vax given this year
          ))
  }

  l <- l %>>%
    dplyr::select(intersect(names(l),
                            c("Patient", "InternalID", "AppointmentDate", "AppointmentTime", "Provider",
                              "DOB", "Age", "vaxtag", "vaxtag_print")))

  return(l)
})



vax_names <- c("Zostavax", "Influenza")

#' List of patients potentially eligible for vaccines
#'
#' Includes patients who may have already had the vaccine,
#' date of previous vaccine is included.
#'
#' Optionally added a HTML ('vaxtag') or printable ('vaxtag_print')
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from from date range (default $date_a)
#' @param date_to to date range (default $date_b)
#' @param clinicians list of clinicians (default $clinicians)
#' @param appointments_list provide an appointment list (default $appointments_list)
#' @param lazy (default FALSE) recalculate an appointment list
#' @param vaxtag (default FALSE) HTML/browser version of tags
#' @param vaxtag_print (default TRUE) printable version of tags
#' @param chosen list of vaccine names (default is all)
#'
#' @return dataframe list of influenza eligible patients
#' @export
list_vax <- function(dMeasure_obj, date_from = NA, date_to = NA, clinicians = NA,
                     appointments_list = NULL,
                     lazy = FALSE,
                     vaxtag = FALSE, vaxtag_print = TRUE,
                     chosen = vax_names) {
  dMeasure_obj$list_vax(date_from, date_to, clinicians,
                        appointments_list,
                        lazy,
                        vaxtag, vaxtag_print,
                        chosen)
}

.public(dMeasure, "list_vax", function(date_from = NA, date_to = NA, clinicians = NA,
                                       appointments_list = NULL,
                                       lazy = FALSE,
                                       vaxtag = FALSE, vaxtag_print = TRUE,
                                       chosen = vax_names) {

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
  if ("Zostavax" %in% chosen) {
    vlist <- rbind(vlist, self$list_zostavax(date_from, date_to, clinicians,
                                             appointments_list,
                                             lazy,
                                             vaxtag, vaxtag_print))
  }
  if ("Influenza" %in% chosen) {
    vlist <- rbind(vlist, self$list_influenza(date_from, date_to, clinicians,
                                              appointments_list,
                                              lazy,
                                              vaxtag, vaxtag_print))
  }

  if (!is.null(vlist)) {
    vlist <- vlist %>>%
      dplyr::group_by(Patient, InternalID, AppointmentDate, AppointmentTime, Provider,
                      DOB, Age) %>>%
      # gathers vaccination notifications on the same appointment into a single row
      {if (vaxtag)
      {dplyr::summarise(., vaxtag = paste(vaxtag, collapse = ""))}
        else {.}} %>>%
      {if (vaxtag_print)
      {dplyr::summarise(., vaxtag_print = paste(vaxtag_print, collapse = ", "))}
        else {.}} %>>%
      dplyr::ungroup()
  }

  return(vlist)
})

