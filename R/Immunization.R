# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
#' @param intID list of internal ID (default is NULL, in which case appointments_list is used)
#' @param intID_Date if intID is not NULL, then date to check (default is Sys.Date())
#' @param appointments_list provide an appointment list (default $appointments_list)
#' @param include_uptodate include those who are up-to-date ('green' tags)
#' @param lazy = FALSE recalculate an appointment list
#' @param vaxtag = FALSE
#' @param vaxtag_print = TRUE
#'
#' @return dataframe list of Zostavax eligible patients
#' @export
list_zostavax <- function(dMeasure_obj,
                          date_from = NA, date_to = NA, clinicians = NA,
                          intID = NULL, intID_Date = Sys.Date(),
                          appointments_list = NULL,
                          include_uptodate = TRUE,
                          lazy = FALSE,
                          vaxtag = FALSE, vaxtag_print = TRUE) {
  dMeasure_obj$list_zostavax(
    date_from, date_to, clinicians,
    intID, intID_Date,
    appointments_list,
    include_uptodate,
    lazy,
    vaxtag, vaxtag_print
  )
}

.public(dMeasure, "list_zostavax", function(date_from = NA, date_to = NA, clinicians = NA,
                                            intID = NULL, intID_Date = Sys.Date(),
                                            appointments_list = NULL,
                                            include_uptodate = TRUE,
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

  if (is.null(intID) && (all(is.na(clinicians)) || length(clinicians) == 0)) {
    # need appointments to view. or vector of internalID
    stop("Choose at least one clinicians\'s appointment to view")
  }

  if (is.null(intID)) {
    # list of internalID has not been supplied
    # in which case internalID and dates will be derived from appointment list

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
      dplyr::filter(Age >= 70 & Age <= 80) %>>%
      dplyr::mutate(Date = AppointmentDate) # used to compare with vax date
    # 'Date' will later be filtered out
  } else {
    intID <- self$db$patients %>>% # check the age of the intID list
      dplyr::filter(InternalID %in% c(intID, -1)) %>>%
      dplyr::select(InternalID, DOB) %>>%
      dplyr::collect() %>>%
      dplyr::mutate(DOB = as.Date(DOB), Date = as.Date(intID_Date)) %>>%
      # initially Date is a dttm (POSIXt) object,
      # which makes the subsequent calc_age very slow,
      # and throws up warnings
      dplyr::mutate(Age = dMeasure::calc_age(DOB, Date)) %>>%
      dplyr::filter(Age >= 70 & Age <= 80) %>>%
      dplyr::pull(InternalID) %>>%
      unique() %>>% c(-1)

    intID_Date <- data.frame(InternalID = intID, Date = intID_Date)
    zostavax_list <- intID_Date
  }

  zostavax_list <- zostavax_list %>>%
    dplyr::left_join(self$db$immunizations %>>%
      dplyr::filter(
        InternalID %in% intID,
        # those who have had the zostavax vaccine
        ((VaccineName %LIKE% "%zostavax%") |
          (VaccineID == 103))
      ),
    by = "InternalID",
    copy = TRUE
    ) %>>%
    dplyr::left_join(self$db$preventive_health %>>%
      # those who have been removed from the reminder system for Zostavax
      dplyr::filter(ITEMID == 15),
    by = "InternalID",
    copy = TRUE
    ) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(GivenDate = as.Date(substr(GivenDate, 1, 10))) %>>%
    dplyr::mutate(GivenDate = dplyr::if_else(GivenDate <= Date, GivenDate, as.Date(NA))) %>>%
    dplyr::filter(InternalID != -1) # get rid of dummy internalID
  # only include immunizations given up to date of appointment,
  # if there are any immunizations at all
  # note that 'if_else' vectorizes,
  # demanding same datatype for TRUE/FALSE alternatives
  # 'ifelse' does not preserve date type in this circumstance

  if (!include_uptodate) {
    # remove entries which are 'up-to-date'!
    zostavax_list <- zostavax_list %>>%
      dplyr::filter(is.na(GivenDate))
    # anyone who has had a Zostavax (and so has a valid 'GivenDate') is 'up-to-date'!
  }

  if (vaxtag) {
    zostavax_list <- zostavax_list %>>%
      dplyr::mutate(
        vaxtag =
          dMeasure::semantic_tag(
            paste0(" Zostavax "),
            colour =
              dplyr::if_else(
                is.na(GivenDate),
                dplyr::if_else(is.na(ITEMID), c("red"), c("purple")),
                c("green")
              ),
            # red if not given, purple if removed from herpes zoster vax reminders
            # and green if has had the vax
            popuphtml =
              paste0(
                "<h4>",
                dplyr::if_else(
                  is.na(GivenDate),
                  dplyr::if_else(
                    is.na(ITEMID),
                    dplyr::if_else(
                      is.na(GivenDate),
                      "Age 70 to 79 years",
                      paste0("Date : ", format(GivenDate))
                    ),
                    "Removed from herpes zoster immunization reminders"
                  ),
                  paste0("Given : ", format(GivenDate))
                ),
                "</h4>"
              )
          )
      )
  }

  if (vaxtag_print) {
    zostavax_list <- zostavax_list %>>%
      dplyr::mutate(
        vaxtag_print =
          paste0(
            "Zostavax", " ", # printable version of information
            dplyr::if_else(
              is.na(GivenDate),
              dplyr::if_else(
                is.na(ITEMID),
                "(DUE) (Age 70 to 79 years)",
                "(Removed from herpes zoster immunization reminders)"
              ),
              paste0("(Given : ", format(GivenDate), ")")
            )
          )
      )
  }

  zostavax_list <- zostavax_list %>>%
    dplyr::select(intersect(
      names(zostavax_list),
      c(
        "Patient", "InternalID", "AppointmentDate", "AppointmentTime",
        "Provider", "DOB", "Age", "vaxtag", "vaxtag_print"
      )
    ))

  return(zostavax_list)
})

#' List of patients potentially eligible for measles vaccine
#'
#' Includes patients who may have already had measles vaccine,
#' date of previous measles vaccine is included.
#'
#' Optionally added a HTML ('vaxtag') or printable ('vaxtag_print')
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from from date range (default $date_a)
#' @param date_to to date range (default $date_b)
#' @param clinicians list of clinicians (default $clinicians)
#' @param intID list of internal ID (default is NULL, in which case appointments_list is used)
#' @param intID_Date if intID is not NULL, then date to check (default is Sys.Date())
#' @param appointments_list provide an appointment list (default $appointments_list)
#' @param include_uptodate include those who are up-to-date ('green' tags)
#' @param lazy = FALSE recalculate an appointment list
#' @param vaxtag = FALSE
#' @param vaxtag_print = TRUE
#'
#' @return dataframe list of measles eligible patients
#' @export
list_measlesVax <- function(dMeasure_obj,
                            date_from = NA, date_to = NA, clinicians = NA,
                            intID = NULL, intID_Date = Sys.Date(),
                            appointments_list = NULL,
                            include_uptodate = TRUE,
                            lazy = FALSE,
                            vaxtag = FALSE, vaxtag_print = TRUE) {
  dMeasure_obj$list_measlesVax(
    date_from, date_to, clinicians,
    intID, intID_Date,
    appointments_list,
    include_uptodate,
    lazy,
    vaxtag, vaxtag_print
  )
}

.public(dMeasure, "list_measlesVax", function(date_from = NA, date_to = NA, clinicians = NA,
                                              intID = NULL, intID_Date = Sys.Date(),
                                              appointments_list = NULL,
                                              include_uptodate = TRUE,
                                              lazy = FALSE,
                                              vaxtag = FALSE, vaxtag_print = TRUE) {
  # return datatable of appointments where measles vaccine is recommended
  # (might already be given)

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

  if (is.null(intID) && (all(is.na(clinicians)) || length(clinicians) == 0)) {
    # need appointments, or list of internalID
    stop("Choose at least one clinicians\'s appointment to view")
  }

  if (is.null(intID)) {
    # no vector of internalID given, so get internalID from appointment list
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
    measlesVax_list <- appointments_list %>>%
      dplyr::filter(DOB >= as.Date("1966-01-01") &
        DOB <= as.Date("1997-12-31")) %>>%
      # from DOB 1966 to 1997 inclusive
      dplyr::mutate(Date = AppointmentDate) # used to compare with vax date
  } else {
    intID <- self$db$patients %>>% # check the age of the intID list
      dplyr::filter(InternalID %in% c(intID, -1)) %>>%
      dplyr::select(InternalID, DOB) %>>%
      dplyr::filter(DOB >= as.Date("1966-01-01") &
        DOB <= as.Date("1997-12-31")) %>>%
      dplyr::pull(InternalID) %>>%
      unique() %>>% c(-1)

    intID_Date <- data.frame(InternalID = intID, Date = intID_Date)
    measlesVax_list <- intID_Date
  }
  pregnantID <- c(self$pregnant_list(intID_Date), -1)
  # pregnancy is a contra-indication to measles vax

  measlesVaxID <- unlist(self$db$vaccine_disease %>>%
    dplyr::filter(DISEASECODE %in% c(9)) %>>%
    dplyr::select(VACCINEID) %>>%
    dplyr::collect(), use.names = FALSE)
  # there are several measles vaccines, these can be found
  # via the db$vaccine_disease database

  measlesVax_list <- measlesVax_list %>>%
    dplyr::left_join(self$db$immunizations %>>%
      dplyr::filter(
        InternalID %in% intID,
        # those who have had the measles vaccine
        ((VaccineName %LIKE% "%measles%") |
          VaccineName %LIKE% "%mmr%" |
          (VaccineID %in% measlesVaxID))
      ),
    by = "InternalID",
    copy = TRUE
    ) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(GivenDate = as.Date(substr(GivenDate, 1, 10))) %>>%
    dplyr::mutate(GivenDate = dplyr::if_else(GivenDate <= Date, GivenDate, as.Date(NA))) %>>%
    # only include immunizations given up to date of appointment (or other defined Date),
    # if there are any immunizations at all
    # note that 'if_else' vectorizes,
    # demanding same datatype for TRUE/FALSE alternatives
    # 'ifelse' does not preserve date type in this circumstance
    dplyr::mutate(Pregnant = InternalID %in% pregnantID) %>>%
    # pregnancy is an exclusion criteria for measles vax
    dplyr::filter(InternalID != -1) # remove dummy row

  if (!include_uptodate) {
    measlesVax_list <- measlesVax_list %>>%
      dplyr::filter(is.na(GivenDate))
    # remove entries which are 'up-to-date'!
    # anyone who has had a measles vax (and so has a valid 'GivenDate') is 'up-to-date'!
  }

  if (vaxtag) {
    measlesVax_list <- measlesVax_list %>>%
      dplyr::mutate(
        vaxtag =
          dMeasure::semantic_tag(
            paste0(" Measles "),
            colour =
              dplyr::if_else(
                is.na(GivenDate),
                dplyr::if_else(!Pregnant, c("red"), c("purple")),
                c("green")
              ),
            # red if not given, purple if exclusion criteria (pregnant)
            # and green if has had the vax
            popuphtml =
              paste0(
                "<h4>",
                dplyr::if_else(
                  is.na(GivenDate),
                  dplyr::if_else(
                    !Pregnant,
                    "Born 1966 to 1997 inclusive",
                    "Contra-indication : pregnancy"
                  ),
                  paste0("Date : ", format(GivenDate))
                ),
                "</h4>"
              )
          )
      )
  }

  if (vaxtag_print) {
    measlesVax_list <- measlesVax_list %>>%
      dplyr::mutate(
        vaxtag_print =
          paste0(
            "Measles", " ", # printable version of information
            dplyr::if_else(
              is.na(GivenDate),
              dplyr::if_else(
                !Pregnant,
                "(DUE) (Born 1966 to 1997 inclusive)",
                "(Contra-indication : pregnancy)"
              ),
              paste0("(Given : ", format(GivenDate), ")")
            )
          )
      )
  }

  measlesVax_list <- measlesVax_list %>>%
    dplyr::select(intersect(
      names(measlesVax_list),
      c(
        "Patient", "InternalID", "AppointmentDate", "AppointmentTime",
        "Provider", "DOB", "Age", "vaxtag", "vaxtag_print"
      )
    ))

  return(measlesVax_list)
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
#' @param intID list of internal ID (default is NULL, in which case appointments_list is used)
#' @param intID_Date if intID is not NULL, then date to check (default is Sys.Date())
#' @param appointments_list provide an appointment list (default $appointments_list)
#' @param include_uptodate include those who are up-to-date ('green' tags)
#' @param lazy = FALSE recalculate an appointment list
#' @param vaxtag = FALSE
#' @param vaxtag_print = TRUE
#'
#' @return dataframe list of influenza eligible patients
#' @export
list_influenza <- function(dMeasure_obj, date_from = NA, date_to = NA, clinicians = NA,
                           intID = NULL, intID_Date = Sys.Date(),
                           appointments_list = NULL,
                           include_uptodate = TRUE,
                           lazy = FALSE,
                           vaxtag = FALSE, vaxtag_print = TRUE) {
  dMeasure_obj$list_influenza(
    date_from, date_to, clinicians,
    intID, intID_Date,
    appointments_list,
    include_uptodate,
    lazy,
    vaxtag, vaxtag_print
  )
}

.public(dMeasure, "list_influenza", function(date_from = NA, date_to = NA, clinicians = NA,
                                             intID = NULL, intID_Date = Sys.Date(),
                                             appointments_list = NULL,
                                             include_uptodate = TRUE,
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

  if (is.null(intID) && (all(is.na(clinicians)) || length(clinicians) == 0)) {
    # need to have appointments to view, or a provided list of internalID
    stop("Choose at least one clinicians\'s appointment to view")
  }

  if (is.null(intID)) {
    # no internalID list given, so look in appointments

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
    list_details <- appointments_list %>>%
      dplyr::mutate(Date = AppointmentDate) %>>% # used to compare with vax date
      dplyr::mutate(AgeInMonths = dMeasure::calc_age_months(DOB, Date))
    use_intID <- FALSE
  } else {
    use_intID <- TRUE
    # a flag to remove AppointmentDate/AppointmentTime/Provider fields later

    intID <- c(unique(intID), -1)

    intID_Date <- data.frame(InternalID = intID, Date = intID_Date)
    list_details <- intID_Date %>>%
      dplyr::left_join(self$db$patients %>>%
        dplyr::filter(InternalID %in% c(intID, -1)) %>>%
        dplyr::select(InternalID, DOB),
      by = "InternalID", copy = TRUE
      ) %>>%
      dplyr::mutate(DOB = as.Date(DOB)) %>>%
      dplyr::mutate(
        AgeInMonths = dMeasure::calc_age_months(DOB, Date),
        Age = dMeasure::calc_age(DOB, Date),
        AppointmentDate = Date,
        AppointmentTime = NA, Patient = NA,
        Provider = NA
      ) # some 'fake' fields
  }

  fluvaxID <- unlist(self$db$vaccine_disease %>>%
    dplyr::filter(DISEASECODE %in% c(7, 30)) %>>%
    dplyr::select(VACCINEID) %>>%
    dplyr::collect(), use.names = FALSE)
  # there are many, many influenza vaccine IDs, but these can be found
  # via the db$vaccine_disease database

  lprevious <- list_details %>>%
    # those who have had influenza vaccines in the past
    dplyr::left_join(self$db$immunizations %>>%
      dplyr::filter(
        InternalID %in% intID,
        VaccineID %in% fluvaxID
      ),
    by = "InternalID",
    copy = TRUE
    ) %>>%
    dplyr::mutate(GivenDate = as.Date(substr(GivenDate, 1, 10))) %>>%
    dplyr::filter(GivenDate <= Date) %>>%
    # only include immunizations given up to date of appointment,
    # if there are any immunizations at all
    # note that 'if_else' is vectorize,
    # demanding same datatype for TRUE/FALSE alternatives
    # 'ifelse' does not preserve date type in this circumstance
    dplyr::group_by(InternalID, AppointmentDate, AppointmentTime, Provider) %>>%
    # group by appointment
    dplyr::arrange(dplyr::desc(GivenDate), .by_group = TRUE) %>>%
    dplyr::filter(dplyr::row_number() == 1) %>>%
    # the 'maximum' TestDate, breaking 'ties'
    dplyr::ungroup() %>>%
    # (one) item with latest vaccinedate (prior to appointmentdate/Date)
    dplyr::mutate(Reason = paste0("Given : ", GivenDate)) %>>%
    dplyr::select(-c("VaccineName", "VaccineID")) # don't need these columns now

  l65 <- list_details %>>%
    dplyr::filter(Age >= 65) %>>%
    dplyr::mutate(
      GivenDate = as.Date(-Inf, origin = "1970-01-01"),
      Reason = "Age 65 years or greater"
    )

  l5 <- list_details %>>%
    dplyr::filter(AgeInMonths >= 6 & AgeInMonths < 60) %>>%
    dplyr::mutate(
      GivenDate = as.Date(-Inf, origin = "1970-01-01"),
      Reason = "Age 6 months to 4 years inclusive"
    )

  lprematurity <- list_details %>>%
    # pre-term infants
    dplyr::filter(AgeInMonths >= 6 & AgeInMonths < 24) %>>%
    dplyr::filter(InternalID %in%
      (self$db$history %>>% dplyr::filter(ConditionID == 2973) %>>%
        dplyr::pull(InternalID))) %>>%
    dplyr::mutate(
      GivenDate = as.Date(-Inf, origin = "1970-01-01"),
      Reason = "Premature infant (if <37 weeks gestation)"
    )

  ldiabetes <- list_details %>>%
    dplyr::filter(InternalID %in% self$diabetes_list(intID_Date)) %>>%
    dplyr::mutate(
      GivenDate = as.Date(-Inf, origin = "1970-01-01"),
      Reason = "Diabetes"
    )

  latsi <- list_details %>>%
    dplyr::filter(InternalID %in% self$atsi_list(intID_Date)) %>>%
    dplyr::mutate(
      GivenDate = as.Date(-Inf, origin = "1970-01-01"),
      Reason = "Aboriginal or Torres Strait Islander"
    )

  lasthma <- list_details %>>%
    dplyr::filter(InternalID %in% self$asthma_list(intID_Date)) %>>%
    dplyr::mutate(
      GivenDate = as.Date(-Inf, origin = "1970-01-01"),
      Reason = "Asthma"
    )

  lmalignancy <- list_details %>>%
    dplyr::filter(InternalID %in% self$malignancy_list(intID_Date)) %>>%
    dplyr::mutate(
      GivenDate = as.Date(-Inf, origin = "1970-01-01"),
      Reason = "Malignancy"
    )

  lhiv <- list_details %>>%
    dplyr::filter(InternalID %in% self$hiv_list(intID_Date)) %>>%
    dplyr::mutate(
      GivenDate = as.Date(-Inf, origin = "1970-01-01"),
      Reason = "HIV"
    )

  lhaemoglobinopathy <- list_details %>>%
    dplyr::filter(InternalID %in% self$haemoglobinopathy_list(intID_Date)) %>>%
    dplyr::mutate(
      GivenDate = as.Date(-Inf, origin = "1970-01-01"),
      Reason = "Haemoglobinopathy"
    )

  lasplenic <- list_details %>>%
    dplyr::filter(InternalID %in% self$asplenic_list(intID_Date)) %>>%
    dplyr::mutate(
      GivenDate = as.Date(-Inf, origin = "1970-01-01"),
      Reason = "Asplenia"
    )

  ltransplant <- list_details %>>%
    dplyr::filter(InternalID %in% self$transplant_list(intID_Date)) %>>%
    dplyr::mutate(
      GivenDate = as.Date(-Inf, origin = "1970-01-01"),
      Reason = "Transplant recipient"
    )

  lcardiac <- list_details %>>%
    dplyr::filter(InternalID %in% self$cardiacdisease_list(intID_Date)) %>>%
    dplyr::mutate(
      GivenDate = as.Date(-Inf, origin = "1970-01-01"),
      Reason = "Heart disease"
    )

  lbmi30 <- list_details %>>%
    dplyr::filter(InternalID %in% self$bmi30_list(intID_Date)) %>>%
    dplyr::mutate(
      GivenDate = as.Date(-Inf, origin = "1970-01-01"),
      Reason = "BMI>30"
    )

  lchroniclung <- list_details %>>%
    dplyr::filter(InternalID %in% self$chroniclungdisease_list(intID_Date)) %>>%
    dplyr::mutate(
      GivenDate = as.Date(-Inf, origin = "1970-01-01"),
      Reason = "Chronic lung disease"
    )

  lneurology <- list_details %>>%
    dplyr::filter(InternalID %in% self$neurologic_list(intID_Date)) %>>%
    dplyr::mutate(
      GivenDate = as.Date(-Inf, origin = "1970-01-01"),
      Reason = "Neurological disease"
    )

  lchronicliver <- list_details %>>%
    dplyr::filter(InternalID %in% self$chronicliverdisease_list(intID_Date)) %>>%
    dplyr::mutate(
      GivenDate = as.Date(-Inf, origin = "1970-01-01"),
      Reason = "Chronic liver disease"
    )

  lrenaldisease <- list_details %>>%
    dplyr::filter(InternalID %in% self$chronicrenaldisease_list(intID_Date)) %>>%
    dplyr::mutate(
      GivenDate = as.Date(-Inf, origin = "1970-01-01"),
      Reason = "Renal disease"
    )

  lchildaspirin <- list_details %>>%
    # children aged 6 months to 10 years on long-term aspirin
    # risk of Reye's syndrome after influenza infection
    dplyr::filter(AgeInMonths >= 6 & AgeInMonths <= 131) %>>%
    dplyr::filter(InternalID %in%
      (self$db$currentRx_raw %>>%
        dplyr::filter(RXSTATUS == 1 & PRODUCTID %in%
          c(
            99, 8489, 222, 522, 534, 12254, 545, 546, 547, 549,
            548, 550, 554, 551, 552, 553, 555,
            8726, 11362, 540, 8060, 8062, 8061, 8063, 8064, 541,
            8304, 560, 559, 558, 562, 563, 8071,
            710, 13262, 1131, 1148, 11361, 11327, 1612, 1613, 1614,
            1619, 11360, 1917, 16891, 2328,
            2340, 2341, 2342, 2345, 2344, 11326, 14681, 2523, 3531,
            16877, 6827, 6918, 12519,
            7651, 7704
          )) %>>%
        # RXSTATUS == 1 (long-term medication), many aspirin productIDs!
        dplyr::pull(InternalID))) %>>%
    dplyr::mutate(
      GivenDate = as.Date(-Inf, origin = "1970-01-01"),
      Reason = "Child aged 6 months to 10 years on long-term aspirin"
    )

  lpregnant <- list_details %>>%
    dplyr::filter(InternalID %in% self$pregnant_list(intID_Date)) %>>%
    dplyr::mutate(
      GivenDate = as.Date(-Inf, origin = "1970-01-01"),
      Reason = "Pregnancy"
    )

  lhomeless <- list_details %>>%
    # homeless infants
    dplyr::filter(InternalID %in%
      (self$db$history %>>% dplyr::filter(ConditionID == 3017 &
        Status == "Active") %>>%
        dplyr::pull(InternalID))) %>>%
    dplyr::mutate(
      GivenDate = as.Date(-Inf, origin = "1970-01-01"),
      Reason = "Homeless"
    )

  l <- rbind(
    lprevious, l65, l5, lprematurity, latsi, ldiabetes, lasthma,
    lmalignancy, lhiv, lhaemoglobinopathy, lasplenic, ltransplant,
    lcardiac, lbmi30, lchroniclung, lneurology, lrenaldisease, lchronicliver,
    lchildaspirin, lpregnant, lhomeless
  ) %>>%
    dplyr::group_by(
      Patient, InternalID, AppointmentDate, AppointmentTime,
      Provider, DOB, Age
    ) %>>%
    dplyr::summarise(
      GivenDate = max(GivenDate),
      Reason = paste0(Reason, collapse = ", ")
    ) %>>%
    # join unique Reasons together
    dplyr::ungroup() %>>%
    dplyr::left_join(self$db$preventive_health %>>%
      # those who have been removed from the reminder system for influenza
      dplyr::filter(ITEMID == 1),
    by = "InternalID",
    copy = TRUE
    ) %>>%
    dplyr::collect()

  if (!include_uptodate) {
    l <- l %>>%
      dplyr::filter(is.na(GivenDate) |
        GivenDate == as.Date(-Inf, origin = "1970-01-01") |
        format(GivenDate, "%Y") != format(AppointmentDate, "%Y"))
    # remove entries which are 'up-to-date'!
    # anyone who has had a flu vax  in the same year as appointment date
    # (and so has a valid 'GivenDate') is 'up-to-date'!
  }

  if (vaxtag) {
    l <- l %>>%
      dplyr::mutate(
        vaxtag =
          dMeasure::semantic_tag(
            paste0(" Influenza "),
            colour =
              dplyr::if_else(
                is.na(GivenDate) |
                  (GivenDate == as.Date(-Inf, origin = "1970-01-01")),
                dplyr::if_else(
                  is.na(ITEMID),
                  c("red"), c("purple")
                ),
                dplyr::if_else(
                  format(GivenDate, "%Y") == format(AppointmentDate, "%Y"),
                  # this compares dates
                  # https://stackoverflow.com/questions/36568070/extract-year-from-date
                  c("green"), c("yellow")
                )
              ),
            # red if not given, purple if removed from flu vax reminders
            # and green if has had the vax this year. yellow if 'old' vax
            popuphtml =
              paste0(
                "<h4>",
                dplyr::if_else(
                  is.na(GivenDate),
                  dplyr::if_else(
                    is.na(ITEMID),
                    as.character(Reason), # co-erce to character (it could be empty)
                    "Removed from influenza immunization reminders"
                  ),
                  paste0(
                    dplyr::if_else(
                      is.na(GivenDate) | (GivenDate == -Inf), # no previous vax
                      "DUE -",
                      dplyr::if_else(
                        format(GivenDate, "%Y") == format(AppointmentDate, "%Y"),
                        # this compares dates
                        # https://stackoverflow.com/questions/36568070/extract-year-from-date
                        " ", " DUE -"
                      )
                    ),
                    " ", Reason
                  )
                ),
                "</h4>"
              )
          )
      )
  }

  if (vaxtag_print) {
    l <- l %>>%
      dplyr::mutate(
        vaxtag_print =
          paste0(
            "Influenza", # printable version of information
            dplyr::if_else(
              is.na(GivenDate),
              dplyr::if_else(
                is.na(ITEMID),
                paste0(" (", as.character(Reason), ")"),
                " (Removed from influenza immunization reminders)"
              ),
              paste0(
                dplyr::if_else(
                  is.na(GivenDate) | (GivenDate == -Inf), # no previous vax
                  " (DUE)",
                  dplyr::if_else(
                    format(GivenDate, "%Y") == format(AppointmentDate, "%Y"),
                    # this compares dates
                    # https://stackoverflow.com/questions/36568070/extract-year-from-date
                    " ", " (DUE)"
                  )
                ),
                " (", Reason, ")"
              )
            ) # ?vax given this year
          )
      )
  }

  if (use_intID == TRUE) {
    l <- l %>>% # remove 'dummy' fields
      dplyr::select(-c("Patient", "DOB", "Age", "AppointmentDate", "AppointmentTime", "Provider"))
  }

  l <- l %>>%
    dplyr::filter(InternalID != -1) %>>% # remove dummy ID
    dplyr::select(intersect(
      names(l),
      c(
        "Patient", "InternalID", "AppointmentDate", "AppointmentTime", "Provider",
        "DOB", "Age", "vaxtag", "vaxtag_print"
      )
    ))

  return(l)
})


.active(dMeasure, "vaccine_choices", function(value) {
  if (!missing(value)) {
    warning("$vaccine_choices is read-only.")
  } else {
    return(c("Zostavax", "Influenza", "Measles"))
    # vector of valid vaccine choices
  }
})

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
#' @param intID list of internal ID (default is NULL, in which case appointments_list is used)
#' @param intID_Date if intID is not NULL, then date to check (default is Sys.Date())
#' @param appointments_list provide an appointment list (default $appointments_list)
#' @param include_uptodate include those who are up-to-date ('green' tag)
#' @param lazy (default FALSE) recalculate an appointment list
#' @param vaxtag (default FALSE) HTML/browser version of tags
#' @param vaxtag_print (default TRUE) printable version of tags
#' @param chosen list of vaccine names (default is all)
#'
#' @return dataframe list of influenza eligible patients
#' @export
list_vax <- function(dMeasure_obj, date_from = NA, date_to = NA, clinicians = NA,
                     intID = NULL, intID_Date = Sys.Date(),
                     appointments_list = NULL,
                     include_uptodate = TRUE,
                     lazy = FALSE,
                     vaxtag = FALSE, vaxtag_print = TRUE,
                     chosen = self$vaccine_choices) {
  dMeasure_obj$list_vax(
    date_from, date_to, clinicians,
    intID, intID_Date,
    appointments_list,
    include_uptodate,
    lazy,
    vaxtag, vaxtag_print,
    chosen
  )
}

.public(dMeasure, "list_vax", function(date_from = NA, date_to = NA, clinicians = NA,
                                       intID = NULL, intID_Date = Sys.Date(),
                                       appointments_list = NULL,
                                       include_uptodate = TRUE,
                                       lazy = FALSE,
                                       vaxtag = FALSE, vaxtag_print = TRUE,
                                       chosen = self$vaccine_choices) {
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

  if (is.null(intID) && (all(is.na(clinicians)) || length(clinicians) == 0)) {
    # needs appointments, or a vector of intID
    stop("Choose at least one clinicians\'s appointment to view")
  }

  if (is.null(intID)) {
    # get internalIDs from appointments
    if (is.null(appointments_list) & !lazy) {
      self$list_appointments(date_from, date_to, clinicians, lazy = FALSE)
      # if not 'lazy' evaluation, then re-calculate self$appointments_billings
      # (that is automatically done by calling the $billed_appointments method)
    }

    if (is.null(appointments_list)) {
      appointments_list <- self$appointments_list
    }

    vlist <- data.frame(
      Patient = character(), InternalID = integer(),
      AppointmentDate = numeric(), AppointmentTime = character(),
      Provider = character(),
      DOB = numeric(), Age = double()
    )
  } else {
    vlist <- data.frame(InternalID = integer())
  }

  if (vaxtag) {
    vlist <- cbind(vlist, vaxtag = character())
  }
  if (vaxtag_print) {
    vlist <- cbind(vlist, vaxtag_print = character())
  }

  if ("Zostavax" %in% chosen) {
    vlist <- rbind(vlist, self$list_zostavax(
      date_from, date_to, clinicians,
      intID = intID, intID_Date = intID_Date,
      appointments_list = appointments_list,
      include_uptodate = include_uptodate,
      lazy = lazy,
      vaxtag = vaxtag, vaxtag_print = vaxtag_print
    ))
  }
  if ("Measles" %in% chosen) {
    vlist <- rbind(vlist, self$list_measlesVax(
      date_from, date_to, clinicians,
      intID = intID, intID_Date = intID_Date,
      appointments_list = appointments_list,
      include_uptodate = include_uptodate,
      lazy = lazy,
      vaxtag = vaxtag, vaxtag_print = vaxtag_print
    ))
  }
  if ("Influenza" %in% chosen) {
    vlist <- rbind(vlist, self$list_influenza(
      date_from, date_to, clinicians,
      intID = intID, intID_Date = intID_Date,
      appointments_list,
      include_uptodate = include_uptodate,
      lazy = lazy,
      vaxtag = vaxtag, vaxtag_print = vaxtag_print
    ))
  }

  if (nrow(vlist) > 0) {
    group_names <- intersect(
      names(vlist),
      c(
        "Patient", "InternalID", "AppointmentDate", "AppointmentTime",
        "Provider", "DOB", "Age"
      )
    )
    vlist <- vlist %>>%
      dplyr::group_by_at(group_names) %>>%
      # gathers vaccination notifications on the same appointment into a single row
      {
        if (vaxtag) {
          dplyr::summarise(., vaxtag = paste(vaxtag, collapse = ""))
        }
        else {
          .
        }
      } %>>% {
        if (vaxtag_print) {
          dplyr::summarise(., vaxtag_print = paste(vaxtag_print, collapse = ", "))
        }
        else {
          .
        }
      } %>>%
      dplyr::ungroup()
  }

  return(vlist)
})
