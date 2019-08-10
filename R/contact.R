#' Contact list fields and methods
#'
#' @name contact
#' @title dMeasureContact contact lists
#'
#' @include dMeasure.R
#' needs the '.public' function from dMeasure.R
#' @include appointments.R
#' needs $appointments_listR() and $appointments_list
NULL

## Fields

## Fields
.public("contact_appointments_list", data.frame(Patient = character(),
                                                InternalID = integer(),
                                                AppointmentDate =
                                                  as.Date(integer(0),
                                                          origin = "1970-01-01")))
.public("contact_visits_list", data.frame(Patient = character(),
                                          InternalID = integer(),
                                          Visit =
                                            as.Date(integer(0),
                                                    origin = "1970-01-01")))
# filtered by chosen dates and clinicians
.public("contact_count_list", data.frame(Patient = character(),
                                         InternalID = integer(),
                                         Count = integer()))
# filtered by chosen dates and clinicians


## Methods

#' List of contacts (appointment type)
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from=dMeasure_obj$date_a start date
#' @param date_to=dMeasure_obj$date_b end date (inclusive)
#' @param clinicians=dMeasure_obj$clinicians list of clinicians to view
#' @param status=NA filter by 'status' if not NA
#'  permissible values are 'Booked', 'Completed', 'At billing',
#'  'Waiting', 'With doctor'
#'  if NA, adopts from active $appointment_status
#' @param lazy=FALSE if TRUE, then re-calculate $appointments_list
#'
#' @return dataframe of Patient (name), InternalID, AppointmentDate
list_contact_appointments <- function(dMeasure_obj,
                                      date_from = NA,
                                      date_to = NA,
                                      clinicians = NA,
                                      status = NA,
                                      lazy = FALSE) {
  dMeasure_obj$list_contact_appointments(date_from, date_to, clinicians, status, lazy)
}

.public("list_contact_appointments", function(date_from = NA,
                                              date_to = NA,
                                              clinicians = NA,
                                              status = NA,
                                              lazy = FALSE) {

  if (is.na(date_from)) {
    date_from <- self$date_a
  }
  if (is.na(date_to)) {
    date_to <- self$date_b
  }
  if (length(clinicians) == 1 && is.na(clinicians)) {
    # sometimes clinicians is a list, in which case it cannot be a single NA!
    # 'if' is not vectorized so will only read the first element of the list
    # but if clinicians is a single NA, then read $clinicians
    clinicians <- self$clinicians
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }


  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {log_id <- private$config_db$write_log_db(
      query = "contact_appointments",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_appointments(date_from, date_to,
                             clinicians, status = status,
                             lazy = FALSE)
      # if not 'lazy' evaluation, then re-calculate self$appointments_list
      # (that is automatically done by calling the $list_appointments method)
    }

    self$contact_appointments_list <- self$appointments_list %>>%
      dplyr::group_by(Patient, InternalID, AppointmentDate) %>>%
      dplyr::summarise() # plucks out unique appointment dates

    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$contact_appointments_list)
})
.reactive_event("contact_appointments_listR",
                quote(
                  shiny::eventReactive(
                    c(self$appointments_listR()), {
                      # update if reactive version of $date_a Rdate_b
                      # or $clinicians are updated.
                      self$list_contact_appointments(lazy = TRUE)
                      # re-calculates the appointments
                    })
                ))



visit_types <- c("Surgery", "Home", "Non Visit", "Hospital", "RACF", "Telephone",
                 "SMS", "Email", "Locum Service", "Out of Office", "Other", "Hostel",
                 "Telehealth")

.private(".visit_type", visit_types)
# by default, all visit types are valid
.active("visit_type", function(value) {
  if (missing(value)) {
    return(private$.visit_type)
  }
  if (is.character(value)) {
    # accepts string, or vector of strings
    private$.visit_type <- value
    private$set_reactive(self$visit_typeR, value)
  } else {
    warning(paste("visit_type can only be set to a string,",
                  "a vector of strings. Valid strings are",
                  visit_types))
  }
})
.reactive("visit_typeR", quote(visit_types))

#' List of contacts (visit type)
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from=dMeasure_obj$date_a start date
#' @param date_to=dMeasure_obj$date_b end date (inclusive)
#' @param clinicians=dMeasure_obj$clinicians list of clinicians to view
#' @param visit_types=NA filter by 'visit_type' if not NA
#'  permissible values are "Home", "Non Visit", "Hospital", "RACF",
#'  "Telephone", "SMS", "Email", "Locum Service", "Out of Office",
#'  "Other", "Hostel", "Telehealth"
#'  if NA, adopts value from active $visit_type
#'
#' @return dataframe of Patient (name), InternalID, VisitDate
list_contact_visits <- function(dMeasure_obj,
                                date_from = NA,
                                date_to = NA,
                                clinicians = NA,
                                visit_type = NA) {
  dMeasure_obj$list_contact_visits(date_from, date_to, clinicians, visit_types)
}

.public("list_contact_visits", function(date_from = NA,
                                        date_to = NA,
                                        clinicians = NA,
                                        visit_type = NA) {

  if (is.na(date_from)) {
    date_from <- self$date_a
  }
  if (is.na(date_to)) {
    date_to <- self$date_b
  }
  if (length(clinicians) == 1 && is.na(clinicians)) {
    # sometimes clinicians is a list, in which case it cannot be a single NA!
    # 'if' is not vectorized so will only read the first element of the list
    # but if clinicians is a single NA, then read $clinicians
    clinicians <- self$clinicians
  }
  if (is.na(visit_type)) {
    visit_type <- self$visit_type
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {log_id <- private$config_db$write_log_db(
      query = "contact_visits",
      data = list(date_from, date_to, clinicians))}

    self$contact_visits_list <- private$db$visits %>>%
      dplyr::filter(VisitDate >= date_from & VisitDate <= date_to) %>>%
      dplyr::filter(DrName %in% clinicians) %>>% # not just doctors!
      dplyr::filter(VisitType %in% visit_type) %>>%
      dplyr::group_by(InternalID, VisitDate) %>>%
      dplyr::summarise() %>>% # plucks out unique visit dates
      dplyr::ungroup() %>>%
      dplyr::left_join(private$db$patients, by = 'InternalID', copy = TRUE) %>>%
      dplyr::select(Firstname, Surname, InternalID, VisitDate) %>>%
      dplyr::collect() %>>%
      dplyr::mutate(Patient = paste(trimws(Firstname), trimws(Surname)),
                    VisitDate = as.Date(VisitDate)) %>>%
      dplyr::select(Patient, InternalID, VisitDate)

    if (self$Log)
    {private$config_db$duration_log_db(log_id)}
  }

  return(self$contact_visits_list)
})
.reactive_event("contact_visits_listR",
                quote(
                  shiny::eventReactive(
                    c(self$date_aR(), self$date_bR(),
                      self$cliniciansR(), self$visit_typeR()), {
                        # update if reactive version of $date_a Rdate_b
                        # or $clinicians are updated.
                        self$list_contact_visits()
                        # re-calculates the appointments
                      })
                ))




#' List of contacts counts
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from=dMeasure_obj$date_a start date
#' @param date_to=dMeasure_obj$date_b end date (inclusive)
#' @param clinicians=dMeasure_obj$clinicians list of clinicians to view
#' @param status=NA filter by 'status' if not NA
#'  permissible values are 'Booked', 'Completed', 'At billing',
#'  'Waiting', 'With doctor'
#'
#' @return dataframe of Patient (name), InternalID, Counts
list_contact_count <- function(dMeasure_obj,
                               date_from = NA,
                               date_to = NA,
                               clinicians = NA,
                               status = NA,
                               lazy = FALSE) {
  dMeasure_obj$list_contact_count(date_from, date_to, clinicians, status, lazy)
}

.public("list_contact_count", function(date_from = NA,
                                       date_to = NA,
                                       clinicians = NA,
                                       status = NA,
                                       lazy = FALSE) {

  if (is.na(date_from)) {
    date_from <- self$date_a
  }
  if (is.na(date_to)) {
    date_to <- self$date_b
  }
  if (length(clinicians) == 1 && is.na(clinicians)) {
    # sometimes clinicians is a list, in which case it cannot be a single NA!
    # 'if' is not vectorized so will only read the first element of the list
    # but if clinicians is a single NA, then read $clinicians
    clinicians <- self$clinicians
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {log_id <- private$config_db$write_log_db(
      query = "contact_count",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_contact_appointments(date_from, date_to,
                                     clinicians, status = status,
                                     lazy = FALSE)
      # if not 'lazy' evaluation, then re-calculate self$appointments_list
      # (that is automatically done by calling the $list_appointments method)
    }

    self$contact_count_list <- self$contact_appointments_list %>>%
      dplyr::group_by(Patient, InternalID) %>>%
      dplyr::summarise(Count = n()) # plucks out unique appointment dates

    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }
  .set_reactive

  return(self$contact_count_list)
})
.reactive_event("contact_count_listR",
                quote(
                  shiny::eventReactive(
                    c(self$contact_appointments_listR()), {
                      # update if reactive version of $date_a Rdate_b
                      # or $clinicians are updated.
                      self$list_contact_count(lazy = TRUE)
                      # re-calculates the counts
                    })
                ))
