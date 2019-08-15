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
.public(dMeasure, "contact_appointments_list",
        data.frame(Patient = character(),
                   InternalID = integer(),
                   AppointmentDate =
                     as.Date(integer(0),
                             origin = "1970-01-01"),
                   stringsAsFactors = FALSE))
.public(dMeasure, "contact_visits_list",
        data.frame(Patient = character(),
                   InternalID = integer(),
                   VisitDate =
                     as.Date(integer(0),
                             origin = "1970-01-01"),
                   stringsAsFactors = FALSE))
.public(dMeasure, "contact_services_list",
        data.frame(Patient = character(),
                   InternalID = integer(),
                   ServiceDate =
                     as.Date(integer(0),
                             origin = "1970-01-01"),
                   stringsAsFactors = FALSE))
# filtered by chosen dates and clinicians
.public(dMeasure, "contact_count_list",
        data.frame(Patient = character(),
                   InternalID = integer(),
                   Count = integer(),
                   Latest = as.Date(integer(0),
                                    origin = "1970-01-01"),
                   stringsAsFactors = FALSE))
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
#'  if NA, adopts from active $dateContact$appointment_status
#'
#' @return dataframe of Patient (name), InternalID, AppointmentDate
list_contact_appointments <- function(dMeasure_obj,
                                      date_from = NA,
                                      date_to = NA,
                                      clinicians = NA,
                                      status = NA) {
  dMeasure_obj$list_contact_appointments(date_from, date_to, clinicians, status)
}

.public(dMeasure, "list_contact_appointments",
        function(date_from = NA,
                 date_to = NA,
                 clinicians = NA,
                 status = NA) {

          if (is.na(date_from)) {
            date_from <- self$dateContact$date_a
          }
          if (is.na(date_to)) {
            date_to <- self$dateContact$date_b
          }
          if (length(clinicians) == 1 && is.na(clinicians)) {
            # sometimes clinicians is a list, in which case it cannot be a single NA!
            # 'if' is not vectorized so will only read the first element of the list
            # but if clinicians is a single NA, then read $clinicians
            clinicians <- self$clinicians
          }
          if (is.na(status)) {
            status <- self$dateContact$appointment_status
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

            self$contact_appointments_list <- private$db$appointments %>>%
              dplyr::filter(AppointmentDate >= date_from & AppointmentDate <= date_to) %>>%
              dplyr::filter(Provider %in% clinicians) %>>%
              dplyr::mutate(Status = trimws(Status)) %>>% # get rid of redundant whitespace
              dplyr::filter(Status %in% status) %>>%
              dplyr::left_join(private$db$patients, by = 'InternalID', copy = TRUE) %>>%
              # need patients database to access date-of-birth
              dplyr::group_by(Patient, InternalID, AppointmentDate) %>>%
              dplyr::summarise() %>>% # plucks out unique appointment dates
              dplyr::ungroup() %>>%
              dplyr::collect() %>>%
              dplyr::mutate(AppointmentDate = as.Date(AppointmentDate))

            if (self$Log) {private$config_db$duration_log_db(log_id)}
          }

          return(self$contact_appointments_list)
        })
.reactive_event(dMeasure, "contact_appointments_listR",
                quote(
                  shiny::eventReactive(
                    c(self$appointments_listR()), {
                      # update if reactive version of $date_a Rdate_b
                      # or $clinicians are updated.
                      self$list_contact_appointments(lazy = TRUE)
                      # re-calculates the appointments
                    })
                ))

#' List of contacts (visit type)
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from=dMeasure_obj$date_a start date
#' @param date_to=dMeasure_obj$date_b end date (inclusive)
#' @param clinicians=dMeasure_obj$clinicians list of clinicians to view
#' @param visit_types=NA filter by 'visit_type' if not NA
#'  permissible values are "Surgery", "Home", "Non Visit", "Hospital", "RACF",
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

.public(dMeasure, "list_contact_visits",
        function(date_from = NA,
                 date_to = NA,
                 clinicians = NA,
                 visit_type = NA) {

          if (is.na(date_from)) {
            date_from <- self$dateContact$date_a
          }
          if (is.na(date_to)) {
            date_to <- self$dateContact$date_b
          }
          if (length(clinicians) == 1 && is.na(clinicians)) {
            # sometimes clinicians is a list, in which case it cannot be a single NA!
            # 'if' is not vectorized so will only read the first element of the list
            # but if clinicians is a single NA, then read $clinicians
            clinicians <- self$clinicians
          }
          if (is.na(visit_type)) {
            visit_type <- self$dateContact$visit_type
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
.reactive_event(dMeasure, "contact_visits_listR",
                quote(
                  shiny::eventReactive(
                    c(self$dateContact$date_aR(), self$dateContact$date_bR(),
                      self$cliniciansR(), self$dateContact$visit_typeR()), {
                        # update if reactive version of $date_a Rdate_b
                        # or $clinicians are updated.
                        self$list_contact_visits()
                        # re-calculates the appointments
                      })
                ))


#' List of contacts (service type)
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from=dMeasure_obj$date_a start date
#' @param date_to=dMeasure_obj$date_b end date (inclusive)
#' @param clinicians=dMeasure_obj$clinicians list of clinicians to view
#'
#' @return dataframe of Patient (name), InternalID, ServiceDate
list_contact_services <- function(dMeasure_obj,
                                  date_from = NA,
                                  date_to = NA,
                                  clinicians = NA) {
  dMeasure_obj$list_contact_services(date_from, date_to, clinicians)
}

.public(dMeasure, "list_contact_services", function(date_from = NA,
                                                    date_to = NA,
                                                    clinicians = NA) {

  if (is.na(date_from)) {
    date_from <- self$dateContact$date_a
  }
  if (is.na(date_to)) {
    date_to <- self$dateContact$date_b
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

  clinicians <-
    c(unlist(self$UserFullConfig[self$UserFullConfig$Fullname %in% clinicians,"UserID"],
             use.names = FALSE), -1) # change to UserID, again minimum length 1

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {log_id <- private$config_db$write_log_db(
      query = "contact_services",
      data = list(date_from, date_to, clinicians))}

    self$contact_services_list <- private$db$servicesRaw %>>%
      dplyr::filter(ServiceDate >= date_from & ServiceDate <= date_to) %>>%
      dplyr::left_join(private$db$invoices, by = "InvoiceID", copy = TRUE) %>>%
      dplyr::filter(UserID %in% clinicians) %>>% # not just doctors!
      dplyr::group_by(InternalID, ServiceDate) %>>%
      dplyr::summarise() %>>% # plucks out unique service dates
      dplyr::ungroup() %>>%
      dplyr::left_join(private$db$patients, by = 'InternalID', copy = TRUE) %>>%
      dplyr::select(Firstname, Surname, InternalID, ServiceDate) %>>%
      dplyr::collect() %>>%
      dplyr::mutate(Patient = paste(trimws(Firstname), trimws(Surname)),
                    ServiceDate = as.Date(ServiceDate)) %>>%
      dplyr::select(Patient, InternalID, ServiceDate)

    if (self$Log)
    {private$config_db$duration_log_db(log_id)}
  }

  return(self$contact_services_list)
})
.reactive_event(dMeasure, "contact_services_listR",
                quote(
                  shiny::eventReactive(
                    c(self$dateContact$date_aR(), self$dateContact$date_bR(),
                      self$cliniciansR()), {
                        # update if reactive version of $date_a Rdate_b
                        # or $clinicians are updated.
                        self$list_contact_services()
                        # re-calculates the appointments
                      })
                ))


.private(dMeasure, ".contact_min", 1)
.active(dMeasure, "contact_min", function(value) {
  # minimum number of contacts listed in $list_contact_count
  if (missing(value)) {
    return(private$.contact_min)
  }
  if (value >= 1) {
    private$.contact_min <- value
    private$set_reactive(self$contact_minR, value)
  } else {
    warning("$contact_min only accepts value greater than or equal to one (1).")
  }
})
.reactive(dMeasure, "contact_minR", 1)

.private(dMeasure, ".contact_minDate", as.Date(-Inf,
                                               origin = "1970-01-01"))
.active(dMeasure, "contact_minDate", function(value) {
  # minimum number of contacts listed in $list_contact_count
  if (missing(value)) {
    return(private$.contact_minDate)
  }
  if (inherits(value, "Date")) {
    private$.contact_minDate <- value
    private$set_reactive(self$contact_minDateR, value)
  } else {
    warning("$contact_minDate only accepts as.Date() values.")
  }
})
.reactive(dMeasure, "contact_minDateR", as.Date(-Inf,
                                                origin = "1970-01-01"))

#' List of contacts counts
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $dateContact$date_a
#' @param date_to end date (inclusive). default is $dateContact$date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
list_contact_count <- function(dMeasure_obj,
                               date_from = NA,
                               date_to = NA,
                               clinicians = NA,
                               min_contact = NA,
                               min_date = NA,
                               lazy = FALSE) {
  dMeasure_obj$list_contact_count(date_from, date_to, clinicians, min_contact, min_date,
                                  lazy)
}

.public(dMeasure, "list_contact_count", function(date_from = NA,
                                                 date_to = NA,
                                                 clinicians = NA,
                                                 min_contact = NA,
                                                 min_date = NA,
                                                 lazy = FALSE) {

  if (is.na(date_from)) {
    date_from <- self$dateContact$date_a
  }
  if (is.na(date_to)) {
    date_to <- self$dateContact$date_b
  }
  if (length(clinicians) == 1 && is.na(clinicians)) {
    # sometimes clinicians is a list, in which case it cannot be a single NA!
    # 'if' is not vectorized so will only read the first element of the list
    # but if clinicians is a single NA, then read $clinicians
    clinicians <- self$clinicians
  }
  if (is.na(min_contact)) {
    min_contact <- self$contact_min
  }
  if (is.na(min_date)) {
    min_date <- self$contact_minDate
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
      a$list_contact_appointments()
      a$list_contact_visits()
      a$list_contact_services()
    }

    self$contact_count_list <- self$contact_appointments_list %>>%
      dplyr::bind_rows(
        (self$contact_visits_list %>>%
           dplyr::rename(AppointmentDate = VisitDate)),
        (self$contact_services_list %>>%
           dplyr::rename(AppointmentDate = ServiceDate))
      ) %>>%
      dplyr::group_by(Patient, InternalID) %>>%
      dplyr::summarise(Count = dplyr::n_distinct(AppointmentDate),
                       Latest = max(AppointmentDate)) %>>%
      # plucks out unique appointment dates
      dplyr::ungroup() %>>%
      dplyr::filter(Count >= min_contact) %>>%
      dplyr::filter(Latest >= min_date)

    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$contact_count_list)
})
.reactive_event(dMeasure, "contact_count_listR",
                quote(
                  shiny::eventReactive(
                    c(self$contact_appointments_listR(),
                      self$contact_visits_listR(),
                      self$contact_services_listR(),
                      self$contact_minR(),
                      self$contact_minDateR()), {
                      # update if reactive version of $date_a Rdate_b
                      # or $clinicians are updated.
                      self$list_contact_count(lazy = TRUE)
                      # re-calculates the counts
                    })
                ))
