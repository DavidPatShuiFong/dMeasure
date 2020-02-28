#' Appointment list fields and methods
#'
#' @name appointments
#' @title dMeasureAppointments class appointment lists
#'
#' @include dMeasure.R
#' needs the '.public' function from dMeasure.R
NULL

##### Appointments_filtered #########################################

## Fields
appointments_filtered_empty <-
  data.frame(Patient = character(), InternalID = integer(),
             AppointmentDate = as.Date(integer(0),
                                       origin = "1970-01-01"),
             AppointmentTime = character(),
             Provider = character(), Status = character())

.public(dMeasure, "appointments_filtered", appointments_filtered_empty)
# filtered by chosen dates and clinicians

appointments_filtered_time_empty <-
  data.frame(Patient = character(), InternalID = integer(),
             AppointmentDate = as.Date(integer(0),
                                       origin = "1970-01-01"),
             AppointmentTime = character(),
             Provider = character(), Status = character())
.public(dMeasure, "appointments_filtered_time", appointments_filtered_time_empty)
# times in more R (and visually) friendly format
# requires appointments_filtered

appointments_list_empty <-
  data.frame(Patient = character(), InternalID = integer(),
             AppointmentDate = as.Date(integer(0), origin = "1970-01-01"),
             AppointmentTime = character(0), Provider = character(0),
             Status = character(0),
             DOB = as.Date(integer(0), origin = "1970-01-01"),
             Age = numeric())

.public(dMeasure, "appointments_list", appointments_list_empty)

# add date of birth to appointments list
# requires appointments_filtered_time

visits_list_empty <-
  data.frame(Patient = character(), InternalID = integer(),
             VisitDate = as.Date(integer(0), origin = "1970-01-01"),
             VisitType = character(0),
             Provider = character(0),
             DOB = as.Date(integer(0), origin = "1970-01-01"),
             Age = numeric())
.public(dMeasure, "visits_list", visits_list_empty)

## Methods

#' List of appointments
#'
#' Filtered by date, and chosen clinicians
#' Stores result in $appointments_filtered
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param status filter by 'status' if not NA
#'  permissible values are 'Booked', 'Completed', 'At billing',
#'  'Waiting', 'With doctor'
#'  default is all possible appointment status
#'   $appointment_status_types
#'
#' @return list of appointments
#' @export
filter_appointments <- function(dMeasure_obj,
                                date_from = NA,
                                date_to = NA,
                                clinicians = NA,
                                status = NA) {
  dMeasure_obj$filter_appointments(date_from, date_to, clinicians, status)
}

.public(dMeasure, "filter_appointments",
        function(date_from = NA,
                 date_to = NA,
                 clinicians = NA,
                 status = NA) {

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
          if (is.na(status)) {
            status <- self$appointment_status_types
            # by default, include all appointments status types
            # e.g. Booked, Waiting, Complete
            # this is unlike list_contact_appointments
          }

          # no additional clinician filtering based on privileges or user restrictions

          if (all(is.na(clinicians)) || length(clinicians) == 0) {
            clinicians <- c("") # dplyr::filter does not work on zero-length list()
          }

          if (self$emr_db$is_open()) {
            # only if EMR database is open
            if (self$Log) {log_id <- self$config_db$write_log_db(
              query = "filter_appointments",
              data = list(date_from, date_to, clinicians))}

            self$appointments_filtered <- self$db$appointments %>>%
              dplyr::filter(AppointmentDate >= date_from & AppointmentDate <= date_to) %>>%
              dplyr::filter(Provider %in% clinicians) %>>%
              dplyr::filter(InternalID != 0) %>>% # get rid of 'dummy' appointments
              dplyr::mutate(Status = trimws(Status)) %>>% # get rid of redundant whitespace
              dplyr::filter(Status %in% status)
            # a database filter on an empty list after %in% will
            # result in an error message
            #
            # this reactive is not "collect()"ed because it is joined to other
            # filtered database lists prior to 'collection'
            if (self$Log) {self$config_db$duration_log_db(log_id)}
          } else {
            self$appointments_filtered <- appointments_filtered_empty
          }

          return(self$appointments_filtered)
        })
.reactive_event(dMeasure, "appointments_filteredR",
                quote(
                  shiny::eventReactive(
                    c(self$date_aR(), self$date_bR(),
                      self$cliniciansR(), self$appointment_statusR()), {
                        # update if reactive version of $date_a Rdate_b
                        # or $clinicians are updated.
                        self$filter_appointments()
                        # re-calculates the appointments
                      })
                ))

#' List of appointments with 'human (and R)' readable time-of-day
#'
#' Filtered by date, and chosen clinicians
#' Stores result in $appointments_filtered_time
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from (default $date_a) start date, inclusive (date object)
#' @param date_to (default $date_b) end date, inclusive (date object)
#' @param clinicians (default $clinicians list) of clinicians to view
#' @param lazy (default FALSE) if lazy=TRUE, then don't re-calculate $appointments_filtered to calculate
#' @param status (default NA) filter by 'status' if not NA
#'  permissible values are 'Booked', 'Completed', 'At billing',
#'  'Waiting', 'With doctor'
#'
#' @return list of appointments
#' @export
filter_appointments_time <- function(dMeasure_obj,
                                     date_from = NA, date_to = NA,
                                     clinicians = NA,
                                     status = NA,
                                     lazy = FALSE) {
  dMeasure_obj$filter_appointments_time(date_from, date_to, clinicians, status)
}

.public(dMeasure, "filter_appointments_time",
        function(date_from = NA,
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
            clinicians <- self$clinicians
          }
          # no additional clinician filtering based on privileges or user restrictions

          if (all(is.na(clinicians)) || length(clinicians) == 0) {
            clinicians <- c("")
          }

          if (self$emr_db$is_open()) {
            # only if EMR database is open
            if (!lazy) {
              self$filter_appointments(date_from, date_to, clinicians, status)
              # if not 'lazy' evaluation, then re-calculate self$appointments_filtered
              # (that is automatically done by calling the $filter_appointments method)
            }

            self$appointments_filtered_time <-
              self$appointments_filtered %>>%
              dplyr::collect() %>>% # force read of database required before mutations
              dplyr::mutate(AppointmentTime = dMeasure::hrmin(AppointmentTime),
                            AppointmentDate = as.Date(substr(AppointmentDate,1,10))) %>>%
              dplyr::arrange(AppointmentDate, AppointmentTime)
          } else {
            self$appointments_filtered_time <- appointments_filtered_time_empty
          }

          return(self$appointments_filtered_time)
        })
.reactive_event(dMeasure, "appointments_filtered_timeR",
                quote(
                  shiny::eventReactive(
                    c(self$appointments_filteredR()), {
                      # update if reactive version of $appointments_filtered
                      # is updated
                      self$filter_appointments_time(lazy = TRUE)
                      # re-calculates, but don't need to re-calculate
                      # $appointments_filtered
                    })
                ))

#' List of appointments with date of birth
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from (default $date_a) start date, inclusive (date object)
#' @param date_to (default $date_b) end date, inclusive (date object)
#' @param clinicians (default $clinicians) list of clinicians to view
#' @param status (default NA) filter by 'status' if not NA
#'  permissible values are 'Booked', 'Completed', 'At billing',
#'  'Waiting', 'With doctor'
#' @param lazy (default FALSE) if lazy=TRUE, then don't re-calculate $appointments_filtered to calculate
#'
#'
#' @return list of appointments
#' @export
list_appointments <- function(dMeasure_obj,
                              date_from = NA, date_to = NA,
                              clinicians = NA,
                              status = NA,
                              lazy = FALSE) {
  dMeasure_obj$list_appointments(date_from, date_to, clinicians, status, lazy)
}

.public(dMeasure, "list_appointments",
        function(date_from = NA,
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
          if (all(is.na(clinicians))) {
            clinicians <- self$clinicians
          }
          # no additional clinician filtering based on privileges or user restrictions

          if (all(is.na(clinicians)) || length(clinicians) == 0) {
            clinicians <- c("")
          }

          if (self$emr_db$is_open()) {
            # only if EMR database is open
            if (!lazy) {
              self$filter_appointments_time(date_from, date_to, clinicians, status, lazy = FALSE)
              # if not 'lazy' evaluation, then re-calculate self$appointments_filtered_time
              # (that is automatically done by calling the $filter_appointments_time method)
            }

            intID <- c(self$appointments_filtered_time %>>% dplyr::pull(InternalID), -1)
            # just the internalID, and add a dummy entry in case the list is empty

            self$appointments_list <-
              self$appointments_filtered_time %>>%
              dplyr::left_join(self$db$patients %>>%
                                 dplyr::filter(InternalID %in% intID),
                               by = 'InternalID', copy = TRUE) %>>%
              # need patients database to access date-of-birth
              dplyr::select(c('Patient', 'InternalID', 'AppointmentDate',
                              'AppointmentTime', 'Status', 'Provider', 'DOB')) %>>%
              dplyr::mutate(DOB = as.Date(substr(DOB, 1, 10))) %>>%
              dplyr::mutate(Age = dMeasure::calc_age(DOB, AppointmentDate))

          } else {
            self$appointments_list <- appointments_list_empty
          }
          return(self$appointments_list)
        })
.reactive_event(dMeasure, "appointments_listR",
                quote(
                  shiny::eventReactive(
                    c(self$appointments_filtered_timeR()), {
                      # update if reactive version of $appointments_filtered_time
                      # is updated
                      self$list_appointments(lazy = TRUE)
                      # re-calculates, but don't need to re-calculate
                      # $appointments_filtered_time
                    })
                ))

#' List of visits with date of birth
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from default is $date_a. start date, inclusive (date object)
#' @param date_to default is $date_b end date, inclusive (date object)
#' @param clinicians default is $clinicians. list of clinicians to view
#' @param visit_type  default is $visit_type. filter by 'visittype' if not NA
#' @param lazy if lazy=TRUE, then don't re-calculate $appointments_filtered to calculate
#'
#' @return list of visits
#' @export
list_visits <- function(dMeasure_obj,
                        date_from = NA, date_to = NA,
                        clinicians = NA,
                        visit_type = NA,
                        lazy = FALSE) {
  dMeasure_obj$list_visits(date_from, date_to, clinicians, visit_type, lazy)
}
.public(dMeasure, "list_visits",
        function(date_from = NA,
                 date_to = NA,
                 clinicians = NA,
                 visit_type = NA,
                 lazy = FALSE) {

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
            clinicians <- c("")
          }
          if (is.na(visit_type)) {
            visit_type <- self$visit_type
          }

          if (self$emr_db$is_open()) {
            # only if EMR database is open

            df <- self$db$visits %>>%
              dplyr::filter(VisitDate >= date_from & VisitDate <= date_to) %>>%
              dplyr::filter(DrName %in% clinicians) %>>% # not just doctors!
              dplyr::filter(VisitType %in% visit_type) %>>%
              dplyr::collect() %>>%
              dplyr::group_by(InternalID, VisitDate, DrName) %>>%
              dplyr::summarise(VisitType = paste(VisitType, collapse = ", ")) %>>% # plucks out unique visit dates
              dplyr::ungroup() %>>%
              dplyr::mutate(VisitDate = as.Date(VisitDate))

            intID <- c(dplyr::pull(df, InternalID), -1)

            self$visits_list <- df %>>%
              dplyr::left_join(self$db$patients %>>%
                                 dplyr::filter(InternalID %in% intID) %>>%
                                 dplyr::select(InternalID, Firstname, Surname, DOB),
                               by = 'InternalID',
                               copy = TRUE) %>>%
              # need patients database to access date-of-birth
              dplyr::mutate(Patient = paste(Firstname, Surname)) %>>%
              dplyr::select(Patient, InternalID, VisitDate,
                            VisitType, Provider = DrName, DOB) %>>%
              dplyr::mutate(DOB = as.Date(substr(DOB, 1, 10))) %>>%
              dplyr::mutate(Age = dMeasure::calc_age(DOB, VisitDate))

          } else {
            self$visits_list <- visits_list_empty
          }
          return(self$visits_list)
        })
.reactive_event(dMeasure, "visits_listR",
                quote(
                  shiny::eventReactive(
                    c(c(self$date_aR(), self$date_bR(),
                        self$cliniciansR(), self$visit_typeR())), {
                          self$list_visits()
                        })
                ))
