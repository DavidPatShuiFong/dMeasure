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
.public("appointments_filtered", data.frame(Patient = character(), InternalID = integer(),
                                            AppointmentDate = as.Date(integer(0),
                                                                      origin = "1970-01-01"),
                                            AppointmentTime = character(),
                                            Provider = character(), Status = character()))
# filtered by chosen dates and clinicians

.public("appointments_filtered_time", data.frame(Patient = character(), InternalID = integer(),
                                                 AppointmentDate = as.Date(integer(0),
                                                                           origin = "1970-01-01"),
                                                 AppointmentTime = character(),
                                                 Provider = character(), Status = character()))
# times in more R (and visually) friendly format
# requires appointments_filtered

.public("appointments_list", data.frame(Patient = character(), InternalID = integer(),
                                        AppointmentDate = as.Date(integer(0), origin = "1970-01-01"),
                                        AppointmentTime = character(), Provider = character(),
                                        DOB = as.Date(integer(0), origin = "1970-01-01"),
                                        Age = numeric())
        )
# add date of birth to appointments list
# requires appointments_filtered_time

.public("appointments_billings", data.frame(Patient = character(), InternalID = integer(),
                                            AppointmentDate = as.Date(integer(0), origin = "1970-01-01"),
                                            AppointmentTime = character(), Provider = character(),
                                            DOB = as.Date(integer(0), origin = "1970-01-01"),
                                            Age = numeric(),
                                            ServiceDate = as.Date(integer(0), origin = "1970-01-01"),
                                            MBSItem = integer(), Description = character())
        )

# appointment list with billings
# collects ALL billings for patients who have displayed appointments
# used by billings view, and CDM billings view
# requires appointments_list

## Methods

#' List of appointments
#'
#' Filtered by date, and chosen clinicians
#' Stores result in $appointments_filtered
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from=dMeasure_obj$date_a start date
#' @param date_to=dMeasure_obj$date_b end date (inclusive)
#' @param clinicians=dMeasure_obj$clinicians list of clinicians to view
#'
#' @return list of appointments
filter_appointments <- function(dMeasure_obj,
                                date_from = NA,
                                date_to = NA,
                                clinicians = NA) {
  dMeasure_obj$filter_appointments(date_from, date_to, clinicians)
}

.public("filter_appointments", function(date_from = NA,
                                        date_to = NA,
                                        clinicians = NA) {

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
      query = "filter_appointments",
      data = list(date_from, date_to, clinicians))}

    self$appointments_filtered <- private$db$appointments %>>%
      dplyr::filter(AppointmentDate >= date_from & AppointmentDate <= date_to) %>>%
      dplyr::filter(Provider %in% clinicians) %>>%
      dplyr::mutate(Status = trimws(Status)) # get rid of redundant whitespace
    # a database filter on an empty list after %in% will result in an error message
    #
    # this reactive is not "collect()"ed because it is joined to other
    # filtered database lists prior to 'collection'
    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$appointments_filtered)
})
.reactive_event("appointments_filteredR",
          quote(
            shiny::eventReactive(
              c(self$date_aR(), self$date_bR(), self$cliniciansR()), {
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
#' @param date_from=dMeasure_obj$date_a start date, inclusive (date object)
#' @param date_to=dMeasure_obj$date_b end date, inclusive (date object)
#' @param clinicians=dMeasure_obj$clinicians list of clinicians to view
#' @param lazy=FALSE if lazy=TRUE, then don't re-calculate $appointments_filtered to calculate
#'
#' @return list of appointments
filter_appointments_time <- function(dMeasure_obj,
                                     date_from = NA, date_to = NA,
                                     clinicians = NA,
                                     lazy = FALSE) {
  dMeasure_obj$filter_appointments_time(date_from, date_to, clinicians)
}

.public("filter_appointments_time", function(date_from = NA,
                                             date_to = NA,
                                             clinicians = NA,
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

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (!lazy) {
      self$filter_appointments(date_from, date_to, clinicians)
      # if not 'lazy' evaluation, then re-calculate self$appointments_filtered
      # (that is automatically done by calling the $filter_appointments method)
    }

    self$appointments_filtered_time <-
      self$appointments_filtered %>>%
      dplyr::collect() %>>% # force read of database required before mutations
      dplyr::mutate(AppointmentTime = self$hrmin(AppointmentTime),
                    AppointmentDate = as.Date(substr(AppointmentDate,1,10))) %>>%
      dplyr::arrange(AppointmentDate, AppointmentTime)
  }

  return(self$appointments_filtered_time)
})
.reactive_event("appointments_filtered_timeR",
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
#' @param date_from=dMeasure_obj$date_a start date, inclusive (date object)
#' @param date_to=dMeasure_obj$date_b end date, inclusive (date object)
#' @param clinicians=dMeasure_obj$clinicians list of clinicians to view
#' @param lazy=FALSE if lazy=TRUE, then don't re-calculate $appointments_filtered to calculate
#'
#' @return list of appointments
list_appointments <- function(dMeasure_obj,
                              date_from = NA, date_to = NA,
                              clinicians = NA,
                              lazy = FALSE) {
  dMeasure_obj$list_appointments(date_from, date_to, clinicians)
}

.public("list_appointments", function(date_from = NA,
                                      date_to = NA,
                                      clinicians = NA,
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

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (!lazy) {
      self$filter_appointments_time(date_from, date_to, clinicians, lazy = FALSE)
      # if not 'lazy' evaluation, then re-calculate self$appointments_filtered_time
      # (that is automatically done by calling the $filter_appointments_time method)
    }

    self$appointments_list <-
      self$appointments_filtered_time %>>%
      dplyr::left_join(private$db$patients, by = 'InternalID', copy = TRUE) %>>%
      # need patients database to access date-of-birth
      dplyr::select(c('Patient', 'InternalID', 'AppointmentDate',
                      'AppointmentTime', 'Provider', 'DOB')) %>>%
      dplyr::mutate(DOB = as.Date(substr(DOB, 1, 10))) %>>%
      dplyr::mutate(Age = self$calc_age(DOB, AppointmentDate))

  }
  return(self$appointments_list)
})
.reactive_event("appointments_listR",
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

#' List of appointments with billings
#'
#' Filtered by date, and chosen clinicians
#' Modified $appointments_billings
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from=dMeasure_obj$date_a start date, inclusive (date object)
#' @param date_to=dMeasure_obj$date_b end date, inclusive (date object)
#' @param clinicians=dMeasure_obj$clinicians list of clinicians to view
#' @param lazy=FALSE if lazy=TRUE, then don't re-calculate $appointments_filtered to calculate
#'
#' @return list of appointments

# Billings for patients who have displayed appointments

# collects ALL billings for patients who have displayed appointments
# used by billings view, and CDM billings view
billed_appointments <- function(dMeasure_obj,
                                date_from = NA, date_to = NA,
                                clinicians = NA,
                                lazy = FALSE) {
  dMeasure_obj$billed_appointments(dMeasure_obj, date_from, date_to, clinicians, lazy)
}

.public("billed_appointments", function(date_from = NA,
                                        date_to = NA,
                                        clinicians = NA,
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

  clinicians <- intersect(clinicians,
                          self$clinician_list(view_name = "billings"))
  # this view is potentially restricted. if 'GlobalBillView' restriction
  # is enabled, reduce the number of clinicians shown, if the authorized
  # user does not have 'GlobalBillView' attribute

  if (is.null(clinicians) || length(clinicians) == 0) {
    clinicians <- c("")
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (!lazy) {
      self$list_appointments(date_from, date_to, clinicians, lazy = FALSE)
      # if not 'lazy' evaluation, then re-calculate self$appointments_list
      # (that is automatically done by calling the $list_appointments method)
    }

    self$appointments_billings <-
      self$appointments_list %>>%
      dplyr::left_join(private$db$services, by = "InternalID", copy=TRUE) %>>%
      dplyr::collect() %>>%
      dplyr::mutate(ServiceDate = as.Date(substr(ServiceDate, 1, 10)))
  }
  return(self$appointments_billings)
})
.reactive_event("appointments_billingsR",
                quote(
                  shiny::eventReactive(
                    c(self$appointments_listR()), {
                      # update if reactive version of $appointments_list
                      # is updated
                      self$billed_appointments(lazy = TRUE)
                      # re-calculates, but don't need to re-calculate
                      # $appointments_list
                    })
                ))
