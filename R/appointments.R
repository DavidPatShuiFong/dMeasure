#' Appointment list fields and methods
#'
#' @title dMeasureAppointments class appointment lists
#'
#' @include dMeasure.R
#' needs the '.public' function from dMeasure.R
NULL

##### Appointments_filtered #########################################

## Fields
.public("appointments_filtered", NULL)
# filtered by chosen dates and clinicians
.public("appointments_filtered_time", NULL)
# times in more R (and visually) friendly format
.public("appointments_list", NULL)
# add date of birth to appointments list
.public("appointments_billings", NULL)
# appointment list with billings

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
                                date_from = NULL,
                                date_to = NULL,
                                clinicians = NULL) {
  dMeasure_obj$filter_appointments(date_from, date_to, clinicians)
}

.public("filter_appointments", function(date_from = NULL,
                                        date_to = NULL,
                                        clinicians = NULL) {

  if (is.null(date_from)) {
    date_from <- self$date_a
  }
  if (is.null(date_to)) {
    date_to <- self$date_b
  }
  if (is.null(clinicians)) {
    clinicians <- self$clinicians
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (is.null(clinicians) || length(clinicians) == 0) {
    stop("Choose at least one clinicians\'s appointment to view")
  } else {
    self$appointments_filtered <- self$db$appointments %>%
      dplyr::filter(AppointmentDate >= date_from & AppointmentDate <= date_to) %>%
      dplyr::filter(Provider %in% clinicians)
    # a database filter on an empty list after %in% will result in an error message
    #
    # this reactive is not "collect()"ed because it is joined to other
    # filtered database lists prior to 'collection'
  }
  return(self$appointments_filtered)
})

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
                                     date_from = NULL, date_to = NULL,
                                     clinicians = NULL,
                                     lazy = FALSE) {
  dMeasure_obj$filter_appointments_time(date_from, date_to, clinicians)
}

.public("filter_appointments_time", function(date_from = NULL,
                                             date_to = NULL,
                                             clinicians = NULL,
                                             lazy = FALSE) {

  if (is.null(date_from)) {
    date_from <- self$date_a
  }
  if (is.null(date_to)) {
    date_to <- self$date_b
  }
  if (is.null(clinicians)) {
    clinicians <- self$clinicians
  }
  # no additional clinician filtering based on privileges or user restrictions

  if (is.null(clinicians) || length(clinicians) == 0) {
    stop("Choose at least one clinicians\'s appointment to view")
  } else {

    if (!lazy) {
      self$filter_appointments(date_from, date_to, clinicians)
      # if not 'lazy' evaluation, then re-calculate self$appointments_filtered
      # (that is automatically done by calling the $filter_appointments method)
    }

    self$appointments_filtered_time <-
      self$appointments_filtered %>%
      dplyr::collect() %>% # force read of database required before mutations
      dplyr::mutate(AppointmentTime = hrmin(AppointmentTime),
                    AppointmentDate = as.Date(substr(AppointmentDate,1,10))) %>%
      dplyr::arrange(AppointmentDate, AppointmentTime)
  }
  return(self$appointments_filtered_time)
})

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
                              date_from = NULL, date_to = NULL,
                              clinicians = NULL,
                              lazy = FALSE) {
  dMeasure_obj$list_appointments(date_from, date_to, clinicians)
}

.public("list_appointments", function(date_from = NULL,
                                      date_to = NULL,
                                      clinicians = NULL,
                                      lazy = FALSE) {

  if (is.null(date_from)) {
    date_from <- self$date_a
  }
  if (is.null(date_to)) {
    date_to <- self$date_b
  }
  if (is.null(clinicians)) {
    clinicians <- self$clinicians
  }
  # no additional clinician filtering based on privileges or user restrictions

  if (is.null(clinicians) || length(clinicians) == 0) {
    stop("Choose at least one clinicians\'s appointment to view")
  } else {

    if (!lazy) {
      self$filter_appointments_time(date_from, date_to, clinicians, lazy = FALSE)
      # if not 'lazy' evaluation, then re-calculate self$appointments_filtered_time
      # (that is automatically done by calling the $filter_appointments_time method)
    }

    self$appointments_list <-
      self$appointments_filtered_time %>%
      dplyr::left_join(self$db$patients, by = 'InternalID', copy = TRUE) %>%
      # need patients database to access date-of-birth
      dplyr::select(c('Patient', 'InternalID', 'AppointmentDate',
                      'AppointmentTime', 'Provider', 'DOB')) %>%
      dplyr::mutate(DOB = as.Date(substr(DOB, 1, 10))) %>%
      dplyr::mutate(Age = calc_age(DOB, AppointmentDate))

  }
  return(self$appointments_list)
})

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
                                date_from = NULL, date_to = NULL,
                                clinicians = NULL,
                                lazy = FALSE) {
  dMeasure_obj$billed_appointments(dMeasure_obj, date_from, date_to, clinicians, lazy)
}

.public("billed_appointments", function(date_from = NULL,
                                        date_to = NULL,
                                        clinicians = NULL,
                                        lazy = FALSE) {
  if (is.null(date_from)) {
    date_from <- self$date_a
  }
  if (is.null(date_to)) {
    date_to <- self$date_b
  }
  if (is.null(clinicians)) {
    clinicians <- self$clinicians
  }

  clinicians <- intersect(clinicians,
                          self$clinician_list(view_name = "billings"))
  # this view is potentially restricted. if 'GlobalBillView' restriction
  # is enabled, reduce the number of clinicians shown, if the authorized
  # user does not have 'GlobalBillView' attribute

  if (is.null(clinicians) || length(clinicians) == 0) {
    stop("Choose at least one clinicians\'s appointment to view")
  } else {

    if (!lazy) {
      self$list_appointments(date_from, date_to, clinicians, lazy = FALSE)
      # if not 'lazy' evaluation, then re-calculate self$appointments_list
      # (that is automatically done by calling the $list_appointments method)
    }

    self$appointments_billings <-
      self$appointments_list %>%
      dplyr::left_join(self$db$services, by = "InternalID", copy=TRUE) %>%
      dplyr::collect() %>%
      dplyr::mutate(ServiceDate = as.Date(substr(ServiceDate, 1, 10)))
  }

  return(self$appointments_billings)
})
