#' Contact list fields and methods
#'
#' @name contact
#' @title dMeasureContact contact lists
#'
#' @include dMeasure.R
#' needs the '.public' function from dMeasure.R
NULL


##### appointment status ################################################################

.active(dMeasure, "appointment_status_types", function(value) {
  if (!missing(value)) {
    warning("'appointment_status_types' cannot be set.")
  }
  return (c("Booked", "Completed", "At billing", "Waiting", "With doctor"))
})

# valid appointment states. note that 'with doctor' applies to any health provider type!
.private_init(dMeasure, ".appointment_status",
              quote(c("With doctor", "At billing", "Completed")))
# by default, all status types are valid
.active(dMeasure, "appointment_status", function(value) {
  if (missing(value)) {
    return(private$.appointment_status)
  }
  if (is.character(value) || is.null(value)) {
    # accepts string, or vector of strings, or NULL
    if (is.null(value)) {value <- ""}
    private$.appointment_status <- value
    private$set_reactive(self$appointment_statusR, private$.appointment_status)
  } else {
    warning(paste0("filter_incoming_Action can only be set to a string,",
                   "a vector of strings or NULL. Valid strings are: '",
                   paste(self$appointment_status_types, collapse = ", "), "'."))
  }
})
.reactive(dMeasure, "appointment_statusR", quote(private$.appointment_status))

##### visit types #################################################################


.active(dMeasure, "visit_types", function(value) {
  if (!missing(value)) {
    warning("'visit_types' cannot be set.")
  }
  return(c("Surgery", "Home", "Non Visit", "Hospital", "RACF", "Telephone",
           "SMS", "Email", "Locum Service", "Out of Office", "Other", "Hostel",
           "Telehealth"))
})

.private_init(dMeasure, ".visit_type", quote(c("Surgery", "Home", "Hospital",
                                               "RACF", "Locum Service",
                                               "Out of Office",
                                               "Hostel", "Telehealth")))
# by default, all visit types are valid
.active(dMeasure, "visit_type", function(value) {
  if (missing(value)) {
    return(private$.visit_type)
  }
  if (is.character(value) || is.null(value)) {
    # accepts string, or vector of strings, or NULL
    if (is.null(value)) {value <- ""}
    private$.visit_type <- value
    private$set_reactive(self$visit_typeR, private$.visit_type)
  } else {
    warning(paste0("visit_type can only be set to a string,",
                   "a vector of strings or NULL. Valid strings are :'",
                   paste(self$visit_types, collapse = ", "), "'."))
  }
})
.reactive(dMeasure, "visit_typeR", quote(private$.visit_type))




###### Fields #############################################################
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
#' @param date_from (default NA -> date_a field) start date
#' @param date_to (default NA -> date_b field) end date (inclusive)
#' @param clinicians (default NA -> clinicians field) list of clinicians to view
#' @param status (default NA) filter by 'status' if not NA
#'  permissible values are 'Booked', 'Completed', 'At billing',
#'  'Waiting', 'With doctor'
#'  if NA, adopts from active $appointment_status
#'
#' @return dataframe of Patient (name), InternalID, AppointmentDate
#' @export
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
            status <- self$appointment_status
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

            self$contact_appointments_list <- self$db$appointments %>>%
              dplyr::filter(AppointmentDate >= date_from & AppointmentDate <= date_to) %>>%
              dplyr::filter(Provider %in% clinicians) %>>%
              dplyr::mutate(Status = trimws(Status)) %>>% # get rid of redundant whitespace
              dplyr::filter(Status %in% status) %>>%
              dplyr::left_join(self$db$patients, by = 'InternalID', copy = TRUE) %>>%
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
                    c(self$date_aR(),
                      self$date_bR(),
                      self$cliniciansR(),
                      self$appointment_statusR()),
                    ignoreNULL = FALSE, {
                      # update if reactive version of $date_a $date_b
                      # or $clinicians are updated.
                      self$list_contact_appointments()
                      # re-calculates the appointments
                    })
                ))

#' List of contacts (visit type)
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from (default NA -> dMeasure_obj$date_a) start date
#' @param date_to (default NA -> dMeasure_obj$date_b) end date (inclusive)
#' @param clinicians (default NA -> dMeasure_obj$clinicians) list of clinicians to view
#' @param visit_types (default NA) filter by 'visit_type' if not NA
#'  permissible values are "Surgery", "Home", "Non Visit", "Hospital", "RACF",
#'  "Telephone", "SMS", "Email", "Locum Service", "Out of Office",
#'  "Other", "Hostel", "Telehealth"
#'  if NA, adopts value from active $visit_type
#'
#' @return dataframe of Patient (name), InternalID, VisitDate
#' @export
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

            self$contact_visits_list <- self$db$visits %>>%
              dplyr::filter(VisitDate >= date_from & VisitDate <= date_to) %>>%
              dplyr::filter(DrName %in% clinicians) %>>% # not just doctors!
              dplyr::filter(VisitType %in% visit_type) %>>%
              dplyr::group_by(InternalID, VisitDate) %>>%
              dplyr::summarise() %>>% # plucks out unique visit dates
              dplyr::ungroup() %>>%
              dplyr::left_join(self$db$patients, by = 'InternalID', copy = TRUE) %>>%
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
                    c(self$date_aR(), self$date_bR(),
                      self$cliniciansR(),
                      self$visit_typeR()),
                    ignoreNULL = FALSE, {
                      # update if reactive version of $date_a $date_b
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
#' @param date_from (default NA -> dMeasure_obj$date_a) start date
#' @param date_to (default NA -> dMeasure_obj$date_b) end date (inclusive)
#' @param clinicians (default NA -> dMeasure_obj$clinicians) list of clinicians to view
#'
#' @return dataframe of Patient (name), InternalID, ServiceDate
#' @export
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

  if ("UserID" %in% colnames(self$UserFullConfig)) {
    clinicians <-
      c(unlist(self$UserFullConfig[self$UserFullConfig$Fullname %in% clinicians,"UserID"],
               use.names = FALSE), -1)} # change to UserID, again minimum length 1
  else {
    clinicians <- c(-1)
    # there might not a "UserID" field in self$UserFullConfig if there is no
    # open EMR database
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {log_id <- private$config_db$write_log_db(
      query = "contact_services",
      data = list(date_from, date_to, clinicians))}

    self$contact_services_list <- self$db$servicesRaw %>>%
      dplyr::filter(ServiceDate >= date_from & ServiceDate <= date_to) %>>%
      dplyr::left_join(self$db$invoices, by = "InvoiceID", copy = TRUE) %>>%
      dplyr::filter(UserID %in% clinicians) %>>% # not just doctors!
      dplyr::group_by(InternalID, ServiceDate) %>>%
      dplyr::summarise() %>>% # plucks out unique service dates
      dplyr::ungroup() %>>%
      dplyr::left_join(self$db$patients, by = 'InternalID', copy = TRUE) %>>%
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
                    c(self$date_aR(), self$date_bR(),
                      self$cliniciansR()), {
                        # update if reactive version of $date_a $date_b
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



.active(dMeasure, "contact_types", function(value) {
  # types of contacts
  #
  # appointments are in the appointment book
  # visits are recorded notes
  # services are billing episodes
  if (!missing(value)) {
    warning("$contact_types is read-only.")
  } else {
    return(c("Appointments", "Visits", "Services"))
  }
})


.private(dMeasure, ".contact_type", c("Services"))
.active(dMeasure, "contact_type", function(value) {
  # types of contacts which are counted
  # vector of strings (can be multiple)
  # acceptable values are 'Appointments', 'Visits' and 'Services'
  #
  # appointments are in the appointment book
  # visits are recorded notes
  # services are billing episodes

  if (missing(value)) {
    return(private$.contact_type)
  }
  value <- intersect(value, self$contact_types)
  # only valid types
  private$.contact_type <- value
  private$set_reactive(self$contact_typeR, value)
})
.reactive(dMeasure, "contact_typeR", quote(private$.contact_type))

#' List of contacts counts
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param contact_type contact types which are accepted. default is $contact_type
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
#' @export
list_contact_count <- function(dMeasure_obj,
                               date_from = NA,
                               date_to = NA,
                               clinicians = NA,
                               min_contact = NA,
                               min_date = NA,
                               contact_type = NA,
                               lazy = FALSE) {
  dMeasure_obj$list_contact_count(date_from, date_to, clinicians,
                                  min_contact, min_date, contact_type,
                                  lazy)
}

.public(dMeasure, "list_contact_count", function(date_from = NA,
                                                 date_to = NA,
                                                 clinicians = NA,
                                                 min_contact = NA,
                                                 min_date = NA,
                                                 contact_type = NA,
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
  if (is.na(min_contact)) {
    min_contact <- self$contact_min
  }
  if (is.na(min_date)) {
    min_date <- self$contact_minDate
  }
  if (is.na(contact_type[[1]])) {
    contact_type <- self$contact_type
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
      if ("Appointments" %in% contact_type) {
        self$list_contact_appointments()
      }
      if ("Visits" %in% contact_type) {
        self$list_contact_visits()
      }
      if ("Services" %in% contact_type) {
        self$list_contact_services()
      }
    }

    self$contact_count_list <- data.frame(Patient = character(),
                                          InternalID = integer(),
                                          Count = integer(),
                                          Latest = as.Date(integer(0),
                                                           origin = "1970-01-01"),
                                          stringsAsFactors = FALSE) %>>%
      {if ("Appointments" %in% contact_type) {
        dplyr::bind_rows(., self$contact_appointments_list)}
        else {.}} %>>%
      {if ("Visits" %in% contact_type) {
        dplyr::bind_rows(.,
                         self$contact_visits_list %>>%
                           dplyr::rename(AppointmentDate = VisitDate))}
        else {.}} %>>%
      {if ("Services" %in% contact_type) {
        dplyr::bind_rows(.,
                         self$contact_services_list %>>%
                           dplyr::rename(AppointmentDate = ServiceDate))}
        else {.}} %>>%
      {if (nrow(.) > 0) {
        dplyr::group_by(., Patient, InternalID) %>>%
          dplyr::summarise(Count = dplyr::n_distinct(AppointmentDate),
                           Latest = max(c(AppointmentDate, -Inf))) %>>%
          # plucks out unique appointment dates
          dplyr::ungroup()}
        else {dplyr::select(., -AppointmentDate)} # removed (just as if nrow>0)
      } %>>%
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
                      self$contact_minDateR(),
                      self$contact_typeR()),
                    ignoreNULL = FALSE, {
                      # update if reactive version of $date_a $date_b
                      # or $clinicians are updated.
                      self$list_contact_count(lazy = TRUE)
                      # re-calculates the counts
                    })
                ))
