#' Administrative list fields and methods
#'
#' @name administrative
#' @title dMeasureAppointments class administrative lists
#'
#' @include dMeasure.R
#' needs the '.public' function from dMeasure.R
NULL

## Fields
.public("investigations_filtered",
        data.frame(InternalID = integer(),
                   ReportID = integer(),
                   TestName = character(),
                   Reported = as.Date(integer(0),
                                      origin = "1970-01-01"),
                   Checked = as.Date(integer(0),
                                     origin = "1970-01-01"),
                   Actioned = as.Date(integer(0),
                                      origin = "1970-01-01"),
                   CheckedBy = character(),
                   Notation = character(),
                   Comment = character(),
                   Action = character(),
                   stringsAsFactors = FALSE))
# filtered by chosen dates and clinicians, action and notification

.public("correspondence_filtered",
        data.frame(InternalID = integer(),
                   DocumentID = integer(),
                   Category = character(),
                   Subject = character(),
                   Detail = character(),
                   CorrespondenceDate = as.Date(integer(0),
                                                origin = "1970-01-01"),
                   CheckDate = as.Date(integer(0),
                                       origin = "1970-01-01"),
                   ActionDate = as.Date(integer(0),
                                        origin = "1970-01-01"),
                   CHECKEDBY = numeric(), # this will be a number! (UserID)
                   NOTATION = numeric(), # this will be a number!
                   Comment = character(),
                   ACTION = numeric(),
                   stringsAsFactors = FALSE)) # this will be a number!

# filtered by chosen dates and clinicians, action and notification

.public("investigations_filtered_appointment",
        data.frame(InternalID = integer(), ReportID = integer(),
                   TestName = character(),
                   Reported = as.Date(integer(0),
                                      origin = "1970-01-01"),
                   Checked = as.Date(integer(0),
                                     origin = "1970-01-01"),
                   Actioned = as.Date(integer(0),
                                      origin = "1970-01-01"),
                   CheckedBy = character(),
                   Notation = character(),
                   Comment = character(),
                   Action = character(),
                   Patient = character(),
                   AppointmentDate =  as.Date(integer(0),
                                              origin = "1970-01-01"),
                   AppointmentTime = character(),
                   Provider = character(),
                   Status = character(),
                   stringsAsFactors = FALSE))

.public("correspondence_filtered_appointment",
        data.frame(InternalID = integer(),
                   DocumentID = integer(),
                   Category = character(),
                   Subject = character(),
                   Detail = character(),
                   CorrespondenceDate = as.Date(integer(0),
                                                origin = "1970-01-01"),
                   CheckDate = as.Date(integer(0),
                                       origin = "1970-01-01"),
                   ActionDate = as.Date(integer(0),
                                        origin = "1970-01-01"),
                   CHECKEDBY = numeric(), # this will be a number! (UserID)
                   NOTATION = numeric(), # this will be a number!
                   Comment = character(),
                   ACTION = numeric(), # this will be a number!
                   Patient = character(),
                   AppointmentDate =  as.Date(integer(0),
                                              origin = "1970-01-01"),
                   AppointmentTime = character(),
                   Provider = character(),
                   Status = character(),
                   stringsAsFactors = FALSE))

.public("investigations_filtered_named",
        data.frame(Patient = character(),
                   InternalID = integer(),
                   ReportID = numeric(),
                   RecordNo = character(),
                   DOB = as.Date(integer(0),
                                 origin = "1970-01-01"),
                   Age = numeric(),
                   TestName = character(),
                   Reported = as.Date(integer(0),
                                      origin = "1970-01-01"),
                   Checked = as.Date(integer(0),
                                     origin = "1970-01-01"),
                   Actioned = as.Date(integer(0),
                                      origin = "1970-01-01"),
                   CheckedBy = character(),
                   Notation = character(),
                   Comment = character(),
                   Action = character(),
                   AppointmentDate =  as.Date(integer(0),
                                              origin = "1970-01-01"),
                   AppointmentTime = character(),
                   Provider = character(),
                   Status = character(),
                   stringsAsFactors = FALSE))

.public("correspondence_filtered_named",
        data.frame(Patient = character(),
                   InternalID = integer(),
                   DocumentID = numeric(),
                   RecordNo = character(),
                   DOB = as.Date(integer(0),
                                 origin = "1970-01-01"),
                   Age = numeric(),
                   DocumentName = character(),
                   CorrespondenceDate = as.Date(integer(0),
                                                origin = "1970-01-01"),
                   CheckDate = as.Date(integer(0),
                                       origin = "1970-01-01"),
                   ActionDate = as.Date(integer(0),
                                        origin = "1970-01-01"),
                   CheckedBy = character(),
                   Notation = character(),
                   Comment = character(),
                   Action = character(),
                   AppointmentDate =  as.Date(integer(0),
                                              origin = "1970-01-01"),
                   AppointmentTime = character(),
                   Provider = character(),
                   Status = character(),
                   stringsAsFactors = FALSE))

.private(".filter_incoming_Action", NULL)
# the default value of 'Action' for method $filter_investigations
# can be NULL, or a vector of strings
.private(".filter_incoming_Actioned", NULL)
# the default value of 'Actioned' for method $filter_investigations
# can be NULL, a logical (TRUE/FALSE), or a Date

.active("filter_incoming_Action", function(value) {
  if (missing(value)) {
    return(private$.filter_incoming_Action)
  }
  if (is.null(value) || is.character(value)) {
    # accepts NULL value
    # accepts string, or vector of strings
    private$.filter_incoming_Action <- value
    private$set_reactive(self$filter_incoming_ActionR, value)
  } else {
    warning(paste("filter_incoming_Action can only be set to a string,",
                  "a vector of strings or NULL"))
  }
})
.reactive("filter_incoming_ActionR", quote(NULL))

.active("filter_incoming_Actioned", function(value) {
  if (missing(value)) {
    return(private$.filter_incoming_Actioned)
  }
  if (is.null(value) || is.logical(value) || inherits(value, "Date")) {
    # accepts NULL
    # accepts TRUE/FALSE
    # accepts Date type
    private$.filter_incoming_Actioned <- value
    private$set_reactive(self$filter_incoming_ActionedR, value)
  } else {
    warning(paste("filter_incoming_Actioned can only be a Date,",
                  "a logical (TRUE/FALSE) or NULL."))
  }
})
.reactive("filter_incoming_ActionedR", quote(NULL))

#' List of investigations
#'
#' Filtered by date, and chosen clinicians
#' Stores result in $investigations_filtered
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from=dMeasure_obj$date_a start date
#' @param date_to=dMeasure_obj$date_b end date (inclusive)
#' @param clinicians=dMeasure_obj$clinicians list of clinicians to view
#' @param Action=NA Filter by action?
#'  a vector of actions (in string form) or NULL
#'  e.g. "Urgent Appointment" and/or "Non-urgent Appointment" or "No action"
#'  if NA, will adopt the value of $filter_incoming_Action
#' @param Actioned=NA Filter by having been 'actioned?' i.e. notified
#'  can be logical (TRUE or FALSE), a NULL
#'  or a Date (actioned prior to or by 'Actioned' Date)
#'  if NA, will adopt the value of $filter_incoming_Actioned
#'
#' @return list of investigations
filter_investigations <- function(dMeasure_obj,
                                  date_from = NA,
                                  date_to = NA,
                                  clinicians = NA,
                                  Action = NA,
                                  Actioned = NA) {
  dMeasure_obj$filter_investigations(date_from, date_to, clinicians,
                                     Action, Actioned)
}
.public("filter_investigations", function(date_from = NA,
                                          date_to = NA,
                                          clinicians = NA,
                                          Action = NA,
                                          Actioned = NA) {

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
  if (!is.null(Action) && is.na(Action)) {
    # note that 'NULL' is a valid value,  but will ERROR the is.na() test
    Action <- self$filter_incoming_Action
  }
  if (!is.null(Actioned) && is.na(Actioned)) {
    Actioned <- self$filter_incoming_Actioned
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {log_id <- private$config_db$write_log_db(
      query = "filter_investigations",
      data = list(date_from, date_to, clinicians))}

    investigations <- private$db$investigations %>>%
      dplyr::filter(Checked >= date_from & Checked <= date_to) %>>%
      dplyr::filter(CheckedBy %in% clinicians) %>>%
      dplyr::mutate(TestName = trimws(TestName))
    # a database filter on an empty list after %in% will result in an error message

    if (!is.null(Action)) {
      ActionValue <- Action
      investigations <- investigations %>>%
        dplyr::filter(Action %in% ActionValue)
    }

    if (!is.null(Actioned)) {
      if (is.logical(Actioned)) {
        if (Actioned == TRUE) {
          investigations <-
            dplyr::filter(investigations, !is.na(Actioned)) # has been actioned
        }
        if (Actioned == FALSE) {
          investigations <-
            dplyr::filter(investigations, is.na(Actioned)) # has not been actioned
        }
      }
      if (inherits(Actioned, "Date")) {
        # a date type
        ComparisonDate <- Actioned
        investigations <-
          dplyr::filter(investigations, Actioned <= ComparisonDate)
      }
    }
    self$investigations_filtered <- investigations
    # this reactive is not "collect()"ed because it is joined to other
    # filtered database lists prior to 'collection'
    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(investigations)
})
.reactive_event("investigations_filteredR",
                quote(
                  shiny::eventReactive(
                    c(self$date_aR(), self$date_bR(), self$cliniciansR(),
                      self$filter_incoming_ActionR(),
                      self$filter_incoming_ActionedR()), {
                        # update if reactive version of $date_a Rdate_b
                        # or $clinicians are updated.
                        self$filter_investigations()
                        # re-calculates the appointments
                      })
                ))

#' List of investigations with appointments
#'
#' Filtered by date, and chosen clinicians
#' added information about appointments scheduled after the 'Checked' date
#' Stores result in $investigations_filtered_appointments
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from=dMeasure_obj$date_a start date
#' @param date_to=dMeasure_obj$date_b end date (inclusive)
#' @param clinicians=dMeasure_obj$clinicians list of clinicians to view
#' @param Action=NA Filter by action?
#'  a vector of actions (in string form) or NULL
#'  e.g. "Urgent Appointment" and/or "Non-urgent Appointment" or "No action"
#'  if NA, will adopt the value of $filter_incoming_Action
#' @param Actioned=NA Filter by having been 'actioned?' i.e. notified
#'  can be logical (TRUE or FALSE), a NULL
#'  or a Date (actioned prior to or by 'Actioned' Date)
#'  if NA, will adopt the value of $filter_incoming_Actioned
#' @param lazy=FALSE if TRUE, don't 'call' filter_investigations
#'  to re-calculate
#'
#' @return list of investigations with relevant appointments
filter_investigations_appointment <- function(dMeasure_obj,
                                              date_from = NA,
                                              date_to = NA,
                                              clinicians = NA,
                                              Action = NA,
                                              Actioned = NA,
                                              lazy = FALSE) {
  dMeasure_obj$filter_investigations_appointment(date_from, date_to, clinicians,
                                                 Action, Actioned)
}
.public("filter_investigations_appointment", function(date_from = NA,
                                                      date_to = NA,
                                                      clinicians = NA,
                                                      Action = NA,
                                                      Actioned = NA,
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
  if (!is.null(Action) && is.na(Action)) {
    # note that 'NULL' is a valid value,  but will ERROR the is.na() test
    Action <- self$filter_incoming_Action
  }
  if (!is.null(Actioned) && is.na(Actioned)) {
    Actioned <- self$filter_incoming_Actioned
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {log_id <- private$config_db$write_log_db(
      query = "filter_incoming_appointment",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$filter_investigations(date_from, date_to, clinicians,
                                 Action, Actioned)
      # if not 'lazy' evaluation, then re-calculate self$investigations_filtered
      # (that is automatically done by calling the $filter_investigations)
    }
    self$investigations_filtered_appointment <- self$investigations_filtered %>>%
      dplyr::left_join(private$db$appointments %>>%
                         # only check against appointments after date_from
                         dplyr::filter(AppointmentDate > date_from),
                       by = "InternalID", copy = TRUE) %>>%
      dplyr::filter(AppointmentDate > Checked) %>>%
      dplyr::mutate(Status = trimws(Status))

    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$investigations_filtered_appointment)
})
.reactive_event("investigations_filtered_appointmentR",
                quote(
                  shiny::eventReactive(
                    self$investigations_filteredR(), {
                      # update if reactive version of $date_a Rdate_b
                      # or $clinicians are updated.
                      self$filter_investigations_appointment()
                      # re-calculates the appointments
                    })
                ))


#' List of investigations, with patient names
#'
#' Filtered by date, and chosen clinicians
#' Stores result in $investigations_filtered_named
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from=dMeasure_obj$date_a start date
#' @param date_to=dMeasure_obj$date_b end date (inclusive)
#' @param clinicians=dMeasure_obj$clinicians list of clinicians to view
#' @param Action=NA Filter by action?
#'  a vector of actions (in string form) or NULL
#'  e.g. "Urgent Appointment" and/or "Non-urgent Appointment" or "No action"
#'  if NA, will adopt the value of $filter_incoming_Action
#' @param Actioned=NA Filter by having been 'actioned?' i.e. notified
#'  can be logical (TRUE or FALSE), a NULL
#'  or a Date (actioned prior to or by 'Actioned' Date)
#'  if NA, will adopt the value of $filter_incoming_Actioned
#'
#' @return list of investigations
filter_investigations_named <- function(dMeasure_obj,
                                        date_from = NA,
                                        date_to = NA,
                                        clinicians = NA,
                                        Action = NA,
                                        Actioned = NA,
                                        lazy = FALSE) {
  dMeasure_obj$filter_investigations_named(date_from, date_to, clinicians,
                                           Action, Actioned, lazy)
}
.public("filter_investigations_named", function(date_from = NA,
                                                date_to = NA,
                                                clinicians = NA,
                                                Action = NA,
                                                Actioned = NA,
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
  if (!is.null(Action) && is.na(Action)) {
    Action <- self$filter_incoming_Action
  }
  if (!is.null(Actioned) && is.na(Actioned)) {
    Actioned <- self$filter_incoming_Actioned
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open

    if (!lazy) {
      self$filter_investigations_appointment(date_from, date_to, clinicians,
                                             Action, Actioned, lazy)
      # if not 'lazy' evaluation, then re-calculate $investigations_filtered_appointment
      # (that is automatically done by calling the $filter_investigations_appointment)
    }

    self$investigations_filtered_named <- self$investigations_filtered_appointment %>>%
      dplyr::left_join(private$db$patients, by = 'InternalID', copy = TRUE) %>>%
      # need patients database to access date-of-birth
      dplyr::select(Patient,
                    DOB, InternalID, RecordNo, TestName, ReportID,
                    Reported, Checked, CheckedBy,
                    Notation, Action, Actioned, Comment,
                    AppointmentDate, AppointmentTime, Provider, Status) %>>%
      dplyr::collect() %>>%
      dplyr::mutate(RecordNo = trimws(RecordNo),
                    Reported = as.Date(Reported), Checked = as.Date(Checked),
                    Actioned = as.Date(Actioned),
                    AppointmentDate = as.Date(AppointmentDate),
                    AppointmentTime = self$hrmin(AppointmentTime)) %>>%
      dplyr::mutate(DOB = as.Date(substr(DOB, 1, 10))) %>>%
      dplyr::mutate(Age = self$calc_age(DOB, # try several different dates for 'age'
                                        dplyr::case_when(!is.na(Reported) ~ Reported,
                                                         !is.na(Checked) ~ Checked,
                                                         TRUE ~ Sys.Date())))

  }

  return(self$investigations_filtered_named)
})
.reactive_event("investigations_filtered_namedR",
                quote(
                  shiny::eventReactive(
                    c(self$investigations_filtered_appointmentR()), {
                      # update if reactive version of $date_a Rdate_b
                      # or $clinicians are updated.
                      self$filter_investigations_named(lazy = TRUE)
                      # re-calculates the appointments
                    })
                ))


#' List of correspondence
#'
#' Filtered by date, and chosen clinicians
#' Stores result in $correspondence_filtered
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from=dMeasure_obj$date_a start date
#' @param date_to=dMeasure_obj$date_b end date (inclusive)
#' @param clinicians=dMeasure_obj$clinicians list of clinicians to view
#' @param Action=NA Filter by action?
#'  a vector of actions (in string form) or NULL
#'  e.g. "Urgent Appointment" and/or "Non-urgent Appointment" or "No action"
#'  if NA, will adopt the value of $filter_incoming_Action
#' @param Actioned=NA Filter by having been 'actioned?' i.e. notified
#'  can be logical (TRUE or FALSE), a NULL
#'  or a Date (actioned prior to or by 'Actioned' Date)
#'  if NA, will adopt the value of $filter_incoming_Actioned
#'
#' @return list of correspondence
filter_correspondence <- function(dMeasure_obj,
                                  date_from = NA,
                                  date_to = NA,
                                  clinicians = NA,
                                  Action = NA,
                                  Actioned = NA) {
  dMeasure_obj$filter_correspondence(date_from, date_to, clinicians,
                                     Action, Actioned)
}
.public("filter_correspondence", function(date_from = NA,
                                          date_to = NA,
                                          clinicians = NA,
                                          Action = NA,
                                          Actioned = NA) {

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
  if (!is.null(Action) && is.na(Action)) {
    # note that 'NULL' is a valid value,  but will ERROR the is.na() test
    Action <- self$filter_incoming_Action
  }
  if (!is.null(Actioned) && is.na(Actioned)) {
    Actioned <- self$filter_incoming_Actioned
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  clinician_n <- c(unlist(self$UserFullConfig[self$UserFullConfig$Fullname %in% clinicians,
                                              "UserID"], use.names = FALSE),0)
  # again, need to add zero to list because cannot search on empty list

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {log_id <- private$config_db$write_log_db(
      query = "filter_correspondence",
      data = list(date_from, date_to, clinicians))}

    correspondence <- private$db$correspondenceInRaw %>>%
      dplyr::filter(CHECKDATE >= date_from & CHECKDATE <= date_to) %>>%
      dplyr::filter(CHECKEDBY %in% clinician_n) %>>%
      dplyr::select(INTERNALID, DOCUMENTID,
                    CATEGORY, SUBJECT, DETAIL,
                    CORRESPONDENCEDATE, CHECKDATE, ACTIONDATE,
                    CHECKEDBY, NOTATION, COMMENT, ACTION) %>>%
      dplyr::rename(InternalID = INTERNALID, DocumentID = DOCUMENTID,
                    Category = CATEGORY, Subject = SUBJECT, Detail = DETAIL,
                    CorrespondenceDate = CORRESPONDENCEDATE,
                    CheckDate = CHECKDATE, ActionDate = ACTIONDATE,
                    Comment = COMMENT) %>>%
      dplyr::mutate(Category = trimws(Category),
                    Subject = trimws(Subject),
                    Detail = trimws(Detail))
    # note leaves CHECKEDBY, NOTATION and ACTION in original names
    # as these values are in numeric (UserID etc.) rather than 'named'/string form

    # a database filter on an empty list after %in% will result in an error message

    if (!is.null(Action)) {
      ActionValue <- match(Action, c("No action", "Reception to advise",
                                     "Nurse to advise", "Doctor to advise",
                                     "Send routine reminder",
                                     "Non-urgent appointment", "Urgent appointment"))
      # the most important being values 6 and 7. could return NA if not found
      ActionValue <- ActionValue[!is.na(ActionValue)]
      correspondence <- dplyr::filter(correspondence, ACTION %in% ActionValue)
    }

    if (!is.null(Actioned)) {
      if (is.logical(Actioned)) {
        if (Actioned == TRUE) {
          correspondence <-
            dplyr::filter(correspondence, !is.na(ActionDate)) # has been actioned
        }
        if (Actioned == FALSE) {
          correspondence <-
            dplyr::filter(correspondence, is.na(ActionDate)) # has not been actioned
        }
      }
      if (inherits(Actioned, "Date")) {
        # a date type
        correspondence <-
          dplyr::filter(correspondence, ActionDate <= Actioned)
      }
    }
    self$correspondence_filtered <- correspondence
    # this reactive is not "collect()"ed because it is joined to other
    # filtered database lists prior to 'collection'
    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(correspondence)
})
.reactive_event("correspondence_filteredR",
                quote(
                  shiny::eventReactive(
                    c(self$date_aR(), self$date_bR(), self$cliniciansR(),
                      self$filter_incoming_ActionR(),
                      self$filter_incoming_ActionedR()), {
                        # update if reactive version of $date_a Rdate_b
                        # or $clinicians are updated.
                        self$filter_correspondence()
                        # re-calculates the appointments
                      })
                ))

#' List of correspondence with appointments
#'
#' Filtered by date, and chosen clinicians
#' added information about appointments scheduled after the 'Checked' date
#' Stores result in $correspondence_filtered_appointments
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from=dMeasure_obj$date_a start date
#' @param date_to=dMeasure_obj$date_b end date (inclusive)
#' @param clinicians=dMeasure_obj$clinicians list of clinicians to view
#' @param Action=NA Filter by action?
#'  a vector of actions (in string form) or NULL
#'  e.g. "Urgent Appointment" and/or "Non-urgent Appointment" or "No action"
#'  if NA, will adopt the value of $filter_incoming_Action
#' @param Actioned=NA Filter by having been 'actioned?' i.e. notified
#'  can be logical (TRUE or FALSE), a NULL
#'  or a Date (actioned prior to or by 'Actioned' Date)
#'  if NA, will adopt the value of $filter_incoming_Actioned
#' @param lazy=FALSE if TRUE, don't 'call' filter_correspondence
#'  to re-calculate
#'
#' @return list of correspondence with relevant appointments
filter_correspondence_appointment <- function(dMeasure_obj,
                                              date_from = NA,
                                              date_to = NA,
                                              clinicians = NA,
                                              Action = NA,
                                              Actioned = NA,
                                              lazy = FALSE) {
  dMeasure_obj$filter_correspondence_appointment(date_from, date_to, clinicians,
                                                 Action, Actioned)
}
.public("filter_correspondence_appointment", function(date_from = NA,
                                                      date_to = NA,
                                                      clinicians = NA,
                                                      Action = NA,
                                                      Actioned = NA,
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
  if (!is.null(Action) && is.na(Action)) {
    # note that 'NULL' is a valid value,  but will ERROR the is.na() test
    Action <- self$filter_incoming_Action
  }
  if (!is.null(Actioned) && is.na(Actioned)) {
    Actioned <- self$filter_incoming_Actioned
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {log_id <- private$config_db$write_log_db(
      query = "filter_correspondence_appointment",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$filter_correspondence(date_from, date_to, clinicians,
                                 Action, Actioned)
      # if not 'lazy' evaluation, then re-calculate self$correspondence_filtered
      # (that is automatically done by calling the $filter_correspondence)
    }

    self$correspondence_filtered_appointment <- self$correspondence_filtered %>>%
      dplyr::left_join(private$db$appointments %>>%
                         # only check against appointments after date_from
                         dplyr::filter(AppointmentDate > date_from),
                       by = "InternalID", copy = TRUE) %>>%
      dplyr::filter(AppointmentDate > CheckDate) %>>%
      dplyr::mutate(Status = trimws(Status))
    # further filter against the 'checked' date for each correspondence

    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$correspondence_filtered_appointment)
})
.reactive_event("correspondence_filtered_appointmentR",
                quote(
                  shiny::eventReactive(
                    c(self$correspondence_filteredR()), {
                      # update if reactive version of $date_a Rdate_b
                      # or $clinicians are updated.
                      self$filter_correspondence_appointment(lazy = TRUE)
                      # re-calculates the appointments
                    })
                ))

#' List of correspondence, with patient names
#'
#' Filtered by date, and chosen clinicians
#' Stores result in $correspondence_filtered_named
#' Values stored as numeric in $correspondence_filtered_appointments
#' are changed to 'human-readable' character strings e.g. ACTION
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from=dMeasure_obj$date_a start date
#' @param date_to=dMeasure_obj$date_b end date (inclusive)
#' @param clinicians=dMeasure_obj$clinicians list of clinicians to view
#' @param Action=NA Filter by action?
#'  a vector of actions (in string form) or NULL
#'  e.g. "Urgent Appointment" and/or "Non-urgent Appointment" or "No action"
#'  if NA, will adopt the value of $filter_incoming_Action
#' @param Actioned=NA Filter by having been 'actioned?' i.e. notified
#'  can be logical (TRUE or FALSE), a NULL
#'  or a Date (actioned prior to or by 'Actioned' Date)
#'  if NA, will adopt the value of $filter_incoming_Actioned
#'
#' @return list of correspondence
filter_correspondence_named <- function(dMeasure_obj,
                                        date_from = NA,
                                        date_to = NA,
                                        clinicians = NA,
                                        Action = NA,
                                        Actioned = NA,
                                        lazy = FALSE) {
  dMeasure_obj$filter_correspondence_named(date_from, date_to, clinicians,
                                           Action, Actioned, lazy)
}
.public("filter_correspondence_named", function(date_from = NA,
                                                date_to = NA,
                                                clinicians = NA,
                                                Action = NA,
                                                Actioned = NA,
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
  if (!is.null(Action) && is.na(Action)) {
    Action <- self$filter_incoming_Action
  }
  if (!is.null(Actioned) && is.na(Actioned)) {
    Actioned <- self$filter_incoming_Actioned
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open

    if (!lazy) {
      self$filter_correspondence_appointment(date_from, date_to, clinicians,
                                             Action, Actioned, lazy)
      # if not 'lazy' evaluation, then re-calculate $correspondence_filtered_appointment
      # (that is automatically done by calling the $filter_correspondence_appointment)
    }

    n_UserNames <- length(c(self$UserFullConfig$Fullname))
    # needed later for changing CHECKEDBY to a user name

    self$correspondence_filtered_named <- self$correspondence_filtered_appointment %>>%
      dplyr::left_join(private$db$patients, by = 'InternalID', copy = TRUE) %>>%
      # need patients database to access date-of-birth
      dplyr::select(Patient,
                    DOB, InternalID, RecordNo, DocumentID,
                    Category, Subject, Detail,
                    CorrespondenceDate, CheckDate, CHECKEDBY,
                    NOTATION, ACTION, ActionDate, Comment,
                    AppointmentDate, AppointmentTime, Provider, Status) %>>%
      dplyr::collect() %>>%
      dplyr::mutate(RecordNo = trimws(RecordNo),
                    DocumentName = paste(dplyr::if_else(is.na(Category), "", Category),
                                         dplyr::if_else(is.na(Subject), "", Subject),
                                         dplyr::if_else(is.na(Detail), "", Detail),
                                         sep = ":"),
                    CorrespondenceDate = as.Date(CorrespondenceDate),
                    CheckDate = as.Date(CheckDate),
                    ActionDate = as.Date(ActionDate),
                    AppointmentDate = as.Date(AppointmentDate),
                    AppointmentTime = self$hrmin(AppointmentTime)) %>>%
      dplyr::select(-c(Category, Subject, Detail)) %>>%
      dplyr::mutate(DOB = as.Date(substr(DOB, 1, 10)),
                    Age = self$calc_age(DOB, # try several different dates for 'age'
                                        dplyr::case_when(
                                          !is.na(CorrespondenceDate) ~ CorrespondenceDate,
                                          !is.na(CheckDate) ~ CheckDate,
                                          TRUE ~ Sys.Date()))) %>>%
      dplyr::mutate(CheckedBy = c(self$UserFullConfig$Fullname, "")
                    [dplyr::if_else(CHECKEDBY == 0,
                                    as.integer(n_UserNames + 1),
                                    CHECKEDBY)],
                    # this strange incantation is to deal with '0' CHECKEDBY
                    Notation = c("Normal", "Abnormal", "Stable",
                                 "Acceptable", "Unacceptable", "Being treated",
                                 "Under specialist care", "")
                    [dplyr::if_else(NOTATION == 0, as.integer(9), NOTATION)],
                    # this strange if_else is to deal with 'empty' NOTATION
                    Action = c("No action", "Reception to advise",
                               "Nurse to advise", "Doctor to advise",
                               "Send routine reminder", "Non-urgent appointment",
                               "Urgent appointment", "")
                    # this strange if_else is to deal with 'empty' ACTION
                    [dplyr::if_else(ACTION == 0, as.integer(8), ACTION)]) %>>%
      dplyr::select(-c(CHECKEDBY, NOTATION, ACTION))

  }

  return(self$correspondence_filtered_named)
})
.reactive_event("correspondence_filtered_namedR",
                quote(
                  shiny::eventReactive(
                    c(self$correspondence_filtered_appointmentR()), {
                      # update if reactive version of $date_a Rdate_b
                      # or $clinicians are updated.
                      self$filter_correspondence_named(lazy = TRUE)
                      # re-calculates the appointments
                    })
                ))


#' incoming (investigations and correspondence) view
#'
#' filter to date range
#' and clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from=dMeasure_obj$date_a start date
#' @param date_to=dMeasure_obj$date_b end date (inclusive)
#' @param clinicians=dMeasure_obj$clinicians list of clinicians to view
#' @param Action=NA Filter by action?
#'  a vector of actions (in string form) or NULL
#'  e.g. "Urgent Appointment" and/or "Non-urgent Appointment" or "No action"
#'  if NA, will adopt the value of $filter_incoming_Action
#' @param Actioned=NA Filter by having been 'actioned?' i.e. notified
#'  can be logical (TRUE or FALSE), a NULL
#'  or a Date (actioned prior to or by 'Actioned' Date)
#'  if NA, will adopt the value of $filter_incoming_Actioned
#' @param lazy if TRUE, then do not recalculate appointment list. otherwise, re-calculate
#' @param screentag=FALSE optionally add a fomantic/semantic HTML description of 'action'
#' @param screentag_print=TRUE optionally add a 'printable' description of 'action'
#'
#' @return list of appointments (with patient details)
incoming_view <- function(dMeasure_obj, date_from = NA, date_to = NA,
                          clinicians = NA,
                          Action = NA,
                          Actioned = NA,
                          lazy = FALSE,
                          screentag = FALSE, screentag_print = TRUE) {
  dMeasure_obj$view_incoming(date_from, date_to, clinicians,
                             Action, Actioned, lazy, screentag, screentag_print)
}
.public("view_incoming", function (date_from = NA, date_to = NA,
                                   clinicians = NA,
                                   Action = NA,
                                   Actioned = NA,
                                   lazy = FALSE,
                                   screentag = FALSE, screentag_print = TRUE) {

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
    clinicians <- c("") # dplyr::filter cannot handle empty list()
  }

  if (!is.null(Action) && is.na(Action)) {
    # note that 'NULL' is a valid value,  but will ERROR the is.na() test
    Action <- self$filter_incoming_Action
  }
  if (!is.null(Actioned) && is.na(Actioned)) {
    Actioned <- self$filter_incoming_Actioned
  }

  # create empty table
  incoming <- data.frame(Patient = character(),
                         InternalID = numeric(), # will be later dropped
                         ReportID = numeric(), # will be later dropped
                         DocumentID = numeric(), # will be later dropped
                         RecordNo = character(),
                         DOB = as.Date(integer(0),
                                       origin = "1970-01-01"),
                         Age = numeric(),
                         TestName = character(),
                         Reported = as.Date(integer(0),
                                            origin = "1970-01-01"),
                         Checked = as.Date(integer(0),
                                           origin = "1970-01-01"),
                         Actioned = as.Date(integer(0),
                                            origin = "1970-01-01"),
                         CheckedBy = character(),
                         Notation = character(),
                         Comment = character(),
                         Action = character(),
                         AppointmentDate =  as.Date(integer(0),
                                                    origin = "1970-01-01"),
                         AppointmentTime = character(),
                         Provider = character(),
                         Status = character(),
                         PastAppointment = logical(),
                         stringsAsFactors = FALSE)


  if (screentag) {
    incoming <- cbind(incoming, data.frame(labeltag = character()))
  }
  if (screentag_print) {
    incoming <- cbind(incoming, data.frame(labeltag_print = character()))
  }

  if (!private$emr_db$is_open()) {
    # EMR database is not open
    # empty data-frame already created to return
  } else {
    # only if EMR database is open

    if (!lazy) {
      self$filter_investigations_named(date_from, date_to,
                                       clinicians, Action, Actioned,
                                       lazy = FALSE)
      self$filter_correspondence_named(date_from, date_to,
                                       clinicians, Action, Actioned,
                                       lazy = FALSE)
      # if not 'lazy' evaluation, then re-calculate
      # $correspondence_filtered_named and $investigations$filtered_named
      # (that is automatically done by calling the above methods
    }

    incoming <- dplyr::bind_rows(incoming, self$investigations_filtered_named)

    incoming <- dplyr::bind_rows(incoming,
                                 dplyr::rename(self$correspondence_filtered_named,
                                               TestName = DocumentName,
                                               Reported = CorrespondenceDate,
                                               Checked = CheckDate,
                                               Actioned = ActionDate))

    incoming <- incoming %>>%
      dplyr::mutate(PastAppointment = AppointmentDate < Sys.Date()) %>>%
      # the listed appointment has already 'past'
      dplyr::mutate(AppointmentDetail = paste(AppointmentDate, " ", AppointmentTime,
                                              " ", Provider, " (", Status, ")",
                                              sep = ""))
    if (screentag) {
      incoming <- incoming %>>%
        dplyr::mutate(viewtag =
                        semantic_button(AppointmentDate, # semantic/fomantic buttons
                                        colour =
                                          dplyr::if_else(PastAppointment,
                                                         'yellow',
                                                         'green'),
                                        popuphtml =
                                          paste0("<h4>Date : ", AppointmentDate,
                                                 ", ", AppointmentTime,
                                                 "</h4><h6>Provider : ", Provider,
                                                 "</h6><p><font size=\'+0\'>Status : ",
                                                 Status, "</p>")),
                      patienttag =
                        semantic_button(Patient,
                                        colour = 'teal',
                                        popuphtml =
                                          paste0("<p><font size = \'+0\'>DOB : ",
                                                 DOB,"</p>",
                                                 "<p>Age : ", Age, "</p>")),
                      testtag =
                        semantic_button(TestName,
                                        colour = 'purple',
                                        popuphtml =
                                          paste0("<p><font size = \'+0\'>Reported : ",
                                                 Reported, "</p"))) %>>%
        dplyr::group_by(patienttag, InternalID, RecordNo,
                        testtag, ReportID, DocumentID,
                        Checked, Actioned,
                        CheckedBy, Notation, Comment, Action,
                        PastAppointment) %>>%
        # group appointments by the investigation report or Document
        # gathers appointments referring to the same report/correspondence into a single row
        dplyr::summarise(labeltag = paste(viewtag, collapse = "")) %>>%
        dplyr::ungroup()
    }

    if (screentag_print) {
      incoming <- incoming %>>%
        dplyr::mutate(viewtag_print = paste(dplyr::if_else(PastAppointment,
                                                           "Past : ",
                                                           ""),
                                            AppointmentDetail,
                                            sep = "")) %>>%
        dplyr::group_by(Patient, InternalID, RecordNo, DOB, Age,
                        TestName, ReportID, DocumentID,
                        Reported, Checked, Actioned,
                        CheckedBy, Notation, Comment, Action,
                        PastAppointment) %>>%
        # group appointments by the investigation report or Document
        # gathers appointments referring to the same report/correspondence into a single row
        dplyr::summarise(labeltag_print =
                           paste(viewtag_print, collapse = ", ")) %>>%
        dplyr::ungroup()
    }
  }

  incoming <- dplyr::select(incoming,
                            -c(InternalID, ReportID, DocumentID, PastAppointment))

  return(incoming)
})
