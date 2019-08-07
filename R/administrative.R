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
        data.frame(InternalID = integer(), ReportID = integer(),
                   TestName = character(),
                   Reported = as.Date(integer(0),
                                      origin = "1970-01-01"),
                   Checked = as.Date(integer(0),
                                     origin = "1970-01-01"),
                   Actioned = as.Date(integer(0),
                                      origin = "1970-01-01"),
                   CheckedBy = character(),
                   Notation = character(), Comment = character(),
                   Action = character()))
# filtered by chosen dates and clinicians, action and notification

.public("investigations_filtered_named",
        data.frame(Patient = character(),
                   InternalID = integer(),
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
                   Notation = character(), Comment = character(),
                   Action = character()))


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
      dplyr::filter(CheckedBy %in% clinicians)
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
  if (is.na(Action)) {
    Action <- self$filter_incoming_Action
  }
  if (is.na(Actioned)) {
    Actioned <- self$filter_incoming_Actioned
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open

    if (!lazy) {
      self$filter_investigations(date_from, date_to, clinicians,
                                 Action, Actioned)
      # if not 'lazy' evaluation, then re-calculate self$investigations_filtered
      # (that is automatically done by calling the $filter_investigations)
    }

    self$investigations_filtered_named <- self$investigations_filtered %>>%
      dplyr::left_join(private$db$patients, by = 'InternalID') %>>%
      # need patients database to access date-of-birth
      dplyr::select(Title, Firstname, Middlename, Surname,
                    DOB, InternalID, RecordNo, TestName,
                    Reported, Checked, CheckedBy,
                    Notation, Action, Actioned, Comment) %>>%
      dplyr::mutate(Title = trimws(Title), Firstname = trimws(Firstname),
                    Middlename = trimws(Middlename), Surname = trimws(Surname),
                    TestName = trimws(TestName)) %>>%
      dplyr::collect() %>>%
      dplyr::mutate(Patient = paste(Title, Firstname,
                                    Middlename), Surname) %>>% # combined name
      dplyr::select(-c(Title, Firstname, Middlename, Surname)) %>>% # drop name parts
      dplyr::mutate(Reported = as.Date(Reported), Checked = as.Date(Checked),
                    Actioned = as.Date(Actioned)) %>>%
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
                    c(self$date_aR(), self$date_bR(), self$cliniciansR(),
                      self$filter_incoming_ActionR(),
                      self$filter_incoming_ActionedR()), {
                        # update if reactive version of $date_a Rdate_b
                        # or $clinicians are updated.
                        self$filter_investigations_named(lazy = TRUE)
                        # re-calculates the appointments
                      })
                ))
