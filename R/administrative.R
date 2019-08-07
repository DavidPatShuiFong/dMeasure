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

#' List of investigations
#'
#' Filtered by date, and chosen clinicians
#' Stores result in $investigations_filtered
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from=dMeasure_obj$date_a start date
#' @param date_to=dMeasure_obj$date_b end date (inclusive)
#' @param clinicians=dMeasure_obj$clinicians list of clinicians to view
#' @param Action=NULL Filter by action?
#'  a vector of actions (in string form)
#'  e.g. "Urgent Appointment" and/or "Non-urgent Appointment" or "No action"
#' @param Actioned=NULL Filter by having been 'actioned?' i.e. notified
#'  can be logical (TRUE or FALSE)
#'  or a Date (actioned prior to or by 'Actioned' Date)
#'
#' @return list of investigations
filter_investigations <- function(dMeasure_obj,
                                date_from = NA,
                                date_to = NA,
                                clinicians = NA,
                                Action = NULL,
                                Actioned = NULL) {
  dMeasure_obj$filter_investigations(date_from, date_to, clinicians,
                                   Action, Actioned)
}
.public("filter_investigations", function(date_from = NA,
                                          date_to = NA,
                                          clinicians = NA,
                                          Action = NULL,
                                          Actioned = NULL) {

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
                    c(self$date_aR(), self$date_bR(), self$cliniciansR()), {
                      # update if reactive version of $date_a Rdate_b
                      # or $clinicians are updated.
                      self$filter_investigations()
                      # re-calculates the appointments
                    })
                ))
