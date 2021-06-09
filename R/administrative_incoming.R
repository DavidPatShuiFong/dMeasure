# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Administrative list fields and methods
#'
#' @name administrative
#' @title dMeasureAppointments class administrative lists
#'
#' @include dMeasure.R
#' needs the '.public' function from dMeasure.R
NULL

## Fields
.public(
  dMeasure, "investigations_filtered",
  data.frame(
    InternalID = integer(),
    ReportID = integer(),
    TestName = character(),
    Reported = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    Checked = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    Actioned = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    CheckedBy = character(),
    Notation = character(),
    Comment = character(),
    Action = character(),
    stringsAsFactors = FALSE
  )
)
# filtered by chosen dates and clinicians, action and notification

.public(
  dMeasure, "correspondence_filtered",
  data.frame(
    InternalID = integer(),
    DocumentID = integer(),
    Category = character(),
    Subject = character(),
    Detail = character(),
    CorrespondenceDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    CheckDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    ActionDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    CheckedBy = numeric(), # this will be a number! (UserID)
    Notation = numeric(), # this will be a number!
    Comment = character(),
    Action = numeric(),
    stringsAsFactors = FALSE
  )
) # this will be a number!

# filtered by chosen dates and clinicians, action and notification

.public(
  dMeasure, "investigations_filtered_appointment",
  data.frame(
    InternalID = integer(), ReportID = integer(),
    TestName = character(),
    Reported = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    Checked = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    Actioned = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    CheckedBy = character(),
    Notation = character(),
    Comment = character(),
    Action = character(),
    Patient = character(),
    AppointmentDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    AppointmentTime = character(),
    Provider = character(),
    Status = character(),
    stringsAsFactors = FALSE
  )
)

.public(
  dMeasure, "correspondence_filtered_appointment",
  data.frame(
    InternalID = integer(),
    DocumentID = integer(),
    Category = character(),
    Subject = character(),
    Detail = character(),
    CorrespondenceDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    CheckDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    ActionDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    CheckedBy = numeric(), # this will be a number! (UserID)
    Notation = numeric(), # this will be a number!
    Comment = character(),
    Action = numeric(), # this will be a number!
    Patient = character(),
    AppointmentDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    AppointmentTime = character(),
    Provider = character(),
    Status = character(),
    stringsAsFactors = FALSE
  )
)

.public(
  dMeasure, "investigations_filtered_named",
  data.frame(
    Patient = character(),
    InternalID = integer(),
    ReportID = numeric(),
    RecordNo = character(),
    DOB = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    Age = numeric(),
    TestName = character(),
    Reported = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    Checked = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    Actioned = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    CheckedBy = character(),
    Notation = character(),
    Comment = character(),
    Action = character(),
    AppointmentDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    AppointmentTime = character(),
    Provider = character(),
    Status = character(),
    stringsAsFactors = FALSE
  )
)

.public(
  dMeasure, "correspondence_filtered_named",
  data.frame(
    Patient = character(),
    InternalID = integer(),
    DocumentID = numeric(),
    RecordNo = character(),
    DOB = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    Age = numeric(),
    DocumentName = character(),
    CorrespondenceDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    CheckDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    ActionDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    CheckedBy = character(),
    Notation = character(),
    Comment = character(),
    Action = character(),
    AppointmentDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    AppointmentTime = character(),
    Provider = character(),
    Status = character(),
    stringsAsFactors = FALSE
  )
)

.private(dMeasure, ".filter_incoming_Action", NULL)
# the default value of 'Action' for method $filter_investigations
# can be NULL, or a vector of strings
.private(dMeasure, ".filter_incoming_Actioned", NULL)
# the default value of 'Actioned' for method $filter_investigations
# can be NULL, a logical (TRUE/FALSE), or a Date
.private(dMeasure, ".filter_incoming_ignorePast", FALSE)
# the default value of 'ignorePast' for method $filter_investigations
# can be a logical (TRUE/FALSE)

.active(dMeasure, "filter_incoming_Action", function(value) {
  if (missing(value)) {
    return(private$.filter_incoming_Action)
  }
  if (is.null(value) || is.character(value)) {
    # accepts NULL value
    # accepts string, or vector of strings
    private$.filter_incoming_Action <- value
    private$set_reactive(self$filter_incoming_ActionR, value)
  } else {
    warning(paste(
      "filter_incoming_Action can only be set to a string,",
      "a vector of strings or NULL"
    ))
  }
})
.reactive(dMeasure, "filter_incoming_ActionR", quote(NULL))

.active(dMeasure, "filter_incoming_Actioned", function(value) {
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
    warning(paste(
      "filter_incoming_Actioned can only be a Date,",
      "a logical (TRUE/FALSE) or NULL."
    ))
  }
})
.reactive(dMeasure, "filter_incoming_ActionedR", quote(NULL))

.active(dMeasure, "filter_incoming_ignorePast", function(value) {
  if (missing(value)) {
    return(private$.filter_incoming_ignorePast)
  }
  if (is.logical(value)) {
    # accepts TRUE/FALSE
    # accepts Date type
    private$.filter_incoming_ignorePast <- value
    private$set_reactive(self$filter_incoming_ignorePastR, value)
  } else {
    warning(paste(
      "filter_incoming_ignorePast can only be",
      "a logical (TRUE/FALSE)."
    ))
  }
})
.reactive(dMeasure, "filter_incoming_ignorePastR", quote(FALSE))

#' List of investigations
#'
#' Filtered by date, and chosen clinicians
#' Stores result in $investigations_filtered
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from (default dMeasure_obj$date_a) start date
#' @param date_to (default dMeasure_obj$date_b) end date (inclusive)
#' @param clinicians (default dMeasure_obj$clinicians) list of clinicians to view
#' @param Action (default NA) Filter by action?
#'  a vector of actions (in string form) or NULL
#'  e.g. "Urgent Appointment" and/or "Non-urgent Appointment" or "No action"
#'  if NA, will adopt the value of dMeasure_obj$filter_incoming_Action
#'  if NULL, then no Actions are chosen
#' @param Actioned (default NA) Filter by having been 'actioned?' i.e. notified
#'  can be logical (TRUE or FALSE), a NULL
#'  or a Date (actioned prior to or by 'Actioned' Date)
#'  if NA, will adopt the value of dMeasure_obj$filter_incoming_Actioned
#'
#' @return list of investigations
#' @export
filter_investigations <- function(dMeasure_obj,
                                  date_from = NA,
                                  date_to = NA,
                                  clinicians = NA,
                                  Action = NA,
                                  Actioned = NA) {
  dMeasure_obj$filter_investigations(
    date_from, date_to, clinicians,
    Action, Actioned
  )
}
.public(dMeasure, "filter_investigations", function(date_from = NA,
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

  investigations <- self$investigations_filtered
  # this will normally be replaced in code below, unless no EMR database is open

  if (self$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {
      log_id <- self$config_db$write_log_db(
        query = "filter_investigations",
        data = list(date_from, date_to, clinicians)
      )
    }

    investigations <- self$db$investigations %>>%
      dplyr::filter(Checked >= date_from & Checked <= date_to) %>>%
      dplyr::filter(CheckedBy %in% clinicians) %>>%
      dplyr::mutate(TestName = trimws(TestName))
    # a database filter on an empty list after %in% will result in an error message

    ActionValue <- c(Action, "") # becomes "" if Action is NULL
    investigations <- investigations %>>%
      dplyr::filter(Action %in% ActionValue)

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
    if (self$Log) {
      self$config_db$duration_log_db(log_id)
    }
  }

  return(investigations)
})
.reactive_event(
  dMeasure, "investigations_filteredR",
  quote(
    shiny::eventReactive(
      c(
        self$date_aR(), self$date_bR(), self$cliniciansR(),
        self$filter_incoming_ActionR(),
        self$filter_incoming_ActionedR()
      ), {
        # update if reactive version of $date_a Rdate_b
        # or $clinicians are updated.
        self$filter_investigations()
        # re-calculates the appointments
      }
    )
  )
)

#' List of investigations with appointments
#'
#' Filtered by date, and chosen clinicians
#' added information about appointments scheduled after the 'Checked' date
#' Stores result in $investigations_filtered_appointments
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from (default dMeasure_obj$date_a) start date
#' @param date_to (default dMeasure_obj$date_b) end date (inclusive)
#' @param clinicians (default dMeasure_obj$clinicians) list of clinicians to view
#' @param Action (default NA) Filter by action?
#'  a vector of actions (in string form) or NULL
#'  e.g. "Urgent Appointment" and/or "Non-urgent Appointment" or "No action"
#'  if NA, will adopt the value of $filter_incoming_Action
#' @param Actioned (default NA) Filter by having been 'actioned?' i.e. notified
#'  can be logical (TRUE or FALSE), a NULL
#'  or a Date (actioned prior to or by 'Actioned' Date)
#'  if NA, will adopt the value of $filter_incoming_Actioned
#' @param ignorePast (default NA) don't include appointments before current Sys.Date()
#'  if NA, adopts $filter_incoming_ignorePast
#' @param lazy (default FALSE)
#'  if TRUE, don't 'call' filter_investigations to re-calculate
#'
#' @return list of investigations with relevant appointments
#' @export
filter_investigations_appointment <- function(dMeasure_obj,
                                              date_from = NA,
                                              date_to = NA,
                                              clinicians = NA,
                                              Action = NA,
                                              Actioned = NA,
                                              ignorePast = NA,
                                              lazy = FALSE) {
  dMeasure_obj$filter_investigations_appointment(
    date_from, date_to, clinicians,
    Action, Actioned, ignorePast,
    lazy
  )
}
.public(dMeasure, "filter_investigations_appointment", function(date_from = NA,
                                                                date_to = NA,
                                                                clinicians = NA,
                                                                Action = NA,
                                                                Actioned = NA,
                                                                ignorePast = NA,
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
  if (is.na(ignorePast)) {
    ignorePast <- self$filter_incoming_ignorePast
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (self$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {
      log_id <- self$config_db$write_log_db(
        query = "filter_incoming_appointment",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (!lazy) {
      self$filter_investigations(
        date_from, date_to, clinicians,
        Action, Actioned
      )
      # if not 'lazy' evaluation, then re-calculate self$investigations_filtered
      # (that is automatically done by calling the $filter_investigations)
    }
    today <- Sys.Date() # later used if ignorePast is true

    self$investigations_filtered_appointment <- self$investigations_filtered %>>%
      dplyr::left_join(self$db$appointments %>>%
        # only check against appointments after date_from
        dplyr::filter(AppointmentDate > date_from) %>>% {
          if (ignorePast) { # ignore appointments before 'today'?
            dplyr::filter(., AppointmentDate >= today)
          } else {
            .
          }
        },
      by = "InternalID", copy = TRUE
      ) %>>%
      dplyr::filter(is.na(AppointmentDate) | AppointmentDate > Checked) %>>%
      # filter appointments, if any, to date after date of checking
      dplyr::mutate(
        Status = trimws(Status),
        TestName = trimws(TestName)
      )

    if (self$Log) {
      self$config_db$duration_log_db(log_id)
    }
  }

  return(self$investigations_filtered_appointment)
})
.reactive_event(
  dMeasure, "investigations_filtered_appointmentR",
  quote(
    shiny::eventReactive(
      c(
        self$investigations_filteredR(),
        self$filter_incoming_ignorePastR()
      ), {
        # update if reactive version of $date_a Rdate_b
        # or $clinicians are updated.
        self$filter_investigations_appointment()
        # re-calculates the appointments
      }
    )
  )
)


#' List of investigations, with patient names
#'
#' Filtered by date, and chosen clinicians
#' Stores result in $investigations_filtered_named
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from (default dMeasure_obj$date_a) start date
#' @param date_to (default dMeasure_obj$date_b) end date (inclusive)
#' @param clinicians (default dMeasure_obj$clinicians) list of clinicians to view
#' @param Action (default NA) Filter by action?
#'  a vector of actions (in string form) or NULL
#'  e.g. "Urgent Appointment" and/or "Non-urgent Appointment" or "No action"
#'  if NA, will adopt the value of $filter_incoming_Action
#' @param Actioned (default NA) Filter by having been 'actioned?' i.e. notified
#'  can be logical (TRUE or FALSE), a NULL
#'  or a Date (actioned prior to or by 'Actioned' Date)
#'  if NA, will adopt the value of $filter_incoming_Actioned
#' @param ignorePast (default NA) don't include appointments before current Sys.Date()
#'  if NA, adopts $filter_incoming_ignorePast
#' @param lazy (default FALSE)
#'  only re-calculate $invesigations_filtered_appointment
#'  if lazy is TRUE
#'
#' @return list of investigations
#' @export
filter_investigations_named <- function(dMeasure_obj,
                                        date_from = NA,
                                        date_to = NA,
                                        clinicians = NA,
                                        Action = NA,
                                        Actioned = NA,
                                        ignorePast = NA,
                                        lazy = FALSE) {
  dMeasure_obj$filter_investigations_named(
    date_from, date_to, clinicians,
    Action, Actioned, ignorePast, lazy
  )
}
.public(dMeasure, "filter_investigations_named", function(date_from = NA,
                                                          date_to = NA,
                                                          clinicians = NA,
                                                          Action = NA,
                                                          Actioned = NA,
                                                          ignorePast = NA,
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
  if (is.na(ignorePast)) {
    ignorePast <- self$filter_incoming_ignorePast
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (self$emr_db$is_open()) {
    # only if EMR database is open

    if (!lazy) {
      self$filter_investigations_appointment(
        date_from, date_to, clinicians,
        Action, Actioned, ignorePast,
        lazy
      )
      # if not 'lazy' evaluation, then re-calculate $investigations_filtered_appointment
      # (that is automatically done by calling the $filter_investigations_appointment)
    }

    self$investigations_filtered_named <- self$investigations_filtered_appointment %>>%
      dplyr::left_join(self$db$patients, by = "InternalID", copy = TRUE) %>>%
      # need patients database to access date-of-birth
      dplyr::select(
        Firstname, Surname,
        DOB, InternalID, RecordNo, TestName, ReportID,
        Reported, Checked, CheckedBy,
        Notation, Action, Actioned, Comment,
        AppointmentDate, AppointmentTime, Provider, Status
      ) %>>%
      dplyr::collect() %>>%
      dplyr::mutate(
        Patient = paste(trimws(Firstname), trimws(Surname)),
        RecordNo = trimws(RecordNo),
        Reported = as.Date(Reported), Checked = as.Date(Checked),
        Actioned = as.Date(Actioned),
        AppointmentDate = as.Date(AppointmentDate),
        AppointmentTime = dMeasure::hrmin(AppointmentTime),
        DOB = as.Date(substr(DOB, 1, 10))
      ) %>>%
      dplyr::mutate(Age = dMeasure::calc_age(
        DOB, # try several different dates for 'age'
        dplyr::case_when(
          !is.na(Reported) ~ Reported,
          !is.na(Checked) ~ Checked,
          TRUE ~ Sys.Date()
        )
      )) %>>%
      dplyr::select(-c(Firstname, Surname))
  }

  return(self$investigations_filtered_named)
})
.reactive_event(
  dMeasure, "investigations_filtered_namedR",
  quote(
    shiny::eventReactive(
      c(self$investigations_filtered_appointmentR()), {
        # update if reactive version of $date_a Rdate_b
        # or $clinicians are updated.
        self$filter_investigations_named(lazy = TRUE)
        # re-calculates the appointments
      }
    )
  )
)


#' List of correspondence
#'
#' Filtered by date, and chosen clinicians
#' Stores result in $correspondence_filtered
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from (default dMeasure_obj$date_a) start date
#' @param date_to (default dMeasure_obj$date_b) end date (inclusive)
#' @param clinicians (default dMeasure_obj$clinicians) list of clinicians to view
#' @param Action (default NA) Filter by action?
#'  a vector of actions (in string form) or NULL
#'  e.g. "Urgent Appointment" and/or "Non-urgent Appointment" or "No action"
#'  if NA, will adopt the value of $filter_incoming_Action
#'  if NULL, then no Correspondence are chosen
#' @param Actioned (default NA) Filter by having been 'actioned?' i.e. notified
#'  can be logical (TRUE or FALSE), a NULL
#'  or a Date (actioned prior to or by 'Actioned' Date)
#'  if NA, will adopt the value of $filter_incoming_Actioned
#'
#' @return list of correspondence
#' @export
filter_correspondence <- function(dMeasure_obj,
                                  date_from = NA,
                                  date_to = NA,
                                  clinicians = NA,
                                  Action = NA,
                                  Actioned = NA) {
  dMeasure_obj$filter_correspondence(
    date_from, date_to, clinicians,
    Action, Actioned
  )
}
.public(dMeasure, "filter_correspondence", function(date_from = NA,
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

  if ("UserID" %in% colnames(self$UserFullConfig)) {
    clinician_n <- c(unlist(self$UserFullConfig[
      self$UserFullConfig$Fullname %in% clinicians,
      "UserID"
    ], use.names = FALSE), 0)
  }
  else {
    # there might not be a "UserID" field in self$UserFullConfig if there is
    # no open clinical database
    clinicina_n <- c(-1)
  }
  # again, need to add zero to list because cannot search on empty list

  correspondence <- self$correspondence_filtered
  # this should usually be replaced by subsequent code, unless no EMR is open

  if (self$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {
      log_id <- self$config_db$write_log_db(
        query = "filter_correspondence",
        data = list(date_from, date_to, clinicians)
      )
    }

    correspondence <- self$db$correspondenceInRaw %>>%
      dplyr::filter(CheckDate >= date_from & CheckDate <= date_to) %>>%
      dplyr::filter(CheckedBy %in% clinician_n) %>>%
      dplyr::select(
        InternalID, DocumentID,
        Category, Subject, Detail,
        CorrespondenceDate, CheckDate, ActionDate,
        CheckedBy, Notation, Comment, Action
      )

    # a database filter on an empty list after %in% will result in an error message

    ActionValue <- match(
      c(Action, ""), # if Action is NULL, then ActionValue will be NA
      c(
        "No action", "Reception to advise",
        "Nurse to advise", "Doctor to advise",
        "Send routine reminder",
        "Non-urgent appointment", "Urgent appointment"
      ))
    # the most important being values 6 and 7. could return NA if not found
    ActionValue <- c(ActionValue[!is.na(ActionValue)], -999) # -999 is a dummy value
    correspondence <- dplyr::filter(correspondence, Action %in% ActionValue)

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
    if (self$Log) {
      self$config_db$duration_log_db(log_id)
    }
  }

  return(correspondence)
})
.reactive_event(
  dMeasure, "correspondence_filteredR",
  quote(
    shiny::eventReactive(
      c(
        self$date_aR(), self$date_bR(), self$cliniciansR(),
        self$filter_incoming_ActionR(),
        self$filter_incoming_ActionedR()
      ), {
        # update if reactive version of $date_a Rdate_b
        # or $clinicians are updated.
        self$filter_correspondence()
        # re-calculates the appointments
      }
    )
  )
)

#' List of correspondence with appointments
#'
#' Filtered by date, and chosen clinicians
#' added information about appointments scheduled after the 'Checked' date
#' Stores result in $correspondence_filtered_appointments
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from (default dMeasure_obj$date_a) start date
#' @param date_to (default dMeasure_obj$date_b) end date (inclusive)
#' @param clinicians (default dMeasure_obj$clinicians) list of clinicians to view
#' @param Action (default NA) Filter by action?
#'  a vector of actions (in string form) or NULL
#'  e.g. "Urgent Appointment" and/or "Non-urgent Appointment" or "No action"
#'  if NA, will adopt the value of $filter_incoming_Action
#' @param Actioned (default NA) Filter by having been 'actioned?' i.e. notified
#'  can be logical (TRUE or FALSE), a NULL
#'  or a Date (actioned prior to or by 'Actioned' Date)
#'  if NA, will adopt the value of $filter_incoming_Actioned
#' @param ignorePast (default NA) don't include appointments before current Sys.Date()
#'  if NA, will adopts value of $filter_incoming_ignorePast
#' @param lazy (default FALSE) if TRUE, don't 'call' filter_correspondence
#'  to re-calculate
#'
#' @return list of correspondence with relevant appointments
#' @export
filter_correspondence_appointment <- function(dMeasure_obj,
                                              date_from = NA,
                                              date_to = NA,
                                              clinicians = NA,
                                              Action = NA,
                                              Actioned = NA,
                                              ignorePast = NA,
                                              lazy = FALSE) {
  dMeasure_obj$filter_correspondence_appointment(
    date_from, date_to, clinicians,
    Action, Actioned, ignorePast,
    lazy
  )
}
.public(dMeasure, "filter_correspondence_appointment", function(date_from = NA,
                                                                date_to = NA,
                                                                clinicians = NA,
                                                                Action = NA,
                                                                Actioned = NA,
                                                                ignorePast = NA,
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
  if (is.na(ignorePast)) {
    ignorePast <- self$filter_incoming_ignorePast
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (self$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {
      log_id <- self$config_db$write_log_db(
        query = "filter_correspondence_appointment",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (!lazy) {
      self$filter_correspondence(
        date_from, date_to, clinicians,
        Action, Actioned
      )
      # if not 'lazy' evaluation, then re-calculate self$correspondence_filtered
      # (that is automatically done by calling the $filter_correspondence)
    }

    today <- Sys.Date() # required if ignorePast is TRUE

    self$correspondence_filtered_appointment <- self$correspondence_filtered %>>%
      dplyr::left_join(self$db$appointments %>>%
        # only check against appointments after date_from
        dplyr::filter(AppointmentDate > date_from) %>>% {
          if (ignorePast) { # ignore appointments before 'today'?
            dplyr::filter(., AppointmentDate >= today)
          } else {
            .
          }
        },
      by = "InternalID", copy = TRUE
      ) %>>%
      dplyr::filter(is.na(AppointmentDate) | AppointmentDate > CheckDate) %>>%
      # filter appointment date (if any) to after the document was checked
      dplyr::mutate(Status = trimws(Status))
    # further filter against the 'checked' date for each correspondence

    if (self$Log) {
      self$config_db$duration_log_db(log_id)
    }
  }

  return(self$correspondence_filtered_appointment)
})
.reactive_event(
  dMeasure, "correspondence_filtered_appointmentR",
  quote(
    shiny::eventReactive(
      c(
        self$correspondence_filteredR(),
        self$filter_incoming_ignorePastR()
      ), {
        # update if reactive version of $date_a Rdate_b
        # or $clinicians are updated.
        self$filter_correspondence_appointment(
          lazy = TRUE,
          ignorePast =
            self$filter_incoming_ignorePastR()
        )
        # re-calculates the appointments
      }
    )
  )
)

#' List of correspondence, with patient names
#'
#' Filtered by date, and chosen clinicians
#' Stores result in $correspondence_filtered_named
#' Values stored as numeric in $correspondence_filtered_appointments
#' are changed to 'human-readable' character strings e.g. Action
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from (default dMeasure_obj$date_a) start date
#' @param date_to (default dMeasure_obj$date_b) end date (inclusive)
#' @param clinicians (default dMeasure_obj$clinicians list) of clinicians to view
#' @param Action (default NA) Filter by action?
#'  a vector of actions (in string form) or NULL
#'  e.g. "Urgent Appointment" and/or "Non-urgent Appointment" or "No action"
#'  if NA, will adopt the value of $filter_incoming_Action
#' @param Actioned (default NA) Filter by having been 'actioned?' i.e. notified
#'  can be logical (TRUE or FALSE), a NULL
#'  or a Date (actioned prior to or by 'Actioned' Date)
#'  if NA, will adopt the value of $filter_incoming_Actioned
#' @param ignorePast (default NA) don't include appointments before current Sys.Date()
#'  if NA, will adopt the value of $filter_incoming_ignorePast
#' @param lazy (default FALSE) if TRUE, don't 'call' filter_correspondence_appointment
#'  to re-calculate
#'
#' @return list of correspondence
#' @export
filter_correspondence_named <- function(dMeasure_obj,
                                        date_from = NA,
                                        date_to = NA,
                                        clinicians = NA,
                                        Action = NA,
                                        Actioned = NA,
                                        ignorePast = NA,
                                        lazy = FALSE) {
  dMeasure_obj$filter_correspondence_named(
    date_from, date_to, clinicians,
    Action, Actioned, ignorePast, lazy
  )
}
.public(dMeasure, "filter_correspondence_named", function(date_from = NA,
                                                          date_to = NA,
                                                          clinicians = NA,
                                                          Action = NA,
                                                          Actioned = NA,
                                                          ignorePast = NA,
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

  if (self$emr_db$is_open()) {
    # only if EMR database is open

    if (!lazy) {
      self$filter_correspondence_appointment(
        date_from, date_to, clinicians,
        Action, Actioned, ignorePast, lazy
      )
      # if not 'lazy' evaluation, then re-calculate $correspondence_filtered_appointment
      # (that is automatically done by calling the $filter_correspondence_appointment)
    }

    n_UserNames <- length(c(self$UserFullConfig$Fullname))
    # needed later for changing CheckedBy to a user name

    self$correspondence_filtered_named <- self$correspondence_filtered_appointment %>>%
      dplyr::left_join(self$db$patients, by = "InternalID", copy = TRUE) %>>%
      # need patients database to access date-of-birth
      dplyr::select(
        Firstname, Surname,
        DOB, InternalID, RecordNo, DocumentID,
        Category, Subject, Detail,
        CorrespondenceDate, CheckDate, CheckedBy,
        Notation, Action, ActionDate, Comment,
        AppointmentDate, AppointmentTime, Provider, Status
      ) %>>%
      dplyr::collect() %>>%
      dplyr::mutate(
        Patient = paste(trimws(Firstname), trimws(Surname)),
        RecordNo = trimws(RecordNo),
        DocumentName = paste(dplyr::if_else(
          is.na(Category),
          "",
          trimws(Category)
        ),
        dplyr::if_else(
          is.na(Subject),
          "",
          trimws(Subject)
        ),
        dplyr::if_else(
          is.na(Detail),
          "",
          trimws(Detail)
        ),
        sep = ":"
        ),
        CorrespondenceDate = as.Date(CorrespondenceDate),
        CheckDate = as.Date(CheckDate),
        ActionDate = as.Date(ActionDate),
        AppointmentDate = as.Date(AppointmentDate),
        AppointmentTime = dMeasure::hrmin(AppointmentTime)
      ) %>>%
      dplyr::select(-c(Firstname, Surname, Category, Subject, Detail)) %>>%
      dplyr::mutate(
        DOB = as.Date(substr(DOB, 1, 10)),
        Age = dMeasure::calc_age(
          DOB, # try several different dates for 'age'
          dplyr::case_when(
            !is.na(CorrespondenceDate) ~ CorrespondenceDate,
            !is.na(CheckDate) ~ CheckDate,
            TRUE ~ Sys.Date()
          )
        )
      ) %>>%
      dplyr::mutate(
        CheckedBy = c(self$UserFullConfig$Fullname, "")
        [dplyr::if_else(
            CheckedBy == 0,
            as.integer(n_UserNames + 1),
            CheckedBy
          )],
        # this strange incantation is to deal with '0' CheckedBy
        Notation = c(
          "Normal", "Abnormal", "Stable",
          "Acceptable", "Unacceptable", "Being treated",
          "Under specialist care", ""
        )
        [dplyr::if_else(Notation == 0, as.integer(9), Notation)],
        # this strange if_else is to deal with 'empty' Notation
        Action = c(
          "No action", "Reception to advise",
          "Nurse to advise", "Doctor to advise",
          "Send routine reminder", "Non-urgent appointment",
          "Urgent appointment", ""
        )
        # this strange if_else is to deal with 'empty' Action
        [dplyr::if_else(Action == 0, as.integer(8), Action)]
      )
  }

  return(self$correspondence_filtered_named)
})
.reactive_event(
  dMeasure, "correspondence_filtered_namedR",
  quote(
    shiny::eventReactive(
      c(self$correspondence_filtered_appointmentR()), {
        # update if reactive version of $date_a Rdate_b
        # or $clinicians are updated.
        self$filter_correspondence_named(lazy = TRUE)
        # re-calculates the appointments
      }
    )
  )
)


#' incoming (investigations and correspondence) view
#'
#' filter to date range
#' and clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from (default dMeasure_obj$date_a) start date
#' @param date_to (default dMeasure_obj$date_b) end date (inclusive)
#' @param clinicians (default dMeasure_obj$clinicians) list of clinicians to view
#' @param Action (default NA) Filter by action?
#'  a vector of actions (in string form) or NULL
#'  e.g. "Urgent Appointment" and/or "Non-urgent Appointment" or "No action"
#'  if NA, will adopt the value of $filter_incoming_Action
#' @param Actioned (default NA) Filter by having been 'actioned?' i.e. notified
#'  can be logical (TRUE or FALSE), a NULL
#'  or a Date (actioned prior to or by 'Actioned' Date)
#'  if NA, will adopt the value of $filter_incoming_Actioned
#' @param ignorePast (default NA) ignore appointments before current Sys.Date() date?
#'  retrieves from $filter_incoming_ignorePast if NA
#' @param lazy if TRUE, then do not recalculate appointment list. otherwise, re-calculate
#' @param screentag (default FALSE) optionally add a fomantic/semantic HTML description of 'action'
#' @param screentag_print (default TRUE) optionally add a 'printable' description of 'action'
#'
#' @return list of appointments (with patient details)
#' @export
incoming_view <- function(dMeasure_obj, date_from = NA, date_to = NA,
                          clinicians = NA,
                          Action = NA,
                          Actioned = NA,
                          ignorePast = NA,
                          lazy = FALSE,
                          screentag = FALSE, screentag_print = TRUE) {
  dMeasure_obj$view_incoming(
    date_from, date_to, clinicians,
    Action, Actioned, ingorePast,
    lazy,
    screentag, screentag_print
  )
}
.public(dMeasure, "view_incoming", function(date_from = NA, date_to = NA,
                                            clinicians = NA,
                                            Action = NA,
                                            Actioned = NA,
                                            ignorePast = NA,
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
  if (is.na(ignorePast)) {
    ignorePast <- self$filter_incoming_ignorePast
  }

  # create empty table
  incoming <- data.frame(
    Patient = character(),
    InternalID = numeric(), # will be later dropped
    ReportID = numeric(), # will be later dropped
    DocumentID = numeric(), # will be later dropped
    RecordNo = character(),
    DOB = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    Age = numeric(),
    TestName = character(),
    Reported = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    Checked = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    Actioned = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    CheckedBy = character(),
    Notation = character(),
    Comment = character(),
    Action = character(),
    AppointmentDate = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    AppointmentTime = character(),
    Provider = character(),
    Status = character(),
    PastAppointment = logical(),
    stringsAsFactors = FALSE
  )


  if (screentag) {
    incoming <- cbind(incoming, data.frame(
      labeltag = character(),
      patienttag = character(),
      testtag = character()
    ))
  }
  if (screentag_print) {
    incoming <- cbind(incoming, data.frame(labeltag_print = character()))
  }

  if (!self$emr_db$is_open()) {
    # EMR database is not open
    # empty data-frame already created to return
  } else {
    # only if EMR database is open

    if (!lazy) {
      self$filter_investigations_named(date_from, date_to,
        clinicians, Action, Actioned,
        ignorePast,
        lazy = FALSE
      )
      self$filter_correspondence_named(date_from, date_to,
        clinicians, Action, Actioned,
        ignorePast,
        lazy = FALSE
      )
      # if not 'lazy' evaluation, then re-calculate
      # $correspondence_filtered_named and $investigations$filtered_named
      # (that is automatically done by calling the above methods
    }

    incoming <- incoming %>>%
      dplyr::bind_rows(self$investigations_filtered_named) %>>%
      dplyr::bind_rows(
        self$correspondence_filtered_named %>>%
          dplyr::rename(
            TestName = DocumentName,
            Reported = CorrespondenceDate,
            Checked = CheckDate,
            Actioned = ActionDate
          )
      ) %>>%
      dplyr::mutate(PastAppointment = AppointmentDate < Sys.Date()) %>>%
      # the listed appointment has already 'past'
      dplyr::mutate(AppointmentDetail = paste(AppointmentDate, " ", AppointmentTime,
        " ", Provider, " (", Status, ")",
        sep = ""
      ))

    if (screentag) {
      incoming <- incoming %>>%
        dplyr::mutate(
          viewtag =
            dMeasure::semantic_button(
              AppointmentDate, # semantic/fomantic buttons
              colour =
                dplyr::if_else(
                  PastAppointment,
                  "yellow",
                  "green"
                ),
              popuphtml =
                paste0(
                  "<h4>Date : ", AppointmentDate,
                  ", ", AppointmentTime,
                  "</h4><h6>Provider : ", Provider,
                  "</h6><p><font size=\'+0\'>Status : ",
                  Status, "</p>"
                )
            ),
          patienttag =
            dMeasure::semantic_button(
              Patient,
              colour = "teal",
              popuphtml =
                paste0(
                  "<p><font size = \'+0\'>DOB : ",
                  DOB, "</p>",
                  "<p>Age : ", Age, "</p>"
                )
            ),
          testtag =
            dMeasure::semantic_button(
              TestName,
              colour = "purple",
              popuphtml =
                paste0(
                  "<p><font size = \'+0\'>Reported : ",
                  Reported, "</p"
                )
            )
        ) %>>%
        dplyr::group_by(
          patienttag, InternalID, RecordNo,
          testtag, ReportID, DocumentID,
          Checked, Actioned,
          CheckedBy, Notation, Comment, Action
        ) %>>%
        dplyr::arrange(AppointmentDetail, .by_group = TRUE) %>>%
        # group appointments by the investigation report or Document
        # gathers appointments referring to the same report/correspondence into a single row
        dplyr::summarise(labeltag = paste(viewtag, collapse = "")) %>>%
        dplyr::ungroup()
    }

    if (screentag_print) {
      incoming <- incoming %>>%
        dplyr::mutate(viewtag_print = paste(dplyr::if_else(
          PastAppointment,
          "(Past) ",
          ""
        ),
        AppointmentDetail,
        sep = ""
        )) %>>%
        dplyr::group_by(
          Patient, InternalID, RecordNo, DOB, Age,
          TestName, ReportID, DocumentID,
          Reported, Checked, Actioned,
          CheckedBy, Notation, Comment, Action
        ) %>>%
        dplyr::arrange(AppointmentDetail, .by_group = TRUE) %>>%
        # group appointments by the investigation report or Document
        # gathers appointments referring to the same report/correspondence into a single row
        dplyr::summarise(
          labeltag_print =
            paste(viewtag_print, collapse = ", ")
        ) %>>%
        dplyr::ungroup()
    }
  }

  incoming <- dplyr::select(
    incoming,
    -c(InternalID, ReportID, DocumentID)
  )

  return(incoming)
})
