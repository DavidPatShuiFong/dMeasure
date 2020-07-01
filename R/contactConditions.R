#' Contact list + Conditions fields and methods
#'
#' @name contactConditions
#' @title dMeasure contactConditions contact lists with conditions
#'
#' @include dMeasure.R
#' needs the '.public' function from dMeasure.R
#' @include contact.R
#' needs to know contact lists
NULL


##### diabetes fields ###########################################################

.public(
  dMeasure, "contact_diabetes_list",
  data.frame(
    Patient = character(),
    InternalID = integer(),
    Count = integer(),
    Latest = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    stringsAsFactors = FALSE
  )
)
# filtered by chosen dates and clinicians and number of contacts

##### diabetes methods ##########################################################
#' List of diabetics in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param max_date most recent contact must be at least max_date. default is $contact_maxDate, initially Sys.Date()
#' @param contact_type contact types which are accepted. default is $contact_type
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
#' @export
list_contact_diabetes <- function(
                                  dMeasure_obj,
                                  date_from = NA,
                                  date_to = NA,
                                  clinicians = NA,
                                  min_contact = NA,
                                  min_date = NA,
                                  max_date = NA,
                                  contact_type = NA,
                                  lazy = FALSE) {
  dMeasure_obj$list_contact_diabetes(
    date_from, date_to, clinicians,
    min_contact, min_date, max_date,
    contact_type,
    lazy
  )
}

.public(dMeasure, "list_contact_diabetes", function(date_from = NA,
                                                    date_to = NA,
                                                    clinicians = NA,
                                                    min_contact = NA,
                                                    min_date = NA,
                                                    max_date = NA,
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
  if (is.na(max_date)) {
    max_date <- self$contact_maxDate
  }
  if (is.na(contact_type[[1]])) {
    contact_type <- self$contact_type
  }


  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (self$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {
      log_id <- self$config_db$write_log_db(
        query = "diabetes_contact",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (!lazy) {
      self$list_contact_count(
        date_from, date_to, clinicians,
        min_contact, min_date, max_date, contact_type,
        lazy
      )
    }

    diabetesID <- self$contact_count_list %>>%
      dplyr::mutate(Date = Latest) %>>%
      self$diabetes_list()

    self$contact_diabetes_list <- self$contact_count_list %>>%
      dplyr::filter(InternalID %in% diabetesID)

    if (self$Log) {
      self$config_db$duration_log_db(log_id)
    }
  }

  return(self$contact_diabetes_list)
})
.reactive_event(
  dMeasure, "contact_diabetes_listR",
  quote(
    shiny::eventReactive(
      c(self$contact_count_listR()), {
        # update if reactive version of $date_a $date_b
        # or $clinicians are updated.
        self$list_contact_diabetes(lazy = TRUE)
        # re-calculates the counts
      }
    )
  )
)


##### chronic lung disease fields ###########################################################

.public(
  dMeasure, "contact_chroniclungdisease_list",
  data.frame(
    Patient = character(),
    InternalID = integer(),
    Count = integer(),
    Latest = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    stringsAsFactors = FALSE
  )
)
# filtered by chosen dates and clinicians and number of contacts

##### chronic lung disease methods ##########################################################

#' List of chronic lung disease in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param max_date most recent contact must be at most max_date. default is $contact_maxDate, initially Sys.Date()
#' @param contact_type contact types which are accepted. default is $contact_type
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
#' @export
list_contact_chroniclungdisease <- function(dMeasure_obj,
                                            date_from = NA,
                                            date_to = NA,
                                            clinicians = NA,
                                            min_contact = NA,
                                            min_date = NA, max_date = NA,
                                            contact_type = NA,
                                            lazy = FALSE) {
  dMeasure_obj$list_contact_chroniclungdisease(
    date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    lazy
  )
}

.public(dMeasure, "list_contact_chroniclungdisease", function(date_from = NA,
                                                              date_to = NA,
                                                              clinicians = NA,
                                                              min_contact = NA,
                                                              min_date = NA, max_date = NA,
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
  if (is.na(max_date)) {
    max_date <- self$contact_maxDate
  }
  if (is.na(contact_type[[1]])) {
    contact_type <- self$contact_type
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (self$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {
      log_id <- self$config_db$write_log_db(
        query = "chroniclungdisease_contact",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (!lazy) {
      self$list_contact_count(
        date_from, date_to, clinicians,
        min_contact, min_date, max_date, contact_type,
        lazy
      )
    }

    chroniclungdiseaseID <- self$contact_count_list %>>%
      dplyr::mutate(Date = Latest) %>>%
      self$chroniclungdisease_list()

    self$contact_chroniclungdisease_list <- self$contact_count_list %>>%
      dplyr::filter(InternalID %in% chroniclungdiseaseID)

    if (self$Log) {
      self$config_db$duration_log_db(log_id)
    }
  }

  return(self$contact_chroniclungdisease_list)
})
.reactive_event(
  dMeasure, "contact_chroniclungdisease_listR",
  quote(
    shiny::eventReactive(
      c(self$contact_count_listR()), {
        # update if reactive version of $date_a $date_b
        # or $clinicians are updated.
        self$list_contact_chroniclungdisease(lazy = TRUE)
        # re-calculates the counts
      }
    )
  )
)

##### asthma fields ###########################################################

.public(
  dMeasure, "contact_asthma_list",
  data.frame(
    Patient = character(),
    InternalID = integer(),
    Count = integer(),
    Latest = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    stringsAsFactors = FALSE
  )
)
# filtered by chosen dates and clinicians and number of contacts

#' List of asthma in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param max_date most recent contact must be at most max_date. default is $contact_maxDate, initially Sys.Date()
#' @param contact_type contact types which are accepted. default is $contact_type
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
#' @export
list_contact_asthma <- function(dMeasure_obj,
                                date_from = NA,
                                date_to = NA,
                                clinicians = NA,
                                min_contact = NA,
                                min_date = NA, max_date = NA,
                                contact_type = NA,
                                lazy = FALSE) {
  dMeasure_obj$list_contact_asthma(
    date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    lazy
  )
}

##### asthma methods ##########################################################
.public(dMeasure, "list_contact_asthma", function(date_from = NA,
                                                  date_to = NA,
                                                  clinicians = NA,
                                                  min_contact = NA,
                                                  min_date = NA, max_date = NA,
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
  if (is.na(max_date)) {
    max_date <- self$contact_maxDate
  }
  if (is.na(contact_type[[1]])) {
    contact_type <- self$contact_type
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (self$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {
      log_id <- self$config_db$write_log_db(
        query = "asthma_contact",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (!lazy) {
      self$list_contact_count(
        date_from, date_to, clinicians,
        min_contact, min_date, max_date, contact_type,
        lazy
      )
    }

    asthmaID <- self$contact_count_list %>>%
      dplyr::mutate(Date = Latest) %>>%
      self$asthma_list()

    self$contact_asthma_list <- self$contact_count_list %>>%
      dplyr::filter(InternalID %in% asthmaID)

    if (self$Log) {
      self$config_db$duration_log_db(log_id)
    }
  }

  return(self$contact_asthma_list)
})
.reactive_event(
  dMeasure, "contact_asthma_listR",
  quote(
    shiny::eventReactive(
      c(self$contact_count_listR()), {
        # update if reactive version of $date_a $date_b
        # or $clinicians are updated.
        self$list_contact_asthma(lazy = TRUE)
        # re-calculates the counts
      }
    )
  )
)

##### age 15 plus fields ###########################################################
# used for Practice Incentive Program Quality Improvement Measures
# Smoking, BMI, Alcohol

.public(
  dMeasure, "contact_15plus_list",
  data.frame(
    Patient = character(),
    InternalID = integer(),
    Count = integer(),
    Latest = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    stringsAsFactors = FALSE
  )
)
# filtered by chosen dates and clinicians and number of contacts

##### age 15 plus methods ##########################################################
#' List of those aged 15 plus (as of 'date_to') in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param max_date most recent contact must be at most max_date. default is $contact_maxDate, initially Sys.Date()
#' @param contact_type contact types which are accepted. default is $contact_type
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
#' @export
list_contact_15plus <- function(dMeasure_obj,
                                date_from = NA,
                                date_to = NA,
                                clinicians = NA,
                                min_contact = NA,
                                min_date = NA,
                                max_date = NA,
                                contact_type = NA,
                                lazy = FALSE) {
  dMeasure_obj$list_contact_15plus(
    date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    lazy
  )
}

.public(dMeasure, "list_contact_15plus", function(date_from = NA,
                                                  date_to = NA,
                                                  clinicians = NA,
                                                  min_contact = NA,
                                                  min_date = NA, max_date = NA,
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
  if (is.na(max_date)) {
    max_date <- self$contact_maxDate
  }
  if (is.na(contact_type[[1]])) {
    contact_type <- self$contact_type
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (self$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {
      log_id <- self$config_db$write_log_db(
        query = "15plus_contact",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (!lazy) {
      self$list_contact_count(
        date_from, date_to, clinicians,
        min_contact, min_date, max_date, contact_type,
        lazy
      )
    }

    fifteenplusID <- self$contact_count_list %>>%
      dplyr::mutate(Date = date_to) %>>%
      self$fifteenplus_list()

    self$contact_15plus_list <- self$contact_count_list %>>%
      dplyr::filter(InternalID %in% fifteenplusID)

    if (self$Log) {
      self$config_db$duration_log_db(log_id)
    }
  }

  return(self$contact_15plus_list)
})
.reactive_event(
  dMeasure, "contact_15plus_listR",
  quote(
    shiny::eventReactive(
      c(self$contact_count_listR()), {
        # update if reactive version of $date_a $date_b
        # or $clinicians are updated.
        self$list_contact_15plus(lazy = TRUE)
        # re-calculates the counts
      }
    )
  )
)



##### age 65 plus fields ###########################################################
# used for Practice Incentive Program Quality Improvement Measures
# influenza (QIM 04)

.public(
  dMeasure, "contact_65plus_list",
  data.frame(
    Patient = character(),
    InternalID = integer(),
    Count = integer(),
    Latest = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    stringsAsFactors = FALSE
  )
)
# filtered by chosen dates and clinicians and number of contacts

##### age 65 plus methods ##########################################################
#' List of those aged 65 plus (as of 'date_to') in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param max_date most recent contact must be at most max_date. default is $contact_maxDate, initially Sys.Date()
#' @param contact_type contact types which are accepted. default is $contact_type
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
#' @export
list_contact_65plus <- function(dMeasure_obj,
                                date_from = NA,
                                date_to = NA,
                                clinicians = NA,
                                min_contact = NA,
                                min_date = NA, max_date = NA,
                                contact_type = NA,
                                lazy = FALSE) {
  dMeasure_obj$list_contact_65plus(
    date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    lazy
  )
}

.public(dMeasure, "list_contact_65plus", function(date_from = NA,
                                                  date_to = NA,
                                                  clinicians = NA,
                                                  min_contact = NA,
                                                  min_date = NA, max_date = NA,
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
  if (is.na(max_date)) {
    max_date <- self$contact_maxDate
  }
  if (is.na(contact_type[[1]])) {
    contact_type <- self$contact_type
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (self$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {
      log_id <- self$config_db$write_log_db(
        query = "65plus_contact",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (!lazy) {
      self$list_contact_count(
        date_from, date_to, clinicians,
        min_contact, min_date, max_date, contact_type,
        lazy
      )
    }

    sixtyfiveplusID <- self$contact_count_list %>>%
      dplyr::mutate(Date = date_to) %>>%
      self$sixtyfiveplus_list()

    self$contact_65plus_list <- self$contact_count_list %>>%
      dplyr::filter(InternalID %in% sixtyfiveplusID)

    if (self$Log) {
      self$config_db$duration_log_db(log_id)
    }
  }

  return(self$contact_65plus_list)
})
.reactive_event(
  dMeasure, "contact_65plus_listR",
  quote(
    shiny::eventReactive(
      c(self$contact_count_listR()), {
        # update if reactive version of $date_a $date_b
        # or $clinicians are updated.
        self$list_contact_65plus(lazy = TRUE)
        # re-calculates the counts
      }
    )
  )
)



##### age 45 to 74 years fields ###########################################################
# used for Practice Incentive Program Quality Improvement Measures
# cardiovascular risk (QIM 08)

.public(
  dMeasure, "contact_45_74_list",
  data.frame(
    Patient = character(),
    InternalID = integer(),
    Count = integer(),
    Latest = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    stringsAsFactors = FALSE
  )
)
# filtered by chosen dates and clinicians and number of contacts

##### age 45 to 74 years methods ##########################################################
#' List of those aged 45 to 74 years (as of 'date_to') in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param max_date most recent contact must be at least max_date. default is $contact_maxDate, initially Sys.Date()
#' @param contact_type contact types which are accepted. default is $contact_type
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
#' @export
list_contact_45_74 <- function(dMeasure_obj,
                               date_from = NA,
                               date_to = NA,
                               clinicians = NA,
                               min_contact = NA,
                               min_date = NA, max_date = NA,
                               contact_type = NA,
                               lazy = FALSE) {
  dMeasure_obj$list_contact_45_74(
    date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    lazy
  )
}

.public(dMeasure, "list_contact_45_74", function(date_from = NA,
                                                 date_to = NA,
                                                 clinicians = NA,
                                                 min_contact = NA,
                                                 min_date = NA, max_date = NA,
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
  if (is.na(max_date)) {
    max_date <- self$contact_maxDate
  }
  if (is.na(contact_type[[1]])) {
    contact_type <- self$contact_type
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (self$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {
      log_id <- self$config_db$write_log_db(
        query = "45_74_contact",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (!lazy) {
      self$list_contact_count(
        date_from, date_to, clinicians,
        min_contact, min_date, max_date, contact_type,
        lazy
      )
    }

    fortyfiveseventyfourID <- self$contact_count_list %>>%
      dplyr::mutate(Date = date_to) %>>%
      self$fortyfiveseventyfour_list()

    self$contact_45_74_list <- self$contact_count_list %>>%
      dplyr::filter(InternalID %in% fortyfiveseventyfourID)

    if (self$Log) {
      self$config_db$duration_log_db(log_id)
    }
  }

  return(self$contact_45_74_list)
})
.reactive_event(
  dMeasure, "contact_45_74_listR",
  quote(
    shiny::eventReactive(
      c(self$contact_count_listR()), {
        # update if reactive version of $date_a $date_b
        # or $clinicians are updated.
        self$list_contact_45_74(lazy = TRUE)
        # re-calculates the counts
      }
    )
  )
)



##### age 75+ fields ###########################################################
# used for Practice Incentive Program Quality Improvement Measures
# cardiovascular risk (QIM 08). note that this age group is by default EXCLUDED

.public(
  dMeasure, "contact_75plus_list",
  data.frame(
    Patient = character(),
    InternalID = integer(),
    Count = integer(),
    Latest = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    stringsAsFactors = FALSE
  )
)
# filtered by chosen dates and clinicians and number of contacts

##### age 75 plus years methods ##########################################################
#' List of those aged 75 years or more (as of 'date_to') in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param min_date most recent contact must be at most max_date. default is $contact_maxDate, initially Sys.Date()
#' @param contact_type contact types which are accepted. default is $contact_type
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
#' @export
list_contact_75plus <- function(dMeasure_obj,
                                date_from = NA,
                                date_to = NA,
                                clinicians = NA,
                                min_contact = NA,
                                min_date = NA, max_date = NA,
                                contact_type = NA,
                                lazy = FALSE) {
  dMeasure_obj$list_contact_75plus(
    date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    lazy
  )
}

.public(dMeasure, "list_contact_75plus", function(date_from = NA,
                                                  date_to = NA,
                                                  clinicians = NA,
                                                  min_contact = NA,
                                                  min_date = NA, max_date = NA,
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
  if (is.na(max_date)) {
    max_date <- self$contact_maxDate
  }
  if (is.na(contact_type[[1]])) {
    contact_type <- self$contact_type
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (self$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {
      log_id <- self$config_db$write_log_db(
        query = "75plus_contact",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (!lazy) {
      self$list_contact_count(
        date_from, date_to, clinicians,
        min_contact, min_date, max_date, contact_type,
        lazy
      )
    }

    seventyfiveplusID <- self$contact_count_list %>>%
      dplyr::mutate(Date = date_to) %>>%
      self$seventyfiveplus_list()

    self$contact_75plus_list <- self$contact_count_list %>>%
      dplyr::filter(InternalID %in% seventyfiveplusID)

    if (self$Log) {
      self$config_db$duration_log_db(log_id)
    }
  }

  return(self$contact_75plus_list)
})
.reactive_event(
  dMeasure, "contact_75plus_listR",
  quote(
    shiny::eventReactive(
      c(self$contact_count_listR()), {
        # update if reactive version of $date_a $date_b
        # or $clinicians are updated.
        self$list_contact_75plus(lazy = TRUE)
        # re-calculates the counts
      }
    )
  )
)


##### age 35 to 44 ATSI fields ###########################################################
# used for Practice Incentive Program Quality Improvement Measures
# cardiovascular risk (QIM 08). note that this age group is by default INCLUDED

.public(
  dMeasure, "contact_ATSI_35_44_list",
  data.frame(
    Patient = character(),
    InternalID = integer(),
    Count = integer(),
    Latest = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    stringsAsFactors = FALSE
  )
)
# filtered by chosen dates and clinicians and number of contacts

##### age 35 to 44 years and ATSI methods ##########################################################
#' List of those aged 35 years to 44 years and ATSI (as of 'date_to') in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param max_date most recent contact must be at most max_date. default is $contact_maxDate, initially Sys.Date()
#' @param contact_type contact types which are accepted. default is $contact_type
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
#' @export
list_contact_ATSI_35_44 <- function(dMeasure_obj,
                                    date_from = NA,
                                    date_to = NA,
                                    clinicians = NA,
                                    min_contact = NA,
                                    min_date = NA, max_date = NA,
                                    contact_type = NA,
                                    lazy = FALSE) {
  dMeasure_obj$list_contact_ATSI_35_44(
    date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    lazy
  )
}

.public(dMeasure, "list_contact_ATSI_35_44", function(date_from = NA,
                                                      date_to = NA,
                                                      clinicians = NA,
                                                      min_contact = NA,
                                                      min_date = NA, max_date = NA,
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
  if (is.na(max_date)) {
    max_date <- self$contact_maxDate
  }
  if (is.na(contact_type[[1]])) {
    contact_type <- self$contact_type
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (self$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {
      log_id <- self$config_db$write_log_db(
        query = "ATSI_35_44_contact",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (!lazy) {
      self$list_contact_count(
        date_from, date_to, clinicians,
        min_contact, min_date, max_date, contact_type,
        lazy
      )
    }

    ATSI_35_44ID <- self$contact_count_list %>>%
      dplyr::mutate(Date = date_to) %>>%
      self$ATSI_35_44_list()

    self$contact_ATSI_35_44_list <- self$contact_count_list %>>%
      dplyr::filter(InternalID %in% ATSI_35_44ID)

    if (self$Log) {
      self$config_db$duration_log_db(log_id)
    }
  }

  return(self$contact_ATSI_35_44_list)
})
.reactive_event(
  dMeasure, "contact_ATSI_35_44_listR",
  quote(
    shiny::eventReactive(
      c(self$contact_count_listR()), {
        # update if reactive version of $date_a $date_b
        # or $clinicians are updated.
        self$list_contact_ATSI_35_44(lazy = TRUE)
        # re-calculates the counts
      }
    )
  )
)




##### cervical screening (cst) eligible fields ###########################################
# used for Practice Incentive Program Quality Improvement Measures
# cervical screening (QIM 09)
# aged 25 to 74 years inclusive
# female
# no history of hysterectomy

.public(
  dMeasure, "contact_cst_list",
  data.frame(
    Patient = character(),
    InternalID = integer(),
    Count = integer(),
    Latest = as.Date(integer(0),
      origin = "1970-01-01"
    ),
    stringsAsFactors = FALSE
  )
)
# filtered by chosen dates and clinicians and number of contacts

##### cervical screening (cst) methods ##########################################################
#' List of those eligible (as of 'date_to') for cervical screeningin the contact list
#'
#' Age 25 to 74 years inclusive (as of 'date_to')
#' Female
#' No history of hysterectomy (except sub-total hysterectomy)
#' -- currently does not take regard of the recorded date of the hysterectomy
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param min_date most recent contact must be at most max_date. default is $contact_maxDate, initially Sys.Date()
#' @param contact_type contact types which are accepted. default is $contact_type
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
#' @export
list_contact_cst <- function(dMeasure_obj,
                             date_from = NA,
                             date_to = NA,
                             clinicians = NA,
                             min_contact = NA,
                             min_date = NA, max_date = NA,
                             contact_type = NA,
                             lazy = FALSE) {
  dMeasure_obj$list_contact_cst(
    date_from, date_to, clinicians,
    min_contact, min_date, max_date, contact_type,
    lazy
  )
}

.public(dMeasure, "list_contact_cst", function(date_from = NA,
                                               date_to = NA,
                                               clinicians = NA,
                                               min_contact = NA,
                                               min_date = NA, max_date = NA,
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
  if (is.na(max_date)) {
    max_date <- self$contact_maxDate
  }
  if (is.na(contact_type[[1]])) {
    contact_type <- self$contact_type
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (self$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {
      log_id <- self$config_db$write_log_db(
        query = "cst_contact",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (!lazy) {
      self$list_contact_count(
        date_from, date_to, clinicians,
        min_contact, min_date, max_date, contact_type,
        lazy
      )
    }

    cstID <- self$contact_count_list %>>%
      dplyr::mutate(Date = date_to) %>>%
      self$cst_eligible_list()

    self$contact_cst_list <- self$contact_count_list %>>%
      dplyr::filter(InternalID %in% cstID)

    if (self$Log) {
      self$config_db$duration_log_db(log_id)
    }
  }

  return(self$contact_cst_list)
})
.reactive_event(
  dMeasure, "contact_cst_listR",
  quote(
    shiny::eventReactive(
      c(self$contact_count_listR()), {
        # update if reactive version of $date_a $date_b
        # or $clinicians are updated.
        self$list_contact_cst(lazy = TRUE)
        # re-calculates the counts
      }
    )
  )
)
