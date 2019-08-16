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

.public(dMeasure, "contact_diabetes_list",
        data.frame(Patient = character(),
                   InternalID = integer(),
                   Count = integer(),
                   Latest = as.Date(integer(0),
                                    origin = "1970-01-01"),
                   stringsAsFactors = FALSE))
# filtered by chosen dates and clinicians and number of contacts

##### diabetes methods ##########################################################
#' List of diabetics in the contact list
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
list_contact_diabetes <- function(dMeasure_obj,
                                  date_from = NA,
                                  date_to = NA,
                                  clinicians = NA,
                                  min_contact = NA,
                                  min_date = NA,
                                  lazy = FALSE) {
  dMeasure_obj$list_contact_diabetes(date_from, date_to, clinicians,
                                     min_contact, min_date,
                                     lazy)
}

.public(dMeasure, "list_contact_diabetes", function(date_from = NA,
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
      query = "diabetes_contact",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_contact_count(date_from, date_to, clinicians,
                              min_contact, min_date,
                              lazy)
    }

    diabetesID <- self$contact_count_list %>>%
      dplyr::mutate(Date = Latest) %>>%
      self$diabetes_list()

    self$contact_diabetes_list <- self$contact_count_list %>>%
      dplyr::filter(InternalID %in% diabetesID)

    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$contact_diabetes_list)
})
.reactive_event(dMeasure, "contact_diabetes_listR",
                quote(
                  shiny::eventReactive(
                    c(self$contact_count_listR()), {
                      # update if reactive version of $date_a $date_b
                      # or $clinicians are updated.
                      self$list_diabetes_contact(lazy = TRUE)
                      # re-calculates the counts
                    })
                ))


##### chronic lung disease fields ###########################################################

.public(dMeasure, "contact_chroniclungdisease_list",
        data.frame(Patient = character(),
                   InternalID = integer(),
                   Count = integer(),
                   Latest = as.Date(integer(0),
                                    origin = "1970-01-01"),
                   stringsAsFactors = FALSE))
# filtered by chosen dates and clinicians and number of contacts

##### chronic lung disease methods ##########################################################
#' List of chronic lung disease in the contact list
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
list_contact_chroniclungdisease <- function(dMeasure_obj,
                                            date_from = NA,
                                            date_to = NA,
                                            clinicians = NA,
                                            min_contact = NA,
                                            min_date = NA,
                                            lazy = FALSE) {
  dMeasure_obj$list_contact_chroniclungdisease(date_from, date_to, clinicians,
                                               min_contact, min_date,
                                               lazy)
}

.public(dMeasure, "list_contact_chroniclungdisease", function(date_from = NA,
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
      query = "chroniclungdisease_contact",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_contact_count(date_from, date_to, clinicians,
                              min_contact, min_date,
                              lazy)
    }

    chroniclungdiseaseID <- self$contact_count_list %>>%
      dplyr::mutate(Date = Latest) %>>%
      self$chroniclungdisease_list()

    self$contact_chroniclungdisease_list <- self$contact_count_list %>>%
      dplyr::filter(InternalID %in% chroniclungdiseaseID)

    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$contact_chroniclungdisease_list)
})
.reactive_event(dMeasure, "contact_chroniclungdisease_listR",
                quote(
                  shiny::eventReactive(
                    c(self$contact_count_listR()), {
                      # update if reactive version of $date_a $date_b
                      # or $clinicians are updated.
                      self$list_chroniclungdisease_contact(lazy = TRUE)
                      # re-calculates the counts
                    })
                ))


##### age 15 plus fields ###########################################################
# used for Practice Incentive Program Quality Improvement Measures
# Smoking, BMI, Alcohol

.public(dMeasure, "contact_15plus_list",
        data.frame(Patient = character(),
                   InternalID = integer(),
                   Count = integer(),
                   Latest = as.Date(integer(0),
                                    origin = "1970-01-01"),
                   stringsAsFactors = FALSE))
# filtered by chosen dates and clinicians and number of contacts

##### age 15 plus methods ##########################################################
#' List of those aged 15 plus (as of 'date_to') in the contact list
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
list_contact_15plus <- function(dMeasure_obj,
                                date_from = NA,
                                date_to = NA,
                                clinicians = NA,
                                min_contact = NA,
                                min_date = NA,
                                lazy = FALSE) {
  dMeasure_obj$list_contact_15plus(date_from, date_to, clinicians,
                                   min_contact, min_date,
                                   lazy)
}

.public(dMeasure, "list_contact_15plus", function(date_from = NA,
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
      query = "15plus_contact",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_contact_count(date_from, date_to, clinicians,
                              min_contact, min_date,
                              lazy)
    }

    fifteenplusID <- self$contact_count_list %>>%
      dplyr::mutate(Date = date_to) %>>%
      self$fifteenplus_list()

    self$contact_15plus_list <- self$contact_count_list %>>%
      dplyr::filter(InternalID %in% fifteenplusID)

    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$contact_15plus_list)
})
.reactive_event(dMeasure, "contact_15plus_listR",
                quote(
                  shiny::eventReactive(
                    c(self$contact_count_listR()), {
                      # update if reactive version of $date_a $date_b
                      # or $clinicians are updated.
                      self$list_15plus_contact(lazy = TRUE)
                      # re-calculates the counts
                    })
                ))



##### age 65 plus fields ###########################################################
# used for Practice Incentive Program Quality Improvement Measures
# influenza (QIM 04)

.public(dMeasure, "contact_65plus_list",
        data.frame(Patient = character(),
                   InternalID = integer(),
                   Count = integer(),
                   Latest = as.Date(integer(0),
                                    origin = "1970-01-01"),
                   stringsAsFactors = FALSE))
# filtered by chosen dates and clinicians and number of contacts

##### age 65 plus methods ##########################################################
#' List of those aged 65 plus (as of 'date_to') in the contact list
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
list_contact_65plus <- function(dMeasure_obj,
                                date_from = NA,
                                date_to = NA,
                                clinicians = NA,
                                min_contact = NA,
                                min_date = NA,
                                lazy = FALSE) {
  dMeasure_obj$list_contact_65plus(date_from, date_to, clinicians,
                                   min_contact, min_date,
                                   lazy)
}

.public(dMeasure, "list_contact_65plus", function(date_from = NA,
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
      query = "65plus_contact",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_contact_count(date_from, date_to, clinicians,
                              min_contact, min_date,
                              lazy)
    }

    sixtyfiveplusID <- self$contact_count_list %>>%
      dplyr::mutate(Date = date_to) %>>%
      self$sixtyfiveplus_list()

    self$contact_65plus_list <- self$contact_count_list %>>%
      dplyr::filter(InternalID %in% sixtyfiveplusID)

    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$contact_65plus_list)
})
.reactive_event(dMeasure, "contact_65plus_listR",
                quote(
                  shiny::eventReactive(
                    c(self$contact_count_listR()), {
                      # update if reactive version of $date_a $date_b
                      # or $clinicians are updated.
                      self$list_65plus_contact(lazy = TRUE)
                      # re-calculates the counts
                    })
                ))



##### age 45 to 74 years fields ###########################################################
# used for Practice Incentive Program Quality Improvement Measures
# influenza (QIM 04)

.public(dMeasure, "contact_45_74_list",
        data.frame(Patient = character(),
                   InternalID = integer(),
                   Count = integer(),
                   Latest = as.Date(integer(0),
                                    origin = "1970-01-01"),
                   stringsAsFactors = FALSE))
# filtered by chosen dates and clinicians and number of contacts

##### age 45 to 74 years methods ##########################################################
#' List of those aged 45 to 74 years (as of 'date_to') in the contact list
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
list_contact_45_74 <- function(dMeasure_obj,
                               date_from = NA,
                               date_to = NA,
                               clinicians = NA,
                               min_contact = NA,
                               min_date = NA,
                               lazy = FALSE) {
  dMeasure_obj$list_contact_45_74(date_from, date_to, clinicians,
                                  min_contact, min_date,
                                  lazy)
}

.public(dMeasure, "list_contact_45_74", function(date_from = NA,
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
      query = "45_74_contact",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_contact_count(date_from, date_to, clinicians,
                              min_contact, min_date,
                              lazy)
    }

    fortyfiveseventyfourID <- self$contact_count_list %>>%
      dplyr::mutate(Date = date_to) %>>%
      self$fortyfiveseventyfour_list()

    self$contact_45_74_list <- self$contact_count_list %>>%
      dplyr::filter(InternalID %in% fortyfiveseventyfourID)

    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$contact_45_74_list)
})
.reactive_event(dMeasure, "contact_45_74_listR",
                quote(
                  shiny::eventReactive(
                    c(self$contact_count_listR()), {
                      # update if reactive version of $date_a $date_b
                      # or $clinicians are updated.
                      self$list_45_74_contact(lazy = TRUE)
                      # re-calculates the counts
                    })
                ))

