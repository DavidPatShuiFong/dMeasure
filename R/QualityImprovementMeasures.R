#' QIM - Practice Incentive Program Quality Improvement Measures fields and methods
#'
#' @name qim
#' @title dMeasure Quality Improvement Measures
#'
#' @include dMeasure.R
#' needs the '.public' function from dMeasure.R
#' @include contact.R
#' needs to know contact lists
NULL

#####  ignore 'old' observations results? ############################################
# if TRUE, ignore results that are too old to be qualify for quality improvement measure

.private(dMeasure, ".qim_ignoreOld", TRUE)
.active(dMeasure, "qim_ignoreOld", function(value) {
  # minimum number of contacts listed in $list_contact_count
  if (missing(value)) {
    return(private$.qim_ignoreOld)
  }
  if (is.logical(value)) {
    private$.qim_ignoreOld <- value
    private$set_reactive(self$qim_ignoreOldR, value)
  } else {
    warning("$qim_ignoreOld only accepts logical values (TRUE/FALSE).")
  }
})
.reactive(dMeasure, "qim_ignoreOldR", TRUE)

##### demographic groupings for reporting ############################################

.active(dMeasure, "qim_demographicGroupings", function(value) {
  if (!missing(value)) {
    warning("$qim_demographicGroupings is read-only.")
  } else {
    return(c("Age5", "Sex", "Ethnicity", "MaritalStatus", "Sexuality"))
    # vector of valid demographic groups (for QIM reporting)
    # Age in 5 year categories
    # Ethnicity
    # MaritalStatus
    # Sexuality
  }
})

.private_init(dMeasure, ".qim_demographicGroup", quote(self$qim_demographicGroupings))
.active(dMeasure, "qim_demographicGroup", function(value) {
  # minimum number of contacts listed in $list_contact_count
  if (missing(value)) {
    return(private$.qim_demographicGroup)
  }
  value <- intersect(value, self$qim_demographicGroupings)
  # make sure groups are valid
  private$.qim_demographicGroup <- value
  private$set_reactive(self$qim_demographicGroupR, value)
})
.reactive(dMeasure, "qim_demographicGroupR", quote(self$qim_demographicGroupings))


##### QIM active fields #############################################################

.public(dMeasure, "qim_active_list",
        data.frame(Patient = character(),
                   RecordNo = character(),
                   Age5 = integer(),
                   Sex = character(),
                   Ethnicity = character(),
                   MaritalStatus = character(),
                   Sexuality = character(),
                   stringsAsFactors = FALSE))
# filtered by chosen dates and clinicians and number of contacts

##### QIM active methods ##########################################################
#' List of active patients, in the contact list
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
#' @param lazy recalculate the diabetes contact list?
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
list_qim_active <- function(dMeasure_obj,
                            date_from = NA,
                            date_to = NA,
                            clinicians = NA,
                            min_contact = NA,
                            min_date = NA,
                            contact_type = NA,
                            lazy = FALSE) {
  dMeasure_obj$list_qim_active(date_from, date_to, clinicians,
                               min_contact, min_date, contact_type,
                               lazy)
}
.public(dMeasure, "list_qim_active", function(date_from = NA,
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
      query = "active_qim",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_contact_count(date_from, date_to, clinicians,
                              min_contact, min_date, contact_type,
                              lazy)
    }

    activeID <- self$contact_count_list %>>%
      dplyr::pull(InternalID) %>>%
      c(-1) # add a dummy ID to prevent empty vector

    self$qim_active_list <- self$contact_count_list %>>%
      dplyr::select(-c(Count, Latest)) %>>% # don't need these fields
      dplyr::left_join(private$db$patients %>>%
                         dplyr::filter(InternalID %in% activeID) %>>%
                         dplyr::select(InternalID, DOB, Sex, Ethnicity),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(private$db$clinical %>>%
                         dplyr::filter(InternalID %in% activeID) %>>%
                         dplyr::select(InternalID, MaritalStatus, Sexuality),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::mutate(Age5 = floor(dMeasure::calc_age(as.Date(DOB), date_to) / 5) * 5) %>>%
      # round age group to nearest 5 years
      dplyr::select(-DOB) %>>%
      dplyr::left_join(private$db$patients %>>%
                         dplyr::filter(InternalID %in% activeID) %>>%
                         dplyr::select(InternalID, RecordNo),
                       by = "InternalID", # add RecordNo
                       copy = TRUE) %>>%
      dplyr::select(-InternalID) # drop the InternalID

    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$qim_active_list)
})
.reactive_event(dMeasure, "qim_active_listR",
                quote(
                  shiny::eventReactive(
                    c(self$contact_count_listR()), {
                      # update if reactive version of $date_a $date_b
                      # or $clinicians are updated.
                      self$list_qim_active(lazy = TRUE)
                      # re-calculates the counts
                    })
                ))


.public(dMeasure, "qim_active_report",
        data.frame(NULL,
                   stringsAsFactors = FALSE))
# empty data frame, number of columns dynamically change

#' Quality Improvement Measure report, in the contact list. Active contacts
#'
#' Filtered by date, and chosen clinicians
#'
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param demographic demographic groupings for reporting.
#'  if not supplied, reads $qim_demographicGroup
#'  list of available demographic groups in $qim_demographicGroupings
#' @param lazy recalculate the diabetes contact list?
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
report_qim_active <- function(dMeasure_obj,
                              date_from = NA,
                              date_to = NA,
                              clinicians = NA,
                              min_contact = NA,
                              min_date = NA,
                              contact_type = NA,
                              demographic = NA,
                              lazy = FALSE) {
  dMeasure_obj$report_qim_active(date_from, date_to, clinicians,
                                 min_contact, min_date,
                                 contact_type,
                                 demographic,
                                 lazy)
}

.public(dMeasure, "report_qim_active", function(date_from = NA,
                                                date_to = NA,
                                                clinicians = NA,
                                                min_contact = NA,
                                                min_date = NA,
                                                contact_type = NA,
                                                demographic = NA,
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
  if (is.na(demographic)) {
    demographic <- self$qim_demographicGroup
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {log_id <- private$config_db$write_log_db(
      query = "qim_active_report",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_qim_active(date_from, date_to, clinicians,
                           min_contact, min_date, contact_type,
                           lazy)
    }

    report_groups <- c(demographic, "")
    # group by demographic groupings
    # add a dummy string in case there are no demographic chosen!

    self$qim_active_report <- self$qim_active_list %>>%
      dplyr::group_by_at(report_groups) %>>%
      # group_by_at takes a vector of strings
      dplyr::summarise(n = n()) %>>%
      dplyr::ungroup() %>>%
      {dplyr::select(., intersect(names(.), c(report_groups, "n")))}
    # if no rows, then grouping will not remove unnecessary columns,
    # so


    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$qim_active_report)
})
.reactive_event(dMeasure, "qim_active_reportR",
                quote(
                  shiny::eventReactive(
                    c(self$qim_active_listR(),
                      self$qim_demographicGroupR()), {
                        # update if reactive version of $date_a $date_b
                        # or $clinicians are updated.
                        # or change in demographic grouping
                        self$report_qim_active(lazy = TRUE)
                        # re-calculates the counts
                      })
                ))


##### QIM diabetes fields ###########################################################

.public(dMeasure, "qim_diabetes_list",
        data.frame(Patient = character(),
                   RecordNo = character(),
                   Age5 = integer(),
                   Sex = character(),
                   Ethnicity = character(),
                   MaritalStatus = character(),
                   Sexuality = character(),
                   HbA1CDate = as.Date(integer(0),
                                       origin = "1970-01-01"),
                   HbA1CValue = double(),
                   HbA1CUnits = character(),
                   FluvaxDate = as.Date(integer(0),
                                        origin = "1970-01-01"),
                   FluvaxName = character(),
                   BPDate = as.Date(integer(0),
                                    origin = "1970-01-01"),
                   BP = character(),
                   stringsAsFactors = FALSE))
# filtered by chosen dates and clinicians and number of contacts

##### QIM diabetes methods ##########################################################
#' List of diabetics, with Quality Improvement Measures, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' QIM 01 - HbA1C - most recent. the QIM measure is within last twelve months
#' QIM 05 - Influenza immunization - most recent. the QIM measure is within last 15 months
#' QIM 10 - Blood pressure - most recent. the QIM measure is within the last six months
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param contact_type contact types which are accepted. default is $contact_type
#' @param ignoreOld ignore results/observatioins that don't qualify for quality improvement measures
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the diabetes contact list?
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
list_qim_diabetes <- function(dMeasure_obj,
                              date_from = NA,
                              date_to = NA,
                              clinicians = NA,
                              min_contact = NA,
                              min_date = NA,
                              contact_type = NA,
                              ignoreOld = NA,
                              lazy = FALSE) {
  dMeasure_obj$list_qim_diabetes(date_from, date_to, clinicians,
                                 min_contact, min_date, contact_type,
                                 ignoreOld,
                                 lazy)
}

.public(dMeasure, "list_qim_diabetes", function(date_from = NA,
                                                date_to = NA,
                                                clinicians = NA,
                                                min_contact = NA,
                                                min_date = NA,
                                                contact_type = NA,
                                                ignoreOld = NA,
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
  if (is.na(ignoreOld)) {
    ignoreOld <- self$qim_ignoreOld
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {log_id <- private$config_db$write_log_db(
      query = "diabetes_qim",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_contact_diabetes(date_from, date_to, clinicians,
                                 min_contact, min_date,
                                 contact_type,
                                 lazy)
    }

    diabetesID <- self$contact_diabetes_list %>>%
      dplyr::pull(InternalID) %>>%
      c(-1) # add a dummy ID to prevent empty vector


    fluvaxList <- self$influenzaVax_obs(diabetesID,
                                        date_from = ifelse(ignoreOld,
                                                           NA,
                                                           as.Date(-Inf, origin = "1970-01-01")),
                                        # if ignoreOld, then influenza_vax will (given NA)
                                        # calculate date_from as fifteen months before date_to
                                        date_to = date_to)
    # returns InternalID, FluVaxName, FluvaxDate

    HbA1CList <- self$HbA1C_obs(diabetesID,
                                date_from = ifelse(ignoreOld,
                                                   NA,
                                                   as.Date(-Inf, origin = "1970-01-01")),
                                # if ignoreOld, then influenza_vax will (given NA)
                                # calculate date_from as fifteen months before date_to
                                date_to = date_to)
    # returns dataframe of InternalID, HbA1CDate, HbA1CValue, HbA1CUnits

    BPList <- self$BloodPressure_obs(diabetesID,
                                     date_from = ifelse(ignoreOld,
                                                        NA,
                                                        as.Date(-Inf, origin = "1970-01-01")),
                                     # if ignoreOld, then influenza_vax will (given NA)
                                     # calculate date_from as fifteen months before date_to
                                     date_to = date_to)
    # returns dataframe of InternalID, BPDate, BPValue

    self$qim_diabetes_list <- self$contact_diabetes_list %>>%
      dplyr::select(-c(Count, Latest)) %>>% # don't need these fields
      dplyr::left_join(HbA1CList,
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::mutate(HbA1CValue = as.double(HbA1CValue)) %>>%
      # was a character. can't be converted to double within the MSSQL query
      dplyr::left_join(fluvaxList,
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(BPList,
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(private$db$patients %>>%
                         dplyr::filter(InternalID %in% diabetesID) %>>%
                         dplyr::select(InternalID, DOB, Sex, Ethnicity, RecordNo),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(private$db$clinical %>>%
                         dplyr::filter(InternalID %in% diabetesID) %>>%
                         dplyr::select(InternalID, MaritalStatus, Sexuality),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::mutate(Age5 = floor(dMeasure::calc_age(as.Date(DOB), date_to) / 5) * 5) %>>%
      # round age group to nearest 5 years
      dplyr::select(-c(DOB, InternalID))

    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$qim_diabetes_list)
})
.reactive_event(dMeasure, "qim_diabetes_listR",
                quote(
                  shiny::eventReactive(
                    c(self$contact_diabetes_listR(),
                      self$qim_ignoreOldR()), {
                        # update if reactive version of $date_a $date_b
                        # or $clinicians are updated.
                        self$list_qim_diabetes(lazy = TRUE)
                        # re-calculates the counts
                      })
                ))


.active(dMeasure, "qim_diabetes_measureTypes", function(value) {
  if (!missing(value)) {
    warning("$qim_diabetes_measureTypes is read-only.")
  } else {
    return(c("HbA1C", "Influenza", "BP"))
    # vector of valid QIM measures for diabetes (for QIM reporting)
    # QIM 01 - HbA1C
    # QIM 05 - Influenza
    # QIM 10 - Blood pressure
  }
})

.private_init(dMeasure, ".qim_diabetes_measure", quote(self$qim_diabetes_measureTypes))
.active(dMeasure, "qim_diabetes_measure", function(value) {
  # minimum number of contacts listed in $list_contact_count
  if (missing(value)) {
    return(private$.qim_diabetes_measure)
  }
  value <- intersect(value, self$qim_diabetes_measureTypes)
  # only valid measure types allowed
  private$.qim_diabetes_measure <- value
  private$set_reactive(self$qim_diabetes_measureR, value)
})
.reactive(dMeasure, "qim_diabetes_measureR", quote(self$qim_diabetes_measureTypes))

.public(dMeasure, "qim_diabetes_report",
        data.frame(NULL,
                   stringsAsFactors = FALSE))
# empty data frame, number of columns dynamically change

#' Diabetes Quality Improvement Measure report, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' Shows chosen QIM measures, and by demographic grouping
#'
#' QIM 01 - HbA1C - most recent. the QIM measure is within last twelve months
#' QIM 05 - Influenza immunization - most recent. the QIM measure is within last 15 months
#' QIM 10 - Blood pressure - most recent. the QIM measure is within the last six monthe
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param contact_type contact types which are accepted. default is $contact_type
#' @param demographic demographic groupings for reporting.
#'  if not supplied, reads $qim_demographicGroup
#'  list of available demographic groups in $qim_demographicGroupings
#' @param measure measures to report
#'  if not supplied, reads $qim_diabetes_measure
#'  list of available measures in $qim_diabetes_measureTypes
#'  currently 'HbA1C', 'Influenza' and 'BP'
#' @param ignoreOld ignore results/observatioins that don't qualify for quality improvement measures
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the diabetes contact list?
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
report_qim_diabetes <- function(dMeasure_obj,
                                date_from = NA,
                                date_to = NA,
                                clinicians = NA,
                                min_contact = NA,
                                contact_type = NA,
                                min_date = NA,
                                demographic = NA,
                                measure = NA,
                                ignoreOld = NA,
                                lazy = FALSE) {
  dMeasure_obj$report_qim_diabetes(date_from, date_to, clinicians,
                                   min_contact, min_date, contact_type,
                                   demographic, measure,
                                   ignoreOld, lazy)
}

.public(dMeasure, "report_qim_diabetes", function(date_from = NA,
                                                  date_to = NA,
                                                  clinicians = NA,
                                                  min_contact = NA,
                                                  min_date = NA,
                                                  contact_type = NA,
                                                  demographic = NA,
                                                  measure = NA,
                                                  ignoreOld = NA,
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
  if (is.na(demographic)) {
    demographic <- self$qim_demographicGroup
  }
  if (is.na(measure)) {
    measure <- self$qim_diabetes_measure
  }
  if (is.na(ignoreOld)) {
    ignoreOld <- self$qim_ignoreOld
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {log_id <- private$config_db$write_log_db(
      query = "qim_diabetes_report",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_qim_diabetes(date_from, date_to, clinicians,
                             min_contact, min_date, contact_type,
                             ignoreOld, lazy)
    }

    measure <- dplyr::recode(measure,
                             'HbA1C' = 'HbA1CDone',
                             'Influenza' = 'InfluenzaDone',
                             'BP' = 'BPDone')
    report_groups <- c(demographic, measure, "")
    # group by both demographic groupings and measures of interest
    # add a dummy string in case there are no demographic or measure groups chosen!

    self$qim_diabetes_report <- self$qim_diabetes_list %>>%
      dplyr::mutate(HbA1CDone = !is.na(HbA1CDate),
                    InfluenzaDone = !is.na(FluvaxDate),
                    BPDone = !is.na(BPDate)) %>>%
      # a measure is 'done' if it exists (not NA)
      # if ignoreOld = TRUE, the the observation must fall within
      #  the required timeframe
      dplyr::group_by_at(report_groups) %>>%
      # group_by_at takes a vector of strings
      dplyr::summarise(n = n()) %>>%
      dplyr::ungroup() %>>%
      {dplyr::select(., intersect(names(.), c(report_groups, "n")))}
    # if no rows, then grouping will not remove unnecessary columns

    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$qim_diabetes_report)
})
.reactive_event(dMeasure, "qim_diabetes_reportR",
                quote(
                  shiny::eventReactive(
                    c(self$qim_diabetes_listR(),
                      self$qim_demographicGroupR(),
                      self$qim_diabetes_measureR()), {
                        # update if reactive version of $date_a $date_b
                        # or $clinicians are updated.
                        # or change in demographic grouping
                        # or change in measures
                        self$report_qim_diabetes(lazy = TRUE)
                        # re-calculates the counts
                      })
                ))


##### QIM cervical screening test (CST) fields ###########################################################

.public(dMeasure, "qim_cst_list",
        data.frame(Patient = character(),
                   RecordNo = character(),
                   Age5 = integer(),
                   Sex = character(),
                   Ethnicity = character(),
                   MaritalStatus = character(),
                   Sexuality = character(),
                   CSTDate = as.Date(integer(0),
                                     origin = "1970-01-01"),
                   CSTName = character(),
                   # CStName is expected to be 'CST' or 'PAP', but might
                   # but could be a longer string containing 'Pap' if sourced from 'Investigations' table
                   stringsAsFactors = FALSE))
# filtered by chosen dates and clinicians and number of contacts

##### QIM cervical screening (CST) methods ##########################################################
#' List of CST eligible patients with Quality Improvement Measures, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' QIM 09 -Proportion of female patients with an up-to-date cervical screening
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param contact_type contact types which are accepted. default is $contact_type
#' @param ignoreOld ignore results/observatioins that don't qualify for quality improvement measures
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the diabetes contact list?
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent CST 'observation' (test) date and name
list_qim_cst <- function(dMeasure_obj,
                         date_from = NA,
                         date_to = NA,
                         clinicians = NA,
                         min_contact = NA,
                         min_date = NA,
                         contact_type = NA,
                         ignoreOld = NA,
                         lazy = FALSE) {
  dMeasure_obj$list_qim_cst(date_from, date_to, clinicians,
                            min_contact, min_date, contact_type,
                            ignoreOld,
                            lazy)
}

.public(dMeasure, "list_qim_cst", function(date_from = NA,
                                           date_to = NA,
                                           clinicians = NA,
                                           min_contact = NA,
                                           min_date = NA,
                                           contact_type = NA,
                                           ignoreOld = NA,
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
  if (is.na(ignoreOld)) {
    ignoreOld <- self$qim_ignoreOld
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {
      log_id <- private$config_db$write_log_db(
        query = "cst_qim",
        data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_contact_cst(date_from, date_to, clinicians,
                            min_contact, min_date,
                            contact_type,
                            lazy)}

    screen_cst_id <- self$contact_cst_list %>>%
      dplyr::pull(InternalID) %>>%
      c(-1) # add a dummy ID to prevent empty vector

    self$qim_cst_list <- self$contact_cst_list %>>%
      dplyr::select(-c(Count, Latest)) %>>% # don't need these fields
      dplyr::left_join(
        dplyr::bind_rows(
          private$db$papsmears %>>%
            # attach reports in papsmears table
            dplyr::filter(InternalID %in% screen_cst_id) %>>%
            dplyr::rename(TestDate = PapDate,
                          TestName = CSTType) %>>%
            dplyr::select(InternalID, TestDate, TestName) %>>%
            dplyr::collect(),
          private$db$investigations %>>%
            # some reports might be in investigations e.g. scanned in
            dplyr::filter(InternalID %in% screen_cst_id &&
                            (TestName %like% "%CERVICAL SCREENING%" ||
                               TestName %like% "%PAP SMEAR%")) %>>%
            dplyr::rename(TestDate = Reported,
                          TestName = TestName) %>>%
            dplyr::select(InternalID, TestDate, TestName) %>>%
            dplyr::collect()),
        by = "InternalID",
        copy = TRUE) %>>%
      dplyr::mutate(TestDate = as.Date(TestDate),
                    TestDate = as.Date(ifelse(TestDate > date_to,
                                              -Inf,
                                              TestDate),
                                       origin = "1970-01-01"),
                    TestDate = as.Date(ifelse(is.na(TestDate),
                                              -Inf,
                                              TestDate),
                                       origin = "1970-01-01")) %>>%
      # remove tests after the appointment date, and provide -Inf value to 'no test' patients
      dplyr::mutate(TestName = ifelse(TestDate == -Inf, NA, TestName)) %>>%
      # only test dates (and names) less than the joined appointment date are kept
      dplyr::group_by(InternalID) %>>%
      # group by patient ID (need most recent investigation for each patient)
      dplyr::filter(TestDate == max(TestDate, na.rm = TRUE)) %>>%
      dplyr::ungroup() %>>%
      dplyr::mutate(TestAge = dMeasure::interval(TestDate, date_to)$year) %>>%
      # 'current' time is date_to
      dplyr::mutate(OutOfDateTest =
                      dplyr::case_when(TestDate == -Inf ~ 1,
                                       # if no date (no detected test)
                                       TestAge < 2 ~ 3,
                                       # if less than two years, always 'up-to-date'
                                       TestAge >= 5 ~ 2,
                                       # if old (5 years for either cervical screening HPV or Pap)
                                       grepl('pap', TestName, ignore.case = TRUE) ~ 2,
                                       # otherwise if 'Pap' and more than two years
                                       # last case is 2 to 4 years (inclusive) and CST
                                       TRUE ~ 3)) %>>%
      (if (ignoreOld && nrow(.) > 0) {
        # remove out-of-date tests
        dplyr::mutate(.,
                      TestDate = dplyr::if_else(OutOfDateTest == 2,
                                                as.Date(-Inf, origin = "1970-01-01"),
                                                TestDate),
                      TestName = dplyr::if_else(OutOfDateTest == 2,
                                                as.character(NA),
                                                TestName))}
       else {.}) %>>%
      dplyr::select(-c(TestAge, OutOfDateTest)) %>>%
      # dplyr::select(-c(TestAge, OutOfDateTest)) %>>% # don't need these columns any more
      dplyr::left_join(private$db$patients %>>%
                         dplyr::filter(InternalID %in% screen_cst_id) %>>%
                         dplyr::select(InternalID, DOB, Sex, Ethnicity),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(private$db$clinical %>>%
                         dplyr::filter(InternalID %in% screen_cst_id) %>>%
                         dplyr::select(InternalID, MaritalStatus, Sexuality),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::mutate(Age5 = floor(dMeasure::calc_age(as.Date(DOB), date_to) / 5) * 5) %>>%
      # round age group to nearest 5 years
      dplyr::select(-DOB) %>>%
      dplyr::left_join(private$db$patients %>>%
                         dplyr::filter(InternalID %in% screen_cst_id) %>>%
                         dplyr::select(InternalID, RecordNo),
                       by = "InternalID", # add RecordNo
                       copy = TRUE) %>>%
      dplyr::select(-InternalID) %>>% # drop the InternalID
      dplyr::rename(CSTDate = TestDate,
                    CSTName = TestName)

    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$qim_cst_list)
})
.reactive_event(dMeasure, "qim_cst_listR",
                quote(
                  shiny::eventReactive(
                    c(self$contact_cst_listR(),
                      self$qim_ignoreOldR()), {
                        # update if reactive version of $date_a $date_b
                        # or $clinicians are updated.
                        self$list_qim_cst(lazy = TRUE)
                        # re-calculates the counts
                      })
                ))


.public(dMeasure, "qim_cst_report",
        data.frame(NULL,
                   stringsAsFactors = FALSE))
# empty data frame, number of columns dynamically change

#' Cervical Screening Test (CST) Quality Improvement Measure report, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' Shows chosen QIM measures, and by demographic grouping
#'
#' QIM 09 -Proportion of female patients with an up-to-date cervical screening
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param contact_type contact types which are accepted. default is $contact_type
#' @param demographic demographic groupings for reporting.
#'  if not supplied, reads $qim_demographicGroup
#'  list of available demographic groups in $qim_demographicGroupings
#' @param ignoreOld ignore results/observatioins that don't qualify for quality improvement measures
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the diabetes contact list?
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
report_qim_cst <- function(dMeasure_obj,
                           date_from = NA,
                           date_to = NA,
                           clinicians = NA,
                           min_contact = NA,
                           contact_type = NA,
                           min_date = NA,
                           demographic = NA,
                           ignoreOld = NA,
                           lazy = FALSE) {
  dMeasure_obj$report_qim_cst(date_from, date_to, clinicians,
                              min_contact, min_date, contact_type,
                              demographic,
                              ignoreOld, lazy)
}

.public(dMeasure, "report_qim_cst", function(date_from = NA,
                                             date_to = NA,
                                             clinicians = NA,
                                             min_contact = NA,
                                             min_date = NA,
                                             contact_type = NA,
                                             demographic = NA,
                                             ignoreOld = NA,
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
  if (is.na(demographic)) {
    demographic <- self$qim_demographicGroup
  }
  if (is.na(ignoreOld)) {
    ignoreOld <- self$qim_ignoreOld
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {log_id <- private$config_db$write_log_db(
      query = "qim_cst_report",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_qim_cst(date_from, date_to, clinicians,
                        min_contact, min_date, contact_type,
                        ignoreOld, lazy)
    }

    report_groups <- c(demographic, "CSTDone")
    # group by both demographic groupings and measure ('only CSTDate') of interest
    # add a dummy string in case there are no demographic groups chosen!

    self$qim_cst_report <- self$qim_cst_list %>>%
      dplyr::mutate(CSTDone = (CSTDate != -Inf)) %>>%
      # a measure is 'done' if it exists (not equal to Infinity)
      # if ignoreOld = TRUE, the the observation must fall within
      #  the required timeframe
      dplyr::group_by_at(report_groups) %>>%
      # group_by_at takes a vector of strings
      dplyr::summarise(n = n()) %>>%
      dplyr::ungroup() %>>%
      {dplyr::select(., intersect(names(.), c(report_groups, "n")))}
    # if no rows, then grouping will not remove unnecessary columns,
    # so


    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$qim_cst_report)
})
.reactive_event(dMeasure, "qim_cst_reportR",
                quote(
                  shiny::eventReactive(
                    c(self$qim_cst_listR(),
                      self$qim_demographicGroupR()), {
                        # update if reactive version of $date_a $date_b
                        # or $clinicians are updated.
                        # or change in demographic grouping
                        self$report_qim_cst(lazy = TRUE)
                        # re-calculates the counts
                      })
                ))



##### QIM 15+ fields ###########################################################
.public(dMeasure, "qim_15plus_list",
        data.frame(Patient = character(),
                   RecordNo = character(),
                   Age5 = integer(),
                   Sex = character(),
                   Ethnicity = character(),
                   MaritalStatus = character(),
                   Sexuality = character(),
                   WeightDate = as.Date(integer(0),
                                        origin = "1970-01-01"),
                   WeightValue = numeric(),
                   HeightDate = as.Date(integer(0),
                                        origin = "1970-01-01"),
                   HeightValue = numeric(),
                   BMIDate = as.Date(integer(0),
                                     origin = "1970-01-01"),
                   BMIValue = numeric(),
                   BMIClass = character(),
                   WaistDate = as.Date(integer(0),
                                       origin = "1970-01-01"),
                   WaistValue = numeric(),
                   SmokingDate = as.Date(integer(0),
                                         origin = "1970-01-01"),
                   SmokingStatus = character(),
                   AlcoholDate = as.Date(integer(0),
                                         origin = "1970-01-01"),
                   NonDrinker = character(),
                   AlcoholDaysPerWeek = numeric(),
                   AlcoholDrinksPerDay = numeric(),
                   AlcoholDescription = character(),
                   PastAlcoholLevel = character(),
                   YearStarted = integer(),
                   YearStopped = integer(),
                   AlcoholComment = character(),
                   stringsAsFactors = FALSE))
# filtered by chosen dates and clinicians and number of contacts

##### QIM diabetes methods ##########################################################
#' List of diabetics, with Quality Improvement Measures, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' QIM 02 - Proportion of patients with a smoking status result
#' QIM 03 - Proportion of patients with a weight classification (12 months)
#' QIM 07 - Proportionof patients with an alcohol consumption status
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param contact_type contact types which are accepted. default is $contact_type
#' @param ignoreOld ignore results/observatioins that don't qualify for quality improvement measures
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the diabetes contact list?
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
list_qim_15plus <- function(dMeasure_obj,
                            date_from = NA,
                            date_to = NA,
                            clinicians = NA,
                            min_contact = NA,
                            min_date = NA,
                            contact_type = NA,
                            ignoreOld = NA,
                            lazy = FALSE) {
  dMeasure_obj$list_qim_15plus(date_from, date_to, clinicians,
                               min_contact, min_date, contact_type,
                               ignoreOld,
                               lazy)
}

.public(dMeasure, "list_qim_15plus", function(date_from = NA,
                                              date_to = NA,
                                              clinicians = NA,
                                              min_contact = NA,
                                              min_date = NA,
                                              contact_type = NA,
                                              ignoreOld = NA,
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
  if (is.na(ignoreOld)) {
    ignoreOld <- self$qim_ignoreOld
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {log_id <- private$config_db$write_log_db(
      query = "fifteenplus_qim",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_contact_15plus(date_from, date_to, clinicians,
                               min_contact, min_date,
                               contact_type,
                               lazy)
    }

    fifteen_plusID <- self$contact_15plus_list %>>%
      dplyr::pull(InternalID) %>>%
      c(-1) # add a dummy ID to prevent empty vector

    smokingList <- self$smoking_obs(fifteen_plusID,
                                    date_from = ifelse(ignoreOld,
                                                       NA,
                                                       as.Date(-Inf, origin = "1970-01-01")),
                                    # if ignoreOld, then influenza_vax will (given NA)
                                    # calculate date_from as fifteen months before date_to
                                    date_to = date_to)

    measure_cols <- c(BMIDate = as.Date(NA),
                      BMIValue = as.numeric(NA),
                      WeightDate = as.Date(NA),
                      WeightValue = as.numeric(NA),
                      HeightDate = as.Date(NA),
                      HeightValue = as.numeric(NA),
                      WaistDate = as.Date(NA),
                      WaistValue = as.numeric(NA))
    # a list of columns which might (or might not) be auto-generated
    # from the observations table

    self$qim_15plus_list <- self$contact_15plus_list %>>%
      dplyr::select(-c(Count, Latest)) %>>% # don't need these fields
      dplyr::left_join(private$db$patients %>>%
                         dplyr::filter(InternalID %in% fifteen_plusID) %>>%
                         dplyr::select(InternalID, DOB, Sex, Ethnicity, RecordNo),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::mutate(Ethnicity = dplyr::na_if(Ethnicity, "")) %>>%
      dplyr::mutate(DOB = as.Date(DOB, origin = "1970-01-01")) %>>%
      dplyr::left_join(private$db$clinical %>>%
                         dplyr::filter(InternalID %in% fifteen_plusID) %>>%
                         dplyr::select(InternalID, MaritalStatus, Sexuality),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::mutate(Age5 = floor(dMeasure::calc_age(as.Date(DOB), date_to) / 5) * 5,
                    MaritalStatus = dplyr::na_if(MaritalStatus, ""),
                    Sexuality = dplyr::na_if(Sexuality, "")) %>>%
      # round age group to lower 5 year group
      dplyr::left_join(private$db$observations %>>%
                         dplyr::filter(InternalID %in% fifteen_plusID &&
                                         (ObservationCode %in% c(9, 7, 8, 17)) &&
                                         # 9 is 'BMI', 7 is 'Height',
                                         # 8 is 'Weight' and  17 is'Waist'
                                         # the string is in 'ObservationName'
                                         ObservationDate <= date_to) %>>%
                         dplyr::group_by(InternalID, ObservationCode) %>>%
                         dplyr::filter(ObservationDate == max(ObservationDate, na.rm = TRUE)) %>>%
                         # the most recent observation by InternalID and ObservationCode
                         dplyr::filter(ObservationTime == max(ObservationTime, na.rm = TRUE)) %>>%
                         dplyr::ungroup() %>>%
                         dplyr::collect() %>>%
                         dplyr::mutate(ObservationDate = as.Date(ObservationDate)) %>>%
                         # convert to R's 'standard' date format
                         # didn't work before collect()
                         {if (ignoreOld && nrow(.) > 0) {
                           # if ignoring results that don't qualify for QIM
                           dplyr::filter(., (dMeasure::calc_age(ObservationDate, date_to) == 0) |
                                           (ObservationName == 'Height'))}
                           # throw out results which are more than twelve months old
                           # except for height, which is valid if taken after age 17 years
                           else {.}} %>>%
                         dplyr::select(InternalID, ObservationName,
                                       Date = ObservationDate, Value = ObservationValue) %>>%
                         dplyr::mutate(Value = as.double(Value),
                                       Date = as.double(Date)) %>>%
                         # converting the date to double avoids the warning during 'gather'
                         #  "Warning message:
                         #   attributes are not identical across measure variables;
                         #   they will be dropped"
                         # because of trying to put a Date and double into the same column
                         tidyr::gather(variable, content, -(InternalID:ObservationName)) %>>%
                         # this should result in InternalID, ObservationName, variable, content
                         # where variable is one of 'Date' and 'Value'
                         # and value is the values of Date or Value
                         tidyr::unite(temp, ObservationName, variable, sep = "") %>>%
                         # this should result in InternalID, temp and content
                         # temp will be names like BMIDate and HeightValue
                         tidyr::spread(temp, content) %>>%
                         {tibble::add_column(., !!!measure_cols[!names(measure_cols) %in% names(.)])} %>>%
                         # add missing columns, because not all possible variations may have been added
                         dplyr::mutate(BMIDate = as.Date(BMIDate, origin = "1970-01-01"),
                                       HeightDate = as.Date(HeightDate, origin = "1970-01-01"),
                                       WeightDate = as.Date(WeightDate, origin = "1970-01-01"),
                                       WaistDate = as.Date(WaistDate, origin = "1970-01-01"),
                                       BMIValue = as.double(BMIValue),
                                       HeightValue = as.double(HeightValue),
                                       WeightValue = as.double(WeightValue),
                                       WaistValue = as.double(WaistValue)),
                       # this should result in InternalID, (... HeightDate, WaistValue etc.)
                       by = "InternalID",
                       copy = TRUE) %>>%
      {dplyr::mutate(., Age17 = dplyr::if_else(!is.na(DOB) > 0,
                                               dMeasure::add_age(DOB, 17),
                                               as.Date(NA)))} %>>%
      dplyr::mutate(HeightValue = dplyr::if_else(HeightDate < Age17,
                                                 as.numeric(NA),
                                                 as.numeric(HeightValue)),
                    HeightDate = dplyr::if_else(HeightDate < Age17,
                                                as.Date(NA),
                                                as.Date(HeightDate)),
                    BMIValue = dplyr::if_else(is.na(BMIValue) & !is.na(HeightValue) & !is.na(WeightValue),
                                              WeightValue / (HeightValue/100) ^ 2,
                                              BMIValue, as.double(NA)),
                    # calculate 'missing' BMI values, if valid height and weight values are available
                    BMIDate = dplyr::if_else(is.na(BMIDate) & !is.na(HeightDate) & !is.na(WeightDate),
                                             pmax(HeightDate, WeightDate), # max() is not vectorized
                                             BMIDate, as.Date(NA)),
                    # take the date of the BMI value as the maximum (lates) of the relevant measurements
                    BMIClass = dplyr::case_when(
                      is.na(BMIValue) ~ as.character(NA),
                      BMIValue < 18.5 ~ "Underweight",
                      BMIValue < 25 ~ "Healthy",
                      BMIValue <= 30 ~ "Overweight",
                      TRUE ~ "Obese"
                    )) %>>%

      # if ObservationDate (Height) is less than 17 years of age, then remove
      # this is not clearly specified in PIP QIM documents I have seen so far
      dplyr::left_join(smokingList,
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(private$db$alcohol %>>%
                         dplyr::filter(InternalID %in% fifteen_plusID &&
                                         Updated <= date_to) %>>%
                         dplyr::rename(AlcoholDate = Updated,
                                       AlcoholDescription = Description,
                                       AlcoholComment = Comment,
                                       DaysPerWeek = DaysPerweek,
                                       DrinksPerDay = DrinksPerday) %>>%
                         dplyr::collect() %>>%
                         dplyr::mutate(AlcoholDate = as.Date(AlcoholDate)) %>>%
                         # convert to R's standard date format
                         dplyr::mutate(AlcoholDate = dplyr::if_else(NonDrinker == "No" &
                                                                      DaysPerWeek == 0 &
                                                                      DrinksPerDay == 0,
                                                                    as.Date(-Inf, origin = "1970-01-01"),
                                                                    as.Date(AlcoholDate))) %>>%
                         # if not marked as a 'non-drinker', but no drinks recorded
                         # then this is actually a 'blank' entry
                         {if (ignoreOld && nrow(.) > 0) {
                           # if ignoring observations that don't qualify for QIM
                           dplyr::filter(., dMeasure::calc_age(AlcoholDate, date_to) < 1)}
                           # throw out observations which are twelve months or older
                           else {.}},
                       by = "InternalID",
                       copy = TRUE) %>>%

      dplyr::select(Patient, RecordNo, Sex, Ethnicity, MaritalStatus, Sexuality, Age5,
                    HeightDate, HeightValue, WeightDate, WeightValue, BMIDate, BMIValue, BMIClass,
                    WaistDate, WaistValue, SmokingDate, SmokingStatus,
                    AlcoholDate, NonDrinker, DaysPerWeek, DrinksPerDay, AlcoholDescription,
                    PastAlcoholLevel, YearStarted, YearStopped, AlcoholComment) # drop the InternalID

    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$qim_15plus_list)
})
.reactive_event(dMeasure, "qim_15plus_listR",
                quote(
                  shiny::eventReactive(
                    c(self$contact_15plus_listR(),
                      self$qim_ignoreOldR()), {
                        # update if reactive version of $date_a $date_b
                        # or $clinicians are updated.
                        self$list_qim_15plus(lazy = TRUE)
                        # re-calculates the counts
                      })
                ))


.active(dMeasure, "qim_15plus_measureTypes", function(value) {
  if (!missing(value)) {
    warning("$qim_15plus_measureTypes is read-only.")
  } else {
    return(c("Smoking", "Weight", "Alcohol"))
    # vector of valid QIM measures for 15 plus (for QIM reporting)
    # QIM 02 - Proportion of patients with a smoking status result
    # QIM 03 - Proportion of patients with a weight classification (12 months)
    # QIM 07 - Proportionof patients with an alcohol consumption status
  }
})

.private_init(dMeasure, ".qim_15plus_measure", quote(self$qim_15plus_measureTypes))
.active(dMeasure, "qim_15plus_measure", function(value) {
  # minimum number of contacts listed in $list_contact_count
  if (missing(value)) {
    return(private$.qim_15plus_measure)
  }
  value <- intersect(value, self$qim_15plus_measureTypes)
  # only valid measure types allowed
  private$.qim_15plus_measure <- value
  private$set_reactive(self$qim_15plus_measureR, value)
})
.reactive(dMeasure, "qim_15plus_measureR", quote(self$qim_15plus_measureTypes))

.public(dMeasure, "qim_15plus_report",
        data.frame(NULL,
                   stringsAsFactors = FALSE))
# empty data frame, number of columns dynamically change

#' Age 15 plus Quality Improvement Measure report, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' Shows chosen QIM measures, and by demographic grouping
#'
#' QIM 02 - Proportion of patients with a smoking status result
#' QIM 03 - Proportion of patients with a weight classification (12 months)
#' QIM 07 - Proportionof patients with an alcohol consumption status
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param contact_type contact types which are accepted. default is $contact_type
#' @param demographic demographic groupings for reporting.
#'  if not supplied, reads $qim_demographicGroup
#'  list of available demographic groups in $qim_demographicGroupings
#' @param measure measures to report
#'  if not supplied, reads $qim_15plus_measure
#'  list of available measures in $qim_15plus_measureTypes
#'  currently 'Smoking', 'Weight', 'Alcohol'
#' @param ignoreOld ignore results/observatioins that don't qualify for quality improvement measures
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the diabetes contact list?
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
report_qim_15plus <- function(dMeasure_obj,
                              date_from = NA,
                              date_to = NA,
                              clinicians = NA,
                              min_contact = NA,
                              contact_type = NA,
                              min_date = NA,
                              demographic = NA,
                              measure = NA,
                              ignoreOld = NA,
                              lazy = FALSE) {
  dMeasure_obj$report_qim_15plus(date_from, date_to, clinicians,
                                 min_contact, min_date, contact_type,
                                 demographic, measure,
                                 ignoreOld, lazy)
}

.public(dMeasure, "report_qim_15plus", function(date_from = NA,
                                                date_to = NA,
                                                clinicians = NA,
                                                min_contact = NA,
                                                min_date = NA,
                                                contact_type = NA,
                                                demographic = NA,
                                                measure = NA,
                                                ignoreOld = NA,
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
  if (is.na(demographic)) {
    demographic <- self$qim_demographicGroup
  }
  if (is.na(measure)) {
    measure <- self$qim_15plus_measure
  }
  if (is.na(ignoreOld)) {
    ignoreOld <- self$qim_ignoreOld
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {log_id <- private$config_db$write_log_db(
      query = "qim_15plus_report",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_qim_15plus(date_from, date_to, clinicians,
                           min_contact, min_date, contact_type,
                           ignoreOld, lazy)
    }

    measure <- dplyr::recode(measure,
                             'Smoking' = 'SmokingDone',
                             'Weight' = 'WeightDone',
                             'Alcohol' = 'AlcoholDone')

    report_groups <- c(demographic, measure, "")
    # group by both demographic groupings and measures of interest
    # add a dummy string in case there are no demographic or measure groups chosen!

    self$qim_15plus_report <- self$qim_15plus_list %>>%
      dplyr::mutate(SmokingDone = !(is.na(SmokingDate) | SmokingDate == -Inf),
                    WeightDone = !(is.na(WeightDate) | BMIDate == -Inf),
                    AlcoholDone = !(is.na(AlcoholDate) | AlcoholDate == -Inf)) %>>%
      # a measure is 'done' if it exists (not NA)
      # if ignoreOld = TRUE, the the observation must fall within
      #  the required timeframe
      dplyr::group_by_at(report_groups) %>>%
      # group_by_at takes a vector of strings
      dplyr::summarise(n = n()) %>>%
      dplyr::ungroup() %>>%
      {dplyr::select(., intersect(names(.), c(report_groups, "n")))}
    # if no rows, then grouping will not remove unnecessary columns

    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$qim_15plus_report)
})
.reactive_event(dMeasure, "qim_15plus_reportR",
                quote(
                  shiny::eventReactive(
                    c(self$qim_15plus_listR(),
                      self$qim_demographicGroupR(),
                      self$qim_15plus_measureR()), {
                        # update if reactive version of $date_a $date_b
                        # or $clinicians are updated.
                        # or change in demographic grouping
                        # or change in measures
                        self$report_qim_15plus(lazy = TRUE)
                        # re-calculates the counts
                      })
                ))


##### QIM 65 plus fields ############################################################
.public(dMeasure, "qim_65plus_list",
        data.frame(Patient = character(),
                   RecordNo = character(),
                   Age5 = integer(),
                   Sex = character(),
                   Ethnicity = character(),
                   MaritalStatus = character(),
                   Sexuality = character(),
                   FluvaxDate = as.Date(integer(0),
                                        origin = "1970-01-01"),
                   FluvaxName = character(),
                   stringsAsFactors = FALSE))

##### QIM 65 plus methods ##########################################################
#' List of 65 years or older, with Quality Improvement Measures, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' QIM 04 - Influenza immunization - most recent. the QIM measure is within last 15 months
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param contact_type contact types which are accepted. default is $contact_type
#' @param ignoreOld ignore results/observatioins that don't qualify for quality improvement measures
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the 65+ contact list?
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
list_qim_65plus <- function(dMeasure_obj,
                            date_from = NA,
                            date_to = NA,
                            clinicians = NA,
                            min_contact = NA,
                            min_date = NA,
                            contact_type = NA,
                            ignoreOld = NA,
                            lazy = FALSE) {
  dMeasure_obj$list_qim_65plus(date_from, date_to, clinicians,
                               min_contact, min_date, contact_type,
                               ignoreOld,
                               lazy)
}

.public(dMeasure, "list_qim_65plus", function(date_from = NA,
                                              date_to = NA,
                                              clinicians = NA,
                                              min_contact = NA,
                                              min_date = NA,
                                              contact_type = NA,
                                              ignoreOld = NA,
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
  if (is.na(ignoreOld)) {
    ignoreOld <- self$qim_ignoreOld
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {log_id <- private$config_db$write_log_db(
      query = "sixtyfiveplus_qim",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_contact_65plus(date_from, date_to, clinicians,
                               min_contact, min_date,
                               contact_type,
                               lazy)
    }

    sixtyfiveplusID <- self$contact_65plus_list %>>%
      dplyr::pull(InternalID) %>>%
      c(-1) # add a dummy ID to prevent empty vector


    fluvaxList <- self$influenzaVax_obs(sixtyfiveplusID,
                                        date_from = ifelse(ignoreOld,
                                                           NA,
                                                           as.Date(-Inf, origin = "1970-01-01")),
                                        # if ignoreOld, then influenza_vax will (given NA)
                                        # calculate date_from as fifteen months before date_to
                                        date_to = date_to)
    # returns InternalID, FluVaxName, FluvaxDate

    self$qim_65plus_list <- self$contact_65plus_list %>>%
      dplyr::select(-c(Count, Latest)) %>>% # don't need these fields
      dplyr::left_join(fluvaxList,
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(private$db$patients %>>%
                         dplyr::filter(InternalID %in% sixtyfiveplusID) %>>%
                         dplyr::select(InternalID, DOB, Sex, Ethnicity, RecordNo),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(private$db$clinical %>>%
                         dplyr::filter(InternalID %in% sixtyfiveplusID) %>>%
                         dplyr::select(InternalID, MaritalStatus, Sexuality),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::mutate(Age5 = floor(dMeasure::calc_age(as.Date(DOB), date_to) / 5) * 5) %>>%
      # round age group to nearest 5 years
      dplyr::select(-c(DOB, InternalID))

    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$qim_65plus_list)
})
.reactive_event(dMeasure, "qim_65plus_listR",
                quote(
                  shiny::eventReactive(
                    c(self$contact_65plus_listR(),
                      self$qim_ignoreOldR()), {
                        # update if reactive version of $date_a $date_b
                        # or $clinicians are updated.
                        self$list_qim_65plus(lazy = TRUE)
                        # re-calculates the counts
                      })
                ))


.public(dMeasure, "qim_65plus_report",
        data.frame(NULL,
                   stringsAsFactors = FALSE))
#' Age 65+ Quality Improvement Measure report, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' Shows chosen QIM measures, and by demographic grouping
#'
#' QIM 04 -Proportion of patients aged 65 and over who were immunised against influenza
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param contact_type contact types which are accepted. default is $contact_type
#' @param demographic demographic groupings for reporting.
#'  if not supplied, reads $qim_demographicGroup
#'  list of available demographic groups in $qim_demographicGroupings
#' @param ignoreOld ignore results/observatioins that don't qualify for quality improvement measures
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the 65+ contact list?
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
report_qim_65plus <- function(dMeasure_obj,
                              date_from = NA,
                              date_to = NA,
                              clinicians = NA,
                              min_contact = NA,
                              contact_type = NA,
                              min_date = NA,
                              demographic = NA,
                              ignoreOld = NA,
                              lazy = FALSE) {
  dMeasure_obj$report_qim_65plus(date_from, date_to, clinicians,
                                 min_contact, min_date, contact_type,
                                 demographic,
                                 ignoreOld, lazy)
}
.public(dMeasure, "report_qim_65plus", function(date_from = NA,
                                                date_to = NA,
                                                clinicians = NA,
                                                min_contact = NA,
                                                min_date = NA,
                                                contact_type = NA,
                                                demographic = NA,
                                                ignoreOld = NA,
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
  if (is.na(demographic)) {
    demographic <- self$qim_demographicGroup
  }
  if (is.na(ignoreOld)) {
    ignoreOld <- self$qim_ignoreOld
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {log_id <- private$config_db$write_log_db(
      query = "qim_65plus_report",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_qim_65plus(date_from, date_to, clinicians,
                           min_contact, min_date, contact_type,
                           ignoreOld, lazy)
    }

    report_groups <- c(demographic, "InfluenzaDone")
    # group by both demographic groupings and measures of interest
    # add a dummy string in case there are no demographic or measure groups chosen!

    self$qim_65plus_report <- self$qim_65plus_list %>>%
      dplyr::mutate(InfluenzaDone = !is.na(FluvaxDate)) %>>%
      # a measure is 'done' if it exists (not NA)
      # if ignoreOld = TRUE, the the observation must fall within
      #  the required timeframe
      dplyr::group_by_at(report_groups) %>>%
      # group_by_at takes a vector of strings
      dplyr::summarise(n = n()) %>>%
      dplyr::ungroup() %>>%
      {dplyr::select(., intersect(names(.), c(report_groups, "n")))}
    # if no rows, then grouping will not remove unnecessary columns

    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$qim_65plus_report)
})
.reactive_event(dMeasure, "qim_65plus_reportR",
                quote(
                  shiny::eventReactive(
                    c(self$qim_65plus_listR(),
                      self$qim_demographicGroupR()), {
                        # update if reactive version of $date_a $date_b
                        # or $clinicians are updated.
                        # or change in demographic grouping
                        # or change in measures
                        self$report_qim_65plus(lazy = TRUE)
                        # re-calculates the counts
                      })
                ))


##### QIM chronic lung disease plus fields ############################################################
.public(dMeasure, "qim_copd_list",
        data.frame(Patient = character(),
                   RecordNo = character(),
                   Age5 = integer(),
                   Sex = character(),
                   Ethnicity = character(),
                   MaritalStatus = character(),
                   Sexuality = character(),
                   FluvaxDate = as.Date(integer(0),
                                        origin = "1970-01-01"),
                   FluvaxName = character(),
                   stringsAsFactors = FALSE))

##### QIM chronic lung disease methods ##########################################################
#' List of patient with chronic lung disease, with Quality Improvement Measures, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' QIM 06 -Proportion of patients with COPD who were immunised against influenza
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param contact_type contact types which are accepted. default is $contact_type
#' @param ignoreOld ignore results/observatioins that don't qualify for quality improvement measures
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the copd contact list?
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
list_qim_copd <- function(dMeasure_obj,
                          date_from = NA,
                          date_to = NA,
                          clinicians = NA,
                          min_contact = NA,
                          min_date = NA,
                          contact_type = NA,
                          ignoreOld = NA,
                          lazy = FALSE) {
  dMeasure_obj$list_qim_copd(date_from, date_to, clinicians,
                             min_contact, min_date, contact_type,
                             ignoreOld,
                             lazy)
}

.public(dMeasure, "list_qim_copd", function(date_from = NA,
                                            date_to = NA,
                                            clinicians = NA,
                                            min_contact = NA,
                                            min_date = NA,
                                            contact_type = NA,
                                            ignoreOld = NA,
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
  if (is.na(ignoreOld)) {
    ignoreOld <- self$qim_ignoreOld
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {log_id <- private$config_db$write_log_db(
      query = "copd_qim",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_contact_chroniclungdisease(date_from, date_to, clinicians,
                                           min_contact, min_date,
                                           contact_type,
                                           lazy)
    }

    copdID <- self$contact_chroniclungdisease_list %>>%
      dplyr::pull(InternalID) %>>%
      c(-1) # add a dummy ID to prevent empty vector


    fluvaxList <- self$influenzaVax_obs(copdID,
                                        date_from = ifelse(ignoreOld,
                                                           NA,
                                                           as.Date(-Inf, origin = "1970-01-01")),
                                        # if ignoreOld, then influenza_vax will (given NA)
                                        # calculate date_from as fifteen months before date_to
                                        date_to = date_to)
    # returns InternalID, FluVaxName, FluvaxDate

    self$qim_copd_list <- self$contact_chroniclungdisease_list %>>%
      dplyr::select(-c(Count, Latest)) %>>% # don't need these fields
      dplyr::left_join(fluvaxList,
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(private$db$patients %>>%
                         dplyr::filter(InternalID %in% copdID) %>>%
                         dplyr::select(InternalID, DOB, Sex, Ethnicity, RecordNo),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(private$db$clinical %>>%
                         dplyr::filter(InternalID %in% copdID) %>>%
                         dplyr::select(InternalID, MaritalStatus, Sexuality),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::mutate(Age5 = floor(dMeasure::calc_age(as.Date(DOB), date_to) / 5) * 5) %>>%
      # round age group to nearest 5 years
      dplyr::select(-c(DOB, InternalID))

    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$qim_copd_list)
})
.reactive_event(dMeasure, "qim_copd_listR",
                quote(
                  shiny::eventReactive(
                    c(self$contact_copd_listR(),
                      self$qim_ignoreOldR()), {
                        # update if reactive version of $date_a $date_b
                        # or $clinicians are updated.
                        self$list_qim_copd(lazy = TRUE)
                        # re-calculates the counts
                      })
                ))

.public(dMeasure, "qim_copd_report",
        data.frame(NULL,
                   stringsAsFactors = FALSE))
#' Chronic Obstructive Pulmonary Disease Quality Improvement Measure report, in the contact list
#'
#' Filtered by date, and chosen clinicians
#'
#' Shows chosen QIM measures, and by demographic grouping
#'
#' QIM 06 -Proportion of patients with COPD who were immunised against influenza
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param contact_type contact types which are accepted. default is $contact_type
#' @param demographic demographic groupings for reporting.
#'  if not supplied, reads $qim_demographicGroup
#'  list of available demographic groups in $qim_demographicGroupings
#' @param ignoreOld ignore results/observatioins that don't qualify for quality improvement measures
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the copd contact list?
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
report_qim_copd <- function(dMeasure_obj,
                            date_from = NA,
                            date_to = NA,
                            clinicians = NA,
                            min_contact = NA,
                            contact_type = NA,
                            min_date = NA,
                            demographic = NA,
                            ignoreOld = NA,
                            lazy = FALSE) {
  dMeasure_obj$report_qim_copd(date_from, date_to, clinicians,
                               min_contact, min_date, contact_type,
                               demographic,
                               ignoreOld, lazy)
}
.public(dMeasure, "report_qim_copd", function(date_from = NA,
                                              date_to = NA,
                                              clinicians = NA,
                                              min_contact = NA,
                                              min_date = NA,
                                              contact_type = NA,
                                              demographic = NA,
                                              ignoreOld = NA,
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
  if (is.na(demographic)) {
    demographic <- self$qim_demographicGroup
  }
  if (is.na(ignoreOld)) {
    ignoreOld <- self$qim_ignoreOld
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {log_id <- private$config_db$write_log_db(
      query = "qim_copd_report",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_qim_copd(date_from, date_to, clinicians,
                         min_contact, min_date, contact_type,
                         ignoreOld, lazy)
    }

    report_groups <- c(demographic, "InfluenzaDone")
    # group by both demographic groupings and measures of interest
    # add a dummy string in case there are no demographic or measure groups chosen!

    self$qim_copd_report <- self$qim_copd_list %>>%
      dplyr::mutate(InfluenzaDone = !is.na(FluvaxDate)) %>>%
      # a measure is 'done' if it exists (not NA)
      # if ignoreOld = TRUE, the the observation must fall within
      #  the required timeframe
      dplyr::group_by_at(report_groups) %>>%
      # group_by_at takes a vector of strings
      dplyr::summarise(n = n()) %>>%
      dplyr::ungroup() %>>%
      {dplyr::select(., intersect(names(.), c(report_groups, "n")))}
    # if no rows, then grouping will not remove unnecessary columns

    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$qim_copd_report)
})
.reactive_event(dMeasure, "qim_copd_reportR",
                quote(
                  shiny::eventReactive(
                    c(self$qim_copd_listR(),
                      self$qim_demographicGroupR()), {
                        # update if reactive version of $date_a $date_b
                        # or $clinicians are updated.
                        # or change in demographic grouping
                        # or change in measures
                        self$report_qim_copd(lazy = TRUE)
                        # re-calculates the counts
                      })
                ))


##### QIM cardiovascular risk fields ############################################################
.public(dMeasure, "qim_cvdRisk_list",
        data.frame(Patient = character(),
                   RecordNo = character(),
                   Age5 = integer(),
                   Sex = character(),
                   Ethnicity = character(),
                   MaritalStatus = character(),
                   Sexuality = character(),
                   CVDisease = logical(),
                   Diabetes = logical(),

                   stringsAsFactors = FALSE))

##### QIM cardiovascular risk assessment methods ##########################################################
#' List of patient with information to complete cardiovascular risk assessment
#'
#' Filtered by date, and chosen clinicians
#'
#' QIM 08.Proportion of patients with the necessary risk factors assessed to enable CVD assessment
#'
#' required parameters
#'  Age, Ethnicity (especially ATSI status)
#'   included - Age 45 or older
#'    OR Age 35 or older + AtSI
#'  Known cardiovascular disease (excluded)
#'  Presence of diabetes. Diabetes and microalbuminuria
#'  eGFR
#'  previous diagnosis of familial hypercholesterolaemia
#'  systolic blood pressure. Diastolic blood pressure
#'  Serum total cholesterol. Serum HDL cholesterol
#'
#' National Vascular Disease Prevention Alliance (NVDPA) guidelines
#' https://www.cvdcheck.org.au/australian-absolute-cardiovascular-disease-risk-calculator
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $date_a
#' @param date_to end date (inclusive). default is $date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
#' @param contact_type contact types which are accepted. default is $contact_type
#' @param ignoreOld ignore results/observatioins that don't qualify for quality improvement measures
#'  if not supplied, reads $qim_ignoreOld
#' @param lazy recalculate the copd contact list?
#'
#' @return dataframe of Patient (name), InternalID, Count, and most recent contact date
list_qim_cvdrisk <- function(dMeasure_obj,
                             date_from = NA,
                             date_to = NA,
                             clinicians = NA,
                             min_contact = NA,
                             min_date = NA,
                             contact_type = NA,
                             ignoreOld = NA,
                             lazy = FALSE) {
  dMeasure_obj$list_qim_cvdrisk(date_from, date_to, clinicians,
                                min_contact, min_date, contact_type,
                                ignoreOld,
                                lazy)
}

.public(dMeasure, "list_qim_cvdrisk", function(date_from = NA,
                                               date_to = NA,
                                               clinicians = NA,
                                               min_contact = NA,
                                               min_date = NA,
                                               contact_type = NA,
                                               ignoreOld = NA,
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
  if (is.na(ignoreOld)) {
    ignoreOld <- self$qim_ignoreOld
  }
  if (ignoreOld) {
    obs_from <- NA
  } else {
    obs_from <- as.Date(-Inf, origin = "1970-01-01")
    # accept very old results
  }

  # no additional clinician filtering based on privileges or user restrictions

  if (all(is.na(clinicians)) || length(clinicians) == 0) {
    clinicians <- c("") # dplyr::filter does not work on zero-length list()
  }

  if (private$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {log_id <- private$config_db$write_log_db(
      query = "cvdrisk_qim",
      data = list(date_from, date_to, clinicians))}

    if (!lazy) {
      self$list_contact_45_74(date_from, date_to, clinicians,
                              min_contact, min_date,
                              contact_type,
                              lazy)
    }

    # PastHistory ItemCode = 1446 for familial hypercholesterolaemia

    cvdriskID <- self$contact_45_74_list %>>%
      dplyr::pull(InternalID) %>>%
      c(-1) # add a dummy ID to prevent empty vector

    diabetesID <- self$diabetes_list(data.frame(InternalID = cvdriskID, Date = date_to))

    fHypercholesterolaemiaID <-
      self$familialHypercholesterolaemia_list(data.frame(InternalID = cvdriskID, Date = date_to))

    lvhID <-
      self$LVH_list(data.frame(InternalID = cvdriskID, Date = date_to))

    self$qim_cvdRisk_list <- self$contact_45_74_list %>>%
      dplyr::select(-c(Count, Latest)) %>>% # don't need these fields
      dplyr::mutate(Diabetes = InternalID %in% diabetesID) %>>%
      dplyr::left_join(self$smoking_obs(cvdriskID,
                                             date_from = obs_from, date_to = date_to),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(self$UrineAlbumin_obs(cvdriskID,
                                             date_from = obs_from, date_to = date_to),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(self$PersistentProteinuria_obs(cvdriskID,
                                                      date_from = obs_from, date_to = date_to),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(self$eGFR_obs(cvdriskID,
                                     date_from = obs_from,
                                     date_to = date_to),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::mutate(FamilialHypercholesterolaemia = InternalID %in%
                      fHypercholesterolaemiaID) %>>%
      dplyr::mutate(LVH = InternalID %in%
                      lvhID) %>>%
      dplyr::left_join(self$Cholesterol_obs(cvdriskID,
                                            date_from = obs_from,
                                            date_to = date_to),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(self$BloodPressure_obs(cvdriskID,
                                              date_from = obs_from,
                                              date_to = date_to),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(private$db$patients %>>%
                         dplyr::filter(InternalID %in% cvdriskID) %>>%
                         dplyr::select(InternalID, DOB, Sex, Ethnicity, RecordNo),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(private$db$clinical %>>%
                         dplyr::filter(InternalID %in% cvdriskID) %>>%
                         dplyr::select(InternalID, MaritalStatus, Sexuality),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::mutate(Age = dMeasure::calc_age(as.Date(DOB), date_to)) %>>%
      {dplyr::left_join(., dMeasure::framingham_riskequation(.),
                        by = "InternalID")} %>>%
      dplyr::mutate(Age5 = floor(Age / 5) * 5) %>>%
      # round age group to nearest 5 years
      dplyr::select(-c(DOB, Age, InternalID))

    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$qim_cvdRisk_list)
})
.reactive_event(dMeasure, "qim_cvdrisk_listR",
                quote(
                  shiny::eventReactive(
                    c(self$contact_45_74_listR(),
                      self$qim_ignoreOldR()), {
                        # update if reactive version of $date_a $date_b
                        # or $clinicians are updated.
                        self$list_qim_cvdrisk(lazy = TRUE)
                        # re-calculates the counts
                      })
                ))
