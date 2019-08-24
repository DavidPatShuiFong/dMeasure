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
  if (is.na(contact_type)) {
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
  if (is.na(contact_type)) {
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
    fluvaxID <- unlist(private$db$vaccine_disease %>>%
                         dplyr::filter(DISEASECODE %in% c(7,30)) %>>%
                         dplyr::select(VACCINEID) %>>%
                         dplyr::collect(), use.names = FALSE)
    # there are many, many influenza vaccine IDs, but these can be found
    # via the db$vaccine_disease database

    # uses BPCode == 1 in $db$reportValues for finding HbA1c results
    #
    # a possible alternative search mechanism for HbA1C would
    # be to look at ResultName "HbA1C%" and "Hb A1c"
    # https://bpsoftware.net/forums/topic/all-patients-with-a-hba1c-value-diagnostic-of-diabetes/

    self$qim_diabetes_list <- self$contact_diabetes_list %>>%
      dplyr::select(-c(Count, Latest)) %>>% # don't need these fields
      dplyr::left_join(private$db$reportValues %>>%
                         dplyr::filter(InternalID %in% diabetesID &&
                                         (BPCode == 1 || BPCode == 19) &&
                                         # BPCode 1 is HbA1C, 19 is SI units
                                         # these reports include 'manual' entries
                                         # in the diabetes assessment dialog
                                         ReportDate <= date_to) %>>%
                         dplyr::group_by(InternalID) %>>%
                         dplyr::filter(ReportDate == max(ReportDate, na.rm = TRUE)) %>>%
                         # the most recent HbA1C report by InternalID
                         dplyr::select(InternalID, ReportDate, ResultValue, Units) %>>%
                         dplyr::rename(HbA1CValue = ResultValue,
                                       HbA1CUnits = Units,
                                       HbA1CDate = ReportDate) %>>%
                         dplyr::collect() %>>%
                         dplyr::mutate(HbA1CDate = as.Date(HbA1CDate)) %>>%
                         # convert to R's 'standard' date format
                         # didn't work before collect()
                         {if (ignoreOld && nrow(.) > 0) {
                           # if ignoring results that don't qualify for QIM
                           dplyr::filter(., dMeasure::calc_age(HbA1CDate, date_to) == 0)}
                           # throw out results which are more than twelve months old
                           else {.}},
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::mutate(HbA1CValue = as.double(HbA1CValue)) %>>%
      # was a character. can't be converted to double within the MSSQL query
      dplyr::left_join(private$db$immunizations %>>%
                         dplyr::filter(InternalID %in% diabetesID &&
                                         VaccineID %in% fluvaxID &&
                                         # influenza vaccines
                                         GivenDate <= date_to) %>>%
                         dplyr::group_by(InternalID) %>>%
                         dplyr::filter(GivenDate == max(GivenDate, na.rm = TRUE)) %>>%
                         # most recent fluvax by InternalID
                         dplyr::rename(FluvaxName = VaccineName,
                                       FluvaxDate = GivenDate) %>>%
                         dplyr::select(-VaccineID) %>>%
                         dplyr::collect() %>>%
                         dplyr::mutate(FluvaxDate = as.Date(FluvaxDate)) %>>%
                         # convert to R's 'standard' date format, didn't work before collect()
                         {if (ignoreOld && nrow(.) > 0) {
                           # if ignoring vaccinations that don't qualify for QIM
                           dplyr::filter(., dMeasure::calc_age_months(FluvaxDate, date_to) < 15)}
                           # throw out vaccinations which are fifteen months or older
                           else {.}},
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(private$db$observations %>>%
                         dplyr::filter(InternalID %in% diabetesID &&
                                         ObservationCode %in% c(3,4) &&
                                         # systolic or diastolic blood pressure
                                         # 3 = systolic, 4 = diastolic
                                         ObservationDate <= date_to) %>>%
                         dplyr::group_by(InternalID) %>>%
                         dplyr::filter(ObservationDate == max(ObservationDate,
                                                              na.rm = TRUE)) %>>%
                         # only the most recent recording(s) DATE
                         dplyr::filter(ObservationTime == max(ObservationTime,
                                                              na.rm = TRUE)) %>>%
                         # only the most recent recording TIME
                         dplyr::ungroup() %>>%
                         dplyr::group_by(InternalID, ObservationCode) %>>%
                         dplyr::filter(RECORDID == max(RECORDID,
                                                       na.rm = TRUE)) %>>%
                         # if still tied (this shouldn't be the case? but it happens
                         # in the sample database), filter by most recent RECORDID
                         dplyr::rename(BPDate = ObservationDate) %>>%
                         dplyr::select(-c(RECORDID, ObservationName, ObservationTime)) %>>%
                         dplyr::collect() %>>%
                         dplyr::mutate(BPDate = as.Date(BPDate)) %>>%
                         # convert to R's standard date format
                         {if (ignoreOld && nrow(.) > 0) {
                           # if ignoring observations that don't qualify for QIM
                           dplyr::filter(., dMeasure::calc_age_months(BPDate, date_to) < 6)}
                           # throw out observations which are six months or older
                           else {.}} %>>%
                         tidyr::spread(ObservationCode, ObservationValue) %>>%
                         # this creates columns `3` or `4` if there are any
                         # qualifying blood pressure observations
                         {cols <- c(`3` = NA, `4` = NA)
                         tibble::add_column(.,
                                            !!!cols[!names(cols) %in% names(.)])} %>>%
                         # add columns `3` and `4` if they don't exist at this point
                         dplyr::mutate(BP = paste0(`3`,"/",`4`)) %>>%
                         # create a BP column combined systolic `3` and diastolic `4` readings
                         # and then remove those individual readings
                         dplyr::select(-c(`3`, `4`)),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(private$db$patients %>>%
                         dplyr::filter(InternalID %in% diabetesID) %>>%
                         dplyr::select(InternalID, DOB, Sex, Ethnicity),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(private$db$clinical %>>%
                         dplyr::filter(InternalID %in% diabetesID) %>>%
                         dplyr::select(InternalID, MaritalStatus, Sexuality),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::mutate(Age5 = floor(dMeasure::calc_age(as.Date(DOB), date_to) / 5) * 5) %>>%
      # round age group to nearest 5 years
      dplyr::select(-DOB) %>>%
      dplyr::left_join(private$db$patients %>>%
                         dplyr::filter(InternalID %in% diabetesID) %>>%
                         dplyr::select(InternalID, RecordNo),
                       by = "InternalID", # add RecordNo
                       copy = TRUE) %>>%
      dplyr::select(-InternalID) # drop the InternalID

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
  if (is.na(contact_type)) {
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
  if (is.na(contact_type)) {
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
                   SmokingDate = as.Date(integer(0),
                                         origin = "1970-01-01"),
                   SmokingStatus = character(),
                   BMIDate = as.Date(integer(0),
                                      origin = "1970-01-01"),
                   BMIValue = numeric(),
                   WeightDate = as.Date(integer(0),
                                         origin = "1970-01-01"),
                   WeightValue = numeric(),
                   HeightDate = as.Date(integer(0),
                                         origin = "1970-01-01"),
                   HeightValue = numeric(),
                   WaistDate = as.Date(integer(0),
                                        origin = "1970-01-01"),
                   WaistValue = numeric(),
                   AlcoholDate = as.Date(integer(0),
                                         origin = "1970-01-01"),
                   AlcoholDrinker = character(),
                   AlcoholDaysPerWeek = numeric(),
                   AlcoholDrinksPerDay = numeric(),
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
                         dplyr::mutate(Value = as.double(Value)) %>>%
                         tidyr::gather(variable, content, -(InternalID:ObservationName),
                                       convert = TRUE) %>>%
                         # this should result in InternalID, ObservationName, variable, content
                         # where variable is one of 'Date' and 'Value'
                         # and value is the values of Date or Value
                         tidyr::unite(temp, ObservationName, variable, sep = "") %>>%
                         # this should result in InternalID, temp and content
                         # temp will be names like BMIDate and HeightValue
                         tidyr::spread(temp, content),
                       # this should result in InternalID, (... HeightDate, WaistValue etc.)
                       by = "InternalID",
                       copy = TRUE) %>>%
      {tibble::add_column(., !!!measure_cols[!names(measure_cols) %in% names(.)])} %>>%
      dplyr::mutate(BMIDate = as.Date(BMIDate, origin = "1970-01-01"),
                    HeightDate = as.Date(HeightDate, origin = "1970-01-01"),
                    WeightDate = as.Date(WeightDate, origin = "1970-01-01"),
                    WaistDate = as.Date(WaistDate, origin = "1970-01-01")) %>>%
      dplyr::left_join(private$db$clinical %>>%
                         dplyr::filter(InternalID %in% fifteen_plusID &&
                                         Updated <= date_to) %>>%
                         dplyr::select(InternalID, SmokingDate = Updated, SmokingStatus) %>>%
                         # the table appears to have one entry per patient
                         dplyr::collect() %>>%
                         dplyr::mutate(SmokingDate = as.Date(SmokingDate)) %>>%
                         dplyr::mutate(SmokingDate = dplyr::if_else(SmokingStatus == "",
                                                                    as.Date(-Inf, origin = "1970-01-01"),
                                                                    as.Date(SmokingDate))) %>>%
                         # smoking status has not actually been recorded if SmokingStatus == ""
                         # convert to R's 'standard' date format, didn't work before collect()
                         {if (ignoreOld && nrow(.) > 0) {
                           # if ignoring smoking status recordings that don't qualify for QIM
                           dplyr::filter(., dMeasure::calc_age_months(SmokingDate, date_to) < 12)}
                           # throw out smoking stats which are twelve months or older
                           else {.}},
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::left_join(private$db$alcohol %>>%
                         dplyr::filter(InternalID %in% fifteen_plusID &&
                                         Updated <= date_to) %>>%
                         dplyr::rename(AlcoholDate = Updated) %>>%
                         dplyr::collect() %>>%
                         dplyr::mutate(AlcoholDate = as.Date(AlcoholDate)) %>>%
                         # convert to R's standard date format
                         dplyr::mutate(AlcoholDate = dplyr::if_else(NonDrinker == "No" &
                                                                      DaysPerweek == 0 &
                                                                      DrinksPerday == 0,
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
      dplyr::left_join(private$db$patients %>>%
                         dplyr::filter(InternalID %in% fifteen_plusID) %>>%
                         dplyr::select(InternalID, DOB, Sex, Ethnicity),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::mutate(DOB = as.Date(DOB, origin = "1970-01-01"))

    if (nrow(self$qim_15plus_list) > 0) {
      self$qim_15plus_list <- self$qim_15plus_list %>>%
        dplyr::mutate(Age17 = dMeasure::add_age(DOB, 17))}
    else {
      self$qim_15plus_list <- self$qim_15plus_list %>>%
        dplyr::mutate(Age17 = as.Date(NA))
    }

    self$qim_15plus_list <- self$qim_15plus_list %>>%
      dplyr::mutate(HeightValue = dplyr::if_else(HeightDate < Age17,
                                                  as.numeric(NA),
                                                  as.numeric(HeightValue)),
                    HeightDate = dplyr::if_else(HeightDate < Age17,
                                                 as.Date(NA),
                                                 as.Date(HeightDate))) %>>%
      # if ObservationDate (Height) is less than 17 years of age, then remove
      # this is not clearly specified in PIP QIM documents I have seen so far
      dplyr::select(-Age17) %>>%
      dplyr::left_join(private$db$clinical %>>%
                         dplyr::filter(InternalID %in% fifteen_plusID) %>>%
                         dplyr::select(InternalID, MaritalStatus, Sexuality),
                       by = "InternalID",
                       copy = TRUE) %>>%
      dplyr::mutate(Age5 = floor(dMeasure::calc_age(as.Date(DOB), date_to) / 5) * 5) %>>%
      # round age group to nearest 5 years
      dplyr::select(-DOB) %>>%
      dplyr::left_join(private$db$patients %>>%
                         dplyr::filter(InternalID %in% fifteen_plusID) %>>%
                         dplyr::select(InternalID, RecordNo),
                       by = "InternalID", # add RecordNo
                       copy = TRUE) %>>%
      dplyr::select(-InternalID) # drop the InternalID

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
    return(c("SmokingDone", "WeightDone", "AlcoholDone"))
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
  if (is.na(contact_type)) {
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

    report_groups <- c(demographic, measure, "")
    # group by both demographic groupings and measures of interest
    # add a dummy string in case there are no demographic or measure groups chosen!

    self$qim_15plus_report <- self$qim_15plus_list %>>%
      dplyr::mutate(SmokingDone = !(is.na(SmokingDate) | SmokingDate == -Inf),
                    WeightDone = !(is.na(WeightDate) | WeightDate == -Inf),
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

