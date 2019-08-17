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
    private$.contact_min <- value
    private$set_reactive(self$qim_ignoreOldR, value)
  } else {
    warning("$qim_ignoreOld only accepts logical values (TRUE/FALSE).")
  }
})
.reactive(dMeasure, "qim_ignoreOldR", TRUE)


##### QIM diabetes fields ###########################################################

.public(dMeasure, "qim_diabetes_list",
        data.frame(Patient = character(),
                   InternalID = integer(),
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
#' QIM 10 - Blood pressure - most recent. the QIM measure is within the last six monthe
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from start date. default is $dateContact$date_a
#' @param date_to end date (inclusive). default is $dateContact$date_b
#' @param clinicians list of clinicians to view. default is $clinicians
#' @param min_contact minimum number of contacts. default is $contact_min, initially one (1)
#' @param min_date most recent contact must be at least min_date. default is $contact_minDate, initially -Inf
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
                              ignoreOld = NA,
                              lazy = FALSE) {
  dMeasure_obj$list_qim_diabetes(date_from, date_to, clinicians,
                                 min_contact, min_date,
                                 ignoreOld,
                                 lazy)
}

.public(dMeasure, "list_qim_diabetes", function(date_from = NA,
                                                date_to = NA,
                                                clinicians = NA,
                                                min_contact = NA,
                                                min_date = NA,
                                                ignoreOld = NA,
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
                                 lazy)
    }

    diabetesID <- self$contact_diabetes_list %>>%
      dplyr::pull(InternalID)
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
                                         BPCode == 1 && # BPCode == 1 is HbA1C
                                         # these reports include 'manual' entries
                                         # in the diabetes assessment dialog
                                         ReportDate <= date_to) %>>%
                         dplyr::group_by(InternalID) %>>%
                         dplyr::filter(ReportDate == max(ReportDate, na.rm = TRUE)) %>>%
                         # the most recent HbA1C report by InternalID
                         dplyr::select(InternalID, ReportDate, ResultValue, Units) %>>%
                         dplyr::mutate(ResultValue = trimws(ResultValue),
                                       Units = trimws(Units)) %>>%
                         # trim excess whitespace
                         dplyr::rename(HbA1CValue = ResultValue,
                                       HbA1CUnits = Units,
                                       HbA1CDate = ReportDate) %>>%
                         dplyr::collect() %>>%
                         dplyr::mutate(HbA1CDate = as.Date(HbA1CDate)) %>>%
                         # convert to R's 'standard' date format
                         # didn't work before collect()
                         {if (ignoreOld) {
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
                         dplyr::mutate(VaccineName = trimws(VaccineName)) %>>%
                         # trim whitespace
                         dplyr::rename(FluvaxName = VaccineName,
                                       FluvaxDate = GivenDate) %>>%
                         dplyr::select(-VaccineID) %>>%
                         dplyr::collect() %>>%
                         dplyr::mutate(FluvaxDate = as.Date(FluvaxDate)) %>>%
                         # convert to R's 'standard' date format, didn't work before collect()
                         {if (ignoreOld) {
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
                         dplyr::mutate(ObservationValue = trimws(ObservationValue)) %>>%
                         dplyr::rename(BPDate = ObservationDate) %>>%
                         dplyr::select(-c(RECORDID, ObservationName, ObservationTime)) %>>%
                         dplyr::collect() %>>%
                         dplyr::mutate(BPDate = as.Date(BPDate)) %>>%
                         # convert to R's standard date format
                         {if (ignoreOld) {
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
                         dplyr::select(-c(`3`, `4`)),
                       by = "InternalID",
                       copy = TRUE)


    if (self$Log) {private$config_db$duration_log_db(log_id)}
  }

  return(self$qim_diabetes_list)
})
.reactive_event(dMeasure, "qim_diabetes_listR",
                quote(
                  shiny::eventReactive(
                    c(self$contact_diabetes_listR()), {
                      # update if reactive version of $date_a $date_b
                      # or $clinicians are updated.
                      self$list_qim_diabetes(lazy = TRUE)
                      # re-calculates the counts
                    })
                ))
