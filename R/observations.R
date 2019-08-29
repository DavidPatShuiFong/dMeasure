## 'helper' methods for observations and recordings

#' List of influenza immunization observations/recordings
#'
#' Filtered by InternalID (vector patient numbers) and dates
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param intID vector of InternalID
#' @param date_from start date. default is $date_b minus 15 months
#' @param date_to end date (inclusive). default is $date_b
#'
#' @return dataframe of InternalID, FluvaxName, FluvaxDate
#' @export
influenzaVax_obs <- function(dMeasure_obj, intID, date_from = NA, date_to = NA) {
  dMeasure_obj$influenzaVax_obs(intID, date_from, date_to)
}
.public(dMeasure, "influenzaVax_obs", function(intID, date_from = NA, date_to = NA) {

  if (is.na(date_to)) {
    date_to <- self$date_b
  }
  if (is.na(date_from)) {
    date_from <- dMeasure::add_age(date_to, 15, by = "-1 month")
  } else if (date_from == -Inf) {
    date_from <- as.Date("1900-01-01") # MSSQL doesn't accept -Inf date!
  }

  fluvaxID <- unlist(private$db$vaccine_disease %>>%
                       dplyr::filter(DISEASECODE %in% c(7,30)) %>>%
                       dplyr::select(VACCINEID) %>>%
                       dplyr::collect(), use.names = FALSE)
  # there are many, many influenza vaccine IDs, but these can be found
  # via the db$vaccine_disease database

  private$db$immunizations %>>%
    dplyr::filter(InternalID %in% intID &&
                    VaccineID %in% fluvaxID &&
                    # influenza vaccines
                    GivenDate <= date_to &&
                    GivenDate >= date_from) %>>%
    dplyr::group_by(InternalID) %>>%
    dplyr::filter(GivenDate == max(GivenDate, na.rm = TRUE)) %>>%
    # most recent fluvax by InternalID
    dplyr::rename(FluvaxName = VaccineName,
                  FluvaxDate = GivenDate) %>>%
    dplyr::select(-VaccineID) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(FluvaxDate = as.Date(FluvaxDate))
  # convert to R's 'standard' date format, didn't work before collect()
})

#' List of HbA1C observations/recordings
#'
#' Filtered by InternalID (vector patient numbers) and dates
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param intID vector of InternalID
#' @param date_from start date. default is $date_b minus 12 months
#' @param date_to end date (inclusive). default is $date_b
#'
#' @return dataframe of InternalID, HbA1CDate, HbA1CValue, HbA1CUnits
#' @export
HbA1C_obs <- function(dMeasure_obj, intID, date_from = NA, date_to = NA) {
  dMeasure_obj$HbA1C_obs(intID, date_from, date_to)
}
.public(dMeasure, "HbA1C_obs", function(intID, date_from = NA, date_to = NA) {

  if (is.na(date_to)) {
    date_to <- self$date_b
  }
  if (is.na(date_from)) {
    date_from <- date_to - 365
  } else if (date_from == -Inf) {
    date_from <- as.Date("1900-01-01") # MSSQL doesn't accept -Inf date!
  }

  # uses BPCode == 1 (or 19) in $db$reportValues for finding HbA1c results
  #
  # a possible alternative search mechanism for HbA1C would
  # be to look at ResultName "HbA1C%" and "Hb A1c"
  # https://bpsoftware.net/forums/topic/all-patients-with-a-hba1c-value-diagnostic-of-diabetes/

  private$db$reportValues %>>%
    dplyr::filter(InternalID %in% intID &&
                    (BPCode == 1 || BPCode == 19) &&
                    # BPCode 1 is HbA1C, 19 is SI units
                    # these reports include 'manual' entries
                    # in the diabetes assessment dialog
                    ReportDate <= date_to &&
                    ReportDate >= date_from
    ) %>>%
    dplyr::group_by(InternalID) %>>%
    dplyr::filter(ReportDate == max(ReportDate, na.rm = TRUE)) %>>%
    # the most recent HbA1C report by InternalID
    dplyr::select(InternalID, ReportDate, ResultValue, Units) %>>%
    dplyr::rename(HbA1CDate = ReportDate,
                  HbA1CValue = ResultValue,
                  HbA1CUnits = Units) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(HbA1CDate = as.Date(HbA1CDate))
  # convert to R's 'standard' date format
  # didn't work before collect()
})


#' List of blood pressure observations/recordings
#'
#' Filtered by InternalID (vector patient numbers) and dates
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param intID vector of InternalID
#' @param date_from start date. default is $date_b minus 12 months
#' @param date_to end date (inclusive). default is $date_b
#'
#' @return dataframe of InternalID, BPDate, BP
#'  BP will be a string of two numbers separated by "/"
#' @export
BloodPressure_obs <- function(dMeasure_obj, intID, date_from = NA, date_to = NA) {
  dMeasure_obj$BloodPressure_obs(intID, date_from, date_to)
}
.public(dMeasure, "BloodPressure_obs", function(intID, date_from = NA, date_to = NA) {

  if (is.na(date_to)) {
    date_to <- self$date_b
  }
  if (is.na(date_from)) {
    date_from <- date_to - 365
  } else if (date_from == -Inf) {
    date_from <- as.Date("1900-01-01") # MSSQL doesn't accept -Inf date!
  }

  private$db$observations %>>%
    dplyr::filter(InternalID %in% intID &&
                    ObservationCode %in% c(3,4) &&
                    # systolic or diastolic blood pressure
                    # 3 = systolic, 4 = diastolic
                    ObservationDate <= date_to &&
                    ObservationDate >= date_from) %>>%
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
    dplyr::select(-c(`3`, `4`))

})

#' List of urine albumin observations/recordings
#'
#' Filtered by InternalID (vector patient numbers) and dates
#'
#' the reference date for 'most recent' measurement is 'date_to'
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param intID vector of InternalID
#' @param date_from start date. default is $date_b minus 12 months
#' @param date_to end date (inclusive). default is $date_b
#'
#' @return dataframe of InternalID, UrineAlbuminDate, UrineAlbuminValue, UrineAlbuminUnits
#' @export
UrineAlbumin_obs <- function(dMeasure_obj, intID, date_from = NA, date_to = NA) {
  dMeasure_obj$UrineAlbumin_obs(intID, date_from, date_to)
}
.public(dMeasure, "UrineAlbumin_obs", function(intID, date_from = NA, date_to = NA) {

  if (is.na(date_to)) {
    date_to <- self$date_b
  }
  if (is.na(date_from)) {
    date_from <- date_to - 365
  } else if (date_from == -Inf) {
    date_from <- as.Date("1900-01-01") # MSSQL doesn't accept -Inf date!
  }

  # uses BPCode == 17 or 7 or 18 in $db$reportValues for finding urine albumin results

  #  17 variously labelled 'ACR' or 'Albumin/Creat Ratio' in SAMPLES database
  #   units will be recorded e.g. mg/mmol
  #
  #  18 "UAE"
  #  units:
  #   "mcg/min"
  #
  #  7 "Microalbuminuria"
  #   units can be "g/day" "mg/L" "mg/mmol" "mcg/min"
  #  this might be simultaneously recorded (from the Diabetes Cycle of Care Page)
  #   as BPCode 18, with the same ReportDate and ReportID!, if units are "mcg/min"


  private$db$reportValues %>>%
    dplyr::filter(InternalID %in% intID &&
                    (BPCode == 17 || BPCode == 7 || BPCode == 18) &&
                    ReportDate <= date_to &&
                    ReportDate >= date_from
    ) %>>%
    dplyr::group_by(InternalID) %>>%
    dplyr::filter(ReportDate == max(ReportDate, na.rm = TRUE)) %>>%
    dplyr::filter(ReportID == max(ReportID, na.rm = TRUE)) %>>%
    dplyr::filter(BPCode == min(BPCode, na.rm = TRUE)) %>>%
    # the most recent HbA1C report by InternalID
    # Best Practice's Diabetes Cycle of Care page will record 7+18
    # TOGETHER at the same time, with the same value and same ReportID
    dplyr::select(InternalID, ReportDate, ResultValue, Units) %>>%
    dplyr::rename(UrineAlbuminDate = ReportDate,
                  UrineAlbuminValue = ResultValue,
                  UrineAlbuminUnits = Units) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(UrineAlbuminDate = as.Date(UrineAlbuminDate))
  # convert to R's 'standard' date format
  # didn't work before collect()
})


#' List of patients with persistent proteinuria
#'
#' Filtered by InternalID (vector patient numbers) and dates
#'
#' From http://cvdcheck.org.au/pdf/Absolute_CVD_Risk_Full_Guidelines.pdf
#'
#' Proteinuria is defined as
#'   urinary albumin:creatinine ratio (UACR)
#'   > 35 mg/mmol in females
#'   > 25 mg/mmol in males.
#'
#' Persistent proteinuria is defined as 2 positive measurements, 3 months apart.
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param intID vector of InternalID
#' @param date_from start date. default is "1900-01-01" (accepts -Inf)
#' @param date_to end date (inclusive). default is $date_b
#'
#' @return dataframe of InternalID, PersistentProteinuria
#' @export
PersistentProteinuria_obs <- function(dMeasure_obj, intID, date_from = NA, date_to = NA) {
  dMeasure_obj$PersistentProteinuria_obs(intID, date_from, date_to)
}
.public(dMeasure, "PersistentProteinuria_obs", function(intID, date_from = NA, date_to = NA) {

  intID <- c(intID, -1) # can't search on empty list! add dummy value
  if (is.na(date_to)) {
    date_to <- self$date_b
  }
  if (is.na(date_from) || date_from == -Inf) {
    date_from <- as.Date("1900-01-01")
  } # MSSQL doesn't accept -Inf date!


  # uses BPCode == 17 or 7 in $db$reportValues for finding urine albumin results

  #  17 variously labelled 'ACR' or 'Albumin/Creat Ratio' in SAMPLES database
  #   units will be recorded e.g. mg/mmol
  #
  #  18 "UAE"
  #  units:
  #   "mcg/min"
  #
  #  7 "Microalbuminuria"
  #   units can be "g/day" "mg/L" "mg/mmol" "mcg/min"
  #  this might be simultaneously recorded (from the Diabetes Cycle of Care Page)
  #   as BPCode 18, with the same ReportDate and ReportID!, if units are "mcg/min"

  results <- private$db$reportValues %>>%
    dplyr::left_join(private$db$patients %>>%
                       dplyr::filter(InternalID %in% intID) %>>%
                       dplyr::select(InternalID, Sex), # add 'Sex', a character string
                     by = "InternalID") %>>%
    dplyr::filter(InternalID %in% intID &&
                    (BPCode == 17 || (BPCode == 7 && Units == "mg/mmol")) &&
                    # two possible ways the result might be recorded
                    (ResultValue > 35 || (ResultValue > 25 && Sex == "Male")) &&
                    # more than 35 for females, more than 25 for males
                    ReportDate <= date_to &&
                    ReportDate >= date_from
    )

  earliest_result <- results %>>%
    dplyr::group_by(InternalID) %>>%
    dplyr::filter(ReportDate == min(ReportDate, na.rm = TRUE)) %>>%
    dplyr::ungroup() %>>%
    dplyr::rename(EarliestDate = ReportDate) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(EarliestDate = as.Date(EarliestDate))


  latest_result <- results %>>%
    dplyr::group_by(InternalID) %>>%
    dplyr::filter(ReportDate == max(ReportDate, na.rm = TRUE)) %>>%
    dplyr::ungroup() %>>%
    dplyr::rename(LatestDate = ReportDate) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(LatestDate = as.Date(LatestDate))

  persistentProteinuria <- earliest_result %>>%
    dplyr::left_join(latest_result, by = "InternalID") %>>%
    dplyr::mutate(PersistentProteinuria =
                    dMeasure::calc_age_months(EarliestDate, LatestDate) >= 3) %>>%
    # if the earliest and latest elevated proteinuria dates are
    # more than 3 months apart

    dplyr::select(InternalID, PersistentProteinuria)

  return(persistentProteinuria)
})
