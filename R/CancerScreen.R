##### Bowel cancer screening ###########################################
#' CancerScreen - bowel cancer and cervical screening
#'
#' @name cancerscreen
#' @include dMeasure.R
#' @include appointments.R
#' needs access to dMeasure and appointments functions and variables
#'
#' @include calculation_definitions.R
#' need function 'interval'
#'
#' @include fomantic_definitions_tags.R
#' 'tags only' fomantic/semantic definitions
NULL


#' Bowel cancer screening list
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from from date range (default self$date_a)
#' @param date_to to date range (default self$date_b)
#' @param clinicians list of clinicians (default self$clinicians)
#' @param appointments_list dataframe, list of appointments to search
#'  (as opposed to using self$appointments_list)
#'  needs Age (presumably at time of appointment),
#'        InternalID (the EMR's identification code fo the patient)
#' @param lazy = FALSE recalculate an appointment list
#' @param action = FALSE includes 'OutOfDate' field
#' @param screentag = FALSE optionally add a fomantic/semantic HTML description of 'action'
#' @param screentag_print = TRUE optionally add a 'printable' description of 'action'
#'
#' @return list of appointments (with patient details)
#'  adds the following fields
#'   TestDate (date object) - date and
#'   TestName - description of the most recent bowel cancer screening test (if any)
#'   OutOfDateTest - 1 = never done, 2 = overdue, 3 = 'up-to-date'
#'
list_fobt <- function(dMeasure_obj, date_from = NA, date_to = NA, clinicians = NA,
                      appointments_list = NULL,
                      lazy = FALSE,
                      action = FALSE, screentag = FALSE, screentag_print = TRUE) {

  dMeasure$list_fobt(date_from, date_to, clinicians,
                     appointments_list,
                     lazy, action, screentag, screentag_print)
}

.public(dMeasure, "list_fobt", function(date_from = NA, date_to = NA, clinicians = NA,
                                        appointments_list = NULL,
                                        lazy = FALSE,
                                        action = FALSE, screentag = FALSE, screentag_print = TRUE) {

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
    stop("Choose at least one clinicians\'s appointment to view")
  }

  if (is.null(appointments_list) & !lazy) {
    self$list_appointments(date_from, date_to, clinicians, lazy = FALSE)
    # if not 'lazy' evaluation, then re-calculate self$appointments_billings
    # (that is automatically done by calling the $billed_appointments method)
  }

  if (is.null(appointments_list)) {
    appointments_list <- self$appointments_list
  }

  ### a lot of definitions
  ##### bowel cancer (FOBT) definitions ######

  bowel_cancer_screen_terms <-
    c(paste("(VALUES('%FOB%'), ('%OCCULT%'), ('%FAECAL HUMAN HAEMOGLOBIN%'),",
            "('%OCB NATIONAL SCREENING%'), ('%FHB%'), ('%FAECAL BLOOD%'),",
            "('%FAECAL IMMUNOCHEMICAL TEST%'), ('%FAECAL HAEMOGLOBIN%'),",
            "('%COLONOSCOPY%'), ('%COLONOSCOPE%')) AS tests(fobtnames)"))

  fobt_investigation_query <-
    paste('SELECT InternalID, Collected, TestName FROM dbo.BPS_Investigations',
          'INNER JOIN', bowel_cancer_screen_terms,
          'ON TestName LIKE tests.fobtnames')
  # SQL code to find investigations which could be bowel cancer screening items

  fobt_letter_subject_query <-
    paste('SELECT InternalID, CorrespondenceDate, Subject FROM dbo.BPS_CorrespondenceIn',
          'INNER JOIN', bowel_cancer_screen_terms,
          'ON Subject LIKE tests.fobtnames')

  fobt_letter_detail_query <-
    paste('SELECT InternalID, CorrespondenceDate, Detail FROM dbo.BPS_CorrespondenceIn',
          'INNER JOIN', bowel_cancer_screen_terms,
          'ON Detail LIKE tests.fobtnames')

  fobt_result_query <-
    paste("SELECT InternalID, ReportDate, ResultName FROM dbo.BPS_ReportValues",
          "WHERE LoincCode IN ('2335-8','27396-1','14563-1','14564-9','14565-6',",
          "'12503-9','12504-7','27401-9','27925-7','27926-5',",
          "'57905-2','56490-6','56491-4','29771-3')")

  ##### search proper #####################

  screen_fobt_list <- appointments_list %>>%
    dplyr::filter(Age >= 50 & Age <=75) # from age 50 to 75 years inclusive

  screen_fobt_ix <- screen_fobt_list %>>%
    dplyr::left_join(
      dplyr::bind_rows(dplyr::inner_join(screen_fobt_list,
                                         private$emr_db$dbGetQuery(
                                           fobt_investigation_query
                                         ) %>>%
                                           dplyr::collect() %>>%
                                           dplyr::rename(TestDate = Collected),
                                         by = 'InternalID'),
                       dplyr::inner_join(screen_fobt_list,
                                         private$emr_db$dbGetQuery(
                                           fobt_letter_subject_query
                                         ) %>>%
                                           dplyr::collect() %>>%
                                           dplyr::rename(TestDate = CorrespondenceDate,
                                                         TestName = Subject),
                                         by = 'InternalID'),
                       dplyr::inner_join(screen_fobt_list,
                                         private$emr_db$dbGetQuery(
                                           fobt_letter_detail_query
                                         ) %>>%
                                           dplyr::collect() %>>%
                                           dplyr::rename(TestDate = CorrespondenceDate,
                                                         TestName = Detail),
                                         by = 'InternalID'),
                       dplyr::inner_join(screen_fobt_list,
                                         private$emr_db$dbGetQuery(
                                           fobt_result_query
                                         ) %>>%
                                           dplyr::collect() %>>%
                                           dplyr::rename(TestDate = ReportDate,
                                                         TestName = ResultName),
                                         by = 'InternalID')
      ) %>>%
        dplyr::mutate(TestDate = as.Date(substr(TestDate, 1, 10))),
      # remove time from date
      by = NULL) %>>%
    dplyr::mutate(TestDate = as.Date(ifelse(TestDate > AppointmentDate,
                                            -Inf,
                                            TestDate), origin = "1970-01-01"),
                  TestDate = as.Date(ifelse(is.na(TestDate), -Inf, TestDate),
                                     origin = "1970-01-01")) %>>%
    dplyr::mutate(TestName = ifelse(TestDate == -Inf, NA, TestName)) %>>%
    # only test dates (and names) less than the joined appointment date are kept
    dplyr::group_by(InternalID, AppointmentDate) %>>%
    # group by patient ID (need most recent investigation for each patient)
    # only keep the latest(/recent) dated investigation prior to each appointment
    dplyr::filter(TestDate == max(TestDate, na.rm = TRUE)) %>>%
    dplyr::ungroup() %>>%
    dplyr::mutate(OutOfDateTest =
                    dplyr::case_when((TestDate == -Inf) ~ 1,
                                     # if no date (no detected test)
                                     dMeasure::interval(TestDate, AppointmentDate)$year >= 2 ~ 2,
                                     # if old (2 years or more)
                                     TRUE ~ 3)) %>>%   # if up-to-date
    tidyr::replace_na(list(TestName = 'FOBT'))

  return_selection <- c("Patient", "InternalID", "AppointmentDate", "AppointmentTime",
                        "Provider", "DOB", "Age")

  if (action) {
    return_selection <- c(return_selection, "OutOfDateTest")
    # include this field in the returned table
  }

  if (screentag) {
    screen_fobt_ix <- screen_fobt_ix %>>%
      dplyr::mutate(screentag =
                      dMeasure::semantic_tag(
                        trimws(TestName),
                        colour = c('red', 'yellow', 'green')[OutOfDateTest],
                        popuphtml = paste0("<h4>Date : ", TestDate, "</h4>"))
      )

    return_selection <- c(return_selection, "screentag")
  }

  if (screentag_print) {
    screen_fobt_ix <- screen_fobt_ix %>>%
      dplyr::mutate(screentag_print =
                      trimws(paste0(trimws(TestName),
                                    # the Testname in BP can have huge whitespace!
                                    dplyr::case_when(OutOfDateTest == 1 ~ " (Never Done) ",
                                                     OutOfDateTest == 2 ~ " (OVERDUE) ",
                                                     OutOfDateTest == 3 ~ " "),
                                    dplyr::if_else(OutOfDateTest != 1,
                                                   paste0("(Date:", TestDate, ")"),
                                                   "")))
      )

    return_selection <- c(return_selection, "screentag_print")
  }

  screen_fobt_ix <- screen_fobt_ix %>>%
    dplyr::select(return_selection)

  return(screen_fobt_ix)
})



#' Cervical screening list
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from from date range (default self$date_a)
#' @param date_to to date range (default self$date_b)
#' @param clinicians list of clinicians (default self$clinicians)
#' @param appointments_list dataframe, list of appointments to search
#'  (as opposed to using self$appointments_list)
#'  needs Age (presumably at time of appointment),
#'        InternalID (the EMR's identification code fo the patient)
#' @param lazy = FALSE recalculate an appointment list
#' @param action = FALSE includes 'OutOfDate' field
#' @param screentag = FALSE optionally add a fomantic/semantic HTML description of 'action'
#' @param screentag_print = TRUE optionally add a 'printable' description of 'action'
#'
#' @return list of appointments (with patient details)
#'  adds the following fields
#'   TestDate (date object) - date and
#'   TestName - description of the most recent bowel cancer screening test (if any)
#'   OutOfDateTest - 1 = never done, 2 = overdue, 3 = 'up-to-date'
#'
list_cst <- function(dMeasure_obj, date_from = NA, date_to = NA, clinicians = NA,
                     appointments_list = NULL,
                     lazy = FALSE,
                     action = FALSE, screentag = FALSE, screentag_print = TRUE) {

  dMeasure$list_cst(date_from, date_to, clinicians,
                    appointments_list,
                    lazy, action, screentag, screentag_print)
}

.public(dMeasure, "list_cst", function(date_from = NA, date_to = NA, clinicians = NA,
                                       appointments_list = NULL,
                                       lazy = FALSE,
                                       action = FALSE, screentag = FALSE, screentag_print = TRUE) {

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
    stop("Choose at least one clinicians\'s appointment to view")
  }

  if (is.null(appointments_list) & !lazy) {
    self$list_appointments(date_from, date_to, clinicians, lazy = FALSE)
    # if not 'lazy' evaluation, then re-calculate self$appointments_billings
    # (that is automatically done by calling the $billed_appointments method)
  }

  if (is.null(appointments_list)) {
    appointments_list <- self$appointments_list
  }

  ##### search proper #####################

  screen_cst_id <- self$cst_eligible_list(appointments_list %>>%
                                            dplyr::select(InternalID,
                                                          Date = AppointmentDate))

  screen_cst_list <- appointments_list %>>%
    dplyr::filter(InternalID %in% screen_cst_id)

  screen_cst_ix <- screen_cst_list %>>%
    dplyr::left_join(
      dplyr::bind_rows(dplyr::inner_join(screen_cst_list,
                                         private$db$papsmears %>>%
                                           dplyr::filter(InternalID %in% screen_cst_id) %>>%
                                           dplyr::rename(TestDate = PapDate,
                                                         TestName = CSTType),
                                         by = 'InternalID', copy = TRUE),
                       dplyr::inner_join(screen_cst_list,
                                         private$db$investigations %>>%
                                           dplyr::filter(InternalID %in% screen_cst_id &&
                                                           (TestName %like% "%CERVICAL SCREENING%" ||
                                                              TestName %like% "%PAP SMEAR%")) %>>%
                                           dplyr::rename(TestDate = Reported),
                                         by = 'InternalID', copy = TRUE)) %>>%
        dplyr::mutate(TestDate = as.Date(substr(TestDate, 1, 10))),
      # remove time from date
      by = NULL) %>>%
    dplyr::mutate(TestDate = as.Date(ifelse(TestDate > AppointmentDate,
                                            -Inf,
                                            TestDate), origin = "1970-01-01"),
                  TestDate = as.Date(ifelse(is.na(TestDate), -Inf, TestDate),
                                     origin = "1970-01-01")) %>>%
    dplyr::mutate(TestName = ifelse(TestDate == -Inf, NA, TestName)) %>>%
    # only test dates (and names) less than the joined appointment date are kept
    dplyr::group_by(InternalID, AppointmentDate) %>>%
    # group by patient ID (need most recent investigation for each patient)
    # only keep the latest(/recent) dated investigation prior to each appointment
    dplyr::filter(TestDate == max(TestDate, na.rm = TRUE)) %>>%
    dplyr::ungroup() %>>%
    dplyr::mutate(TestAge = dMeasure::interval(TestDate, AppointmentDate)$year) %>>%
    dplyr::mutate(OutOfDateTest =
                    dplyr::case_when((TestDate == -Inf) ~ 1,
                                     # if no date (no detected test)
                                     TestAge < 2 ~ 3,
                                     TestAge >= 5 ~ 2,
                                     # if old (5 years for either cervical screening HPV or Pap)
                                     grepl('pap', TestName, ignore.case = TRUE) ~ 2,
                                     # otherwise if 'Pap' and more than two years
                                     # last case is 2 to 4 years (inclusive) and CST
                                     TRUE ~ 3)) %>>%
    tidyr::replace_na(list(TestName = 'Cervical screening'))

  return_selection <- c("Patient", "InternalID", "AppointmentDate", "AppointmentTime",
                        "Provider", "DOB", "Age")

  if (action) {
    return_selection <- c(return_selection, "OutOfDateTest")
    # include this field in the returned table
  }

  if (screentag) {
    screen_cst_ix <- screen_cst_ix %>>%
      dplyr::mutate(screentag =
                      dMeasure::semantic_tag(
                        trimws(TestName),
                        colour = c('red', 'yellow', 'green')[OutOfDateTest],
                        popuphtml = paste0("<h4>Date : ", TestDate, "</h4>"))
      )

    return_selection <- c(return_selection, "screentag")
  }

  if (screentag_print) {
    screen_cst_ix <- screen_cst_ix %>>%
      dplyr::mutate(screentag_print =
                      trimws(paste0(trimws(TestName),
                                    # the Testname in BP can have huge whitespace!
                                    dplyr::case_when(OutOfDateTest == 1 ~ " (Never Done) ",
                                                     OutOfDateTest == 2 ~ " (OVERDUE) ",
                                                     OutOfDateTest == 3 ~ " "),
                                    dplyr::if_else(OutOfDateTest != 1,
                                                   paste0("(Date:", TestDate, ")"),
                                                   "")))
      )

    return_selection <- c(return_selection, "screentag_print")
  }

  screen_cst_ix <- screen_cst_ix %>>%
    dplyr::select(return_selection)

  return(screen_cst_ix)
})
