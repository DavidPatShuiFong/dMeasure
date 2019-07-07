##### Bowel cancer screening ###########################################
#' CancerScreen - bowel cancer
#'
#' @name cancerscreen_bowel
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
#' @param appointments_list dataframe, list of appointments to search
#'  needs Age (presumably at time of appointment),
#'        InternalID (the EMR's identification code fo the patient)
#' @param emr_db R6 object, accesss to Best Practice (EMR) database
#' @param action=TRUE includes 'OutOfDate' field
#' @param screentag=FALSE optionally add a fomantic/semantic HTML description of 'action'
#' @param screentag_print=FALSE optionally add a 'printable' description of 'action'
#'
#' @return list of appointments (with patient details)
#'  adds the following fields
#'   TestDate (date object) - date and
#'   TestName - description of the most recent bowel cancer screening test (if any)
#'   OutOfDateTest - 1 = never done, 2 = overdue, 3 = 'up-to-date'
#'
fobt_list <- function(dMeasure_obj, appointments_list = NA, emr_db = NA,
                      action = TRUE, screentag = FALSE, screentag_print = FALSE) {

  dMeasure$fobt_list(appointments_list, emr_db, action, screentag, screentag_print)
}

.public("fobt_list", function(appointments_list = NA, emr_db = NA,
                              action = TRUE, screentag = FALSE, screentag_print = FALSE) {

  if (is.na(appointments_list)) {
    appointments_list <- self$appointments_list
    # if no default provided
  }
  if (is.null(appointments_list)) {
    stop("No appointments provided in search list. Perhaps $list_appointments() first?")
  }
  if (is.na(emr_db)) {
    emr_db <- self$emr_db
    # if no default provided
  }

  ### a lot of definitions
  ##### bowel cancer (FOBT) definitions ######

  bowel_cancer_screen_terms <-
    c("(VALUES('%FOB%'), ('%OCCULT%'), ('%FAECAL HUMAN HAEMOGLOBIN%'),
      ('%OCB NATIONAL SCREENING%'), ('%FHB%'), ('%FAECAL BLOOD%'),
      ('%FAECAL IMMUNOCHEMICAL TEST%'), ('%FAECAL HAEMOGLOBIN%'),
      ('%COLONOSCOPY%'), ('%COLONOSCOPE%')) AS tests(fobtnames)")

  fobt_investigation_query <-
    paste('SELECT InternalID, Collected, TestName FROM dbo.BPS_Investigations
          INNER JOIN', bowel_cancer_screen_terms,
          'ON TestName LIKE tests.fobtnames')
  # SQL code to find investigations which could be bowel cancer screening items

  fobt_letter_subject_query <-
    paste('SELECT InternalID, CorrespondenceDate, Subject FROM dbo.BPS_CorrespondenceIn
          INNER JOIN', bowel_cancer_screen_terms,
          'ON Subject LIKE tests.fobtnames')

  fobt_letter_detail_query <-
    paste('SELECT InternalID, CorrespondenceDate, Detail FROM dbo.BPS_CorrespondenceIn
          INNER JOIN', bowel_cancer_screen_terms,
          'ON Detail LIKE tests.fobtnames')

  fobt_result_query <-
    paste("SELECT InternalID, ReportDate, ResultName FROM dbo.BPS_ReportValues
          WHERE LoincCode IN ('2335-8','27396-1','14563-1','14564-9','14565-6',
          '12503-9','12504-7','27401-9','27925-7','27926-5',
          '57905-2','56490-6','56491-4','29771-3')")

  ##### search proper #####################

  screen_fobt_list <- appointments_list %>>%
    dplyr::filter(Age >= 50 & Age <=75) # from age 50 to 75 years inclusive

  screen_fobt_ix <- screen_fobt_list %>>%
    dplyr::left_join(
      dplyr::bind_rows(dplyr::inner_join(screen_fobt_list,
                                         DBI::dbGetQuery(emr_db$conn(),
                                                         fobt_investigation_query) %>>%
                                           dplyr::collect() %>>%
                                           dplyr::rename(TestDate = Collected),
                                         by = 'InternalID'),
                       dplyr::inner_join(screen_fobt_list,
                                         DBI::dbGetQuery(emr_db$conn(),
                                                         fobt_letter_subject_query) %>>%
                                           dplyr::collect() %>>%
                                           dplyr::rename(TestDate = CorrespondenceDate,
                                                         TestName = Subject),
                                         by = 'InternalID'),
                       dplyr::inner_join(screen_fobt_list,
                                         DBI::dbGetQuery(emr_db$conn(),
                                                         fobt_letter_detail_query) %>>%
                                           dplyr::collect() %>>%
                                           dplyr::rename(TestDate = CorrespondenceDate,
                                                         TestName = Detail),
                                         by = 'InternalID'),
                       dplyr::inner_join(screen_fobt_list,
                                         DBI::dbGetQuery(emr_db$conn(),
                                                         fobt_result_query) %>>%
                                           dplyr::collect() %>>%
                                           dplyr::rename(TestDate = ReportDate,
                                                         TestName = ResultName),
                                         by = 'InternalID')
      ) %>>%
        dplyr::mutate(TestDate = as.Date(substr(TestDate, 1, 10))) %>>%
        # remove time from date
        dplyr::group_by(InternalID) %>>%
        # group by patient ID (need most recent investigation for each patient)
        # only keep the latest(/recent) dated investigation
        dplyr::filter(TestDate == max(TestDate, na.rm = TRUE)),
      by = NULL) %>>%
    dplyr::mutate(OutOfDateTest =
                    dplyr::case_when(is.na(TestDate) ~ 1,
                                     # if no date (no detected test)
                                     interval(TestDate, AppointmentDate)$year >= 2 ~ 2,
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
                      semantic_tag(
                        trimws(TestName),
                        colour = c('red', 'yellow', 'green')[OutOfDateTest],
                        popuphtml = paste0("<h4>Date : ", TestDate, "</h4>"))
                    )

    return_selection <- c(return_selection, "screentag")
  }

  if (screentag_print) {
    screen_fobt_ix <- screen_fobt_ix %>>%
      dplyr::mutate(screentag_print =
                      paste0(trimws(TestName), # the Testname in BP can have huge whitespace!
                             dplyr::case_when(OutOfDateTest == 1 ~ " (Never Done) ",
                                              OutOfDateTest == 2 ~ " (OVERDUE) ",
                                              OutOfDateTest == 3 ~ " "),
                             dplyr::if_else(OutOfDateTest != 1,
                                            paste0("(Date:", TestDate, ")"),
                                            ""))
      )

      return_selection <- c(return_selection, "screentag_print")
  }

  screen_fobt_ix <- screen_fobt_ix %>>%
    dplyr::select(return_selection)

  return(screen_fobt_ix)
})
