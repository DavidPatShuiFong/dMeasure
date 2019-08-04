##### conditions ###########################################
#' condition methods
#'
#' @name conditions
#' @include dMeasure.R
#' @include appointments.R
#' needs access to dMeasure and appointments functions and variables
NULL

#' list of patients with diabetes
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments list. requires $InternalID
#'  by default, the appointments in $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
diabetes_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$diabetes_list(appointments)
}
.public("diabetes_list", function(appointments = NULL) {
  # Best Practice Diabetes code
  diabetes_codes <- c(3, 775, 776, 778, 774, 7840, 11998)

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered
    # just needs $InternalID
  }

  intid <- appointments %>>% dplyr::pull(InternalID)

  # Returns vector of InternalID of patients who have diabetes
  private$db$history %>>%
    dplyr::filter(ConditionID %in% diabetes_codes) %>>%
    dplyr::filter(InternalID %in% intid) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})


### Asthma sub-code
#' list of patients with diabetes
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments list. requires $InternalID
#'  by default, the appointments in $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
asthma_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$asthma_list(appointments)
}
.public("asthma_list", function(appointments = NULL) {
  # Best Practice Asthma code
  asthma_codes <- c(281, 285, 283, 284, 282)

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered
    # just needs $InternalID
  }

  intid <- appointments %>>% dplyr::pull(InternalID)

  # Returns vector of InternalID of patients who have diabetes
  private$db$history %>>%
    dplyr::filter(ConditionID %in% asthma_codes) %>>%
    dplyr::filter(InternalID %in% intid) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### Aboriginal and Torres Strait Islander sub-code
#' list of patients recorded ATSI ethnicity
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments list. requires $InternalID
#'  by default, the appointments in $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
atsi_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$atsi_list(appointments)
}
.public("atsi_list", function(appointments = NULL) {
  # Best Practice Aboriginal or Torres Strait Islander codes
  atsi_codes <- c("Aboriginal", "Torres Strait Islander",
                  "Aboriginal/Torres Strait Islander")

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered
    # just needs $InternalID
  }

  intid <- appointments %>>% dplyr::pull(InternalID)

  # returns vector of InternalID of patients who are
  # Aboriginal or Torres Strait Islander as recorded in patient into
  private$db$patients %>>%
    dplyr::filter(Ethnicity %in% atsi_codes) %>>%
    dplyr::filter(InternalID %in% intid) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### Maligancy sub-code
#' list of patients recorded malignancy
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments list. requires $InternalID
#'  by default, the appointments in $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
malignancy_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$malignancy_list(appointments)
}
.public("malignancy_list", function(appointments = NULL) {
  # Best Practice codes including for many cancers, carcinomas, lymphomas, leukaemias
  malignancy_codes <- c(463, 478, 485, 7845, 449, 6075, 453, 456, 473, 490, 11927,
                        445, 444, 446, 447, 448, 451, 457, 458, 459, 460, 462, 469, 454,
                        472, 474, 477, 480, 481, 482, 486, 487, 488, 489, 11911, 491,
                        492, 9391, 7751, 483, 8027, 470, 471, 476, 8261, 2475, 6835,
                        6827, 6817, 6818, 6813, 6824, 6830, 6820, 6822, 6819, 6815, 6828,
                        6826, 6821, 6833, 6831, 6823, 6834, 6825, 6832, 6829, 6814, 3221,
                        4975, 2273, 2287, 4976, 5604, 5599, 5602, 5600, 5609, 5601, 5603,
                        5608, 5607, 329, 2350, 2222, 5054, 2223, 6541, 2224, 2225, 2226,
                        6003, 5480, 2230, 452, 3215, 7005, 2173, 2174, 2175, 2176, 2177,
                        2178, 2179, 1440)

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered
    # just needs $InternalID
  }

  intid <- appointments %>>% dplyr::pull(InternalID)

  # returns vector of InternalID of patients who
  # have a recorded malignancy
  private$db$history %>>%
    dplyr::filter(ConditionID %in% malignancy_codes) %>>%
    dplyr::filter(InternalID %in% intid) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

.public("hiv_list", function(appointments = NULL) {
  # returns vector of InternalID of patients who
  # have HIV

  # Best Practice codes for HIV
  hiv_codes <- c(1727)

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered
    # just needs $InternalID
  }

  intid <- appointments %>>% dplyr::pull(InternalID)

  private$db$history %>>%
    dplyr::filter(ConditionID %in% hiv_codes) %>>%
    dplyr::filter(InternalID %in% intid) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

.public("haemoglobinopathy_list", function(appointments = NULL) {
  # returns vector of InternalID of patients who
  # have haemoglobinopathy

  # Best Practice codes for haemoglobinopathy
  haemoglobinopathy_codes <- c(205, 208, 209, 210)

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered
    # just needs $InternalID
  }

  intid <- appointments %>>% dplyr::pull(InternalID)

  private$db$history %>>%
    dplyr::filter(ConditionID %in% haemoglobinopathy_codes) %>>%
    dplyr::filter(InternalID %in% intid) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

.public("asplenic_list", function(appointments = NULL) {
  # returns vector of InternalID of patients who
  # are asplenic

  # Best Practice codes for asplenia
  asplenic_codes <- c(3958, 5805, 6493, 3959)

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered
    # just needs $InternalID
  }

  intid <- appointments %>>% dplyr::pull(InternalID)

  private$db$history %>>%
    dplyr::filter(ConditionID %in% asplenic_codes) %>>%
    dplyr::filter(InternalID %in% intid) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

.public("transplant_list", function(appointments = NULL) {
  # returns vector of InternalID of patients who
  # have had transplants

  # Best Practice codes for transplants (not corneal or hair)
  transplant_codes <- c(4160, 3691, 3814, 3826, 12026, 3765, 3989)
  # bone marrow, heart, liver, lung, pancreas, renal, thymus

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered
    # just needs $InternalID
  }

  intid <- appointments %>>% dplyr::pull(InternalID)

  private$db$history %>>%
    dplyr::filter(ConditionID %in% transplant_codes) %>>%
    dplyr::filter(InternalID %in% intid) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

.public("cardiacdisease_list", function(appointments = NULL) {
  # returns vector of InternalID of patients who
  # have had cardiac disease

  # Best Practice codes for cardiac disease
  transplant_codes <- c(7810, 226, 227, 228, 2376, 2377, 2378, 2379, 2380, 2381,
                        2382, 3576, 3577, 3578, 3579, 1534, 2556, 6847, 7847,
                        1347, 2376, 2377, 2378, 2379, 2380, 2381, 2382, 7847, 6847, 2556)
  # cyanotic congenital heart disease, ischaemic heart disease, AMI and congestive failure

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered
    # just needs $InternalID
  }

  intid <- appointments %>>% dplyr::pull(InternalID)

  private$db$history %>>%
    dplyr::filter(ConditionID %in% transplant_codes) %>>%
    dplyr::filter(InternalID %in% intid) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

.public("trisomy21_list", function(appointments = NULL) {
  # returns vector of InternalID of patients who
  # have trisomy 21

  # Best Practice codes for trisomy 21
  trisomy21_codes <- c(836)

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered
    # just needs $InternalID
  }

  intid <- appointments %>>% dplyr::pull(InternalID)

  private$db$history %>>%
    dplyr::filter(ConditionID %in% trisomy21_codes) %>>%
    dplyr::filter(InternalID %in% intid) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

.public("bmi30_list", function(appointments = NULL) {
  # returns vector of InternalID of patients who
  # have bmi 30 or more (obesity)

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered
    # just needs $InternalID
  }

  appointments %>>% dplyr::collect() %>>%
    dplyr::inner_join(private$db$observations %>>%
                        dplyr::filter(DATACODE == 9),
                      # this is BMI. also in DATANAME, but different spellings/cases
                      by = "InternalID", copy = TRUE) %>>%
    dplyr::filter(as.Date(OBSDATE) <= as.Date(AppointmentDate)) %>>%
    # observation done before the appointment time
    dplyr::group_by(InternalID, AppointmentDate, AppointmentTime) %>>%
    dplyr::slice(which.max(OBSDATE)) %>>% # choose the observation with the most recent observation date
    # unfortunately, as the code stands, this generates a vector which is not appointment date specific
    # if a range of appointment dates has been chosen
    dplyr::ungroup() %>>%
    dplyr::filter(DATAVALUE >= 30) %>>% # those with BMI >= 30
    dplyr::pull(InternalID) %>>%
    unique()
})

.public("chroniclungdisease_list", function(appointments = NULL) {
  # returns vector of InternalID of patients who
  # have lung disease such as bronchiectasis, cystic fibrosis, COPD/COAD
  # asthma is in a separate list

  # Best Practice codes for trisomy 21
  cld_codes <- c(598, 4740, 414, 702)

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered
    # just needs $InternalID
  }

  intid <- appointments %>>% dplyr::pull(InternalID)

  private$db$history %>>%
    dplyr::filter(ConditionID %in% cld_codes) %>>%
    dplyr::filter(InternalID %in% intid) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

.public("neurologic_list", function(appointments = NULL) {
  # returns vector of InternalID of patients who
  # have neurologic disease

  # Best Practice codes for neurology
  neuro_codes <- c(2351, 963, 965, 966, 968, 969, 971, 6604,
                   2022, 2630, 3093)
  # multiple sclerosis, epilepsy, spinal cord injury, paraplegia, quadriplegia

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered
    # just needs $InternalID
  }

  intid <- appointments %>>% dplyr::pull(InternalID)

  private$db$history %>>%
    dplyr::filter(ConditionID %in% neuro_codes) %>>%
    dplyr::filter(InternalID %in% intid) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

.public("chronicliverdisease_list", function(appointments = NULL) {
  # returns vector of InternalID of patients who
  # have chronic liver disease

  # Best Practice codes for chronic liver disease
  cld_codes <- c(11763, 584, 81)
  # liver disease (BP doesn't have 'chronic liver disease'!), cirrhosis, alcoholism

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered
    # just needs $InternalID
  }

  intid <- appointments %>>% dplyr::pull(InternalID)

  private$db$history %>>%
    dplyr::filter(ConditionID %in% cld_codes) %>>%
    dplyr::filter(InternalID %in% intid) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

.public("chronicrenaldisease_list", function(appointments = NULL) {
  # returns vector of InternalID of patients who
  # have chronic renal disease

  # Best Practice codes for chronic liver disease
  crf_codes <- c(662, 258, 3132, 662, 2486, 2487, 6379, 2489, 7469, 1274,
                 7502, 7503, 7504, 7505, 7506, 2882)
  # chronic renal failure, renal impairment, dialysis

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered
    # just needs $InternalID
  }

  intid <- appointments %>>% dplyr::pull(InternalID)

  private$db$history %>>%
    dplyr::filter(ConditionID %in% crf_codes) %>>%
    dplyr::filter(InternalID %in% intid) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

.public("pregnant_list", function(appointments = NULL) {
  # returns vector of InternalID of patients who
  # are pregnant

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered
    # just needs $InternalID
  }

  intid <- appointments %>>% dplyr::pull(InternalID)

  appointments %>>% dplyr::collect() %>>%
    dplyr::inner_join(private$db$pregnancies %>>%
                        dplyr::filter(InternalID %in% intid),
                      by = "InternalID", copy = TRUE) %>>%
    dplyr::filter(is.null(ENDDATE) | as.Date(ENDDATE) > as.Date(AppointmentDate)) %>>%
    dplyr::filter((as.Date(EDCBYDATE) > as.Date(AppointmentDate)) &
                    (as.Date(EDCBYDATE) < as.Date(AppointmentDate+280))) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})
