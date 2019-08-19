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
#' @param appointments dataframe of appointments $InternalID and $Date
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
diabetes_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$diabetes_list(appointments)
}
.public(dMeasure, "diabetes_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # Returns vector of InternalID of patients who have diabetes

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  # Best Practice Diabetes code
  diabetes_codes <- c(3, 775, 776, 778, 774, 7840, 11998)

  private$db$history %>>%
    dplyr::filter(ConditionID %in% diabetes_codes) %>>%
    dplyr::filter(InternalID %in% intID) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})


### Asthma sub-code
#' list of patients with diabetes
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
asthma_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$asthma_list(appointments)
}
.public(dMeasure, "asthma_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # Returns vector of InternalID of patients who have diabetes

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  # Best Practice Asthma code
  asthma_codes <- c(281, 285, 283, 284, 282)

  private$db$history %>>%
    dplyr::filter(ConditionID %in% asthma_codes) %>>%
    dplyr::filter(InternalID %in% intID) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### Aboriginal and Torres Strait Islander sub-code
#' list of patients recorded ATSI ethnicity
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
atsi_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$atsi_list(appointments)
}
.public(dMeasure, "atsi_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # returns vector of InternalID of patients who are
  # Aboriginal or Torres Strait Islander as recorded in patient into

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  # Best Practice Aboriginal or Torres Strait Islander codes
  atsi_codes <- c("Aboriginal", "Torres Strait Islander",
                  "Aboriginal/Torres Strait Islander")


  private$db$patients %>>%
    dplyr::filter(Ethnicity %in% atsi_codes) %>>%
    dplyr::filter(InternalID %in% intID) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### Maligancy sub-code
#' list of patients recorded malignancy
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
malignancy_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$malignancy_list(appointments)
}
.public(dMeasure, "malignancy_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # returns vector of InternalID of patients who
  # have a recorded malignancy

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

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


  private$db$history %>>%
    dplyr::filter(ConditionID %in% malignancy_codes) %>>%
    dplyr::filter(InternalID %in% intID) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### HIV sub-code
#' list of patients HIV
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
hiv_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$hiv_list(appointments)
}
.public(dMeasure, "hiv_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # returns vector of InternalID of patients who
  # have HIV

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  # Best Practice codes for HIV
  hiv_codes <- c(1727)

  private$db$history %>>%
    dplyr::filter(ConditionID %in% hiv_codes) %>>%
    dplyr::filter(InternalID %in% intID) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### Haemoglobinopathy sub-code
#' list of patients Haemoglobinopathy
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
haemoglobinopathy_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$haemoglobinopathy_list(appointments)
}
.public(dMeasure, "haemoglobinopathy_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # returns vector of InternalID of patients who
  # have haemoglobinopathy

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  # Best Practice codes for haemoglobinopathy
  haemoglobinopathy_codes <- c(205, 208, 209, 210)

  private$db$history %>>%
    dplyr::filter(ConditionID %in% haemoglobinopathy_codes) %>>%
    dplyr::filter(InternalID %in% intID) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### Asplenia sub-code
#' list of patients Asplenia
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
asplenia_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$asplenia_list(appointments)
}
.public(dMeasure, "asplenic_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # returns vector of InternalID of patients who
  # are asplenic

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  # Best Practice codes for asplenia
  asplenic_codes <- c(3958, 5805, 6493, 3959)

  private$db$history %>>%
    dplyr::filter(ConditionID %in% asplenic_codes) %>>%
    dplyr::filter(InternalID %in% intID) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### Transplant sub-code
#' list of patients with transplant
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
transplant_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$transplant_list(appointments)
}
.public(dMeasure, "transplant_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # returns vector of InternalID of patients who
  # have had transplants

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  # Best Practice codes for transplants (not corneal or hair)
  transplant_codes <- c(4160, 3691, 3814, 3826, 12026, 3765, 3989)
  # bone marrow, heart, liver, lung, pancreas, renal, thymus

  private$db$history %>>%
    dplyr::filter(ConditionID %in% transplant_codes) %>>%
    dplyr::filter(InternalID %in% intID) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### Cardiac sub-code
#' list of patients with cardiac conditioins
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
cardiacdisease_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$cardiacdisease_list(appointments)
}
.public(dMeasure, "cardiacdisease_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # returns vector of InternalID of patients who
  # have had cardiac disease

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  # Best Practice codes for cardiac disease
  transplant_codes <- c(7810, 226, 227, 228, 2376, 2377, 2378, 2379, 2380, 2381,
                        2382, 3576, 3577, 3578, 3579, 1534, 2556, 6847, 7847,
                        1347, 2376, 2377, 2378, 2379, 2380, 2381, 2382, 7847, 6847, 2556)
  # cyanotic congenital heart disease, ischaemic heart disease, AMI and congestive failure

  private$db$history %>>%
    dplyr::filter(ConditionID %in% transplant_codes) %>>%
    dplyr::filter(InternalID %in% intID) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### Trisomy 21 sub-code
#' list of patients with trisomy21
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
trisomy21_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$trisomy21_list(appointments)
}
.public(dMeasure, "trisomy21_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # returns vector of InternalID of patients who
  # have trisomy 21

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  # Best Practice codes for trisomy 21
  trisomy21_codes <- c(836)

  private$db$history %>>%
    dplyr::filter(ConditionID %in% trisomy21_codes) %>>%
    dplyr::filter(InternalID %in% intID) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### bmi30 sub-code
#' list of patients with BMI>=30 (obesity)
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments list of appointments. default is $appointments_filtered
#'  needs appointments, as looks for recording prior to the appointment time
#'
#' @return a list of numbers, which are the InternalIDs
bmi30_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$cardiacdisease_list(appointments)
}
.public(dMeasure, "bmi30_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, then
  #  derives from $appointments_filtered
  #
  # returns vector of InternalID of patients who
  # have bmi 30 or more (obesity)

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  appointments %>>% dplyr::collect() %>>%
    dplyr::inner_join(private$db$observations %>>%
                        dplyr::filter(DATACODE == 9),
                      # this is BMI. also in DATANAME, but different spellings/cases
                      by = "InternalID", copy = TRUE) %>>%
    dplyr::filter(as.Date(OBSDATE) <= as.Date(Date)) %>>%
    # observation done before the appointment date
    dplyr::group_by(InternalID, Date) %>>%
    dplyr::slice(which.max(OBSDATE)) %>>% # choose the observation with the most recent observation date
    # unfortunately, as the code stands, this generates a vector which is not appointment date specific
    # if a range of appointment dates has been chosen
    dplyr::ungroup() %>>%
    dplyr::filter(DATAVALUE >= 30) %>>% # those with BMI >= 30
    dplyr::pull(InternalID) %>>%
    unique()
})

### chronic lung diseaes sub-code
#' list of patients with chronic lung disease
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
chroniclungdisease_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$chroniclungdisease_list(appointments)
}
.public(dMeasure, "chroniclungdisease_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # returns vector of InternalID of patients who
  # have lung disease such as bronchiectasis, cystic fibrosis, COPD/COAD
  # asthma is in a separate list

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  # Best Practice codes for lung disease
  cld_codes <- c(598, 4740, 414, 702)

  private$db$history %>>%
    dplyr::filter(ConditionID %in% cld_codes) %>>%
    dplyr::filter(InternalID %in% intID) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### neurologic diseaes sub-code
#' list of patients with neurologic disease
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
neurologic_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$neurologic_list(appointments)
}
.public(dMeasure, "neurologic_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # returns vector of InternalID of patients who
  # have chronic liver disease

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  # Best Practice codes for neurology
  neuro_codes <- c(2351, 963, 965, 966, 968, 969, 971, 6604,
                   2022, 2630, 3093)
  # multiple sclerosis, epilepsy, spinal cord injury, paraplegia, quadriplegia

  private$db$history %>>%
    dplyr::filter(ConditionID %in% neuro_codes) %>>%
    dplyr::filter(InternalID %in% intID) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### chronic liver disease sub-code
#' list of patients with chronic liver disease
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
chronicliverdisease_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$chronicliverdisease_list(appointments)
}
.public(dMeasure, "chronicliverdisease_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # returns vector of InternalID of patients who
  # have chronic liver disease

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  # Best Practice codes for chronic liver disease
  cld_codes <- c(11763, 584, 81)
  # liver disease (BP doesn't have 'chronic liver disease'!), cirrhosis, alcoholism

  private$db$history %>>%
    dplyr::filter(ConditionID %in% cld_codes) %>>%
    dplyr::filter(InternalID %in% intID) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### chronic renal diseaes sub-code
#' list of patients with chronic lung disease
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
chronicrenaldisease_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$chronicrenaldisease_list(appointments)
}
.public(dMeasure, "chronicrenaldisease_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # returns vector of InternalID of patients who
  # have chronic renal disease

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  # Best Practice codes for chronic liver disease
  crf_codes <- c(662, 258, 3132, 662, 2486, 2487, 6379, 2489, 7469, 1274,
                 7502, 7503, 7504, 7505, 7506, 2882)
  # chronic renal failure, renal impairment, dialysis

  private$db$history %>>%
    dplyr::filter(ConditionID %in% crf_codes) %>>%
    dplyr::filter(InternalID %in% intID) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})


### pregnancy sub-code
#' list of patients who are pregnant at the 'appointment' date
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
pregnant_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$pregnant_list(appointments)
}
.public(dMeasure, "pregnant_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # returns vector of $InternalID of patients who
  # are pregnant at time $Date

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  appointments %>>% dplyr::collect() %>>%
    dplyr::inner_join(private$db$pregnancies %>>%
                        dplyr::filter(InternalID %in% intID),
                      by = "InternalID", copy = TRUE) %>>%
    dplyr::filter(is.null(ENDDATE) | as.Date(ENDDATE) > as.Date(Date)) %>>%
    dplyr::filter((as.Date(EDCBYDATE) > as.Date(Date)) &
                    (as.Date(EDCBYDATE) < as.Date(Date+280))) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### fifteen plus age sub-code
#' list of patients who are fifteen years or more in age at time of $Date
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
fifteenplus_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$fifteenplus_list(appointments)
}
.public(dMeasure, "fifteenplus_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # returns vector of InternalID of patients who
  # are fifteen or more years of age

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  private$db$patients %>>%
    dplyr::filter(InternalID %in% intID) %>>%
    dplyr::select(InternalID, DOB) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(DOB = as.Date(DOB)) %>>%
    dplyr::left_join(appointments, by = "InternalID") %>>%
    dplyr::filter(dMeasure::calc_age(DOB, Date) >= 15) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})


### sixty-five plus age sub-code
#' list of patients who are sixty-five years or more in age at time of $Date
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
sixtyfiveplus_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$sixtyfiveplus_list(appointments)
}
.public(dMeasure, "sixtyfiveplus_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # returns vector of InternalID of patients who
  # are sixty-five (65) or more years of age

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  private$db$patients %>>%
    dplyr::filter(InternalID %in% intID) %>>%
    dplyr::select(InternalID, DOB) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(DOB = as.Date(DOB)) %>>%
    dplyr::left_join(appointments, by = "InternalID") %>>%
    dplyr::filter(dMeasure::calc_age(DOB, Date) >= 65) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### forty-five to seventy-four plus age sub-code
#' list of patients who are 45 to 74 years age at time of $Date
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
fortyfiveseventyfour_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$fortyfiveseventyfour_list(appointments)
}
.public(dMeasure, "fortyfiveseventyfour_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # returns vector of InternalID of patients who
  # are 45 to 74 years of age

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  private$db$patients %>>%
    dplyr::filter(InternalID %in% intID) %>>%
    dplyr::select(InternalID, DOB) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(DOB = as.Date(DOB)) %>>%
    dplyr::left_join(appointments, by = "InternalID") %>>%
    dplyr::filter(dplyr::between(dMeasure::calc_age(DOB, Date), 45, 74)) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### cervical screen (cst) eligible sub-code
#' list of patients who are cervical screneing eligible at time of $Date
#' age twenty-five to seventy-four years inclusive
#' female
#' no history of hysterectomy
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a list of numbers, which are the InternalIDs
cst_eligible_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$cst_eligible_list(appointments)
}
.public(dMeasure, "cst_eligible_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # returns vector of InternalID of patients who
  # are eligible for cervical screening

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  hysterectomy_codes <- c(4053, 4046, 4054, 8214, 4055)
  # hysterectomy, hysterectomy & BSO, "hysterectomy, abdominal",
  # "hysterectomy, laparoscopic", "hysterectomy, vaginal"
  # does NOT include "hysterectomy, subtotal" = 7521

  private$db$patients %>>%
    dplyr::filter(InternalID %in% intID && Sex == "Female") %>>%
    dplyr::select(InternalID, DOB) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(DOB = as.Date(DOB)) %>>%
    dplyr::left_join(appointments, by = "InternalID") %>>%
    dplyr::filter(dplyr::between(dMeasure::calc_age(DOB, Date), 25, 74)) %>>%
    dplyr::select(InternalID, Date) %>>%
    dplyr::left_join(private$db$history %>>%
                       dplyr::filter(InternalID %in% intID &&
                                ConditionID %in% hysterectomy_codes),
                     by = "InternalID", copy = TRUE) %>>%
    dplyr::filter(is.na(ConditionID)) %>>%
    # remove all who have a hysterectomy code
    # currently, does not remove according to DATE of hysterectomy
    dplyr::pull(InternalID) %>>%
    unique()
})
