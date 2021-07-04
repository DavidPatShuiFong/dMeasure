# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
#'
#'  If no dataframe provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
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

  self$db$history %>>%
    dplyr::filter(
      ConditionID %in% diabetes_codes,
      InternalID %in% intID
    ) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

#' list of patients with diabetes type 1
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  If no dataframe provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
diabetes_type1_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$diabetes_type1_list(appointments)
}
.public(dMeasure, "diabetes_type1_list", function(appointments = NULL) {
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
  diabetes_codes <- c(776)

  self$db$history %>>%
    dplyr::filter(
      ConditionID %in% diabetes_codes,
      InternalID %in% intID
    ) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

#' list of patients with diabetes type 2
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  If no dataframe provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
diabetes_type2_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$diabetes_type2_list(appointments)
}
.public(dMeasure, "diabetes_type2_list", function(appointments = NULL) {
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
  diabetes_codes <- c(778)

  self$db$history %>>%
    dplyr::filter(
      ConditionID %in% diabetes_codes,
      InternalID %in% intID
    ) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### Asthma sub-code
#' list of patients with asthma
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
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

  self$db$history %>>%
    dplyr::filter(
      ConditionID %in% asthma_codes,
      InternalID %in% intID
    ) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

#' list_asthma
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param contact contact system (if TRUE) or appointment system (if FALSE)
#' @param date_from by default, dM$date_a
#' @param date_to by default, dM$date_b
#' @param clinicians by default, is dM$clinicians
#' @param min_contact used for the 'contact' system. minimum number of contacts in time period
#' @param min_date used for the 'contact' system. must have been seen since min_date
#' @param max_date used for the 'contact' system. must have been before the max_date
#' @param contact_type used for the 'contact' system
#' @param ignoreOld ignore old administrations (>15 months from date_to).
#'  by default, FALSE
#' @param include_uptodate remove uptodate vaccinations?
#' @param lazy lazy evaluation?
#'
#' @return a dataframe
#'  structure of dataframe depends on whether 'contact' or 'appointment' system
#'  in both cases, the columns includes :
#'    Patient, InternalID, DOB, RecordNo, FluvaxName, FluvaxDate
#'  in the case of 'appointment' system, also includes appointment details
#' @export
list_asthma_details <- function(dMeasure_obj,
                                contact = FALSE,
                                date_from = NA, date_to = NA,
                                clinicians = NA,
                                min_contact = NA, min_date = NA, max_date = NA,
                                contact_type = NA,
                                ignoreOld = FALSE,
                                include_uptodate = TRUE,
                                lazy = FALSE) {
  dMeasure_obj$list_asthma_details(
    contact,
    date_from, date_to,
    clinicians,
    min_contact, min_date, max_date,
    contact_type,
    ignoreOld,
    include_uptodate,
    lazy
  )
}

.public(dMeasure, "list_asthma_details", function(contact = FALSE,
                                                  date_from = NA,
                                                  date_to = NA,
                                                  clinicians = NA,
                                                  min_contact = NA,
                                                  min_date = NA,
                                                  max_date = NA,
                                                  contact_type = NA,
                                                  ignoreOld = FALSE,
                                                  include_uptodate = TRUE,
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

  detailed_asthma_list <- data.frame(
    Patient = character(),
    InternalID = character(),
    RecordNo = character(),
    FluvaxDate = as.Date(numeric(), origin = "1970-01-01"),
    FluvaxName = character(),
    PlanDate = as.Date(numeric(), origin = "1970-01-01")
  ) # empty data.frame

  if (self$emr_db$is_open()) {
    # only if EMR database is open
    if (self$Log) {
      log_id <- self$config_db$write_log_db(
        query = "asthma_condition",
        data = list(date_from, date_to, clinicians)
      )
    }

    if (contact) {
      # contact method
      if (!lazy) {
        self$list_contact_asthma(
          date_from, date_to, clinicians,
          min_contact, min_date, max_date,
          contact_type,
          lazy
        )
      }
      asthma_list <- self$contact_asthma_list %>>%
        dplyr::select(Patient, InternalID) # don't need these fields
      asthmaID <- asthma_list %>>%
        dplyr::pull(InternalID) %>>%
        c(-1) # make sure not empty vector, which is bad for SQL filter
    } else {
      # appointment method
      if (!lazy) {
        self$list_appointments(lazy = FALSE)
      }
      asthma_appt <- self$appointments_list %>>%
        dplyr::select(Patient, InternalID, AppointmentDate, AppointmentTime, Provider)
      asthmaID <- self$asthma_list(asthma_appt) %>>%
        c(-1)
      asthma_list <- asthma_appt %>>%
        dplyr::filter(InternalID %in% asthmaID)
    }

    # add DOB, RecordNo to asthma list
    asthma_list <- asthma_list %>>%
      dplyr::left_join(self$db$patients %>>%
        dplyr::filter(InternalID %in% asthmaID) %>>%
        dplyr::select(InternalID, DOB, RecordNo) %>>%
        dplyr::mutate(DOB = as.Date(DOB)),
      by = "InternalID", copy = TRUE
      )

    fluvaxList <- self$influenzaVax_obs(
      asthmaID,
      date_from = ifelse(ignoreOld,
        as.Date(NA, origin = "1970-01-01"),
        as.Date(-Inf, origin = "1970-01-01")
      ),
      # if ignoreOld, then influenza_vax will (given NA)
      # calculate date_from as fifteen months before date_to
      date_to = date_to
    )
    # returns InternalID, FluvaxName, FluvaxDate
    # add the flu vax list to the asthma list
    detailed_asthma_list <- asthma_list %>>%
      dplyr::left_join(
        fluvaxList,
        by = "InternalID",
        copy = TRUE
      )

    asthmaPlanList <- self$asthmaplan_obs(asthmaID,
      date_from = ifelse(
        ignoreOld,
        as.Date(NA, origin = "1970-01-01"),
        as.Date(-Inf, origin = "1970-01-01")
      ),
      # if ignoreOld, then influenza_vax will (given NA)
      # calculate date_from as fifteen months before date_to
      date_to = date_to
    )
    # returns InternalID, FluvaxName, FluvaxDate
    # add the flu vax list to the asthma list
    detailed_asthma_list <- detailed_asthma_list %>>%
      dplyr::left_join(
        asthmaPlanList,
        by = "InternalID",
        copy = TRUE
      )

    # then remove up-to-date items if required
    # only remove if BOTH/ALL of vax and asthmaplan are up-to-date
    if (!include_uptodate) {
      if (contact) {
        detailed_asthma_list <- detailed_asthma_list %>>%
          dplyr::filter(is.na(FluvaxDate) |
            FluvaxDate == as.Date(-Inf, origin = "1970-01-01") |
            format(FluvaxDate, "%Y") != format(date_to, "%Y") |
            is.na(PlanDate) |
            dMeasure::interval(PlanDate, self$date_b)$year > 0)
        # remove entries which are 'up-to-date'!
        # anyone who has had a flu vax  in the same year as end of contact period
        # (and so has a valid 'GivenDate') is 'up-to-date'!
        # or plan date less than one year old
      } else {
        # appointment method
        detailed_asthma_list <- detailed_asthma_list %>>%
          dplyr::filter(
            is.na(FluvaxDate) |
              FluvaxDate == as.Date(-Inf, origin = "1970-01-01") |
              format(FluvaxDate, "%Y") != format(AppointmentDate, "%Y") |
              is.na(PlanDate) |
              dMeasure::interval(PlanDate, AppointmentDate)$year > 0
          )
        # remove entries which are 'up-to-date'!
        # anyone who has had a flu vax  in the same year as appointment date
        # (and so has a valid 'GivenDate') is 'up-to-date'!
      }
    }

    if (self$Log) {
      self$config_db$duration_log_db(log_id)
    }
  }
  return(detailed_asthma_list)
})


### Aboriginal and Torres Strait Islander sub-code
#' list of patients recorded ATSI ethnicity
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'   if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
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
  atsi_codes <- c(
    "Aboriginal", "Torres Strait Islander",
    "Aboriginal/Torres Strait Islander"
  )


  self$db$patients %>>%
    dplyr::filter(
      Ethnicity %in% atsi_codes,
      InternalID %in% intID
    ) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### Maligancy sub-code
#' list of patients recorded malignancy
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
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
  malignancy_codes <- c(
    463, 478, 485, 7845, 449, 6075, 453, 456, 473, 490, 11927,
    445, 444, 446, 447, 448, 451, 457, 458, 459, 460, 462, 469, 454,
    472, 474, 477, 480, 481, 482, 486, 487, 488, 489, 11911, 491,
    492, 9391, 7751, 483, 8027, 470, 471, 476, 8261, 2475, 6835,
    6827, 6817, 6818, 6813, 6824, 6830, 6820, 6822, 6819, 6815, 6828,
    6826, 6821, 6833, 6831, 6823, 6834, 6825, 6832, 6829, 6814, 3221,
    4975, 2273, 2287, 4976, 5604, 5599, 5602, 5600, 5609, 5601, 5603,
    5608, 5607, 329, 2350, 2222, 5054, 2223, 6541, 2224, 2225, 2226,
    6003, 5480, 2230, 452, 3215, 7005, 2173, 2174, 2175, 2176, 2177,
    2178, 2179, 1440
  )


  self$db$history %>>%
    dplyr::filter(
      ConditionID %in% malignancy_codes,
      InternalID %in% intID
    ) %>>%
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
#' @return a vector of numbers, which are the InternalIDs
#' @export
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

  self$db$history %>>%
    dplyr::filter(
      ConditionID %in% hiv_codes,
      InternalID %in% intID
    ) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### Haemoglobinopathy sub-code
#' list of patients Haemoglobinopathy
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
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

  self$db$history %>>%
    dplyr::filter(
      ConditionID %in% haemoglobinopathy_codes,
      InternalID %in% intID
    ) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### Asplenia sub-code
#' list of patients Asplenia
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
asplenic_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$asplenic_list(appointments)
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

  self$db$history %>>%
    dplyr::filter(
      ConditionID %in% asplenic_codes,
      InternalID %in% intID
    ) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### Transplant sub-code
#' list of patients with transplant
#'
#' bone marrow, heart, liver, lung, pancreas, renal, thymus
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
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

  self$db$history %>>%
    dplyr::filter(
      ConditionID %in% transplant_codes,
      InternalID %in% intID
    ) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### Cardiovascular disease sub-code
#' list of patients with cardiovascular disease
#'
#' ischaemic heart disease
#'
#' renovascular hypertension, peripheral arterial disease, peripheral arterial disease - diabetic
#'
#' cerebrovascular disease
#'
#' for CVD risk assessment purposes
#'  these patients are already at high risk and
#'  so excluded from CVD risk assessment
#'
#' https://www.cvdcheck.org.au/australian-absolute-cardiovascular-disease-risk-calculator
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
cvd_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$cvd_list(appointments)
}
.public(dMeasure, "cvd_list", function(appointments = NULL) {
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

  # Best Practice codes for cardio-vascular disease
  cvd_codes <- c(226, 227, 228, 2376, 2377, 2378, 2379, 2380, 2381, 2382, 3576, 3577, 3578, 3579, 1534, 2556, 6847, 7847)
  # ischaemic heart disease
  cvd_codes <- c(cvd_codes, 1480, 3083, 777)
  # renovascular hypertension, peripheral arterial disease, peripheral arterial disease - diabetic
  cvd_codes <- c(cvd_codes, 1522, 677, 678, 679, 680, 681, 1522)
  # cerebrovascular disease

  self$db$history %>>%
    dplyr::filter(
      ConditionID %in% cvd_codes,
      InternalID %in% intID
    ) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### Cardiac sub-code
#' list of patients with cardiac conditions
#'
#' cyanotic congenital heart disease, ischaemic heart disease,
#' acute myocardial infarct (AMI) and congestive failure
#'
#' for influenza immunization purposes
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
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
  cardiac_codes <- c(
    7810, 226, 227, 228, 2376, 2377, 2378, 2379, 2380, 2381,
    2382, 3576, 3577, 3578, 3579, 1534, 2556, 6847, 7847,
    1347, 2376, 2377, 2378, 2379, 2380, 2381, 2382, 7847, 6847, 2556
  )
  # cyanotic congenital heart disease, ischaemic heart disease, AMI and congestive failure

  self$db$history %>>%
    dplyr::filter(
      ConditionID %in% cardiac_codes,
      InternalID %in% intID
    ) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### Trisomy 21 sub-code
#' list of patients with trisomy21
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
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

  self$db$history %>>%
    dplyr::filter(
      ConditionID %in% trisomy21_codes,
      InternalID %in% intID
    ) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### bmi30 sub-code
#' list of patients with BMI>=30 (obesity)
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments list of appointments. default is $appointments_filtered
#'
#'  needs appointments, as looks for recording prior to the appointment time
#'
#' @return a vector of numbers, which are the InternalIDs of patients who have
#'  BMI 30 or more (obesity)
#' @export
bmi30_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$bmi30_list(appointments)
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

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  appointments %>>% dplyr::collect() %>>%
    dplyr::inner_join(self$db$observations %>>%
      dplyr::filter(
        ObservationCode == 9,
        InternalID %in% intID
      ),
    # this is BMI. also in DATANAME, but different spellings/cases
    by = "InternalID", copy = TRUE
    ) %>>%
    dplyr::mutate(ObservationDate = as.Date(ObservationDate)) %>>%
    # although defined 'as.Date' in self$db$observations,
    # it needs to be explicitly converted to as.Date again
    # because the 'in SQL table' as.Date did not actually convert to Date
    # for the purposes of R
    dplyr::filter(ObservationDate <= as.Date(Date)) %>>%
    # observation done before the appointment date
    dplyr::group_by(InternalID, Date) %>>%
    dplyr::arrange(dplyr::desc(ObservationDate), .by_group = TRUE) %>>%
    dplyr::filter(dplyr::row_number() == 1) %>>% # the 'maximum' ObservationDate, breaking 'ties'
    # choose the observation with the most recent observation date
    # unfortunately, as the code stands, this generates a vector which
    # is not appointment date specific
    # if a range of appointment dates has been chosen
    dplyr::ungroup() %>>%
    dplyr::filter(as.numeric(ObservationValue) >= 30) %>>% # those with BMI >= 30
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
#' @return a vector of numbers, which are the InternalIDs
#' @export
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

  self$db$history %>>%
    dplyr::filter(
      ConditionID %in% cld_codes,
      InternalID %in% intID
    ) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### neurologic diseaes sub-code
#' list of patients with neurologic disease
#'
#' multiple sclerosis, epilepsy, spinal cord injury, paraplegia, quadriplegia
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
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
  neuro_codes <- c(
    2351, 963, 965, 966, 968, 969, 971, 6604,
    2022, 2630, 3093
  )
  # multiple sclerosis, epilepsy, spinal cord injury, paraplegia, quadriplegia

  self$db$history %>>%
    dplyr::filter(
      ConditionID %in% neuro_codes,
      InternalID %in% intID
    ) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### chronic liver disease sub-code
#' list of patients with chronic liver disease
#'
#' liver disease (BP doesn't have 'chronic liver disease'!), cirrhosis, alcoholism
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
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

  self$db$history %>>%
    dplyr::filter(
      ConditionID %in% cld_codes,
      InternalID %in% intID
    ) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### chronic renal diseaes sub-code
#' list of patients with chronic lung disease
#'
#' chronic renal failure, renal impairment, dialysis
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
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
  crf_codes <- c(
    662, 258, 3132, 662, 2486, 2487, 6379, 2489, 7469, 1274,
    7502, 7503, 7504, 7505, 7506, 2882
  )
  # chronic renal failure, renal impairment, dialysis

  self$db$history %>>%
    dplyr::filter(
      ConditionID %in% crf_codes,
      InternalID %in% intID
    ) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})


### pregnancy sub-code
#' list of patients who are pregnant at the 'appointment' date
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
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

  appointments %>>%
    dplyr::inner_join(self$db$pregnancies %>>%
      dplyr::filter(InternalID %in% intID),
    by = "InternalID", copy = TRUE
    ) %>>%
    dplyr::filter(is.na(EndDate) | is.null(EndDate) |
      EndDate > Date) %>>%
    dplyr::filter(UseScan == 1 | # 'pass-through' is using scan date
      # (using LNMP date)
      # using SQL function 'DATEDIFF'
      # assume not 'pregnant' is more than 30 days post-dates
      (!is.na(EDCbyDate) &
        (Date - as.Date(EDCbyDate)) < 30)) %>>%
    dplyr::filter(UseScan == 0 | # 'pass-through' if using LNMP date
      # (using LNMP date)
      (!is.na(EDCbyScan) &
        (Date - as.Date(EDCbyScan)) < 30)) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### post-natal sub-code
#' list of patients who are pregnant at the 'appointment' date
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#' @param include_edc include post-natal by EDC (TRUE or FALSE)
#' @param days_min minimum number of days post-natal
#' @param days_max maximum number of days post-natal
#' @param outcome allowable outcomes (vector)
#'   possible outcomes 0 = none recorded, 1 = "Live birth",
#'   2 = Miscarriage, 3 = Termination, 4 = Ectopic,
#'   5 = IUFD (intra-uterine fetal death), 6 = stillbirth
#'   7 = hydatiform mole
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a dataframe : InternalID, EDCbyDate, EDCbyScan, EndDate, OutcomeCode
#' @export
postnatal_list <- function(dMeasure_obj, appointments = NULL,
                           include_edc = FALSE,
                           days_min = 0, days_max = 180,
                           outcome = c(0, 1, 2, 3, 4, 5, 6, 7)) {
  dMeasure_obj$postnatal_list(appointments)
}
.public(dMeasure, "postnatal_list", function(appointments = NULL,
                                             include_edc = FALSE,
                                             days_min = 0, days_max = 180,
                                             outcome = c(0:7)) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # returns vector of $InternalID of patients who
  # are postnatal at time $Date

  include_edc <- as.numeric(include_edc)
  # can't compare TRUE/FALSE inside a filter with SQL
  # but we can compare numerics (TRUE = 1, FALSE = 0)

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  if (nrow(appointments) == 0) {
    # no apppointments to choose from!
    # database might not even be open
    # return an (empty) dataframe in the same shape
    # as the expected return dataframe
    return(appointments %>>%
      dplyr::select(-c(Date)) %>>%
      dplyr::mutate(
        EDCbyDate = as.Date(NA, origin = "1970-01-01"),
        EDCbyScan = as.Date(NA, origin = "1970-01-01"),
        EndDate = as.Date(NA, origin = "1970-01-01"),
        OutcomeCode = as.numeric(NA)
      ))
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  appointments %>>%
    dplyr::inner_join(self$db$pregnancies %>>%
      dplyr::filter(
        InternalID %in% intID,
        OutcomeCode %in% outcome
      ),
    by = "InternalID", copy = TRUE
    ) %>>%
    dplyr::filter(is.na(EndDate) | # if no recorded end date, then 'pass-through'
      # if there is a recorded end-date, then use SQL function DATEDIFF
      dplyr::between(Date - as.Date(EndDate), days_min, days_max)) %>>%
    dplyr::filter(!is.na(EndDate) | # if recorded end date, then 'pass-through'
      ((include_edc == 1) & UseScan == 0 & !is.na(EDCbyDate) & # use EDCbyDAte
        dplyr::between(Date - as.Date(EDCbyDate), days_min, days_max)) |
      ((include_edc == 1) & UseScan == 1 & !is.na(EDCbyScan) & # use EDCbyScan
        dplyr::between(Date - as.Date(EDCbyScan), days_min, days_max))) %>>%
    dplyr::select(InternalID, EDCbyDate, EDCbyScan, EndDate, OutcomeCode)
})

### fifteen plus age sub-code
#' list of patients who are fifteen years or more in age at time of $Date
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
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

  self$db$patients %>>%
    dplyr::filter(InternalID %in% intID) %>>%
    dplyr::select(InternalID, DOB) %>>%
    dplyr::left_join(appointments,
      by = "InternalID", copy = TRUE
    ) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(DOB = as.Date(DOB), Date = as.Date(Date)) %>>%
    # initially Date is a dttm (POSIXt) object,
    # which makes the subsequent calc_age very slow,
    # and throws up warnings
    dplyr::filter(dMeasure::calc_age(DOB, Date) >= 15) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})


### sixty-five plus age sub-code
#' list of patients who are sixty-five years or more in age at time of $Date
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
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

  self$db$patients %>>%
    dplyr::filter(InternalID %in% intID) %>>%
    dplyr::select(InternalID, DOB) %>>%
    dplyr::left_join(appointments,
      by = "InternalID", copy = TRUE
    ) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(
      DOB = as.Date(DOB),
      Date = as.Date(Date)
    ) %>>%
    dplyr::filter(dMeasure::calc_age(DOB, Date) >= 65) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### seventy-five plus age sub-code
#' list of patients who are seventy-five years or more in age at time of $Date
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
seventyfiveplus_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$seventyfiveplus_list(appointments)
}
.public(dMeasure, "seventyfiveplus_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # returns vector of InternalID of patients who
  # are seventy-five (65) or more years of age

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  self$db$patients %>>%
    dplyr::filter(InternalID %in% intID) %>>%
    dplyr::select(InternalID, DOB) %>>%
    dplyr::left_join(appointments,
      by = "InternalID", copy = TRUE
    ) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(DOB = as.Date(DOB), Date = as.Date(Date)) %>>%
    dplyr::filter(dMeasure::calc_age(DOB, Date) >= 75) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### thirty-five to forty-four years ATSI age sub-code
#' list of patients who are 35 to 44 years, and ATSI, at time of $Date
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
ATSI_35_44_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$ATSI_35_44_list(appointments)
}
.public(dMeasure, "ATSI_35_44_list", function(appointments = NULL) {
  # @param Appointments dataframe of $InternalID and $Date
  #  if no parameter provided, derives from $appointments_filtered
  #
  # returns vector of InternalID of patients who
  # are 35 or more years of age, and ATSI

  if (is.null(appointments)) {
    appointments <- self$appointments_filtered %>>%
      dplyr::select(InternalID, AppointmentDate) %>>%
      dplyr::rename(Date = AppointmentDate)
    # just needs $InternalID and $Date
  }

  intID <- c(dplyr::pull(appointments, InternalID), -1)
  # internalID in appointments. add a -1 in case this is an empty list

  # Best Practice Aboriginal or Torres Strait Islander codes
  atsi_codes <- c(
    "Aboriginal", "Torres Strait Islander",
    "Aboriginal/Torres Strait Islander"
  )

  self$db$patients %>>%
    dplyr::filter(
      InternalID %in% intID,
      Ethnicity %in% atsi_codes
    ) %>>%
    dplyr::select(InternalID, DOB) %>>%
    dplyr::left_join(appointments,
      by = "InternalID", copy = TRUE
    ) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(DOB = as.Date(DOB), Date = as.Date(Date)) %>>%
    dplyr::filter(dplyr::between(dMeasure::calc_age(DOB, Date), 35, 44)) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### forty-five to seventy-four plus age sub-code
#' list of patients who are 45 to 74 years age at time of $Date
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
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

  self$db$patients %>>%
    dplyr::filter(InternalID %in% intID) %>>%
    dplyr::select(InternalID, DOB) %>>%
    dplyr::left_join(appointments,
      by = "InternalID", copy = TRUE
    ) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(DOB = as.Date(DOB), Date = as.Date(Date)) %>>%
    dplyr::filter(dplyr::between(dMeasure::calc_age(DOB, Date), 45, 74)) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

### cervical screen (cst) eligible sub-code
#' list of patients who are cervical screening eligible at time of $Date
#'
#' \itemize{
#'  \item age twenty-five to seventy-four years inclusive
#'  \item female
#'  \item no history of hysterectomy
#' }
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments `$InternalID` and `$Date`
#'
#'  if no parameter provided, derives from `$appointments_filtered`
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
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

  self$db$patients %>>%
    dplyr::filter(
      InternalID %in% intID,
      Sex == "Female"
    ) %>>%
    dplyr::select(InternalID, DOB) %>>%
    dplyr::left_join(appointments,
      by = "InternalID", copy = TRUE
    ) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(
      DOB = as.Date(DOB),
      Date = as.Date(Date)
    ) %>>%
    dplyr::filter(dplyr::between(dMeasure::calc_age(DOB, Date), 25, 74)) %>>%
    dplyr::select(InternalID, Date) %>>%
    dplyr::left_join(self$db$history %>>%
      dplyr::filter(
        InternalID %in% intID,
        ConditionID %in% hysterectomy_codes
      ),
    by = "InternalID", copy = TRUE
    ) %>>%
    dplyr::filter(is.na(ConditionID)) %>>%
    # remove all who have a hysterectomy code
    # currently, does not remove according to DATE of hysterectomy
    dplyr::pull(InternalID) %>>%
    unique()
})

### breast cancer screen (mammogram) eligible sub-code
#' list of patients who are breast cancer screening eligible at time of $Date
#'
#' \itemize{
#'  \item age fifty to seventy-four years inclusive
#'  \item female
#' }
#'
#' https://www.cancer.org.au/about-cancer/early-detection/screening-programs/breast-cancer-screening.html
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
mammogram_eligible_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$mammogram_eligible_list(appointments)
}
.public(dMeasure, "mammogram_eligible_list", function(appointments = NULL) {
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

  self$db$patients %>>%
    dplyr::filter(
      InternalID %in% intID,
      Sex == "Female"
    ) %>>%
    dplyr::select(InternalID, DOB) %>>%
    dplyr::left_join(appointments,
      by = "InternalID", copy = TRUE
    ) %>>%
    dplyr::collect() %>>%
    dplyr::mutate(DOB = as.Date(DOB), Date = as.Date(Date)) %>>%
    dplyr::filter(dplyr::between(dMeasure::calc_age(DOB, Date), 50, 74)) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

#' list of patients with familial hypercholesterolaemia
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
familialHypercholesterolaemia_list <- function(dMeasure_obj,
                                               appointments = NULL) {
  dMeasure_obj$familialHypercholesterolaemia_list(appointments)
}
.public(
  dMeasure, "familialHypercholesterolaemia_list",
  function(appointments = NULL) {
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

    self$db$history %>>%
      dplyr::filter(
        ConditionID == 1446,
        InternalID %in% intID
      ) %>>%
      dplyr::pull(InternalID) %>>%
      unique()
  }
)

#' list of patients with left ventricular hypertrophy
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
LVH_list <- function(dMeasure_obj, appointments = NULL) {
  dMeasure_obj$LVH_list(appointments)
}
.public(dMeasure, "LVH_list", function(appointments = NULL) {
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

  self$db$history %>>%
    dplyr::filter(
      ConditionID == 2214,
      InternalID %in% intID
    ) %>>%
    dplyr::pull(InternalID) %>>%
    unique()
})

#' list of patients who are listed as the head of family
#' with a child between defined ages
#'
#' This is found through the use of 'HeadOfFamilyID' in
#' db$patientsRaw
#'
#' Note that this does *not* include postnatal_list, and
#' so a separate check is required if requiring postnatal
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param appointments dataframe of appointments $InternalID and $Date
#' @param months_min minimum age in months (inclusive, 'from')
#' @param months_max maximum age in months (exclusive, 'up to')
#'
#'  if no parameter provided, derives from $appointments_filtered
#'
#' @return a vector of numbers, which are the InternalIDs
#' @export
parent_list <- function(dMeasure_obj, appointments = NULL,
  months_min = 0, months_max = 12) {
  dMeasure_obj$parent_list(appointments)
}
.public(dMeasure, "parent_list", function(appointments = NULL,
  months_min = 0,
  months_max = 12) {
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

  intID <- self$db$patientsRaw %>>%
    dplyr::filter(
      HeadOfFamilyID %in% intID # exclude possible parents!
    ) %>>%
    dplyr::select(InternalID, HeadOfFamilyID, DOB) %>>%
    dplyr::collect() %>>%
    dplyr::left_join(
      appointments,
      by = c("HeadOfFamilyID" = "InternalID")) %>>%
    dplyr::filter(InternalID != HeadOfFamilyID) %>>% # can't be parent of yourself
    dplyr::mutate(AgeInMonths = dMeasure::calc_age_months(as.Date(DOB), as.Date(Date))) %>>%
    dplyr::filter(AgeInMonths >= months_min & AgeInMonths < months_max) %>>%
    dplyr::pull(HeadOfFamilyID) %>>%
    unique()

  return(intID)
})
