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

  # Returns vector of InternalID of patients who have diabetes
  appointments %>%
    dplyr::inner_join(self$db$history %>%
                        dplyr::filter(ConditionID %in% diabetes_codes),
                      by = c('InternalID')) %>%
    dplyr::pull(InternalID) %>%
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

  # Returns vector of InternalID of patients who have diabetes
  appointments %>%
    dplyr::inner_join(self$db$history %>%
                        dplyr::filter(ConditionID %in% asthma_codes),
                      by = c('InternalID')) %>%
    dplyr::pull(InternalID) %>%
    unique()
})
