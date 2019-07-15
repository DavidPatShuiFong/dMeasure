##### Chronic Disease Management item numbers ###########################################
#' Chronic Disease Management (CDM) item numbers
#'
#' @name cdm
#' @include dMeasure.R
#' @include appointments.R
#' needs access to dMeasure and appointments functions and variables
#'
#' @include conditions.R
#'
#' @include calculation_definitions.R
#' need function 'interval'
#'
#' @include fomantic_definitions_tags.R
#' 'tags only' fomantic/semantic definitions
NULL

##### constants #########################################################################
# MBS (medicare benefits schedule) item numbers for CDM
cdm_item <- data.frame(
  code = c(721, 723, 732, 703, 705, 707, 2517, 2521, 2525, 2546, 2552, 2558, 2700, 2701, 2715, 2717),
  name = c('GPMP', 'TCA', 'GPMP R/V', 'HA', 'HA', 'HA', 'DiabetesSIP', 'DiabetesSIP', 'DiabetesSIP',
           'AsthmaSIP', 'AsthmaSIP', 'AsthmaSIP', 'MHCP', 'MHCP', 'MHCP', 'MHCP')
)

cdm_item_names <- as.character(unique(cdm_item$name)) # de-factored and unique cdm_item$name

##### fields ############################################################################

### AHA 75 (annual health assessment for those aged 75 years and above)
.active("aha75_list_cdm", function(value) {
  if (!missing(value)) {
    stop("Can't set `$aha75_list_cdm`", call. = FALSE)
  }

  self$appointments_list %>>%
    dplyr::filter(Age >= 75) %>>%
    dplyr::select(c('InternalID', 'AppointmentDate', 'AppointmentTime', 'Provider')) %>>%
    dplyr::mutate(MBSName = c('HA'), Description = c('Age 75 years or older'),
                  ServiceDate = as.Date(-Inf, origin = '1970-01-01'), MBSItem = NA) %>>%
    unique()
})

### Diabetes list for CDM
.active("diabetes_list_cdm", function(value) {
  if (!missing(value)) {
    stop("Can't set `$diabetes_list_cdm`", call. = FALSE)
  }

  a <- self$appointments_list %>>%
    dplyr::filter(InternalID %in% self$diabetes_list()) %>>%
    dplyr::select(c('InternalID', 'AppointmentDate', 'AppointmentTime', 'Provider')) %>>%
    dplyr::mutate(MBSName = c('DiabetesSIP'), Description = c('History : Diabetes'),
                  ServiceDate = as.Date(-Inf, origin = '1970-01-01'), MBSItem = NA) %>>%
    unique()
  # invalid date set as -Inf, which looks like NA, but is not (is equal to -Inf)
  # setting invalid date to NA is not good for later comparisons,
  # where max(... , na.rm=TRUE) needs to be used

  b <- a %>>% dplyr::mutate(MBSName = "GPMP")
  # people with diabetes also qualify for GPMP. duplicate list with 'GPMP' MBSName
  rbind(a, b)
})

### asthma list for CDM
.active("asthma_list_cdm", function(value) {
  if (!missing(value)) {
    stop("Can't set `$asthma_list_cdm`", call. = FALSE)
  }

  a <- self$appointments_list %>>%
    dplyr::filter(InternalID %in% self$asthma_list()) %>>%
    dplyr::select(c('InternalID', 'AppointmentDate', 'AppointmentTime', 'Provider')) %>>%
    dplyr::mutate(MBSName = c('AsthmaSIP'), Description = c('History : Asthma'),
                  ServiceDate = as.Date(-Inf, origin = '1970-01-01'), MBSItem = NA) %>>%
    unique()
  # invalid date set as -Inf, which looks like NA, but is not (is equal to -Inf)
  # setting invalid date to NA is not good for later comparisons,
  # where max(... , na.rm=TRUE) needs to be used

  b <- a %>>% dplyr::mutate(MBSName = c('GPMP'))
  # people with asthma also qualify for GPMP. duplicate list with 'GPMP' MBSName
  rbind(a, b)
})

##### methods ###########################################################################

#' CDM item status of patients in appointment view
#'
#' filter to CDM item billed prior to (or on) the day of displayed appointments
#' only show most recent billed item in each category
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from=dMeasure_obj$date_a start date
#' @param date_to=dMeasure_obj$date_b end date (inclusive)
#' @param clinicians=dMeasure_obj$clinicians list of clinicians to view
#' @param cdm_chosen=cdm_item_names item types to show, defaults to all available
#' @param lazy if TRUE, then do not recalculate appointment list. otherwise, re-calculate
#' @param screentag=FALSE optionally add a fomantic/semantic HTML description of 'action'
#' @param screentag_print=TRUE optionally add a 'printable' description of 'action'
#'
#' @return list of appointments (with patient details)
appointments_billings_cdm <- function(dMeasure_obj, date_from = NA, date_to = NA, clinicians = NA,
                                      cdm_chosen = cdm_item_names,
                                      lazy = FALSE,
                                      screentag = FALSE, screentag_print = TRUE) {
  dMeasure_obj$appointments_billings_cdm(date_from, date_to, clinicians,
                                         cdm_chosen, lazy, screentag, screentag_print)
}
.public("appointments_billings_cdm", function (date_from = NA, date_to = NA, clinicians = NA,
                                               cdm_chosen = cdm_item_names,
                                               lazy = FALSE,
                                               screentag = FALSE, screentag_print = TRUE) {

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

  if (!lazy) {
    self$billed_appointments(date_from, date_to, clinicians, lazy = FALSE)
    # if not 'lazy' evaluation, then re-calculate self$appointments_billings
    # (that is automatically done by calling the $billed_appointments method)
  }

  appointments <- self$appointments_billings %>>%
    dplyr::filter(MBSItem %in% cdm_item$code) %>>%
    # only chronic disease management items
    dplyr::filter(ServiceDate <= AppointmentDate) %>>%
    # only items billed before the appointment day
    dplyr::select(InternalID, AppointmentDate, AppointmentTime, Provider,
                  ServiceDate, MBSItem, Description) %>>%
    dplyr::mutate(MBSName = cdm_item$name[match(MBSItem, cdm_item$code)])

  if ("GPMP R/V" %in% cdm_chosen) {
    gpmprv <- appointments %>>%
      # GPMP R/V tags.
      # unlike other items, this is on a 3 month schedule, and can follow
      # an item 'other' than itself (e.g. it can follow a GPMP or TCA)
      #
      # only show if a GPMP R/V is due (greater than three months since gpmp or tca or gpmp r/v)
      # or if GPMP R/V is the most recent of gpmp/tca/gpmp r/v
      #
      # green if 'up-to-date' (GPMP R/V is the most recent, and less than 3/months)
      # yellow if 'done, but old' (GPMP R/V is the most recent, and more than 3/months)
      # red if 'not done' (GPMP/TCA most recent, and more than three)
      dplyr::filter(MBSName %in% c("GPMP", "TCA", "GPMP R/V")) %>>%
      # r/v only applies if gpmp/tca or r/v already claimed
      dplyr::group_by(InternalID, AppointmentDate, AppointmentTime, Provider) %>>%
      # group by appointment
      dplyr::slice(which.max(ServiceDate)) %>>%
      dplyr::ungroup() %>>%
      # (one) item with latest servicedate
      dplyr::filter((MBSName == "GPMP R/V") |
                      interval(ServiceDate, AppointmentDate, unit = "month")$month >= 3)
    # minimum 3-month gap since claiming previous GPMP/TCA,
    # or most recent claim is a GPMP R/V

    # add screentags as necessary
    if (screentag) {
      gpmprv <- gpmprv %>>%
        dplyr::mutate(mbstag =
                        semantic_tag("GPMP R/V", # semantic/fomantic buttons
                                     colour =
                                       dplyr::if_else(MBSName %in% c("GPMP", "TCA"),
                                                      'red',
                                                      # no GPMP R/V since the last GPMP/TCA
                                                      dplyr::if_else(interval(ServiceDate, AppointmentDate, unit = "month")$month >= 3,
                                                                     # GPMP R/V. Less than or more than 3 months?
                                                                     'green',
                                                                     'yellow')),
                                     popuphtml =
                                       paste0("<h4>Date : ", ServiceDate,
                                              "</h4><h6>Item : ", MBSItem,
                                              "</h6><p><font size=\'+0\'>", Description, "</p>")))

    }
    if (screentag_print) {
      gpmprv <- gpmprv %>>%
        dplyr::mutate(mbstag_print =
                        paste0("GPMP R/V", " ", # printable version of information
                               dplyr::if_else(MBSName %in% c("GPMP", "TCA"),
                                              paste0("(", MBSName, ": ", ServiceDate, ") Overdue"),
                                              dplyr::if_else(interval(ServiceDate, AppointmentDate, unit = "month")$month >= 3,
                                                             paste0("(", ServiceDate, ")"),
                                                             paste0("(", ServiceDate, ") Overdue")))))
    }

  } else {
    gpmprv <- NULL
  }

  appointments <- appointments %>>%
    dplyr::filter(!(MBSName == "GPMP R/V")) %>>% # GPMP R/V will be added back in as a 'tagged' version
    rbind(self$diabetes_list_cdm) %>>%
    rbind(self$asthma_list_cdm) %>>%
    rbind(self$aha75_list_cdm) %>>%
    dplyr::filter(MBSName %in% cdm_chosen) %>>%
    dplyr::group_by(InternalID, AppointmentDate, AppointmentTime, Provider, MBSName) %>>%
    # group by patient, apppointment and CDM type (name)
    dplyr::filter(ServiceDate == max(ServiceDate, na.rm = TRUE)) %>>%
    # only keep most recent service
    dplyr::ungroup()

  if (screentag) {
    appointments <- appointments %>>%
      dplyr::mutate(mbstag =
                      semantic_tag(MBSName, # semantic/fomantic buttons
                                   colour =
                                     dplyr::if_else(
                                       ServiceDate == -Inf,
                                       'red',
                                       # invalid date is '-Inf', means item not claimed yet
                                       dplyr::if_else(
                                         interval(ServiceDate, AppointmentDate)$year < 1,
                                         'green',
                                         'yellow')),
                                   popuphtml =
                                     paste0("<h4>Date : ", ServiceDate,
                                            "</h4><h6>Item : ", MBSItem,
                                            "</h6><p><font size=\'+0\'>", Description, "</p>")))
  }

  if (screentag_print) {
    appointments <- appointments %>>%
      dplyr::mutate(mbstag_print = paste0(MBSName, # printable version of information
                                   dplyr::if_else(
                                     ServiceDate == -Inf,
                                     '',
                                     paste0(" (", ServiceDate, ")",
                                            dplyr::if_else(
                                              interval(ServiceDate, AppointmentDate)$year < 1,
                                              "",
                                              " Overdue")))))
  }

  appointments <- appointments %>>%
    rbind(gpmprv) %>>% # add in GPMP reviews
    dplyr::group_by(InternalID, AppointmentDate, AppointmentTime, Provider) %>>%
    # gathers item numbers on the same day into a single row
    {if (screentag) {dplyr::summarise(., cdm = paste(mbstag, collapse = ""))}
      else {.}} %>>%
    {if (screentag_print) {dplyr::summarise(., cdm_print =
                                              paste(mbstag_print, collapse = ", "))}
      else {.}} %>>%
    dplyr::ungroup()

  return(appointments)
})
