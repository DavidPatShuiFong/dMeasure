#' billings
#'
#' MBS (medicare benefits schedule) item numbers for CDM
#'
#' @name billings
#'
#' @include dMeasure.R
#' @include appointments.R
#' @include fomantic_definitions_tags.R
NULL


#' show only 'own' billings next to provider name
#'
#' @name own_billings
#'
#' @param dMeasure_obj dMeasure R6 object
#'
#' @return current value of own_billings if no parameter provided
#'
#' @export
own_billings <- function(dMeasure_obj, value) {
  if (missing(value)) {
    return(dMeasure_obj$own_billings)}
  else {
    dMeasure_obj$own_billings <- value
  }
}
.private(dMeasure, ".own_billings", TRUE)
.active(dMeasure, "own_billings", function(value) {
  if (missing(value)) {
    return(private$.own_billings)
  } else if (is.logical(value)) {
    private$.own_billings <- value
  } else {
    warning("$own_billings only accepts logical values.")
  }

  private$set_reactive(self$own_billingsR, private$.own_billings) # set reactive version
  return(private$.own_billings)
})
.reactive(dMeasure, "own_billingsR", quote(private$.own_billings))

.public(dMeasure, "services_list",
        data.frame(Patient = character(), InternalID = integer(),
                   ServiceDate = as.Date(integer(0), origin = "1970-01-01"),
                   MBSItem = integer(0),
                   Description = character(0),
                   Provider = character(0),
                   DOB = as.Date(integer(0), origin = "1970-01-01"),
                   Age = numeric())
)

#' List of services with date of birth
#'
#' Filtered by date, and chosen clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from default is $date_a. start date, inclusive (date object)
#' @param date_to default is $date_b end date, inclusive (date object)
#' @param clinicians default is $clinicians. list of clinicians to view
#' @param lazy if lazy=TRUE, then don't re-calculate $appointments_filtered to calculate
#'
#' @return list of services
#' @export
list_services <- function(dMeasure_obj,
                          date_from = NA, date_to = NA,
                          clinicians = NA,
                          lazy = FALSE) {
  dMeasure_obj$list_services(date_from, date_to, clinicians, lazy)
}
.public(dMeasure, "list_services",
        function(date_from = NA,
                 date_to = NA,
                 clinicians = NA,
                 screentag = FALSE,
                 screentag_print = TRUE,
                 lazy = FALSE) {

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

          clinicians <- intersect(clinicians,
                                  self$clinician_list(view_name = "billings"))
          # this view is potentially restricted. if 'GlobalBillView' restriction
          # is enabled, reduce the number of clinicians shown, if the authorized
          # user does not have 'GlobalBillView' attribute

          if (all(is.na(clinicians)) || length(clinicians) == 0) {
            clinicians <- c("")
          }

          if (self$emr_db$is_open()) {
            # only if EMR database is open

            self$services_list <- self$db$servicesRaw %>>%
              dplyr::filter(ServiceDate >= date_from & ServiceDate <= date_to) %>>%
              dplyr::left_join(self$db$invoices, by = "InvoiceID") %>>%
              dplyr::left_join(self$db$users %>>%
                                 dplyr::select(UserID, Title,
                                               PFirstname = Firstname, PSurname = Surname),
                               by = "UserID") %>>%
              dplyr::left_join(self$db$patients %>>%
                                 dplyr::select(InternalID, Firstname, Surname, DOB),
                               by = "InternalID") %>>%
              dplyr::collect() %>>%
              dplyr::mutate(ServiceDate = as.Date(ServiceDate),
                            DOB = as.Date(DOB),
                            Provider = paste(Title, PFirstname, PSurname),
                            Patient = paste(Firstname, Surname),
                            Age = dMeasure::calc_age(DOB, ServiceDate)) %>>%
              dplyr::filter(Provider %in% clinicians) %>>%
              dplyr::select(Patient, InternalID, ServiceDate, MBSItem, Description, Provider,
                            DOB, Age)
          }
          return(self$services_list)
        })
.reactive_event(dMeasure, "services_listR",
                quote(
                  shiny::eventReactive(
                    c(self$date_aR(), self$date_bR(),
                      self$cliniciansR()), {
                        self$list_services()
                      })
                ))


.public(dMeasure, "services_list_allclinicians",
        data.frame(Patient = character(), InternalID = integer(),
                   ServiceDate = as.Date(integer(0), origin = "1970-01-01"),
                   MBSItem = integer(0),
                   Description = character(0),
                   Provider = character(0),
                   DOB = as.Date(integer(0), origin = "1970-01-01"),
                   Age = numeric())
)

#' List of services with date of birth
#'
#' Filtered by date. Not filted by clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from default is $date_a. start date, inclusive (date object)
#' @param date_to default is $date_b end date, inclusive (date object)
#' @param lazy if lazy=TRUE, then don't re-calculate $appointments_filtered to calculate
#'
#' @return list of services
#' @export
list_services_allclinicians <- function(dMeasure_obj,
                                        date_from = NA, date_to = NA,
                                        lazy = FALSE) {
  dMeasure_obj$list_services_allclinicians(date_from, date_to, lazy)
}
.public(dMeasure, "list_services_allclinicians",
        function(date_from = NA,
                 date_to = NA,
                 lazy = FALSE) {

          if (is.na(date_from)) {
            date_from <- self$date_a
          }
          if (is.na(date_to)) {
            date_to <- self$date_b
          }

          if (self$emr_db$is_open()) {
            # only if EMR database is open

            self$services_list_allclinicians <- self$db$servicesRaw %>>%
              dplyr::filter(ServiceDate >= date_from & ServiceDate <= date_to) %>>%
              dplyr::left_join(self$db$invoices, by = "InvoiceID") %>>%
              dplyr::left_join(self$db$patients %>>%
                                 dplyr::select(InternalID, Firstname, Surname, DOB),
                               by = "InternalID") %>>%
              dplyr::collect() %>>%
              dplyr::mutate(ServiceDate = as.Date(ServiceDate),
                            DOB = as.Date(DOB),
                            Patient = paste(Firstname, Surname),
                            Age = dMeasure::calc_age(DOB, ServiceDate)) %>>%
              dplyr::select(Patient, InternalID, ServiceDate, MBSItem, Description,
                            DOB, Age)

          }
          return(self$services_list_allclinicians)
        })
.reactive_event(dMeasure, "services_list_allcliniciansR",
                quote(
                  shiny::eventReactive(
                    c(self$date_aR(), self$date_bR()), {
                      self$list_services()
                    })
                ))

#' Billings done on same day as appointments or visit
#'
#' filter to billings which are done on the same day as displayed appointments
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from (default dMeasure_obj$date_a) start date
#' @param date_to (default dMeasure_obj$date_b) end date (inclusive)
#' @param clinicians (default dMeasure_obj$clinicians) list of clinicians to view
#' @param lazy if TRUE, then do not recalculate appointment list. otherwise, re-calculate
#' @param screentag (default FALSE) optionally add a fomantic/semantic HTML description of 'action'
#' @param screentag_print (default TRUE) optionally add a 'printable' description of 'action'
#'
#' @return list of appointments (with patient details)
#' @export
appointments_billings_sameday <- function(dMeasure_obj, date_from, date_to, clinicians,
                                          lazy, screentag, screentag_print) {
  dMeasure_obj$appointments_billlings_sameday(date_from, date_to, clinicians,
                                              lazy, screentag, screentag_print)
}
.public(dMeasure, "appointments_billings_sameday", function(date_from = NA,
                                                            date_to = NA,
                                                            clinicians = NA,
                                                            lazy = FALSE,
                                                            screentag = FALSE,
                                                            screentag_print = TRUE) {
  # as of 2019-Sep-08, this method's functionality has been replaced
  # by $list_billings

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

  if (!lazy) {
    self$list_appointments(date_from, date_to, clinicians, lazy = FALSE)
    # if not 'lazy' evaluation, then re-calculate
    # (that is automatically done by calling the above methods)
  }

  billings_sameday <- self$appointments_billings %>>%
    dplyr::filter(ServiceDate == AppointmentDate) %>>%
    # billings done on the same day as displayed appointments
    dplyr::select(Patient, InternalID, AppointmentDate, AppointmentTime,
                  Provider, MBSItem, Description)
  # need to preserve ApppointmentTime and Provider
  # in the case where there are multiple apppointments
  # for the patient in the same time period/day and providers

  if (screentag) {
    # change MBSITEMS into fomantic/semantic tags
    billings_sameday <- billings_sameday %>>%
      dplyr::mutate(billingtag =
                      dMeasure::semantic_button(MBSItem,
                                                colour = 'green',
                                                popuphtml = paste0('<h4>', AppointmentDate,
                                                                   "</h3><p><font size=\'+0\'>",
                                                                   Description, '</p>')))
  }

  if (screentag_print) {
    billings_sameday <- billings_sameday %>>%
      dplyr::mutate(billingtag_print = MBSItem)
  }

  return(billings_sameday)
})

.public(dMeasure, "billings_list",
        data.frame(Patient = character(), InternalID = integer(),
                   Date = as.Date(integer(0), origin = "1970-01-01"),
                   AppointmentTime = character(0),
                   Status = character(0),
                   VisitType = character(0),
                   Provider = character(0),
                   DOB = as.Date(integer(0), origin = "1970-01-01"),
                   Age = numeric(0),
                   billingtag = character(0),
                   billingtag_print = character(0))
)

#' Billings, appointments and visit combined view
#'
#' filter to chosen date range and practitioners
#'
#' billings are aggregated/group to patient/day
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param date_from (default date_a field) start date
#' @param date_to (default date_b field) end date (inclusive)
#' @param clinicians (default clinicians field) list of clinicians to view
#' @param own_billings (default own_billings field) logical TRUE/FALSE
#'
#'  only show billings done by the specified provider on the
#'  contact (visit/appointment/billing) day.
#'
#'  if FALSE, the all billings
#'  done on the patient on that day are shown.
#' @param lazy if TRUE, then do not recalculate appointment list. otherwise, re-calculate
#' @param screentag (default FALSE) optionally add a fomantic/semantic HTML description of 'action'
#' @param screentag_print (default TRUE) optionally add a 'printable' description of 'action'
#'
#' @return list of appointments (with patient details)
#' @export
list_billings <- function(dMeasure_obj, date_from, date_to, clinicians, own_billings,
                          lazy, screentag, screentag_print) {
  dMeasure_obj$list_billings(date_from, date_to, clinicians, own_billings,
                             lazy, screentag, screentag_print)
}
.public(dMeasure, "list_billings", function(date_from = NA,
                                            date_to = NA,
                                            clinicians = NA,
                                            own_billings = NA,
                                            lazy = FALSE,
                                            screentag = FALSE,
                                            screentag_print = TRUE) {

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
  if (is.na(own_billings)) {
    own_billings <- self$own_billings
  }

  if (!lazy) {
    self$list_appointments(date_from, date_to, clinicians, lazy = FALSE)
    self$list_visits(date_from, date_to, clinicians, lazy = FALSE)
    self$list_services(date_from, date_to, clinicians, lazy = FALSE)
    if (!own_billings) {
      self$list_services_allclinicians(date_from, date_to, lazy = FALSE)
    }
  }

  df <- self$appointments_list %>>%
    dplyr::rename(Date = AppointmentDate) %>>%
    dplyr::full_join(self$visits_list %>>%
                       dplyr::rename(Date = VisitDate),
                     by = c("Patient", "InternalID", "Date", "DOB", "Age", "Provider")) %>>%
    dplyr::full_join(self$services_list %>>%
                       {if (screentag & own_billings) {
                         # if 'screen' (not print) display
                         # and 'own_billings'. if own_billings is FALSE then
                         #  the tags will be provided by self$service_list_allclinicians below
                         dplyr::mutate(., billingtag =
                                         dMeasure::semantic_button(
                                           MBSItem,
                                           colour = 'green',
                                           popuphtml = paste0('<h4>', ServiceDate,
                                                              "</h3><p><font size=\'+0\'>",
                                                              Description, '</p>')))
                       } else {.}} %>>%
                       {if (screentag_print & own_billings) {
                         dplyr::mutate(., billingtag_print = MBSItem)
                       } else {.}} %>>%
                       dplyr::select(-c(MBSItem, Description)) %>>%
                       dplyr::group_by(Patient, InternalID, ServiceDate, Provider, DOB, Age) %>>%
                       # gathers services from the same date/provider into a single row
                       {if (screentag & own_billings) {
                         dplyr::summarise(., billingtag = paste(billingtag, collapse = ""))
                       } else {.} } %>>%
                       {if (screentag_print & own_billings) {
                         dplyr::summarise(., billingtag_print = paste(billingtag_print, collapse = ", "))
                       } else {.} } %>>%
                       dplyr::ungroup() %>>%
                       dplyr::rename(Date = ServiceDate),
                     by = c("Patient", "InternalID", "Date","DOB", "Age", "Provider")) %>>%
    {if (!own_billings)
      # if displaying 'all' billings for the contacted patient (on the 'contact' day)
      # if own_billings is TRUE, then only billings done by the listed
      # provider is displayed
    {dplyr::full_join(. ,
                      # billings and billings_tag from self$services_list
                      self$services_list_allclinicians %>>%
                        {if (screentag) {
                          dplyr::mutate(., billingtag =
                                          dMeasure::semantic_button(
                                            MBSItem,
                                            colour = 'green',
                                            popuphtml = paste0('<h4>', ServiceDate,
                                                               "</h3><p><font size=\'+0\'>",
                                                               Description, '</p>')))
                        } else {.}} %>>%
                        {if (screentag_print) {
                          dplyr::mutate(., billingtag_print = MBSItem)
                        } else {.}} %>>%
                        dplyr::select(-c(MBSItem, Description)) %>>%
                        dplyr::group_by(Patient, InternalID, ServiceDate, DOB, Age) %>>%
                        # gathers services from the same date/provider into a single row
                        {if (screentag) {
                          dplyr::summarise(., billingtag = paste(billingtag, collapse = ""))
                        } else {.}} %>>%
                        {if (screentag_print) {
                          dplyr::summarise(., billingtag_print = paste(billingtag_print, collapse = ", "))
                        } else {.}} %>>%
                        dplyr::ungroup() %>>%
                        dplyr::rename(Date = ServiceDate),
                      by = c("Patient", "InternalID", "Date", "DOB", "Age"))}
      else {.}} %>>%
    # merges appointments and visit and services lists
    {if (screentag) {dplyr::select(., Patient, InternalID, Date, AppointmentTime,
                                   Status, VisitType, Provider, DOB, Age, billingtag)
    } else {.}} %>>%
    {if (screentag_print) {
      dplyr::select(., Patient, InternalID, Date, AppointmentTime,
                    Status, VisitType, Provider, DOB, Age, billingtag_print)
    } else {.}}

  self$billings_list <- df

  return(self$billings_list)
})
.reactive_event(dMeasure, "billings_listR",
                quote(
                  shiny::eventReactive(
                    c(self$appointments_listR(),
                      self$services_listR(),
                      self$visits_listR(),
                      self$own_billingsR()), {
                        self$list_billings(lazy = FALSE)
                      })
                ))
