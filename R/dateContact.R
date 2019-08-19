##### dateContact ###########################################
#' @include r6_helpers.R
#' functions to help create R6 classes
NULL

#' dateContact class
#' @title dateContact class
#' @description date information for dMeasure
#' @field date_a starting date of search (inclusive)
#' @field date_b ending date of search (inclusive)
#'
#' @section Methods:
#' \itemize{
#' \item{\code{\link{choose_date}} : read (or choose) dates}
#' }
#'
#' @examples
#'
#' @export
dateContact <-
  R6::R6Class("dateContact",
              public = list(
                initialize = function () {
                  if (length(public_init_fields$name) > 0) { # only if any defined
                    for (i in 1:length(public_init_fields$name)) {
                      if (public_init_fields$obj[[i]] == "dateContact") {
                        self[[public_init_fields$name[[i]]]] <-
                          eval(public_init_fields$value[[i]]) # could 'quote' the value
                      }
                    }
                  }
                  if (length(private_init_fields$name) > 0) { # only if any defined
                    for (i in 1:length(private_init_fields$name)) {
                      if (private_init_fields$obj[[i]] == "dateContact") {
                        private[[private_init_fields$name[[i]]]] <-
                          eval(private_init_fields$value[[i]]) # could 'quote' the value
                      }
                    }
                  }

                  if (requireNamespace("shiny", quietly = TRUE)) {
                    # set reactive version only if shiny is available
                    # note that this is for reading (from programs calling this object) only!
                    if (length(reactive_fields$name) > 0) { # only if any .reactive() defined
                      for (i in 1:length(reactive_fields$name)) {
                        if (reactive_fields$obj[[i]] == "dateContact") {
                          self[[reactive_fields$name[[i]]]] <- shiny::reactiveVal(
                            eval(reactive_fields$value[[i]]) # could 'quote' the value
                          )
                        }
                      }
                    }
                    if (length(reactive_event$name) > 0) { # only if any .reactive() defined
                      for (i in 1:length(reactive_event$name)) {
                        if (reactive_event$obj[[i]] == "dateContact") {
                          self[[reactive_event$name[[i]]]] <-
                            eval(reactive_event$value[[i]]) # could 'quote' the value
                        }
                      }
                    }
                  }
                }
              )
              # this is a 'skeleton' class
              # it is filled in the with the '.public' function

  )


##### special reactive functions ##########################


.private(dateContact, "set_reactive", function(myreactive, value) {
  # reactive (if shiny/reactive environment is available) is set to 'value'
  # myreactive is passed by reference
  if (requireNamespace("shiny", quietly = TRUE)) {
    myreactive(value)
  }
})
.private(dateContact, "trigger", function(myreactive) {
  # toggles a reactive between (usually) 0 and 1
  myreactive(1 - shiny::isolate(myreactive()))
})


##### date variables and location #####################################

## fields

.public_init(dateContact, "date_a", quote(Sys.Date())) # 'from' date. by default, it is 'today'
.public_init(dateContact, "date_b", quote(Sys.Date())) # 'to' date

## methods

#' Choose date
#'
#' Sets 'from' and 'to' dates used in subsequent searches
#'
#' @param dateContact_obj dateContact R6 object
#' @param date_from 'From' date. default is current date_from
#' @param date_to 'To' date. default is current date_to
#'
#' @return list(date_a, date_b)
#'
#' if date_a is later than date_b, a warning is returned,
#' and the dates are NOT changed
choose_date <- function(dateContact_obj,
                        date_from = dateContact_obj$date_a,
                        date_to = dateContact_obj$date_b) {
  dateContact_obj$choose_date(date_from, date_to)
}

.public(dateContact, "choose_date", function(date_from = self$date_a,
                                             date_to = self$date_b) {
  if (date_from > date_to) {
    warning("'From' date cannot be later than 'To' date")
    date_from <- self$date_a
    date_to <- self$date_b
  }
  self$date_a <- date_from
  self$date_b <- date_to

  private$set_reactive(self$date_aR, self$date_a)
  private$set_reactive(self$date_bR, self$date_b)

  return(list(self$date_a, self$date_b))
})
.reactive(dateContact, "date_aR", quote(self$date_a))
.reactive(dateContact, "date_bR", quote(self$date_b))

