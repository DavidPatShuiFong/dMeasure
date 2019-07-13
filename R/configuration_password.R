##### Configuration - password ###########################################
#' Configuration - password
#'
#' @name configuration_password
#' @include dMeasure.R
#' needs access to dMeasure and methods and fields
#' @include calculation_definitions.R
#' needs access to functions
NULL

#' check password against the current identified user
#'
#' @param dMeasure_obj dMeasure object
#' @param password the password
#'
#' @return TRUE if password is correct
#' otherwise stops with error
user_login <- function(dMeasure_obj, password) {
  dMeasure_obj$user_login(password)
}

.public("user_login", function(password) {
  if (is.null(private$.identified_user)) {
    stop("No user identified!")
  }
  if (self$empty_password()) {
    stop("No password set for currently identified user!")
  }
  if (!simple_tag_compare(password, private$.identified_user$Password)) {
    stop("Wrong password")
  } else {
    self$authenticated <- TRUE
  }
  return(self$authenticated)
})

#' is password for the current identified user available?
#'
#' returns TRUE if password is not set (NA) or empty
#'
#' @param dMeasure_obj dMeasure object
#'
#' @return TRUE if password is not available
empty_password <- function(dMeasure_obj) {
  dMeasure_obj$empty_password()
}
.public("empty_password", function() {
  # returns true if password for identified user is not defined, or empty
  # this is used by Dailymeasure to prompt for a password to be set
  empty = FALSE
  if (is.na(private$.identified_user$Password) || nchar(private$.identified_user$Password) == 0) {
    empty = TRUE
  }
  return(empty)
})

#' Logout current identified user
#'
#' @param dMeasure_obj dMeasure object
#' @param password the password
#'
#' @return authentication status
#'  error (stop) if no identified user
#'  warning if not authenticated (logged in)
user_logout <- function(dMeasure_obj) {
  dMeasure_obj$user_logout()
}

.public("user_logout", function() {
  if (is.null(private$.identified_user)) {
    stop("No user identified!")
  }
  if (self$authenticated == FALSE) {
    warning("Current user was not authenticated prior to logout.")
  }
  self$authenticated <- TRUE
  return(self$authenticated)
})

#' Set password of currently identified user
#'
#' if there is an old password, that must be specified
#' if there is no old password, then 'oldpassword' does not need to be defined
#'
#' @param dMeasure_obj dMeasure object
#' @param newpassword the new password
#' @param oldpassword=NULL the old password (if one exists)
#'
#' @return TRUE if password is successfully set
#' otherwise, stops with error
set_password <- function(dMeasure_obj, newpassword, oldpassword = NULL) {
  dMeasure_obj$set_password(newpassword, oldpassword)
}

.public("set_password", function(newpassword, oldpassword = NULL) {
  if (is.null(private$.identified_user)) {
    stop("No user identified!")
  }

  if (stringi::stri_length(newpassword) < 6) {
    stop("Password must be at least six (6) characters long")
  }

  if (self$empty_password()) {
    # no password yet set for currentl identified user, so just accept the 'newpassword'
    setPassword(newpassword, private$UserConfig, private$.identified_user, private$config_db)
    self$authenticated <- TRUE
  } else {
    # there is an old password, which needs to be compared with 'oldpassword'
    if (!simple_tag_compare(oldpassword, private$.identified_user$Password)) {
      stop("Old password incorrect")
    } else {
      # old password specified correctly
      setPassword(newpassword, self, private$config_db)
      self$authenticated <- TRUE
    }
  }
  self$match_user() # re-read 'identified user' configuration, as password has changed

  return(self$authenticated)
})

#' password.reset
#'
#' Reset pass of (another) user to empty string
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param user Full name of user (not the current identified user)
#' @param newpassword the new password ("" empty string by default)
#'
#' No return value
#'  Error (stop) if trying to reset a user which
#'  doesn't exist, or trying to reset own password
password.reset <- function(dMeasure_obj, user, newpassword = "") {
  dMeasure_obj$password.reset(user, newpassword = "")
}

.public("password.reset", function(user,
                                   newpassword = "") {

  tryCatch(permission <- self$useradmin.permission(),
           warning = function(w)
             stop(paste(w,
                        "'UserAdmin' permission required to reset/edit other passwords.")))

  if (!(user %in% private$UserConfig$Fullname)) {
    stop("Only configured users can have a password reset!")
  }

  if (user == private$.identified_user$Fullname) {
    stop("Can't remove/reset your own password!")
  }

  if (!(newpassword == "")) {
    # 'resetting' password is ""
    if (stringi::stri_length(newpassword) < 6) {
      stop("Password must be at least six (6) characters long")
    }
  }

  if (!newpassword == "") {
    # only encode password if it isn't a reset ("")
    newpassword <- simple_encode(newpassword)
  }
  # encode the password, if not an empty string

  private$UserConfig <-
    private$UserConfig %>>%
    dplyr::mutate(Password = replace(Password,
                                     Fullname == user,
                                     newpassword))
  # replace password with empty string

  UserRow <- private$UserConfig %>>%
    dplyr::filter(Fullname == user)
  # just select the user for whom we are removing the password

  query <- "UPDATE Users SET Password = ? WHERE id = ?"
  # write to configuration database
  data_for_sql <- list(newpassword, UserRow$id[[1]])

  private$config_db$dbSendQuery(query, data_for_sql)
  # if the connection is a pool, can't send write query (a statement) directly
  # so use the object's method

  invisible(self)
})
