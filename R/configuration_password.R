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
#' @export
user_login <- function(dMeasure_obj, password) {
  dMeasure_obj$user_login(password)
}

.public(dMeasure, "user_login", function(password) {
  if (is.null(private$.identified_user)) {
    stop("No user identified!")
  }
  if (self$empty_password()) {
    stop("No password set for currently identified user!")
  }
  if (!dMeasure::simple_tag_compare(password, private$.identified_user$Password)) {
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
#' @export
empty_password <- function(dMeasure_obj) {
  dMeasure_obj$empty_password()
}
.public(dMeasure, "empty_password", function() {
  # returns true if password for identified user is not defined, or empty
  # this is used by Dailymeasure to prompt for a password to be set
  empty = FALSE
  if (is.null(private$.identified_user$Password) || # NULL
      is.na(private$.identified_user$Password) || # NA
      length(private$.identified_user$Password) == 0 || # integer(0)
      nchar(private$.identified_user$Password) == 0) { # empty string
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

.public(dMeasure, "user_logout", function() {
  if (is.null(private$.identified_user) ||
      nrow(private$.identified_user) == 0) {
    # no identified user
  } else if (self$authenticated == FALSE) {
    # user not authetnicated
  }
  self$authenticated <- FALSE
  return(self$authenticated)
})

.private(dMeasure, "change_password", function(name, newpassword) {
  # change password of named 'name' (Fullname) user to 'newpassword'

  if (!newpassword == "") {
    # only encode password if it isn't a reset ("")
    newpassword <- dMeasure::simple_tag(newpassword)
  }
  # encode (actually 'tag') the password, if not an empty string
  # tagging (hash) defined in calculation_definitions.R

  private$.UserConfig <-
    private$.UserConfig %>>%
    dplyr::mutate(Password =
                    replace(Password,
                            name == Fullname,
                            newpassword))
  # replace password

  id <- private$.UserConfig %>>%
    dplyr::filter(Fullname == name) %>>%
    dplyr::pull(id)

  query <- "UPDATE Users SET Password = ? WHERE id = ?"
  # write to configuration database
  data_for_sql <- list(newpassword, id)

  self$config_db$dbSendQuery(query, data_for_sql)
  # if the connection is a pool, can't send write query (a statement) directly
  # so use the object's method
  private$trigger(self$config_db_trigR) # send a trigger signal

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
#' @export
password.set <- function(dMeasure_obj, newpassword, oldpassword = NULL) {
  dMeasure_obj$password.set(newpassword, oldpassword)
}

.public(dMeasure, "password.set", function(newpassword, oldpassword = NULL) {
  if (is.null(private$.identified_user)) {
    stop("No user identified!")
  }

  if (stringi::stri_length(newpassword) < 6 &
      stringi::stri_length(newpassword) != 0 &
      !("RequirePasswords" %in% self$userrestriction.list())) {
    # passwords not actually required
    stop("Password must be at least six (6) characters long, or empty.")
  }

  if (stringi::stri_length(newpassword) < 6 &
      ("RequirePasswords" %in% self$userrestriction.list())) {
    # passwords required
    stop("Password must be at least six (6) characters long, and passwords are required.")
  }

  if (self$empty_password()) {
    # no password yet set for currently identified user,
    # so just accept the 'newpassword'
    private$change_password(private$.identified_user$Fullname, newpassword)
    # change private$.UserConfig and SQLite configuration database
    self$authenticated <- TRUE
  } else {
    # there is an old password, which needs to be compared with 'oldpassword'
    if (!dMeasure::simple_tag_compare(oldpassword, private$.identified_user$Password)) {
      stop("Old password incorrect")
    } else {
      # old password specified correctly
      private$change_password(private$.identified_user$Fullname, newpassword)
      # change private$.UserConfig and SQLite configuration database
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
#' @export
password.reset <- function(dMeasure_obj, user, newpassword = "") {
  dMeasure_obj$password.reset(user, newpassword = "")
}

.public(dMeasure, "password.reset", function(user,
                                             newpassword = "") {

  tryCatch(permission <- self$useradmin.permission(),
           warning = function(w)
             stop(paste(w,
                        "'UserAdmin' permission required to reset/edit other passwords.")))

  if (!(user %in% private$.UserConfig$Fullname)) {
    stop("Only configured users can have a password reset!")
  }

  if (length(private$.identified_user$Fullname) == 0) {
    stop("You are not logged in as an identified user.")
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

  private$change_password(user, newpassword)
  # change private$.UserConfig and SQLite configuration database

  invisible(self)
})
