##### Configuration - user ###########################################
#' Configuration - user
#'
#' @name configuration_user
#' @include dMeasure.R
#' needs access to dMeasure and methods and fields
#' @include calculation_definitions.R
#' needs access to functions
NULL

#' password.reset
#'
#' Reset pass of (another) user to empty string
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param user Full name of user
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
                        "'UserAdmin' permission required to view or edit location list.")))

  if (!(user %in% self$UserConfig$Fullname)) {
    stop("Only configured users can have a password reset!")
  }

  if (user == self$identified_user$Fullname) {
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

  self$UserConfig <-
    self$UserConfig %>>%
    dplyr::mutate(Password = replace(Password,
                                     Fullname == user,
                                     newpassword))
  # replace password with empty string

  UserRow <- self$UserConfig %>>%
    dplyr::filter(Fullname == user)
  # just select the user for whom we are removing the password

  query <- "UPDATE Users SET Password = ? WHERE id = ?"
  # write to configuration database
  data_for_sql <- list(newpassword, UserRow$id[[1]])

  self$config_db$dbSendQuery(query, data_for_sql)
  # if the connection is a pool, can't send write query (a statement) directly
  # so use the object's method

  invisible(self)
})

#' useradmin.permission
#'
#' Does the current user have user admin access permission?
#'
#' Can only be 'false' if $UserRestrictions$Restriction contains
#' 'UserAdmin'
#'
#' if 'UserAdmin' in the $UserRestrictions, then a user needs
#' to be identified and 'authenticated'. Authentication requires
#' identification (via Sys.info()$user), and might also require
#' a password
#'
#' @param dMeasure_obj dMeasure R6 object
#'
#' @return TRUE or FALSE
#'  additionally returns a warning if permission is FALSE
useradmin.permission <- function(dMeasure_obj) {
  dMeasure_obj$useradmin.permission()
}

.public("useradmin.permission", function() {
  if ("UserAdmin" %in% unlist(self$UserRestrictions$Restriction)) {
    # only some users allowed to see/change server settings
    if ("UserAdmin" %in% unlist(self$identified_user$Attributes) &
        self$authenticated == TRUE) {
      permission <- TRUE
    } else {
      # this user is not authorized to access the locations list
      permission <- FALSE
      warning("No 'UserAdmin' attribute for this user.")
    }
  } else {
    # no 'UserAdmin' attribute required
    permission <- TRUE
  }
  return(permission)
})
