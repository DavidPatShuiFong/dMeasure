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

# restriction types
#  ServerAdmin - only users with ServerAdmin attribute can view/change server settings
#  UserAdmin - only users with UserAdmin attribute can view/change user settings
#  GlobalActionView - only users with GlobalActionView attribute can see
#                     potential actions in "other" people's appointment books
#  GlobalBillView   - only users with GlobalBillView attribute can see
#                     billings in "other" people's appointment books
#  GlobalCDMView    - only users with GlobalCDMView attribute can see
#                     potential CDM actions in "other" people's appointment books
#  RequirePasswords - identified users need to use password to be 'authenticated'

restrictionTypes <- list(
  list(
    id = "ServerAdmin", label = "Server Administrator",
    Description = "Only ServerAdmin users can change database server settings",
    userAttribute = TRUE,
    # is this actually a userattribute
    callback = function (state, AttributeList, anyPassword) {
      # these callbacks have no immediate access to the parent environment
      # only called if state is changed from the old state
      # @param state          : attempted new state
      # @param AttributeList  : current list of user attributes in use
      # @param anyPassword    : any passwords TRUE or FALSE
      #
      # @return list (state, error, warn)  : returns 'state', if permissible
      #                                       error$message and error$title
      #                                       warn$message and warn$title
      #
      # note that this function does not change UserRestrictions()
      # changing UserRestrictions() is the responsibility of the calling function
      newstate = state # note, can't use "<-" as operator in subsequent data.frame(Reduce(...))
      error = list() # will hold $message and $title if set
      warn = list() # will hold $message and $title if set

      if (state == TRUE) {
        # if trying to turn on ServerAdmin restriction
        if (!("ServerAdmin" %in% AttributeList)) {
          # no user is listed as having ServerAdmin attribute!
          # if this restriction is established, no one can edit the server
          # (though this could be worked around by changing user settings)
          error$message =
            "At least one user must have 'ServerAdmin' attribute to enable 'ServerAdmin' restriction."
          error$title =  "Can't enable 'ServerAdmin' restriction"
          newstate = FALSE
        } else {
          newstate = TRUE
          # allow ServerAdmin to be restricted
        }
      } else {
        # turning off ServerAdmin restriction
        warn$message =
          "Without this restriction, anyone can edit and change Best Practice database settings!"
        warn$title = "Disabling 'ServerAdmin' restriction"
      }
      return(list(state = newstate, error = error, warn = warn))
    }
  ),
  list(
    id = "UserAdmin", label = "User Administrator",
    Description = "Only UserAdmin users can change user permissions",
    userAttribute = TRUE,
    callback = function (state, AttributeList, anyPassword) {
      # these callbacks have no immediate access to the parent environment
      # only called if state is changed from the old state
      # @param state          : attempted new state
      # @param AttributeList  : current list of user attributes in use
      # @param anyPassword    : any passwords TRUE or FALSE
      #
      # @return list (state, error, warn)  : returns 'state', if permissible
      #                                       error$message and error$title
      #                                       warn$message and warn$title
      #
      # note that this function does not change UserRestrictions()
      # changing UserRestrictions() is the responsibility of the calling function
      newstate = state
      error = list() # will hold $message and $title if set
      warn = list() # will hold $message and $title if set

      if (state == TRUE) {
        # if trying to turn on ServerAdmin restriction
        if (!("UserAdmin" %in% AttributeList)) {
          # no user is listed as having UserAdmin attribute!
          # if this restriction is established, no one can edit the user settings
          error$message =
            "A least one user must have 'UserAdmin' attribute to enable the 'UserAdmin' restriction."
          error$title = "Can't enable 'UserAdmin' restriction"
          newstate = FALSE
        } else {
          newstate = TRUE
          # allow UserAdmin to be restricted
        }
      } else {
        warn$message =
          "Without this restriction, anyone can edit and change user permission settings!"
        warn$title = "Disabling 'UserAdmin' restriction"
      }
      return(list(state = newstate, error = error, warn = warn))
    }
  ),
  list(
    id = "GlobalActionView", label = "Global Action View",
    Description = "GlobalActionView users can view actions in 'other' appointment lists",
    userAttribute = TRUE,
    callback = function (state, AttributeList, anyPassword) {
      # this can always be set/unset
      return(state)}
  ),
  list(
    id = "GlobalBillView", label = "Global Bill View",
    Description = "GlobalBillView users can view billing status in 'other' appointment lists",
    userAttribute = TRUE,
    callback = function (state, AttributeList, anyPassword) {
      # this can always be set/unset
      return(state)
    }
  ),
  list(
    id = "GlobalCDMView", label = "Global CDM View",
    Description = "GlobalCDMView users can view CDM status in 'other' appointment lists",
    userAttribute = TRUE,
    callback = function (state, AttributeList, anyPassword) {
      # this can always be set/unset
      return(state)
    }
  ),
  list(
    id = "RequirePasswords", label = "Require Passwords",
    Description = "Password required from all users",
    userAttribute = FALSE,
    # 'RequirePasswords' is not actually a user attribute
    callback = function (state, AttributeList, anyPassword) {
      # these callbacks have no immediate access to the parent environment
      # only called if state is changed from the old state
      # @param state          : attempted new state
      # @param AttributeList  : current list of user attributes in use
      # @param anyPassword    : any passwords TRUE or FALSE
      #
      # @return list (state, error, warn)  : returns 'state', if permissible
      #                                       error$message and error$title
      #                                       warn$message and warn$title
      #
      # note that this function does not change UserRestrictions()
      # changing UserRestrictions() is the responsibility of the calling function
      newstate = state
      error = list() # will hold $message and $title if set
      warn = list() # will hold $message and $title if set

      if (state == TRUE) {
        # if trying to turn on RequirePasswords restriction
        if (anyPassword == FALSE) {
          # no user is listed as having a password!
          # if this restriction is established, no one will be able to log in
          error$message =
            "A least one user must have a password to enable the 'Require Passwords' restriction."
          error$title = "Can't enable 'Require Passwords' restriction"
          newstate = FALSE
        } else {
          newstate = TRUE
          # allow RequirePassword
        }
      } else {
        warn$message =
          "Without this restriction, users do not require passwords"
        warn$title = "Disabling 'Require Passwords' restriction"
      }
      return(list(state = newstate, error = error, warn = warn))
    }
  )
)

restrictionTypes_df <- data.frame(Reduce(rbind, restrictionTypes))
# converts the list to a dataframe
restrictionTypes_id <- unlist(restrictionTypes_df$id,
                              use.names = FALSE)
user_attribute_types <- unlist(dplyr::filter(restrictionTypes_df, userAttribute == TRUE)$id,
                               use.names = FALSE)
# user attribute types is defined in restrictionTypes. only those with userAttribute TRUE

#' userrestriction.change
#'
#' Change user restrictions
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param restriction name of restriction
#' @param state new state (TRUE or FALSE)
#'
#' @return list (newstate, error, warn)  : returns 'state', if permissible
#                                       error$message and error$title
#                                       warn$message and warn$title
userrestriction.change <- function(dMeasure_obj, restriction, state) {
  dMeasure_obj$userrestriction.change(restriction, state)
}

.public("userrestriction.change", function(restriction, state) {

  tryCatch(permission <- self$useradmin.permission(),
           warning = function(w)
             stop(paste(w,
                        "'UserAdmin' permission required to change user restrictions")))

  if (!restriction %in% restrictionTypes_id) {
    stop("That restriction is not recognized.")
  }

  update_UserRestrictions_database <- function() {
    # update UserRestrictions database
    # this is manually called when (one) restriction is added or removed
    # so only has to find 'one' row of difference between the 'new' list and the 'old' list
    originalRestrictions <- self$config_db$conn() %>>%
      dplyr::tbl("UserRestrictions") %>>%
      dplyr::collect()
    newRestrictions <- self$UserRestrictions

    new_row <- dplyr::anti_join(newRestrictions, originalRestrictions, by = "uid")
    if (nrow(new_row) > 0) {
      # if there is a new row, then add to configuration database
      query <- "INSERT INTO UserRestrictions (uid, Restriction) VALUES (?, ?)"
      data_for_sql <- as.list.data.frame(c(new_row$uid, new_row$Restriction))

      self$config_db$dbSendQuery(query, data_for_sql)
      # if the connection is a pool, can't send write query (a statement) directly
      # so use the object's method
    } else {
      deleted_row <- dplyr::anti_join(originalRestrictions, newRestrictions, by = "uid")
      if (nrow(deleted_row) > 0) {
        # if a row was deleted, then remove from configuration database
        query <- "DELETE FROM UserRestrictions WHERE uid = ?"
        data_for_sql <- as.list.data.frame(c(deleted_row$uid))

        self$config_db$dbSendQuery(query, data_for_sql)
        # if the connection is a pool, can't send write query (a statement) directly
        # so use the object's method
      }
    }
  }

  restrictionLocal <- restrictionTypes_df %>>% dplyr::filter(id == restriction)

  newstate <-
    restrictionLocal$callback[[1]](state,
                                   self$UserConfig$Attributes %>>%
                                     unlist(use.names = FALSE),
                                   # list of attributes in use
                                   (nchar(apply(cbind(unlist(self$UserConfig$Password,
                                                             use.names = FALSE)),
                                                1,
                                                function(x)
                                                  paste(x[!is.na(x)], collapse = ""))) > 0)
                                   # any passwords are set?
                                   # this code to concatenate strings, NA or not, was found on StackOverflow
                                   # by 'Joe' https://stackoverflow.com/questions/13673894/suppress-nas-in-paste)
  )

  # returns state. same as the 'changed to' state, if it is permissible
  # e.g. it isn't permissible to set ServerAdmin/UserAdmin to 'TRUE' if
  # there is no user who has UserAdmin attribute
  if (state != newstate$state) {
    # state returned is not the same as the state which was attempted
    # so change the state 'back' to the what is, in fact, the old state
  } else {
    # state returned is the same as the attempted change
    if (state == TRUE) {
      self$UserRestrictions <-
        dplyr::bind_rows(self$UserRestrictions,
                         data.frame(uid = max(self$UserRestrictions$uid, 0) + 1,
                                    Restriction = restriction,
                                    stringsAsFactors = FALSE))
      # add entry to datatable of UserRestrictions
      # add one to maximum UID (at least zero), and add restriction to new row
      # note that this table in the database uses 'uid' rather than 'id'
      #  to reduce confusion with the use of 'id' for input names
      update_UserRestrictions_database()
    } else {
      # remove entry in datatable of UserRestrictions
      self$UserRestrictions <-
        dplyr::filter(self$UserRestrictions,
                      self$UserRestrictions$Restriction != restriction)
      update_UserRestrictions_database()
    }
  }

  return(newstate)
})

#' userrestriction.list
#'
#' List user restrictions in use
#'
#' @param dMeasure_obj dMeasure R6 object
#'
#' @return vector of user restrictions
userrestriction.list <- function(dMeasure_obj) {
  dMeasure_obj$userrestriction.list()
}

.public("userrestriction.list", function() {
  self$UserRestrictions$Restriction
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
