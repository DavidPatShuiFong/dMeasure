# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

##### Configuration - user ###########################################
#' Configuration - user
#'
#' @name configuration_user
#' @include dMeasure.R
#' needs access to dMeasure and methods and fields
#' @include calculation_definitions.R
#' needs access to functions
NULL

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

#' returns list of the restriction types
#'
#' no paramters
#'
#' @return a list
#' @export
restrictionTypes_list <- function() {
  return(
    list(
      list(
        id = "ServerAdmin", label = "Server Administrator",
        Description = "Only ServerAdmin users can change database server settings",
        userAttribute = TRUE,
        # is this actually a userattribute
        callback = function(state, AttributeList, anyPassword) {
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
          newstate <- state # note, can't use "<-" as operator in subsequent data.frame(Reduce(...))
          error <- list() # will hold $message and $title if set
          warn <- list() # will hold $message and $title if set

          if (state == TRUE) {
            # if trying to turn on ServerAdmin restriction
            if (!("ServerAdmin" %in% AttributeList)) {
              # no user is listed as having ServerAdmin attribute!
              # if this restriction is established, no one can edit the server
              # (though this could be worked around by changing user settings)
              error$message <-
                "At least one user must have 'ServerAdmin' attribute to enable 'ServerAdmin' restriction."
              error$title <- "Can't enable 'ServerAdmin' restriction"
              newstate <- FALSE
            } else {
              newstate <- TRUE
              # allow ServerAdmin to be restricted
            }
          } else {
            # turning off ServerAdmin restriction
            warn$message <-
              "Without this restriction, anyone can edit and change Best Practice database settings!"
            warn$title <- "Disabling 'ServerAdmin' restriction"
          }
          return(list(state = newstate, error = error, warn = warn))
        }
      ),
      list(
        id = "UserAdmin", label = "User Administrator",
        Description = "Only UserAdmin users can change user permissions",
        userAttribute = TRUE,
        callback = function(state, AttributeList, anyPassword) {
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
          newstate <- state
          error <- list() # will hold $message and $title if set
          warn <- list() # will hold $message and $title if set

          if (state == TRUE) {
            # if trying to turn on ServerAdmin restriction
            if (!("UserAdmin" %in% AttributeList)) {
              # no user is listed as having UserAdmin attribute!
              # if this restriction is established, no one can edit the user settings
              error$message <-
                "A least one user must have 'UserAdmin' attribute to enable the 'UserAdmin' restriction."
              error$title <- "Can't enable 'UserAdmin' restriction"
              newstate <- FALSE
            } else {
              newstate <- TRUE
              # allow UserAdmin to be restricted
            }
          } else {
            warn$message <-
              "Without this restriction, anyone can edit and change user permission settings!"
            warn$title <- "Disabling 'UserAdmin' restriction"
          }
          return(list(state = newstate, error = error, warn = warn))
        }
      ),
      list(
        id = "GlobalActionView", label = "Global Action View",
        Description = "GlobalActionView users can view actions in 'other' appointment lists",
        userAttribute = TRUE,
        callback = function(state, AttributeList, anyPassword) {
          # this can always be set/unset
          return(list(state = state, error = list(), warn = list()))
        }
      ),
      list(
        id = "GlobalBillView", label = "Global Bill View",
        Description = "GlobalBillView users can view billing status in 'other' appointment lists",
        userAttribute = TRUE,
        callback = function(state, AttributeList, anyPassword) {
          # this can always be set/unset
          return(list(state = state, error = list(), warn = list()))
        }
      ),
      list(
        id = "GlobalCDMView", label = "Global CDM View",
        Description = "GlobalCDMView users can view CDM status in 'other' appointment lists",
        userAttribute = TRUE,
        callback = function(state, AttributeList, anyPassword) {
          # this can always be set/unset
          return(list(state = state, error = list(), warn = list()))
        }
      ),
      list(
        id = "RequirePasswords", label = "Require Passwords",
        Description = "Password required from all users",
        userAttribute = FALSE,
        # 'RequirePasswords' is not actually a user attribute
        callback = function(state, AttributeList, anyPassword) {
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
          newstate <- state
          error <- list() # will hold $message and $title if set
          warn <- list() # will hold $message and $title if set

          if (state == TRUE) {
            # if trying to turn on RequirePasswords restriction
            if (anyPassword == FALSE) {
              # no user is listed as having a password!
              # if this restriction is established, no one will be able to log in
              error$message <-
                "A least one user must have a password to enable the 'Require Passwords' restriction."
              error$title <- "Can't enable 'Require Passwords' restriction"
              newstate <- FALSE
            } else {
              newstate <- TRUE
              # allow RequirePassword
            }
          } else {
            warn$message <-
              "Without this restriction, users do not require passwords"
            warn$title <- "Disabling 'Require Passwords' restriction"
          }
          return(list(state = newstate, error = error, warn = warn))
        }
      )
    )
  )
}

.public_init(dMeasure, "restrictionTypes", quote(
  dMeasure::restrictionTypes_list()
))


.public_init(
  dMeasure, "restrictionTypes_df",
  quote(data.frame(Reduce(rbind, self$restrictionTypes)))
)

# converts the list to a dataframe
.public_init(
  dMeasure, "restrictionTypes_id",
  quote(unlist(self$restrictionTypes_df$id,
    use.names = FALSE
  ))
)

.public_init(
  dMeasure, "user_attribute_types",
  quote(unlist(dplyr::filter(
    self$restrictionTypes_df,
    userAttribute == TRUE
  )$id,
  use.names = FALSE
  ))
)

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
#'                                       error$message and error$title
#'                                       warn$message and warn$title
#' @export
userrestriction.change <- function(dMeasure_obj, restriction, state) {
  dMeasure_obj$userrestriction.change(restriction, state)
}

.public(dMeasure, "userrestriction.change", function(restriction, state) {
  tryCatch(permission <- self$useradmin.permission(),
    warning = function(w)
      stop(paste(
        w,
        "'UserAdmin' permission required to change user restrictions"
      ))
  )

  if (!restriction %in% self$restrictionTypes_id) {
    stop("That restriction is not recognized.")
  }

  update_UserRestrictions_database <- function(d) {
    # update UserRestrictions database with 'd'
    # this is manually called when (one) restriction is added or removed
    # so only has to find 'one' row of difference between the 'new' list and the 'old' list
    originalRestrictions <- private$.UserRestrictions %>>%
      dplyr::collect()
    newRestrictions <- d

    new_row <- dplyr::anti_join(newRestrictions, originalRestrictions, by = "uid")
    if (nrow(new_row) > 0) {
      # if there is a new row, then add to configuration database
      query <- "INSERT INTO UserRestrictions (uid, Restriction) VALUES (?, ?)"
      data_for_sql <- as.list.data.frame(c(new_row$uid, new_row$Restriction))

      self$config_db$dbSendQuery(query, data_for_sql)
      # if the connection is a pool, can't send write query (a statement) directly
      # so use the object's method
      private$trigger(self$config_db_trigR) # send a trigger signal
    } else {
      deleted_row <- dplyr::anti_join(originalRestrictions, newRestrictions, by = "uid")
      if (nrow(deleted_row) > 0) {
        # if a row was deleted, then remove from configuration database
        query <- "DELETE FROM UserRestrictions WHERE uid IN ({d*})"
        d <- as.list.data.frame(c(deleted_row$uid))

        self$config_db$dbSendGlueQuery(query, d = d)
        # if the connection is a pool, can't send write query (a statement) directly
        # so use the object's method
        # dbSendGlueQuery allows removing multiple $uid
        private$trigger(self$config_db_trigR) # send a trigger signal
      }
    }
  }

  restrictionLocal <- self$restrictionTypes_df %>>%
    dplyr::filter(id == restriction)

  newstate <-
    restrictionLocal$callback[[1]](
      state,
      self$UserConfig %>>% dplyr::pull(Attributes) %>>% unlist(),
      # list of attributes in use
      (nchar(paste(apply(
        cbind(private$.UserConfig %>>%
          dplyr::pull(Password)),
        1,
        function(x)
          paste(x[!is.na(x)],
            collapse = ""
          )
      ),
      collapse = ""
      )) > 0)
      # any passwords are set?
    )
  # the code to concatenate strings, NA or not, was found on StackOverflow
  # by 'Joe' https://stackoverflow.com/questions/13673894/suppress-nas-in-paste)

  # returns state. same as the 'changed to' state, if it is permissible
  # e.g. it isn't permissible to set ServerAdmin/UserAdmin to 'TRUE' if
  # there is no user who has UserAdmin attribute
  if (state != newstate$state) {
    # state returned is not the same as the state which was attempted
    # so change the state 'back' to the what is, in fact, the old state
  } else {
    # state returned is the same as the attempted change
    if (state == TRUE) {
      d <- # new restrictions
        dplyr::bind_rows(
          private$.UserRestrictions %>>% dplyr::collect(),
          data.frame(
            uid = max(private$.UserRestrictions %>>%
              dplyr::pull(uid), 0) + 1,
            Restriction = restriction,
            stringsAsFactors = FALSE
          )
        )
      # add entry to datatable of UserRestrictions
      # add one to maximum UID (at least zero), and add restriction to new row
      # note that this table in the database uses 'uid' rather than 'id'
      #  to reduce confusion with the use of 'id' for input names
      update_UserRestrictions_database(d)
      # only if reactive environments available, make public
      private$set_reactive(self$UserRestrictions, private$.UserRestrictions)
    } else {
      # remove entry in datatable of UserRestrictions
      d <- private$.UserRestrictions %>>% # new restriction
        dplyr::filter(Restriction != restriction) %>>% dplyr::collect()
      # only if reactive environments available, make public
      update_UserRestrictions_database(d)
      private$set_reactive(self$UserRestrictions, private$.UserRestrictions)
    }
  }

  return(newstate)
})

.private(dMeasure, "validate.userconfig.description", function(description) {
  # validate Fullname, Location and Attributes
  # returns TRUE if validates
  #  returns error (stop) if fales to validate

  if (!(description$Fullname %in% self$UserFullConfig$Fullname)) {
    # does this user name exist at all?
    stop(paste0(
      "'",
      description$Fullname,
      "' is not a recognized user name."
    ))
  }

  if (length(setdiff(
    description$Location[[1]],
    # note that 'setdiff' is assymetrical
    # need to pick first element of list (which is itself a list)
    c("", (private$PracticeLocations %>>%
      dplyr::collect())$Name)
  )) > 0) {
    # if there is an location being set which is not in self$location_list, or ""
    stop(paste0(
      "'",
      paste(setdiff(
        description$Location[[1]],
        c("", (private$PracticeLocations %>>%
          dplyr::collect())$Name)
      ),
      collapse = ", "
      ),
      "', not recognized location(s)/group(s)."
    ))
  }

  if (length(setdiff(
    description$Attributes[[1]],
    c(self$user_attribute_types, "")
  )) > 0) {
    # if there is an attribute being set which is not in user_attribute_types, or ""
    stop(paste0(
      "'",
      paste(setdiff(
        description$Attributes[[1]],
        c(self$user_attribute_types, "")
      ), collapse = ", "),
      "', not recognized user attribute type(s)."
    ))
  }

  return(TRUE)
})

.private(dMeasure, "validate.proposed.userconfig", function(proposed_UserConfig) {
  # check whether proposed userconfig satisfied some requirements
  # e.g. if a restriction has been placed, then at least one user might need
  #  an attribute to 'override' that restriction
  #
  # @return TRUE if passed
  #  otherwise will return an error

  # if restrictions have been placed on who can modify the server or user configuration
  # then at least one user must have the restricted attribute
  if ("ServerAdmin" %in% self$userrestriction.list()) {
    if (!("ServerAdmin" %in% unlist(proposed_UserConfig %>>%
      dplyr::select(Attributes), use.names = FALSE))) {
      # modified data would no longer have anyone with ServerAdmin attribute
      stop("Only 'ServerAdmin' users can change server settings.
             At least one user must have the 'ServerAdmin' attribute!")
    }
  }

  if ("UserAdmin" %in% self$userrestriction.list()) {
    if (!("UserAdmin" %in% unlist(proposed_UserConfig %>>%
      dplyr::select(Attributes), use.names = FALSE))) {
      # modified data would no longer have anyone with UserAdmin attribute
      stop("Only 'UserAdmin' users can change user permissions.
             At least one user must have the 'UserAdmin' attribute!")
    }
  }

  return(TRUE)
})

#' userconfig.insert
#'
#' add user configuration
#'
#' if setting multiple attributes or locations, preferable to set the attribute
#' as a list e.g.
#'
#' list(Fullname = "Mrs. Diabetes Educator",
#'   Location = list(c("Jemena", "Lakeside")))
#'
#' note that the list does not need to enclose with I(),
#' since 'as_tibble' is used for translation. If attributes or location is
#' not a list, userconfig.insert will turn them into a list
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param description list $Fullname, $AuthIdentity, $Location, $Attributes
#'  $License
#'
#' @return $UserConfig
#'
#' @examples
#' \dontrun{
#' dMeasure_obj <- dMeasure::dMeasure$new()
#' dMeasure_obj$open_emr_db()
#' dMeasure_obj$userconfig.insert(
#'   list(
#'     Fullname = "Mrs. Diabetes Educator",
#'     Location = list(c("Jemena", "Lakeside")),
#'     Attributes = list(c("UserAdmin", "ServerAdmin"))
#'   )
#' )
#' }
#'
#' @export
userconfig.insert <- function(dMeasure_obj, description) {
  dMeasure_obj$userconfig.insert(description)
}

.public(dMeasure, "userconfig.insert", function(description) {
  # adding a new user configuration

  tryCatch(permission <- self$useradmin.permission(),
    warning = function(w)
      stop(paste(
        w,
        "'UserAdmin' permission required to change user configuration"
      ))
  )

  if (!is.null(description$password)) { # actually, this isn't written subsequently...
    description$password <- dMeasure::simple_encode(description$password)
    # immediately encode password if it was provided
  }

  # create empty entries for description, if necessary
  for (x in c("AuthIdentity", "Location", "Attributes")) {
    if (is.null(description[[x]])) {
      description[[x]] <- ""
      # if named field not present, add empty string
    }
  }

  # create NA entries for description, if necessary
  for (x in c("Password", "License")) {
    if (is.null(description[[x]])) { # Password is not written subsequently...
      description[[x]] <- NA
      # if named field not present, set to NA
    }
  }

  if (description$Fullname %in% (private$.UserConfig %>>% dplyr::pull(Fullname))) {
    # if the proposed new name is the same as one that is configured elsewhere
    stop("This user is already configured")
  }

  if (!is.list(description$Location)) {
    # this makes it easier for rbind/tibble later, if $Attributes is a vector
    description$Location <- list(description$Location)
  }

  if (!is.list(description$Attributes)) {
    # this makes it easier for rbind/tibble later, if $Attributes is a vector
    description$Attributes <- list(description$Attributes)
  }

  tryCatch({
    private$validate.userconfig.description(description)
  },
  # find invalid Location or Attribute descriptions
  error = function(e) {
    stop(paste(
      "Error in description validation : ", e[[1]],
      "Unable to insert this user description"
    ))
  }
  )

  newid <- max(private$.UserConfig %>>% dplyr::pull(id), 0) + 1
  # initially, $.UserConfig$id might be an empty set, so need to append a '0'
  description$id <- newid

  query <- paste0(
    "INSERT INTO Users",
    "(id, Fullname, AuthIdentity, Location, Attributes, License)",
    "VALUES ($id, $fn, $au, $lo, $at, $li)"
  )
  data_for_sql <- list(
    id = newid, fn = description$Fullname, au = paste0(description$AuthIdentity, ""),
    # $Location and $Attribute could both have multiple (or no) entries
    lo = paste0(description$Location[[1]], "", collapse = ";"),
    at = paste0(description$Attributes[[1]], "", collapse = ";"),
    li = description$License
  )
  # [[1]] 'de-lists' $Location and $Attributes, which prevents some strange
  # behaviour which *sometimes* results in a string being created in the form
  # of 'c("ServerAdmin", "UserAdmin")'

  self$config_db$dbSendQuery(query, data_for_sql)
  # if the connection is a pool, can't send write query (a statement) directly
  # so use the object's method
  private$trigger(self$config_db_trigR) # send a trigger signal

  invisible(self$UserConfig)
  # update the dataframe in memory
  # as_tibble allows the use of lists in a field without I()
  #
  # for example, to set two locations, with tibble can use
  #  list(Fullname = "Mrs. Diabetes Educator",
  #    Location = list(c("Jemena", "Lakeside")))
  #
  # with data.frame need to enclose the list with I()
  #  list(Fullname = "Mrs. Diabetes Educator",
  #    Location = I(list(c("Jemena", "Lakeside"))))

  # calculating self$UserConfig will 'auto-calculate'
  # self$UserConfig and also $UserConfigR (if reactive/shiny available)
  return(self$UserConfigLicense)
})

#' userconfig.update
#'
#' update user configuration
#'
#' if setting multiple attributes or locations, preferably set the attribute
#' or locations as a list e.g.
#'
#' list(Fullname = "Mrs. Diabetes Educator",
#'   Location = list(c("Jemena", "Lakeside")))
#'
#' note that the list does not need to enclose with I(),
#' since 'as_tibble' is used for translation. If attributes or location is
#' not a list, userconfig.insert will turn them into a list
#'
#' The description to update is defined by $Fullname
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param description list $Fullname, $AuthIdentity, $Location, $Attributes
#'  $License
#'
#' @return self$UserConfig
#'
#' @examples
#' \dontrun{
#' dMeasure_obj <- dMeasure::dMeasure$new()
#' dMeasure_obj$open_emr_db()
#' dMeasure_obj$userconfig.update(
#'   list(
#'     Fullname = "Mrs. Diabetes Educator",
#'     Location = list(c("Jemena", "Lakeside")),
#'     Attributes = list(c("UserAdmin", "ServerAdmin"))
#'   )
#' )
#' }
#'
#' @export
userconfig.update <- function(dMeasure_obj, description) {
  dMeasure_obj$userconfig.update(description)
}

.public(dMeasure, "userconfig.update", function(description) {
  # change (update) a user configuration

  tryCatch(permission <- self$useradmin.permission(),
    warning = function(w)
      stop(paste(
        w,
        "'UserAdmin' permission required to change user configuration"
      ))
  )

  if (!is.null(description$password)) {
    description$password <- dMeasure::simple_encode(description$password)
    # immediately encode password if it was provided
  }

  if (is.null(description$Fullname)) {
    stop("Must include $Fullname in the description.")
  }

  if (!(description$Fullname %in% (private$.UserConfig %>>% dplyr::pull(Fullname)))) {
    # if the proposed configuration to change is not actually configured
    stop("This user is not configured")
  }

  old_description <- self$UserConfig %>>% dplyr::collect() %>>%
    dplyr::filter(Fullname == description$Fullname) %>>%
    tail(1) # in case there is more than one match, remove the last one
  # the 'old' configuration for the selected user
  description$id <- old_description$id # need to copy the id


  # create empty entries for description, if necessary
  for (x in c(
    "AuthIdentity", "Location", "Attributes",
    "License"
  )) {
    if (is.null(description[[x]])) {
      description[[x]] <- old_description[[x]]
    }
  }

  if (!is.list(description$Location)) {
    # this makes it easier for rbind/tibble later, if $Attributes is a vector
    description$Location <- list(description$Location)
  }

  if (!is.list(description$Attributes)) {
    # this makes it easier for rbind/tibble later, if $Attributes is a vector
    description$Attributes <- list(description$Attributes)
  }

  tryCatch({
    private$validate.userconfig.description(description)
  },
  # find invalid Location or Attribute descriptions
  error = function(e) {
    stop(paste(
      "Error in description validation :", e[[1]],
      "- Unable to update this user description"
    ))
  }
  )

  proposed_UserConfig <- self$UserConfig %>>% dplyr::collect() %>>%
    dplyr::select(
      "id", "Fullname", "AuthIdentity", "Location", "Attributes",
      "License"
    ) %>>%
    dplyr::filter(Fullname != description$Fullname) %>>%
    rbind(tibble::as_tibble(description))

  # this is the proposed user configuration

  tryCatch({
    private$validate.proposed.userconfig(proposed_UserConfig)
  },
  error = function(e) {
    stop(paste(
      "Error in change in attributes :", e[[1]],
      "Unable to update this user configuration."
    ))
  }
  )
  # if restrictions have been placed on who can modify the server or user configuration
  # then at least one user must have the restricted attribute

  query <- paste("UPDATE Users SET Fullname = ?, AuthIdentity = ?, ",
    "Location = ?, Attributes = ?, License = ? ",
    "WHERE id = ?",
    sep = ""
  )
  data_for_sql <- as.list(c(
    description$Fullname, paste0(description$AuthIdentity, ""),
    paste0(unlist(description$Location[[1]]), "", collapse = ";"),
    paste0(unlist(description$Attributes[[1]]), "", collapse = ";"),
    description$License,
    description$id
  ))
  # note extra "" within paste0 is required in the case of empty data

  self$config_db$dbSendQuery(query, data_for_sql)
  # if the connection is a pool, can't send write query (a statement) directly
  # so use the object's method
  private$trigger(self$config_db_trigR) # send a trigger signal

  # calculating self$UserConfig will 'auto-calculate'
  # self$UserConfig and also $UserConfigR (if reactive/shiny available)
  return(self$UserConfigLicense)
})

#' userconfig.delete
#'
#' delete user configuration
#'
#' will not delete user configuration if that user is the only holder
#' of an attribute which is restricted, but essential for operation
#' e.g. ServerAdmin
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param description list $Fullname
#'
#' @return self$UserConfig
#' @export
userconfig.delete <- function(dMeasure_obj, description) {
  dMeasure_obj$userconfig.delete(description)
}

.public(dMeasure, "userconfig.delete", function(description) {
  # delete a user configuration

  tryCatch(permission <- self$useradmin.permission(),
    warning = function(w)
      stop(paste(
        w,
        "'UserAdmin' permission required to change/delete user configuration."
      ))
  )

  if (is.null(description$Fullname)) {
    stop("Must include $Fullname in the description.")
  }

  UserConfigRow <- self$UserConfig %>>% dplyr::collect() %>>%
    dplyr::filter(Fullname == description$Fullname) %>>%
    tail(1) # in case there is more than one match, remove the last one

  if (nrow(UserConfigRow) == 0) {
    stop(paste0("'", description$Fullname, "' not a configured user."))
  }

  proposed_UserConfig <- self$UserConfig %>>% dplyr::collect() %>>%
    dplyr::filter(Fullname != description$Fullname)

  # this is the proposed user configuration

  tryCatch({
    private$validate.proposed.userconfig(proposed_UserConfig)
  },
  error = function(e) {
    paste("Error in change in attributes :", e[[1]])
    stop("Cannot delete this user configuration.")
  }
  )
  # is restrictions have been placed on who can modify the server or user configuration
  # then at least one user must have the restricted attribute

  query <- "DELETE FROM Users WHERE id = ?"
  data_for_sql <- as.list.data.frame(UserConfigRow$id)

  self$config_db$dbSendQuery(query, data_for_sql)
  # if the connection is a pool, can't send write query (a statement) directly
  # so use the object's method
  private$trigger(self$config_db_trigR) # send a trigger signal

  # calculating self$UserConfig will 'auto-calculate'
  # self$UserConfig and also $UserConfigR (if reactive/shiny available)
  return(self$UserConfigLicense)
})

#' userconfig.list
#'
#' list user configuration
#'
#' @param dMeasure_obj dMeasure R6 object
#'
#' @return $UserConfig
#' @export
userconfig.list <- function(dMeasure_obj) {
  dMeasure_obj$userconfig.list()
}

.public(dMeasure, "userconfig.list", function() {
  tryCatch(permission <- self$useradmin.permission(),
    warning = function(w)
      stop(paste(
        w,
        "'UserAdmin' permission required",
        "to view user configurations."
      ))
  )

  return(private$.UserConfig %>>% dplyr::collect())
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

.public(dMeasure, "userrestriction.list", function() {
  private$.UserRestrictions %>>% dplyr::pull(Restriction)
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
#' @export
useradmin.permission <- function(dMeasure_obj) {
  dMeasure_obj$useradmin.permission()
}

.public(dMeasure, "useradmin.permission", function() {
  if ("UserAdmin" %in% self$userrestriction.list()) {
    # only some users allowed to see/change server settings
    if ("UserAdmin" %in% (self$UserConfig %>>%
      dplyr::filter(Fullname ==
        paste(self$.identified_user$Fullname,
          collapse = ""
        )) %>>%
      # paste with collapse = "" changes character(0) to ""
      dplyr::pull(Attributes) %>>% unlist()) &&
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
