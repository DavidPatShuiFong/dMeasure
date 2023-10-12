# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

##### Configuration - server ###########################################
#' Configuration - server
#'
#' @name configuration_server
#' @include dMeasure.R
#' needs access to dMeasure and methods and fields
#' @include calculation_definitions.R
#' needs access to functions
NULL

#' server.insert
#'
#' insert a new server description
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param description list object : $Name, $Driver, $Address, $Database, $UserID, $dbPassword, $dbPasswordExtraEncryption
#'
#'  'Driver' will be the MSSQL driver to use. If left blank or not defined,
#'   then 'SQL Server' will be used.
#'   Other possible choices include "ODBC Driver 11 for SQL Server"
#'   or "ODBC Driver 13 for SQL Server".
#'   A list of available drivers can be seen (in R) with odbc::odbcListDrivers()
#'
#'  'Address' will typically be of the form COMPUTERNAME and BPSINSTANCE, with two
#'  backslashes in between.
#'
#'  'Address' will require 'two' backslashes in the address, because the backslash
#'   needs to be 'escaped' with a preceding backslash (see examples).
#'
#'  'Database' will typically be BPSPATIENTS. the samples database will be BPSSAMPLES
#'
#'  'UserID' will always be 'bpsrawdata'
#'
#'  'dbPassword' will be immediately encrypted, using the default encryption and, if
#'  provided, an extra user-selected encryption key 'dbPasswordExtraEncryption'.
#'
#'  'dbPasswordExtraEncryption' will be stored as the hashed (with sodium::password_store)
#'  version of the user-selected encryption key for 'dbPassword'.
#'
#'  Setting 'dbPasswordEncryption' to an empty string "" or NULL will result in
#'  no optional user-selected encryption key being used.
#'
#'  As 'dbPasswordExtraEncryption' is hashed, it cannot be used to decrypt 'dbPassword',
#'  but can be used to check key validity
#'
#' @return dataframe - full list of database descriptions
#'  can also return error (stop) if description is invalid
#' @examples
#' \dontrun{
#' a <- dMeasure::dMeasure$new()
#' a$open_configuration_db()
#' a$read_configuration_db()
#' a$server.insert(list(
#'   Name = "MyServer", Address = "127.0.0.1\\BPSINSTANCE",
#'   Database = "BPSSAMPLES", UserID = "bpsrawdata",
#'   dbPassword = "mypassword", dbPasswordExtraEncryption = ""
#' ))
#' }
#' @export
server.insert <- function(dMeasure_obj, description) {
  dMeasure_obj$server.insert(description)
}

.public(dMeasure, "server.insert", function(description) {
  tryCatch(permission <- self$server.permission(),
    warning = function(w)
      stop(paste(
        w,
        "'ServerAdmin' permission required to modify server list."
      ))
  )

  if (toupper(description$Name) %in% toupper(append(private$.BPdatabase$Name, "None"))) {
    # if the proposed server is the same as one that already exists
    # (ignoring case)
    stop("New server name cannot be the same as existing names, or 'None'")
  } else if (is.null(description$Name) |
    is.null(description$Address) |
    is.null(description$Database) |
    is.null(description$UserID) |
    is.null(description$dbPassword)) {
    stop(paste(
      "Entries ($id, $Name, $Address, $Database, $UserID, $dbPassword)",
      "must be described"
    ))
  } else if (stringi::stri_length(description$Name) == 0 |
    stringi::stri_length(description$Address) == 0 |
    stringi::stri_length(description$Database) == 0 |
    stringi::stri_length(description$UserID) == 0 |
    stringi::stri_length(description$dbPassword) == 0) {
    stop(paste(
      "Entries ($id, $Name, $Address, $Database, $UserID, $dbPassword)",
      "must be described.",
      "Only $dbPasswordExtraEncryption can be an empty, zero-length string,",
      "(in which case it won't be used)."
    ))
  } else {
    if (is.null(description$Driver)) {
      description$Driver <- "" # this is the only field which is assigned a default!
      # 'empty' will result in 'SQL Server' being used
    }
    if (!(description$Driver %in% c(odbc::odbcListDrivers()$name, ""))) {
      description$Driver <- ""
      warning(paste(
        "Invalid driver choice.",
        "Must be one of : ",
        paste(unique(odbc::odbcListDrivers()$name),
          collapse = ", "
        ),
        ". Will be set to empty ''."
      ))
    }

    newid <- max(c(as.data.frame(private$.BPdatabase)$id, 0)) + 1
    # initially, private$.BPdatabase$id might be an empty set, so need to append a '0'
    description$id <- newid
    description$dbPassword <- dMeasure::simple_encode(description$dbPassword)
    # immediately encode password.
    # stored encrypted both in memory and in configuration file
    if (is.null(description$dbPasswordExtraEncryption)) {
      # We need to clarify the 'lack of definition' of $dbPasswordExtraEncryption before
      # we deal with $dbPassword
      description$dbPasswordExtraEncryption <- ""
    }
    if (description$dbPasswordExtraEncryption != "") {
      # if dbPasswordExtraEncryption is 'defined' then
      # firstly encode the database password a second time
      description$dbPassword <- dMeasure::simple_encode(
        description$dbPassword,
        key = description$dbPasswordExtraEncryption
      )
      # immediately 'hash' the password extra encryption, if defined
      description$dbPasswordExtraEncryption <-
        sodium::password_store(description$dbPasswordExtraEncryption)
    }

    query <- paste(
      "INSERT INTO Server",
      "(id, Name, Driver, Address, Database, UserID, dbPassword, dbPasswordExtraEncryption)",
      "VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
    )
    data_for_sql <- as.list.data.frame(c(
      newid, description$Name, description$Driver,
      description$Address,
      description$Database, description$UserID,
      description$dbPassword, description$dbPasswordExtraEncryption
    ))
    self$config_db$dbSendQuery(query, data_for_sql)
    # if the connection is a pool, can't send write query (a statement) directly
    # so use the object's method

    private$trigger(self$config_db_trigR)

    private$.BPdatabase <- rbind(private$.BPdatabase, description,
      stringsAsFactors = FALSE
    )
    # update the dataframe in memory
    return(private$.BPdatabase %>>%
      dplyr::select(-dbPassword))
  }
})

#' server.update
#'
#' change (update) a server description
#' the server to change is specified by $id
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param description list $id, $Name, $Driver, $Address, $Database, $UserID, $dbPassword, $dbPasswordExtraEncryption
#'
#'  any of $Name, $Driver, $Address, $Database, $UserID, $dbPassword, $dbPasswordExtraEncryption
#'  can be defined
#'
#' @return dataframe - full list of database descriptions
#'  can also return error (stop) if description is invalid
#' @export
server.update <- function(dMeasure_obj, description) {
  dMeasure_obj$server.update(description)
}

.public(dMeasure, "server.update", function(description) {
  tryCatch(permission <- self$server.permission(),
    warning = function(w)
      stop(paste(
        w,
        "'ServerAdmin' permission required to modify server list."
      ))
  )

  if (is.null(description$id)) {
    stop("Server to change is to be identified by $id")
  }
  if (!description$id %in% (private$.BPdatabase %>>% dplyr::pull(id))) {
    stop(paste("No server definition with id = ", description$id), "!", sep = "")
  }
  if (self$BPdatabaseChoice != "None") { # only if there is a chosen database
    if (private$.BPdatabase %>>% dplyr::filter(Name == self$BPdatabaseChoice) %>>%
      dplyr::pull(id) == description$id) {
      stop(paste(
        "Cannot update server definition id = ", description$id,
        ", currently in use!"
      ))
    }
  }

  # if definition is not provided, then 'fill it in' from the current definition
  if (is.null(description$Name)) {
    description$Name <- private$.BPdatabase %>>%
      dplyr::filter(id == description$id) %>>%
      dplyr::pull(Name)
  } else {
    if (toupper(description$Name) %in%
      toupper(append(
        private$.BPdatabase[!(private$.BPdatabase$id == description$id), ]$Name,
        "None"
      ))) {
      # if the proposed server is the same as one that already exists
      # (ignoring case, and removing the 'id' which is specified in the description)
      stop("New server name cannot be the same as existing names, or 'None'")
    }
  }
  if (is.null(description$Driver)) {
    description$Driver <- private$.BPdatabase %>>%
      dplyr::filter(id == description$id) %>>%
      dplyr::pull(Driver)
  }
  if (!(description$Driver %in% c("", odbc::odbcListDrivers()$name))) {
    description$Driver <- private$.BPdatabase %>>%
      dplyr::filter(id == description$id) %>>%
      dplyr::pull(Driver)
    warning(paste(
      "Invalid driver choice.",
      "Must be one of : ",
      paste(unique(odbc::odbcListDrivers()$name),
        collapse = ", "
      )
    ))
  }
  if (is.null(description$Address)) {
    description$Address <- private$.BPdatabase %>>%
      dplyr::filter(id == description$id) %>>%
      dplyr::pull(Address)
  }
  if (is.null(description$Database)) {
    description$Database <- private$.BPdatabase %>>%
      dplyr::filter(id == description$id) %>>%
      dplyr::pull(Database)
  }
  if (is.null(description$UserID)) {
    description$UserID <- private$.BPdatabase %>>%
      dplyr::filter(id == description$id) %>>%
      dplyr::pull(UserID)
  }
  if (is.null(description$dbPasswordExtraEncryption)) {
    # We need to clarify the 'lack of definition' of $dbPasswordExtraEncryption before
    # we deal with $dbPassword
    description$dbPasswordExtraEncryption <- ""
    # note that the state of $dbPasswordExtraEncryption is *not* read from
    # the current database configuration. If extra encryption of the database password is required
    # then $dbPasswordExtraEncryption needs to be explicitly set each time the database password
    # is changed.
  } else {
    if (is.null(description[["dbPassword"]])) {
      stop(
        "If `dbpasswordExtraEncryption` is defined as something other than NULL or \"\",",
        "then `dbPassword` must also be defined."
      )
    }
  }
  if (is.null(description[["dbPassword"]])) {
    # cannot use description$dbPassword because $ allows inexact matching
    # i.e. will match 'dbPasswordExtraEncryption'!!!
    description$dbPassword <- private$.BPdatabase %>>%
      dplyr::filter(id == description$id) %>>%
      dplyr::pull(dbPassword)
  } else {
    description$dbPassword <- dMeasure::simple_encode(description$dbPassword)
    # immediately encode password.
    # stored encrypted both in memory and in configuration file
    if (description$dbPasswordExtraEncryption != "") {
      # if dbPasswordExtraEncryption is 'defined' then
      # firstly encode the database password a second time
      description$dbPassword <- dMeasure::simple_encode(
        description$dbPassword,
        key = description$dbPasswordExtraEncryption
      )
      # immediately 'hash' the password extra encryption, if defined
      description$dbPasswordExtraEncryption <-
        sodium::password_store(description$dbPasswordExtraEncryption)
    }
  }

  query <- paste(
    "UPDATE Server SET Name = ?, Driver = ?, Address = ?, Database = ?,",
    "UserID = ?, dbPassword = ?, dbPasswordExtraEncryption = ? WHERE id = ?"
  )
  data_for_sql <- as.list.data.frame(c(
    description$Name, description$Driver,
    description$Address,
    description$Database, description$UserID,
    description$dbPassword, description$dbPasswordExtraEncryption,
    description$id
  ))

  self$config_db$dbSendQuery(query, data_for_sql)
  # if the connection is a pool, can't send write query (a statement) directly
  # so use the object's method

  private$trigger(self$config_db_trigR)

  private$.BPdatabase <- rbind(private$.BPdatabase %>>% dplyr::filter(id != description$id),
    description,
    stringsAsFactors = FALSE
  )
  # store new values in copy of settings in memory
  return(private$.BPdatabase %>>%
    dplyr::select(-dbPassword))
})

#' server.delete
#'
#' remove a server description
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param description list, with '$id'
#'  the server description to be deleted identified by '$id'
#'
#' @return dataframe - full list of database descriptions
#'  can also return error (stop) if description is invalid
#' @export
server.delete <- function(dMeasure_obj, description) {
  dMeasure_obj$server.delete(description)
}

.public(dMeasure, "server.delete", function(description) {
  # delete a server description

  tryCatch(permission <- self$server.permission(),
    warning = function(w)
      stop(paste(
        w,
        "'ServerAdmin' permission required to modify server list."
      ))
  )

  name <- private$.BPdatabase[private$.BPdatabase$id == description$id, ]$Name
  if (length(name) == 0) { # id not found
    stop(paste0("Cannot remove id = '", description$id, "', not defined!"))
  }
  if (toupper(name) == toupper(self$BPdatabaseChoice)) {
    stop(paste0("Cannot remove '", name, "', currently in use!"))
  }

  # remove from config SQLite database
  query <- "DELETE FROM Server WHERE id = ?"
  data_for_sql <- as.list.data.frame(c(description$id))

  self$config_db$dbSendQuery(query, data_for_sql)
  # if the connection is a pool, can't send write query (a statement) directly
  # so use the object's method
  private$trigger(self$config_db_trigR) # send a trigger signal

  private$.BPdatabase <- private$.BPdatabase %>>% dplyr::filter(id != description$id)

  return(private$.BPdatabase %>>%
    dplyr::select(-dbPassword))
})

#' server.list
#'
#' List server description. does not include passwords!
#' If 'ServerAdmin' restriction is set, only 'ServerAdmin'
#' users can see the server list with this method.
#'
#' @param dMeasure_obj dMeasure R6 object
#'
#' @return dataframe - full list of database descriptions
#'  can also return error (stop) if description is invalid
#' @export
server.list <- function(dMeasure_obj) {
  dMeasure_obj$server.list()
}

.public(dMeasure, "server.list", function() {
  # list server descriptions

  tryCatch(permission <- self$server.permission(),
    warning = function(w)
      warning(paste(
        w,
        "'ServerAdmin' permission required to view server list."
      ))
  )

  if (permission) {
    description <- private$.BPdatabase %>>%
      dplyr::select(-dbPassword, -dbPasswordExtraEncryption)
  }
  else {
    description <- c("")
  }
  return(description)
})

#' server.permission
#'
#' Does the current user have server access permission?
#'
#' Can only be 'false' if $UserRestrictions$Restriction contains
#' 'ServerAdmin'
#'
#' if 'ServerAdmin' in the $UserRestrictions, then a user needs
#' to be identified and 'authenticated'. Authentication requires
#' identification (via Sys.info()$user), and might also require
#' a password
#'
#' @param dMeasure_obj dMeasure R6 object
#'
#' @return TRUE or FALSE
#'  additionally returns a warning if permission is FALSE
#' @export
server.permission <- function(dMeasure_obj) {
  dMeasure_obj$server.permission()
}

.public(dMeasure, "server.permission", function() {
  if ("ServerAdmin" %in% self$userrestriction.list()) {
    # only some users allowed to see/change server settings
    if ("ServerAdmin" %in% (self$UserConfig %>>%
      dplyr::filter(Fullname ==
        paste(self$.identified_user$Fullname,
          collapse = ""
        )) %>>%
      # paste with collapse = "" changes character(0) to ""
      dplyr::pull(Attributes) %>>% unlist()) &&
      self$authenticated == TRUE) {
      permission <- TRUE
    } else {
      # this user is not authorized to access the server list
      permission <- FALSE
      warning("No 'ServerAdmin' attribute for this user.")
    }
  } else {
    # no 'ServerAdmin' attribute required
    permission <- TRUE
  }
  return(permission)
})

.active(dMeasure, "Log", function(setting) {
  # state of logging, or sets logging state
  #
  # @param setting
  #   set to TRUE if to start logging
  #   set to FALSE if to stop logging
  # if no @param setting, then returns whether
  #  currently logging or not

  tryCatch(permission <- self$server.permission(),
    warning = function(w) {
      warning(paste(
        w,
        "'ServerAdmin' permission required",
        "to read/change logging status."
      ))
      return(NULL)
    }
  )

  if (!self$config_db$is_open()) {
    warning("Unable to read or set logging status. Configuration database is not open")
    return(NULL)
  }

  current_setting <- self$config_db$conn() %>>%
    # read directly from configuration database
    dplyr::tbl("LogSettings") %>>%
    dplyr::pull(Log) %>>% as.logical()

  if (missing(setting)) {
    setting <- current_setting
  }

  if (setting == TRUE) {
    # try to start logging
    if (self$LogFile == "") {
      # empty string is intended to be 'no choice', but RSQLite will
      # open a 'temporary' database anyway if given an empty string!
      warning("No logging database file has been selected.")
      setting <- FALSE
    } else {
      if (!self$config_db$keep_log) {
        # configuration database is open (from earlier check),
        # but not currently logging
        self$config_db$open_log_db(
          filename = self$LogFile,
          tag = Sys.info()[["user"]]
        )
        # tries to open the logging database
        if (is.null(self$config_db$log_db)) {
          # log database not successfully opened
          warning("Failed to open logging database file.")
          setting <- FALSE
        }
      }
      if (self$emr_db$is_open() & !self$emr_db$keep_log) {
        # EMR database is open, but not currently logging
        self$emr_db$open_log_db(
          filename = self$LogFile,
          tag = Sys.info()[["user"]]
        )
        # tries to open the logging database
        if (is.null(self$emr_db$log_db)) {
          # log database not successfully opened
          warning("Failed to open logging database file.")
          setting <- FALSE
        }
      }
    }
  } else {
    # stop logging
    if (self$config_db$keep_log) {
      # configuration database is open (from earlier check),
      # and currently logging
      self$config_db$close_log_db()
    }
    if (self$emr_db$is_open() & self$emr_db$keep_log) {
      # EMR database is open, and currently logging
      self$emr_db$close_log_db()
    }
  }

  if (current_setting != setting) {
    # change in setting.record to configuration database
    query <- "UPDATE LogSettings SET Log = ? WHERE id = 1"
    data_for_sql <- as.list.data.frame(c(as.numeric(setting)))
    self$config_db$dbSendQuery(query, data_for_sql) # populate with "None" choice
  }

  private$set_reactive(self$LogR, setting)
  return(setting)
})
.reactive(dMeasure, "LogR", FALSE)

.active(dMeasure, "LogFile", function(filename) {
  # logging filename, or sets logging filename
  #
  # @param filename
  #
  # if no @param filename, then returns current log filename

  tryCatch(permission <- self$server.permission(),
    warning = function(w) {
      warning(paste(
        w,
        "'ServerAdmin' permission required",
        "to read/change logging parameters."
      ))
      return(NULL)
    }
  )

  if (!self$config_db$is_open()) {
    warning("Unable to read or set logging filename. Configuration database is not open")
    return(NULL)
  }

  current_filename <- self$config_db$conn() %>>%
    # read directly from configuration database
    dplyr::tbl("LogSettings") %>>%
    dplyr::pull(Filename) %>>% as.character() %>>% paste0("")

  if (missing(filename)) {
    filename <- current_filename
  } # no filename provided, so return current value

  if (filename != current_filename) {
    if (self$Log) {
      warning("Unable to change Logfile when logging is turned on. Turn logging off first.")
      filename <- current_filename
      # 'reset' logging filename to current filename
    } else {
      query <- "UPDATE LogSettings SET Filename = ? WHERE id = 1"
      data_for_sql <- as.list.data.frame(c(as.character(filename)))
      self$config_db$dbSendQuery(query, data_for_sql) # populate with "None" choice
    }
  }

  private$set_reactive(self$LogFileR, filename)
  return(filename)
})
.reactive(dMeasure, "LogFileR", "")

#' WriteLog
#'
#' writes to Logfile (if available, and logging is turned on)
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param message
#'
#' @return nothing
#' @export
WriteLog <- function(dMeasure_obj, message) {
  dMeasure_obj$WriteLog(message)
}

.public(dMeasure, "WriteLog", function(message) {
  # write message to logfile database

  if (!self$config_db$is_open()) {
    warning("Unable to write log message, configuration database is not open.")
  } else if (!self$config_db$keep_log) {
    warning("Unable to write log message, logging database is not open.")
  } else {
    self$config_db$write_log_db(message)
  }
})

# ReadLog
#
# reads from logfile (if available)
.active(dMeasure, "ReadLog", function(value) {
  # logging filename, or sets logging filename
  #
  # @param filename
  #
  # if no @param filename, then returns current log filename

  if (!missing(value)) {
    stop("cannot be set, $ReadLog is read-only")
  }

  tryCatch(permission <- self$server.permission(),
    warning = function(w) {
      warning(paste(
        w,
        "'ServerAdmin' permission required",
        "to read logs."
      ))
      return(NULL)
    }
  )

  if (!self$config_db$is_open()) {
    warning("Unable to read logs. Configuration database is not open")
    return(NULL)
  }

  if (is.null(self$config_db$log_db)) {
    warning("Unable to read logs. Log database is not open")
    return(NULL)
  }

  if (!self$config_db$log_db$is_open()) {
    warning("Unable to read logs. Log database is not open")
    return(NULL)
  }

  return(self$config_db$log_db$conn() %>>% dplyr::tbl("logs") %>>%
    dplyr::collect())
})
