##### dMeasure ###########################################

#' @include r6_helpers.R
#' functions to help create R6 classes
NULL

#' dMeasure class
#' @title dMeasure class
#' @description case-finding in EMR (Best Practice)
#' @field yaml_config_filepath - filepath of YAML configuration
#' @field sql_config_filepath - filepath of SQL configuration (NULL is not connected)
#' @field local_config - in-memory copy of YAML configuration
#'
#' @section Methods:
#' \itemize{
#' \item{\code{\link{configuration_file_path}} : read (or initiate) YAML/SQL DB filepaths}
#' \item{\code{\link{open_configuration_db}} : open SQLite configuration database}
#' \item{\code{\link{read_configuration_db}} : read SQLite configuration database}
#' \item{\code{\link{open_emr_db}} : open Best Practice database}
#' \item{\code{\link{initialize_emr_tables}} : configure Best Practice datatables}
#' \item{\code{\link{location_list}} : list practice locations/groups}
#' \item{\code{\link{choose_location}} : change, or read, current location}
#' }
#'
#' @examples
#'
#' @export
dMeasure <-
  R6::R6Class("dMeasure",
    public = list(
      initialize = function() {
        if (length(public_init_fields$name) > 0) { # only if any defined
          for (i in 1:length(public_init_fields$name)) {
            if (public_init_fields$obj[[i]] == "dMeasure") {
              self[[public_init_fields$name[[i]]]] <-
                eval(public_init_fields$value[[i]]) # could 'quote' the value
            }
          }
        }
        if (length(private_init_fields$name) > 0) { # only if any defined
          for (i in 1:length(private_init_fields$name)) {
            if (private_init_fields$obj[[i]] == "dMeasure") {
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
              if (reactive_fields$obj[[i]] == "dMeasure") {
                self[[reactive_fields$name[[i]]]] <- shiny::reactiveVal(
                  eval(reactive_fields$value[[i]]) # could 'quote' the value
                )
              }
            }
          }
          if (length(reactive_event$name) > 0) { # only if any .reactive() defined
            for (i in 1:length(reactive_event$name)) {
              if (reactive_event$obj[[i]] == "dMeasure") {
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


.private(dMeasure, "set_reactive", function(myreactive, value) {
  # reactive (if shiny/reactive environment is available) is set to 'value'
  # myreactive is passed by reference
  # print(myreactive)
  # print(deparse(sys.call(-1)))
  if (requireNamespace("shiny", quietly = TRUE) && shiny::is.reactive(myreactive)) {
    shiny::isolate(eval(substitute(myreactive, env = parent.frame()))(value))
  }
})
.private(dMeasure, "trigger", function(myreactive) {
  # toggles a reactive between (usually) 0 and 1
  if (requireNamespace("shiny", quietly = TRUE)) {
    myreactive(1 - shiny::isolate(myreactive()))
  }
})

##### close and finalize object ##########################

.public(dMeasure, "close", function() {
  # close any open database connections
  if (!is.null(self$.identified_user)) {
    self$user_logout()
  }
  if (self$config_db$is_open()) {
    if (self$config_db$keep_log) { # if currently logging
      log_id <- self$config_db$write_log_db(
        query = "Closing databases"
      )
      self$config_db$close_log_db() # close logging database
    }
    self$config_db$close()

    # empty the configuration fields
    private$.BPdatabase <- BPdatabase_empty
    private$.BPdatabaseChoice <- "None"

    private$PracticeLocations <- data.frame(
      id = numeric(),
      Name = character(),
      Description = character()
    )
    invisible(self$location_list)
    # $location_list() will refresh the reactive location_listR if available

    private$.UserConfig <- UserConfig_empty
    invisible(self$UserConfig) # this will also set $userConfigR reactive version

    private$.UserRestrictions <- data.frame(
      uid = integer(),
      Restriction = character(),
      stringsAsFactors = FALSE
    )
    private$set_reactive(self$UserRestrictions, private$.UserRestrictions)

    invisible(self$.identified_user)
    # see if 'identified' system user is matched with a configured user
  }
  if (self$emr_db$is_open()) {
    if (self$emr_db$keep_log) { # if currently logging
      self$emr_db$close_log_db() # close logging database
    }
    self$emr_db$close()
    self$db$practice <- NULL
    self$db$users <- NULL
    self$db$patients <- NULL
    self$db$patientsRaw <- NULL
    self$db$clinical <- NULL
    self$db$reactions <- NULL
    self$db$investigations <- NULL
    self$db$papsmears <- NULL
    self$db$appointments <- NULL
    self$db$immunizations <- NULL
    self$db$preventive_health <- NULL
    self$db$correspondenceIn <- NULL
    self$db$reportValues <- NULL
    self$db$services <- NULL
    self$db$servicesRaw <- NULL
    self$db$history <- NULL
    self$db$currentRx <- NULL
    self$db$currentRx_raw <- NULL
    self$db$familyhistory <- NULL
    self$db$familyhistorydetail <- NULL
    self$db$relationcode <- NULL
    self$clinician_choice_list <- NULL
  }
  self$authenticated <- FALSE

  invisible(self)
})

.public(dMeasure, "finalize", function() {
  # object being destroyed/removed
  # close all open connections
  self$close()
})

##### Configuration file location ########################
## fields

.public(dMeasure, "yaml_config_filepath", character())
.public(dMeasure, "sql_config_filepath", character())
.private(dMeasure, "local_config", character())

## active fields

#' read (or set) configuration filepath
#'
#' By default, the YAML configuration is either in the working
#' directory (where a local installation of R lives),
#' or the user's home directory
#'
#' '~/.DailyMeasure_cfg.yaml'
#'
#' this method will read or set $sql_config_filepath
#' it will read the YAML configuration filepath, which if already
#' existing might contain the 'real' location of the $sql_config_filepath
#'
#' returns the SQL filepath
#'
#' @name configuration_file_path
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param value (opt) filepath to set
#'
#' @return SQL filepath (only returned if no 'value' provided)
#'
#' @examples
#' dMeasure_obj <- dMeasure$new()
#' dMeasure_obj$configuration_file_path # read filepath
#' dMeasure_obj$configuration_file_path <- "c:/config.sqlite"
#' # sets filepath
#' @export
configuration_file_path <- function(dMeasure_obj, value) {
  if (missing(value)) {
    return(dMeasure_obj$configuration_file_path)
  } else {
    dMeasure_obj$configuration_file_path <- value
  }
}
.active(dMeasure, "configuration_file_path", function(filepath) {
  if (missing(filepath)) {
    # reads configuration file (if it exists)
    private$local_config <- self$configuration_file_yaml
    self$sql_config_filepath <- private$local_config$config_file
  } else {
    self$close() # close any open database connections
    self$sql_config_filepath <- filepath # set the new config filepath

    self$configuration_file_yaml <- filepath # write to YAML file
  }

  private$set_reactive(self$configuration_file_pathR, self$sql_config_filepath)
  return(self$sql_config_filepath)
})
.reactive(dMeasure, "configuration_file_pathR", NULL)

#' read (or set) configuration filepath in YAML
#'
#' By default, the YAML configuration is either in the working
#' directory (where a local installation of R lives),
#' or the user's home directory
#'
#' '~/.DailyMeasure_cfg.yaml'
#'
#' this method will read or write the .sqlite filepath
#' to the YAML configuration file. If already
#' existing might contain the 'real' location of the $sql_config_filepath
#'
#' Does not change the configuration file used by
#' the current dM object.
#'
#' returns the SQL filepath
#'
#' @name configuration_file_yaml
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param value (opt) filepath to set
#'
#' @return local_config
#'  $config_file : SQL filepath (only returned if no 'value' provided)
#'
#' @export
configuration_file_yaml <- function(dMeasure_obj, value) {
  if (missing(value)) {
    return(dMeasure_obj$configuration_file_yaml)
  } else {
    dMeasure_obj$configuration_file_yaml <- value
  }
}
.active(dMeasure, "configuration_file_yaml", function(filepath) {
  if (Sys.getenv("R_CONFIG_ACTIVE") == "shinyapps") {
    # shinyapps.io environment
    yaml_config_filepath <- ".DailyMeasure_cfg.yaml"
    sql_config_filepath <- ".DailyMeasure_cfg.sqlite"
  } else {
    yaml_config_filepath <- "~/.DailyMeasure_cfg.yaml"
    sql_config_filepath <- "~/.DailyMeasure_cfg.sqlite"
  }

  self$yaml_config_filepath <- yaml_config_filepath
  # the location the of the '.yaml' configuration file
  # always in the user's home directory
  if (!missing(filepath)) {
    # the 'new' configuration filepath is being set
    sql_config_filepath <- filepath # set the new config filepath

    local_config <- list()
    local_config$config_file <- sql_config_filepath
    # main configuration file, could (potentially) be set to 'common location'
  } else {
    # no configuration filepath has been provided
    # read the old configuration filepath, or create one
    if (configr::is.yaml.file(self$yaml_config_filepath)) {
      # if config file exists and is a YAML-type file
      local_config <- configr::read.config(self$yaml_config_filepath)
      sql_config_filepath <- local_config$config_file
      # config in local location
    } else {
      # local config file does not exist. possibly first-run
      if (grepl("Program Files", normalizePath(R.home()))) {
        # this is a system-wide install
        self$sql_config_filepath <- sql_config_filepath
        # store in user's home directory
      } else {
        # this is a 'local' user install, not a system-wide install
        # e.g. C:/Users/MyName/AppData/Programs/...
        # as opposed to 'C:/Program Files/...'
        self$sql_config_filepath <- sql_config_filepath
        # this file can be stored in the AppData folder, out of sight of the user
      }
      local_config <- list()
      local_config$config_file <- self$sql_config_filepath
      # main configuration file, could (potentially) be set to 'common location'
    }
  }

  # write the (minimalist) local config file
  if (!configr::is.yaml.file(self$yaml_config_filepath) | !missing(filepath)) {
    # either there is no .YAML configuration file,
    # or the .sqlite filepath has been changed
    configr::write.config(
      local_config,
      file.path = self$yaml_config_filepath,
      write.type = "yaml"
    )
  }

  return(local_config)
})

##### Configuration details - databases, locations, users ###########

## Fields
.public_init(dMeasure, "config_db", quote(dbConnection::dbConnection$new()))
# R6 connection to database
# using either DBI or pool
.reactive(dMeasure, "config_db_trigR", 0)
# $config_db_trigR will trigger (0/1) with each configuration
# database change

.public_init(dMeasure, "subscription_db", quote(dbConnection::dbConnection$new()))
# R6 connection to subscription database
# using either DBI or pool
# two subscription databases available
#  1. vkelim.3322.org and 2. vkelim.dsmynas.com
#  port 3306, username = "guest", password - not required, dbname = "DailyMeasureUsers"

BPdatabase_empty <- data.frame(
  id = integer(),
  Name = character(),
  Address = character(),
  Database = character(),
  UserID = character(),
  dbPassword = character(),
  stringsAsFactors = FALSE
)
.private(dMeasure, ".BPdatabase", BPdatabase_empty)
#' show database configurations
#'
#' @name BPdatabase
#'
#' @param dMeasure_obj dMeasure R6 object
#'
#' reactive version : BPdatabaseR
#'
#' @return dataframe of database descriptions
#'  id, Name, Address, Database, UserID, dbPassword
#'
#'  Address will look something like "COMPUTERNAME\\BPSINSTANCE"
#'   note that '\' needed to be quoted, so becomes '\\'
#'  Database should be 'BPSPATIENTS' (or perhaps 'BPSSAMPLES')
#'  userID should always be 'bpsrawdata'
#'
#' @examples
#' dMeasure_obj <- dMeasure$new()
#' dMeasure_obj$open_configuration_db()
#' dMeasure_obj$read_configuration_db()
#' dMeasure_obj$BPdatabase
#' @export
BPdatabase <- function(dMeasure_obj) {
  return(dMeasure_obj$BPdatabase)
}
.active(dMeasure, "BPdatabase", function(value) {
  if (!missing(value)) {
    stop("cannot be set, $BPdatabase is read-only")
  } else {
    if (self$server.permission()) {
      private$set_reactive(self$BPdatabaseR, private$.BPdatabase)
      return(private$.BPdatabase)
      # this identified user has permission
      # to read the database configuration
    } else {
      return(NULL)
    }
  }
})
.reactive(dMeasure, "BPdatabaseR", quote(data.frame(
  id = integer(),
  Name = character(),
  Address = character(),
  Database = character(),
  Driver = character(),
  UserID = character(),
  dbPassword = character(),
  stringsAsFactors = FALSE
)))
#' show database configuration names
#'
#' @name BPdatabase
#'
#' @param dMeasure_obj dMeasure R6 object
#'
#' @return vector of names of database configurations
#'
#' @examples
#' dMeasure_obj <- dMeasure$new()
#' dMeasure_obj$open_configuration_db()
#' dMeasure_obj$read_configuration_db()
#' dMeasure_obj$BPdatabaseNames
#' @export
BPdatabaseNames <- function(dMeasure_obj) {
  return(dMeasure_obj$BPdatabaseNames)
}
.active(dMeasure, "BPdatabaseNames", function(value) {
  if (!missing(value)) {
    stop("cannot set, $BPdatabaseNames is read-only!")
  } else {
    private$.BPdatabase %>>% dplyr::pull(Name)
  }
})
.private(dMeasure, ".BPdatabaseChoice", "None")
# database choice will be the same as the 'Name' of
# the chosen entry in BPdatabase
.private(dMeasure, "PracticeLocations", data.frame(
  id = integer(),
  Name = character(),
  Description = character(),
  stringsAsFactors = FALSE
))
# id needed for editing this dataframe later
# need default value for practice location filter
# interface initialization
UserConfig_empty <- data.frame(
  id = integer(),
  Fullname = character(),
  AuthIdentity = character(),
  Location = character(),
  Attributes = character(),
  Password = character(),
  License = character(),
  stringsAsFactors = FALSE
)
.private(dMeasure, ".UserConfig", UserConfig_empty)
#' show user configurations
#'
#' @name UserConfig
#'
#' @param dMeasure_obj dMeasure R6 object
#'
#' @return dataframe of user configuration descriptions
#'  id, Fullname, AuthIdentity, Location, Attributes, License
#'
#'  Fullname - Best Practice full user name
#'  AuthIdentity - Windows login identity
#'  Location - vector of groups/locations
#'  Attributes - vector of user's attributes/permissions
#'  License - undecoded license
#'
#' @examples
#' dMeasure_obj <- dMeasure$new()
#' dMeasure_obj$open_configuration_db()
#' dMeasure_obj$read_configuration_db()
#' dMeasure_obj$UserConfig
#' @export
UserConfig <- function(dMeasure_obj) {
  return(dMeasure_obj$UserConfig)
}
.active(dMeasure, "UserConfig", function(value) {
  if (!missing(value)) {
    stop("self$UserConfig is read-only!")
  }

  empty_as_na <- function(x) {
    if ("factor" %in% class(x)) x <- as.character(x) # since ifelse wont work with factors
    ifelse(as.character(x) != "", x, NA)
  }
  if (self$config_db$is_open()) {
    userconfig <- private$.UserConfig %>>% dplyr::collect() %>>%
      dplyr::mutate(
        Location = stringi::stri_split(Location, regex = ";"),
        Attributes = stringi::stri_split(Attributes, regex = ";")
      ) %>>%
      # splits Location and Attributes into multiple entries (in the same column)
      dplyr::mutate(License = empty_as_na(License)) %>>%
      dplyr::select(-Password) # same as $.UserConfig, except the password
  } else {
    userconfig <-
      data.frame(
        id = integer(), Fullname = character(),
        AuthIdentity = character(),
        Location = character(),
        Attributes = character(),
        License = character()
      )
  }
  private$set_reactive(self$UserConfigR, userconfig) # set reactive version
  return(userconfig)
})
.reactive(
  dMeasure, "UserConfigR",
  quote(data.frame(
    id = integer(), Fullname = character(),
    AuthIdentity = character(),
    Location = character(),
    Attributes = character(),
    License = character()
  ))
)

#' show user configurations with license
#'
#' @name UserConfigLicense
#'
#' @param dMeasure_obj dMeasure R6 object
#'
#' @return dataframe of user configuration descriptions
#'  id, Fullname, AuthIdentity, Location, Attributes, License
#'
#'  Fullname - Best Practice full user name
#'  AuthIdentity - Windows login identity
#'  Location - vector of groups/locations
#'  Attributes - vector of user's attributes/permissions
#'  License - undecoded license
#'  Identifier - identifier used to interrogate subscription database
#'  LicenseDate - date of license
#'
#' @examples
#' dMeasure_obj <- dMeasure$new()
#' dMeasure_obj$open_configuration_db()
#' dMeasure_obj$read_configuration_db()
#' dMeasure_obj$UserConfigLicense
#' @export
UserConfigLicense <- function(dMeasure_obj) {
  return(dMeasure_obj$UserConfigLicense)
}
.active(dMeasure, "UserConfigLicense", function(value) {
  if (!missing(value)) {
    stop("self$UserConfigLicense is read-only!")
  }

  if (self$emr_db$is_open() && self$config_db$is_open()) {
    userconfiglicense <- self$UserConfig %>>%
      dplyr::left_join(self$UserFullConfig %>>%
        dplyr::select(Fullname, Identifier, LicenseDate),
      by = "Fullname"
      )
  } else {
    userconfiglicense <-
      data.frame(
        id = integer(), Fullname = character(),
        AuthIdentity = character(),
        Location = character(),
        Attributes = character(),
        License = character(),
        Identifier = character(),
        LicenseDate = as.Date(numeric(0),
          origin = "1970-01-01"
        )
      )
  }
  return(userconfiglicense)
})
.reactive_event(
  dMeasure, "UserConfigLicenseR",
  quote(
    shiny::eventReactive(
      c(self$UserConfigR()), {
        self$UserConfigLicense
      }
    )
  )
)

.private(dMeasure, ".UserRestrictions", data.frame(
  uid = integer(),
  Restriction = character(),
  stringsAsFactors = FALSE
))

.reactive(dMeasure, "UserRestrictions", quote(data.frame(
  uid = integer(),
  Restriction = character(),
  stringsAsFactors = FALSE
)))

# this lists the 'enabled' restrictions,
#  relevant to the 'Attributes' field of 'UserConfig'
# without the restriction, all users have the 'permission'
#  for the 'non-specified' action
# use 'uid' rather than 'id', because 'id' is
# later used to identify the restrictions...

## 'active' fields

#' choose (or read) database choice
#'
#' This must be one of 'None' or one of the defined databases.
#' Tries to open the database. If fails, will be set to 'None'.
#'
#' Sets $BPdatabasechoiceR reactive, if shiny/reactive
#' environment available
#'
#' (Stored in private$.BPdatabaseChoice)
#'
#' @name BPdatabaseChoice
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param choice (optional) name of database choice
#'
#'  posible value includes "None", which will close any current database
#'
#' @return the current database choice, if choice not provided
#'
#' @examples
#' dMeasure_obj$BPdatabaseChoice # returns the current choice
#' dMeasure_obj$BPdatabaseChoice <- "None" # sets database to none
#' @export
BPdatabaseChoice <- function(dMeasure_obj, choice) {
  if (missing(choice)) {
    return(dMeasure_obj$BPdatabaseChoice)
  } else {
    dMeasure_obj$BPdatabaseChoice <- choice
  }
}
.active(dMeasure, "BPdatabaseChoice", function(choice) {
  if (missing(choice)) {
    return(private$.BPdatabaseChoice)
  } else {
    if (!(choice %in% c("None", private$.BPdatabase$Name))) {
      stop(paste0(
        "Database choice must be one of ",
        paste0("'", private$.BPdatabase$Name, "'", collapse = ", "),
        " or 'None'."
      ))
    }

    # close existing database connection
    # safe to call $close() if no database is open

    if (choice == private$.BPdatabaseChoice) {
      # do nothing
      return(private$.BPdatabaseChoice)
    }
    # the chosen database is not the current database,
    # so close the current database
    self$emr_db$close()

    if (choice == "None") {
      # do nothing
    } else if (!is.null(choice)) {
      server <- private$.BPdatabase %>>%
        dplyr::filter(Name == choice) %>>%
        dplyr::collect()
      print("Opening EMR database")
      if (is.null(server$Driver) || is.na(server$Driver) || server$Driver == "") {
        # if driver id not defined
        server_driver <- "SQL Server" # the old 'default'
      } else {
        server_driver <- server$Driver
      }

      self$emr_db$connect(odbc::odbc(),
        driver = server_driver,
        server = server$Address, database = server$Database,
        uid = server$UserID,
        pwd = dMeasure::simple_decode(server$dbPassword)
      )
      # the open firewall ports required at the Best Practice database server are:
      #  TCP - 139    : File & Print Sharing - Subnet
      #  TCP - 54968  : BP Dynamic - SQL Express
      #  UDP - 137    : File & Print Sharing - Subnet
      #  UDP - 1434   : SQL Browser - Scope Sensitive
      #
      # additional firewall recommendations on Microsoft SQL docs
      #  https://docs.microsoft.com/en-us/sql/sql-server/install/
      #  configure-the-windows-firewall-to-allow-sql-server-access?view=sql-server-ver15
      #
      #  TCP - 135, 1433, 1434, 4022
      #  UDP - 1434
      #
      #  dynamic ports - Windows Firewall Advanced Security
      #   inbound rules - Rule type - Program
      #   e.g. C:\Program Files\Microsoft SQL Server\MSSQL12.BPSINSTANCE\Binn\sqlservr
    }

    if (!self$emr_db$is_open() || !DBI::dbIsValid(self$emr_db$conn())) {
      # || 'short-circuits' the evaluation, so if not an environment,
      # then dbIsValid() is not evaluated (will return an error if emr_db$conn() is NULL)

      # either database not opened, or has just been closed, including set to 'None'
      self$db$practice <- NULL
      self$db$users <- NULL
      self$db$patients <- NULL
      self$db$patientsRaw <- NULL
      self$db$clinical <- NULL
      self$db$reactions <- NULL
      self$db$investigations <- NULL
      self$db$papsmears <- NULL
      self$db$appointments <- NULL
      self$db$immunizations <- NULL
      self$db$preventive_health <- NULL
      self$db$correspondenceIn <- NULL
      self$db$reportValues <- NULL
      self$db$services <- NULL
      self$db$servicesRaw <- NULL
      self$db$invoices <- NULL
      self$db$history <- NULL
      self$db$currentRx <- NULL
      self$db$currentRx_raw <- NULL
      self$db$familyhistory <- NULL
      self$db$familyhistorydetail <- NULL
      self$db$relationcode <- NULL
      self$clinician_choice_list <- NULL
      choice <- "None" # set choice of database to 'None'
    } else {
      if (self$Log) {
        log_id <- self$config_db$write_log_db(
          query = "opened EMR database",
          data = choice
        )
      }
      # successfully opened database
      # set choice of database to attempted choice
      self$initialize_emr_tables() # initialize data tables

      invisible(self$clinician_list()) # and list all 'available' clinicians
    }

    private$.BPdatabaseChoice <- choice
    # the same as 'choice' initially was, if database successfully opened
    # otherwise will be 'None'. (also returns 'None' if tried to open 'None')
    invisible(self$BPdatabase) # will also set $BPdatabaseR

    if (nrow(self$config_db$conn() %>>% dplyr::tbl("ServerChoice") %>>%
      dplyr::filter(id == 1) %>>% dplyr::collect()) == 0) {
      # create a new entry
      query <- "INSERT INTO ServerChoice (id, Name) VALUES (?, ?)"
      data_for_sql <- as.list.data.frame(c(1, private$.BPdatabaseChoice))
      self$config_db$dbSendQuery(query, data_for_sql)
      # write to SQLite configuration database
    }

    if ((self$config_db$conn() %>>% dplyr::tbl("ServerChoice") %>>%
      dplyr::filter(id == 1) %>>%
      dplyr::pull(Name)) != private$.BPdatabaseChoice) {
      # if new choice is not recorded in current configuration database
      # already an entry in the ServerChoice table
      query <- "UPDATE ServerChoice SET Name = ? WHERE id = ?"
      data_for_sql <- as.list.data.frame(c(private$.BPdatabaseChoice, 1))
      self$config_db$dbSendQuery(query, data_for_sql)
      # write to SQLite configuration database
    }
    private$trigger(self$config_db_trigR) # send a trigger signal

    private$set_reactive(self$BPdatabaseChoiceR, choice)
    # set reactive, if reactive environment available
    return(private$.BPdatabaseChoice)
    # same name as the requested database if successful
    # 'None' if not successful, or if 'None' was chosen
  }
})
.reactive(dMeasure, "BPdatabaseChoiceR", NULL) # reactive version

## methods

#' Open the SQL connection to the configuration from the SQL configuration file
#'
#' Opens SQL connection to SQLite configuration file.
#' Does not read the configuration file (that is done by $read_configuration_db)
#'
#' Also check the SQL database
#' is compliant. new tables are added, and old ones
#' are checked to see if all required fields/columns
#' are present. if a field/column is missing, then
#' the missing field/column is added.
#'
#' @param dMeasure_obj dMeasure object
#' @param configuration_file_path (location of SQL configuration)
#'
#' @return nothing, modifies \code{dMeasure_obj}
#'
#' @examples
#' dMeasure_obj <- dMeasure$new()
#' dMeasure_obj$open_configuration_db()
#' dMeasure_obj$read_configuration_db()
#' dMeasure_obj$UserConfig
#' @export
open_configuration_db <-
  function(dMeasure_obj,
             configuration_file_path = dMeasure_obj$configuration_file_path) {
    dMeasure_obj$open_configuration_db(configuration_file_path)
  }

.public(dMeasure, "open_configuration_db",
  function(configuration_file_path = self$configuration_file_path) {

  # if no configuration filepath is defined, then try to read one
  if (length(configuration_file_path) == 0) {
    configuration_file_path <- self$configuration_file_path
  }

  config_db <- self$config_db # for convenience

  if (file.exists(configuration_file_path)) {
    # open config database file
    config_db$connect(RSQLite::SQLite(),
      dbname = self$configuration_file_path
    )
  } else {
    # if the config database doesn't exist,
    # then create it (note create = TRUE option)
    config_db$connect(RSQLite::SQLite(),
      dbname = self$configuration_file_path
    )
    # create = TRUE not a valid option?
    # always tries to create file if it doesn't exist
  }

  if (!config_db$is_open()) {
    # failed to read, or create, configuration database
    if (Sys.getenv("R_CONFIG_ACTIVE") == "shinyapps") {
      # shinyapps.io environment
      self$configuration_file_path <- ".DailyMeasure_cfg.sqlite"
    } else {
      self$configuration_file_path <- "~/.DailyMeasure_cfg.sqlite"
    }
    # try to create the default configuration file
    config_db$connect(RSQLite::SQLite(),
      dbname = self$configuration_file_path
    )
  }

  initialize_data_table <- function(config_db, tablename, variable_list) {
    # make sure the table in the database has all the right variable headings
    # allows 'update' of old databases
    #
    # input - config_db : R6 object of configuration database
    # input - tablename : name of table
    # input - variable_list : list of variable headings, with variable type
    #   e.g. list(c("id", "integer"), c("Name", "character"))
    #
    # alters table in database directly
    #
    # returns - nothing

    tablenames <- config_db$conn() %>>% DBI::dbListTables()

    if (tablename %in% tablenames) {
      # if table exists in config_db database
      data <- DBI::dbReadTable(config_db$conn(), tablename) %>>%
        dplyr::collect()
      # get a copy of the table's data
      # note that 'config_db$conn() %>>% dplyr::tbl(tablename) can't handle
      #  a BLOB column

      columns <- data  %>>% colnames()
      # list of column (variable) names
    } else {
      # table does not exist, needs to be created
      columns <- NULL
      data <- data.frame(NULL)
    }

    changed <- FALSE
    # haven't changed anything yet

    for (a in variable_list) {
      if (!(a[[1]] %in% columns)) {
        # if a required variable name is not in the table
        data <- data %>>%
          dplyr::mutate(!!a[[1]] := vector(a[[2]], nrow(data)))
        # use of !! and := to dynamically specify a[[1]] as a column name
        # potentially could use data[,a[[1]]] <- ...
        changed <- TRUE
      }
    }
    if (changed == TRUE) {
      DBI::dbWriteTable(config_db$conn(), tablename, data, overwrite = TRUE)
    }
  }


  if (!is.null(config_db$conn())) {
    # check that tables exist in the config file
    # also create new columns (variables) as necessary
    initialize_data_table(
      config_db, "Server",
      list(
        c("id", "integer"),
        c("Name", "character"),
        c("Driver", "character"), # which MSSQL ODBC driver to use
        c("Address", "character"),
        c("Database", "character"),
        c("UserID", "character"),
        c("dbPassword", "character")
      )
    )
    # initialize_data_table will create table and/or
    # ADD 'missing' columns to existing table

    initialize_data_table(
      config_db, "ServerChoice",
      list(
        c("id", "integer"),
        c("Name", "character")
      )
    )
    # there should only be (at most) one entry in this table!
    # with id '1', and a 'Name' the same as the chosen entry in table "Server"
    if (length(config_db$conn() %>>%
      dplyr::tbl("ServerChoice") %>>%
      dplyr::filter(id == 1) %>>%
      dplyr::pull(Name)) == 0) { # empty table
      query <- "INSERT INTO ServerChoice (id, Name) VALUES (?, ?)"
      data_for_sql <- as.list.data.frame(c(1, "None"))
      config_db$dbSendQuery(query, data_for_sql) # populate with "None" choice
    }

    initialize_data_table(
      config_db, "LogSettings",
      list(
        c("id", "integer"), # will always be '1'
        c("Log", "integer"),
        c("Filename", "character")
      )
    )
    # Log = true (1) if logging, or false (0) if not
    # Filename = SQLite log file
    # there should only be (at most) one entry in this table!
    # with id '1', and a 'Log' set to 0/1 (FALSE/TRUE), and
    # perhaps a filename
    if (length(config_db$conn() %>>%
      dplyr::tbl("LogSettings") %>>%
      dplyr::filter(id == 1) %>>%
      dplyr::pull(Log)) == 0) { # empty table})
      query <- "INSERT INTO LogSettings (id, Log, Filename) VALUES (?, ?, ?)"
      data_for_sql <- as.list.data.frame(c(1, as.numeric(FALSE), ""))
      config_db$dbSendQuery(query, data_for_sql) # starts as 'FALSE'
    }

    initialize_data_table(
      config_db, "Location",
      list(
        c("id", "integer"),
        c("Name", "character"),
        c("Description", "character")
      )
    )

    initialize_data_table(
      config_db, "Users",
      list(
        c("id", "integer"),
        c("Fullname", "character"),
        c("AuthIdentity", "character"),
        c("Location", "character"),
        c("Password", "character"),
        c("Attributes", "character"),
        c("License", "character") # contains license code (if any)
      )
    )

    initialize_data_table(
      config_db, "UserRestrictions",
      list(
        c("uid", "integer"),
        c("Restriction", "character")
      )
    )
    # list of restrictions for users
    # use of 'uid' rather than 'id'
    # (this relates to the 'Attributes' field in "Users")

    if (requireNamespace("dMeasureCustom", quietly = TRUE)) {
      if (
        exists(
          "initialize_data_table",
          where = asNamespace("dMeasureCustom"),
          mode = "function"
        )
      ) {
        x <- dMeasureCustom::initialize_data_table()
        initialize_data_table(
          config_db,
          tablename = x$tablename,
          variable_list = x$variable_list
        )
      }
    }

  }
  invisible(self)
  })

#' read the SQL configuration database
#'
#' @param dMeasure_obj dMeasure object
#' @param config_db R6 object to open SQL database
#'  default is the internally stored value in self$config_db
#'
#' @examples
#' dMeasure_obj <- dMeasure$new()
#' dMeasure_obj$open_configuration_db()
#' dMeasure_obj$read_configuration_db()
#' dMeasure_obj$UserConfig
#' @export
read_configuration_db <- function(dMeasure_obj,
                                  config_db) {
  if (exists(config_db)) {
    dMeasure_obj$read_configuration_db(config_db)
  } else {
    dMeasure_obj$read_configuration_db()
  }
}
.public(dMeasure, "read_configuration_db", function(config_db = self$config_db) {
  if (!config_db$is_open()) {
    # if config_db is not yet opened/defined
    # then try to open configuration database
    self$open_configuration_db()
    config_db <- self$config_db
  }

  if (!config_db$is_open()) {
    warning("Configuration database not opened or defined.")
    return(invisible(self))
  }

  private$.BPdatabase <- config_db$conn() %>>%
    dplyr::tbl("Server") %>>% dplyr::collect()
  invisible(self$BPdatabase) # will also set $BPdatabaseR
  invisible(self$BPdatabaseChoice_new)
  # reads the database choice, but does not yet open that choice

  invisible(self$LogFile)
  invisible(self$Log) # will also set $LogR
  # self$Log will also call self$Logfile to read the filename of the SQLite
  # because if $Log is TRUE, then will immediate try to open the logfile

  private$PracticeLocations <- config_db$conn() %>>%
    dplyr::tbl("Location") %>>%
    dplyr::mutate(Name = trimws(Name))
  invisible(self$location_list)
  # $location_list() will refresh the reactive location_listR if available

  private$.UserConfig <- config_db$conn() %>>%
    dplyr::tbl("Users")
  # in .UserConfig, there can be multiple Locations/Attributes per user
  # this is only translated in the public version 'self$UserConfig'
  invisible(self$UserConfig) # this will also set $userConfigR reactive version

  private$.UserRestrictions <- config_db$conn() %>>%
    dplyr::tbl("UserRestrictions")
  private$set_reactive(self$UserRestrictions, private$.UserRestrictions)

  invisible(self$.identified_user)
  # see if 'identified' system user is matched with a configured user

  private$trigger(self$config_db_trigR)
  # notification of configuration database change

  if (exists("dMCustom")) {
    if (exists("read_configuration_db", where = dMCustom, mode = "function")) {
      dMCustom$read_configuration_db(config_db$conn())
    }
  }
  invisible(self)
})
.public(dMeasure, "BPdatabaseChoice_new", function() {
  if (self$config_db$is_open()) {
    # config database is open
    new <- self$config_db$conn() %>>% dplyr::tbl("ServerChoice") %>>%
      dplyr::filter(id == 1) %>>% dplyr::pull(Name)
  } else {
    # config database is not open
    new <- self$BPdatabaseChoice # the current choice
  }
  return(new)
})

#' read the subscription database
#'
#'  also update the configuration database with new Licenses (if available)
#'  and the date of checks
#'
#' @param dMeasure_obj dMeasure object
#' @param forcecheck check, even if already checked 'today'. TRUE/FALSE
#' @param users vector of user names. if NULL (the default) then all user in $UserFullConfig
#'
#' @param UserFullConfig updated UserFullConfig (includes subscription information)
#'  returns warning if RMariaDB module is not available to open database
#'  returns warning if unable to open subscription database
#'
#' @examples
#' dMeasure_obj$read_subscription_db()
#' @export
read_subscription_db <- function(dMeasure_obj,
                                 forcecheck = FALSE,
                                 users = NULL) {
  dMeasure_obj$read_subscription_db(forcecheck)
}
.public(dMeasure, "read_subscription_db", function(forcecheck = FALSE,
                                                   users = NULL) {
  # read subscription information

  Sys.setenv("AIRTABLE_API_KEY" = "keyKqBa9WxbM63qqu")

  airtable <- airtabler::airtable("appLa2AH6S1SUCxE3", "Subscriptions")
  # the actual table is 'DailyMeasureUsers'

  subscription_is_open <-
    is.list(tryCatch(airtable$Subscriptions$select(filterByFormula = "Key = 'dummy'"),
      error = function(e) {
        NA
      }
    ))
  #

  if (subscription_is_open &&
    self$emr_db$is_open() && self$config_db$is_open()) {
    # successfully opened subscription database
    # neees the configuration and EMR databases to also be open
    print("Subscription database opened")

    a <- self$UserFullConfig
    if (!is.null(users)) { # if null, then search for all users
      # if not null, then restrict checked users to those in 'users' vector
      a <- a %>>% dplyr::filter(Fullname %in% users)
    }

    a <- a %>>%
      dplyr::mutate(
        LicenseCheck =
          forcecheck |
            # if forcecheck == TRUE
            (is.na(LicenseDate) |
              # check if no valid license expiry
              # or license is expiring soon
              LicenseDate < (Sys.Date() + 60)),
        IdentifierUpper = toupper(Identifier)
      ) # convert identifier to upper-case

    b <- a %>>% dplyr::filter(LicenseCheck == TRUE) %>>%
      dplyr::pull(IdentifierUpper) %>>% simple_encode(key = "karibuni")
    # vector of Identifier to check in subscription database
    # these are 'encoded'
    search_string <- paste0("OR(", paste0("{Key} = '", c(b, "dummy"), "'", collapse = ", "), ")")
    # this ends up looking something like....
    #  "OR({Key} = 'a', {Key} = 'j', {Key} = 'tea')"
    #  adds 'dummy' to the vector, because if no entries are returned, then
    #  the search will return an empty list! (instead of a dataframe)

    a <- a %>>%
      dplyr::left_join(airtable$Subscriptions$select(filterByFormula = search_string) %>>%
        # dplyr::filter(Key != "dummy") %>>% # get rid of the dummy, interferes with decode
        dplyr::mutate(IdentifierUpper = simple_decode(Key, key = "karibuni")) %>>%
        dplyr::select(IdentifierUpper, NewLicense = License),
      by = "IdentifierUpper"
      ) %>>%
      dplyr::mutate(
        License =
          mapply(function(x, y, z, zz) {
            if (is.na(z)) {
              y # no new expiry date, 'License'
            } else {
              # need to set to new license
              # and also need to update our configuration database

              if (nrow(self$userconfig.list() %>>%
                dplyr::filter(Fullname == x)) == 0) {
                # the user has NO entry in the configuration database, so create one
                self$userconfig.insert(list(Fullname = x))
              }
              # update the license
              self$update_subscription(
                Fullname = x,
                License = z, # new license
                Identifier = zz,
                verify = FALSE
              )
              # not verified at this stage
              z # NewLicence
            }
          }, Fullname, License, NewLicense, Identifier,
          USE.NAMES = FALSE
          )
      )

    # close before exit
    self$subscription_db$close()

    return(self$UserFullConfig) # this contains the updated license informatioin
  } else {
    warning("Unable to open subscription database")
  }
})

#' Update subscription database
#'
#' @param dMeasure_obj dMeasure object
#' @param Fullname name of the user to change license
#' @param License the (undecoded) license string
#' @param Identifier the identifier of the user
#' @param verify verify before changing
#'
#' @return TRUE if license written, FALSE if not
#'  only meaningful if Verify is TRUE, in which case
#'  FALSE indicates the license was not valid
update_subscription <- function(dMeasure_obj,
                                Fullname = NA,
                                License = NA,
                                Identifier = NA,
                                verify = TRUE) {
  dMeasure_obj$update_subscription(
    Fullname, License, Identifier,
    verify
  )
}
.public(dMeasure, "update_subscription", function(Fullname = NA,
                                                  License = NA,
                                                  Identifier = NA,
                                                  verify = TRUE) {
  if (verify) {
    verified <- !is.na(dMeasure::verify_license(License, Identifier))
    # verify_license returns NA if not a valid license
  }
  if (!verify || verified) {
    # either no verification required, or is verified
    query <- paste("UPDATE Users SET License = ? WHERE Fullname = ?")
    data_for_sql <- as.list.data.frame(c(License, Fullname))
    self$config_db$dbSendQuery(query, data_for_sql)
    # if the connection is a pool,
    # can't send write query (a statement) directly
    # so use the object's method
    return(TRUE)
  } else {
    return(FALSE)
  }
})

#' check the subscription database
#'
#' @param dMeasure_obj dMeasure object
#' @param clinicians vector of users to check
#' @param date_from date from, by default $date_a
#' @param date_to date to, by default $date_b
#' @param adjust_days number of days to adjust
#'
#' if the date is adjusted then reactive $check_subscription_datechange_trigR
#' is triggered
#'
#' @return a list $changedate, $date_from, $date_to
#'  $changedate (TRUE/FALSE), and the (possibly) adjusted dates
#'  warning generated if $changedate is TRUE
#'
#' @examples
#' dMeasure_obj$check_subscription()
#' @export
check_subscription <- function(dMeasure_obj,
                               clinicians = NA,
                               date_from = NA, date_to = NA,
                               adjust_days = 7) {
  dMeasure_obj$check_subscription(
    users, date_from, date_to,
    adjust_days
  )
}
.public(dMeasure, "check_subscription", function(clinicians = NA,
                                                 date_from = NA,
                                                 date_to = NA,
                                                 adjust_days = 7) {
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
    clinicians <- c("") # dplyr::filter cannot handle empty list()
  }

  LicenseDates <- self$UserFullConfig %>>%
    dplyr::filter(Fullname %in% clinicians) %>>%
    dplyr::pull(LicenseDate)

  changedate <- FALSE # do dates need to be changed
  # i.e. is there a chosen user with no license, or expired license

  if (date_to > (Sys.Date() - adjust_days)) {
    # only if date range includes future, or insufficiently 'old' appointments
    changedate <- (NA %in% LicenseDates) # a chosen user has no license
    if (!changedate) {
      # no NA, but are any dates expired
      for (a in LicenseDates) {
        b <- as.Date(a, origin = "1970-01-01")
        if (b < Sys.Date()) {
          # expired subscription
          changedate <- TRUE
          break # no need to check other users
        }
      }
    }
  }

  if (changedate) {
    if (date_to > (Sys.Date() - adjust_days)) {
      date_to <- Sys.Date() - adjust_days
      if (date_from > date_to) {
        date_from <- date_to
      }
      warning(
        "A chosen user has no subscription for chosen date range. ",
        "Without subscription, dates need to be minimum ",
        adjust_days,
        " days old."
      )
      # change the dates
      new_trigger_value <-
        -sign(self$check_subscription_datechange_trigR()) * adjust_days
      # reverses the 'sign' of the trigger
      private$set_reactive(
        self$check_subscription_datechange_trigR,
        new_trigger_value
      )
    }
  }
  return(list(changedate = changedate, date_from = date_from, date_to = date_to))
})
.reactive(dMeasure, "check_subscription_datechange_trigR", 1)
# this trigger will 'flip-flop' from positive to negative values
# the absolute value of the this trigger will be the number of days to be adjusted

##### User login ##################################################

#' returns information about the identified user
#'
#' also sets reactive $identified_user, and sets $authenticated
#'
#' @param dMeasure_obj dMeasure object
#'
#' @return dataframe, single row, the identified user
#'
#' @export
.identified_user <- function(dMeasure_obj) {
  return(dMeasure_obj$.identified_user)
}
.active(dMeasure, ".identified_user", function(value) {
  if (!missing(value)) {
    stop("cannot be set, $.identified_user is read-only")
  }

  current_user <- Sys.info()[["user"]]
  d <- NULL
  if (self$config_db$is_open()) {
    private$set_reactive(
      self$identified_user,
      self$UserConfig %>>%
        dplyr::filter(AuthIdentity == current_user) %>>%
        dplyr::select(Fullname, AuthIdentity, Location, Attributes)
    )
    # set reactive version if reactive (shiny) environment available
    # does not include password

    if ("RequirePasswords" %in% (private$.UserRestrictions %>>% dplyr::pull(Restriction))) {
      # password not yet entered, so not yet authenticated
      self$authenticated <- FALSE
    } else {
      # no password required, current user attributes are 'authenticated' by Sys.info()
      self$authenticated <- TRUE
    }

    d <- private$.UserConfig %>>%
      dplyr::filter(AuthIdentity == current_user) %>>% dplyr::collect()
  }

  return(d)
})
# data.frame(id = integer(), Fullname = character(),
#  AuthIdentity = character(), Location = character(),
#  Password = character(), Attributes = character()))

# user information for just the identified user
.public(dMeasure, "authenticated", FALSE)
# has the current 'identified' user been authenticated yet?

## methods

#' Match user with current 'identified' system user
#'
#' Matches 'dMeasure_obj$.UserConfig$AuthIdentity' with Sys.info()[["user"]]
#'
#' @param dMeasure_obj dMeasure object
#'
#' @return self
match_user <- function(dMeasure_obj) {
  dMeasure_obj$match_user()
}

.public(dMeasure, "match_user", function() {
  current_user <- Sys.info()[["user"]]
  private$set_reactive(
    self$identified_user,
    self$UserConfig %>>%
      dplyr::filter(AuthIdentity == current_user) %>>%
      dplyr::select(Fullname, AuthIdentity, Location, Attributes)
  )
  # set reactive version if reactive (shiny) environment available
  # does not include password

  if ("RequirePasswords" %in% (private$.UserRestrictions %>>% dplyr::pull(Restriction))) {
    # password not yet entered, so not yet authenticated
    self$authenticated <- FALSE
  } else {
    # no password required, current user attributes are 'authenticated' by Sys.info()
    self$authenticated <- TRUE
  }

  invisible(self)
})
.reactive(dMeasure, "identified_user", NULL)

##### clinician choice list #######################################

## fields
.public(dMeasure, "clinician_choice_list", NULL)
# available clinicians appointments
.public(dMeasure, "clinicians", NULL)
# chosen clinician list

## constants
.public(dMeasure, "view_restrictions", list(
  # if a view restriction is active, then by default users
  # can only see patients in their own appointment book for
  # the specified topic
  # this restriction does not apply if the user has the
  # 'Global' attribute for the topic in the user's attribute list
  list(
    restriction = "GlobalActionView",
    view_to_hide = list("immunization", "cancerscreen")
  ),
  list(
    restriction = "GlobalBillView",
    view_to_hide = list("billings")
  ),
  list(
    restriction = "GlobalCDMView",
    view_to_hide = list("cdm")
  )
))

## methods

#' find available list of clinician appointments to view
#'
#' adjusts self$clinician_choice_list
#' according to 'view_name' (and applicable view restrictions
#' for the identified authenticated user)
#' and self$location
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param view_name name of view. default is All i.e. 'no specific view'
#' @param location location. default is whatever is already set
#'
#' @return the clinician choice list
#' @export
clinician_list <- function(dMeasure_obj,
                           view_name = "All",
                           location = NULL) {
  dMeasure_obj$clinician_list(view_name, location)
}

.public(dMeasure, "clinician_list", function(view_name = "All",
                                             location = NULL) {
  if (is.null(location)) {
    location <- self$location
  } else {
    self$location <- location
  }
  if (self$location == "All") {
    # note that 'ifelse' only returns result in the
    # same 'shape' as the comparison statement
    clinician_list <- self$UserFullConfig$Fullname
  } else {
    clinician_list <- subset(
      self$UserConfig$Fullname,
      sapply(
        self$UserConfig$Location,
        function(y) self$location %in% y
      )
    )
    # filter clinicians by location choice
    # it is possible for a clinician to have multiple locations
    # initially, $Location might include a lot of NA
    #
    # if filtered, then only configured users can be in the list
  }

  for (restriction in self$view_restrictions) {
    # go through list of view restrictions
    if (restriction$restriction %in% (private$.UserRestrictions %>>%
      dplyr::pull(Restriction))) {
      # if the restriction has been activated
      if (view_name %in% restriction$view_to_hide) {
        # if the relevant view is being shown
        if (self$authenticated == FALSE |
          !(restriction$restriction %in% (self$UserConfig %>>%
            dplyr::filter(Fullname == self$.identified_user$Fullname) %>>%
            dplyr::pull(Attributes) %>>% unlist()))) {
          # if user is not authenticated or
          # if the current user does not have this 'Global' attribute
          # then can only view one's own appointments
          clinician_list <- subset(
            clinician_list,
            clinician_list == self$.identified_user$Fullname
          )
        }
      }
    }
  }
  self$clinician_choice_list <- clinician_list
  return(clinician_list)
})

#' chosen clinicians
#'
#' clinicians chosen for appointment viewing
#' modifies self$clinicians
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param choices="" list of clinicians chosen
#' @param view_name="All" view
#'
#' @return list of clinicians chosen
#' this will be 'checked' against actual available clinicians ($clinicians_list)
#' @export
choose_clinicians <- function(dMeasure_obj, choices = "", view_name = "All") {
  dMeasure_obj$choose_clinicians(choices, view_name)
}

.public(dMeasure, "choose_clinicians", function(choices = "", view_name = "All") {
  choices <- intersect(choices, self$clinician_list(view_name))
  # can only actually choose clinicians available in chosen view

  self$clinicians <- choices
  private$set_reactive(self$cliniciansR, self$clinicians)

  return(choices)
})
.reactive(dMeasure, "cliniciansR", quote(self$clinicians))

##### Electronic Medical Record (EMR) database configuration ######

## fields
.public_init(dMeasure, "emr_db", quote(dbConnection::dbConnection$new()))
# R6 object containing database object
.public(dMeasure, "db", list(dbversion = 0)) # later will be the EMR databases.
# $db$dbversion is number of EMR database openings
# there is also a 'reactive' version if shiny is available
.reactive(dMeasure, "dbversion", 0)

## methods

#' opens the EMR database
#'
#' @param dMeasure_obj dMeasure object
#' @param BPdatabaseChoice the chosen database from the config_db list
#'
#' @return BPdatabaseChoice the same as the chosen database if successfully opened
#'  otherwise returns "None"
#'
#' if no arguments passed, the defaults are what is stored in
#' the  object
#' @export
open_emr_db <- function(dMeasure_obj,
                        BPdatabaseChoice = dMeasure_obj$BPdatabaseChoice) {
  dMeasure_obj$open_emr_db(BPdatabaseChoice)
}

.public(dMeasure, "open_emr_db", function(BPdatabaseChoice = NULL) {
  if (!self$config_db$is_open() || length(self$BPdatabaseChoice) == 0) {
    # no BPdatabase has been defined, or the current configuration is not valid
    # try to define the current configuration and open the BP database
    self$read_configuration_db()
  }

  if (is.null(BPdatabaseChoice)) {
    BPdatabaseChoice <- self$BPdatabaseChoice_new()
    # read the SQLite configuration file (if open)
    # otherwise will just be the same as self$BPdatabaseChoice
  }

  print(paste("ChosenServerName:", BPdatabaseChoice))

  if (BPdatabaseChoice != self$BPdatabaseChoice) {
    # if specified choice is not the same as the current choice
    self$BPdatabaseChoice <- BPdatabaseChoice
    # this 'active' field will automatically try to open the selected database
  }

  return(self$BPdatabaseChoice)
})

#' initialize the tables of the EMR database
#'
#' @name initialize_emr_tables
#'
#' @param dMeasure_obj dMeasure object
#' @param emr_db R6 object connecting to EMR database
#'
#' @return none
#'
#' @export
initialize_emr_tables <- function(dMeasure_obj,
                                  emr_db = dMeasure_obj$emr_db) {
  dMeasure_obj$initialize_emr_tables(emr_db)
}

.public(dMeasure, "initialize_emr_tables", function(emr_db) {
  if (missing(emr_db)) {
    emr_db <- self$emr_db
  }

  print("Re-initializing databases")

  self$db$practice <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "PRACTICE")) %>>%
    dplyr::select(PracticeName = PRACTICENAME) %>>%
    dplyr::mutate(PracticeName = trimws(PracticeName))

  self$db$users <- emr_db$conn() %>>%
    # this is a function! a collect() is later called prior to mutate/join,
    # (as a result is no longer a 'lazy eval') and cannot be evaluated just once.
    # output - Fullname, UserID, Surname, Firstname, LocationName, Title, ProviderNo
    dplyr::tbl(dbplyr::in_schema("dbo", "BPS_Users")) %>>%
    dplyr::select(c(
      "UserID", "Surname", "Firstname",
      "LocationName", "Title", "ProviderNo"
    )) %>>%
    dplyr::mutate(
      Surname = trimws(Surname),
      Firstname = trimws(Firstname),
      Title = trimws(Title),
      ProviderNo = trimws(ProviderNo)
    )
  invisible(self$UserConfig)
  # will also set $UserConfigR reactive
  # does not include password in public/reactive

  self$db$patients <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "BPS_Patients")) %>>%
    dplyr::mutate(
      Title = trimws(Title),
      Firstname = trimws(Firstname),
      Middlename = trimws(Middlename),
      Surname = trimws(Surname),
      Ethnicity = trimws(Ethnicity),
      Sex = trimws(Sex),
      RecordNo = trimws(RecordNo),
      ExternalID = trimws(ExternalID),
      StatusText = trimws(StatusText),
      Preferredname = trimws(Preferredname),
      Address1 = trimws(Address1),
      Address2 = trimws(Address2),
      City = trimws(City),
      PostalAddress = trimws(PostalAddress),
      PostalCity = trimws(PostalCity)
    )

  self$db$patientsRaw <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "PATIENTS")) %>>%
    dplyr::select(
      InternalID = INTERNALID,
      HeadOfFamilyID = HEADOFFAMILYID,
      DOB
      ) %>>%
    dplyr::mutate(DOB = as.Date(DOB))

  # fields include InternalID, ExternalID, RecordNo, StatusText
  # Title, Firstname, Middlename, Surname, Preferredname
  # DOB, Sex, Ethnicity

  self$db$MARITALSTATUS <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "MARITALSTATUS"))
  # has the fields MARITALSTATUSCODE (a number)
  # and MARITALSTATUSNAME (a string)

  self$db$SEXUALITY <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "SEXUALITY"))
  # has the fields SEXUALITYCODE (a number)
  # and SEXUALITYNAME (a string)

  self$db$SMOKINGSTATUS <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "SMOKINGSTATUS")) %>>%
    dplyr::mutate(SMOKINGTEXT = trimws(SMOKINGTEXT))
  # has fields SMOKINGCODE (a number)
  # and SMOKINGTEXT (a string)

  self$db$ALCOHOLSTATUS <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "ALCOHOLSTATUS")) %>>%
    dplyr::mutate(ALCOHOLTEXT = trimws(ALCOHOLTEXT))
  # has fields ALCOHOLCODE (a number)
  # and ALCOHOLTEXT (a string)
  # 0 - no entry, 1 = Nil, 2 = Occasional
  # 3 - Moderate, 4 = Heavy
  # note that '0' in the CLINICAL will be the case
  # if either current or **Past** alcohol consumption not entered

  self$db$clinical <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "CLINICAL")) %>>%
    dplyr::select(
      INTERNALID, KNOWNALLERGIES, MARITALSTATUS, SEXUALITY,
      SOCIALHX, ACCOMODATION, LIVESWITH, HASCARER, ISCARER,
      SMOKINGSTATUS, ALCOHOLSTATUS, RECREATION,
      CREATED, UPDATED
    ) %>>%
    dplyr::left_join(self$db$MARITALSTATUS,
      by = c("MARITALSTATUS" = "MARITALSTATUSCODE")
    ) %>>%
    dplyr::left_join(self$db$SEXUALITY,
      by = c("SEXUALITY" = "SEXUALITYCODE")
    ) %>>%
    dplyr::select(-c(MARITALSTATUS, SEXUALITY)) %>>%
    dplyr::rename(
      InternalID = INTERNALID,
      KnownAllergies = KNOWNALLERGIES, # 0 = not recorded, 1 = unknown, 2 = some recorded
      MaritalStatus = MARITALSTATUSNAME,
      SocialHx = SOCIALHX,
      # note that social history details are also available in other fields
      # such as ACCOMODATION, LIVESWITH, HASCARER, ISCARER, RECREATION
      # HASCARER = 0 '', 1 - 'Yes', 2 - 'No', 3 - 'Self' (?)
      # ISCARER = 0 '', 1 - 'Yes, 2 - 'No'
      Accomodation = ACCOMODATION,
      LivesWith = LIVESWITH,
      HasCarer = HASCARER, IsCarer = ISCARER,
      Recreation = RECREATION,
      Sexuality = SEXUALITYNAME
    ) %>>%
    dplyr::mutate(
      MaritalStatus = trimws(MaritalStatus),
      Sexuality = trimws(Sexuality),
      Recreation = trimws(Recreation),
      SocialHx = trimws(SocialHx)
    ) %>>%
    # for some reason, dbo.BPS_Clinical contains multiple entries per InternalID
    #  (which are not dated or given additional identifiers)
    # MaritalStatusName and SexualityName provided as strings
    # 'codes' can be found in dbo.CLINICAL
    # and interpretation of codes can be found in dbo.MARITALSTATUS
    # and dbo.SEXUALITY
    #
    # this table appears to have one entry per patient
    dplyr::left_join(self$db$SMOKINGSTATUS,
      by = c("SMOKINGSTATUS" = "SMOKINGCODE")
    ) %>>%
    # current SMOKINGCODE is 0 - nothing, 1 = "Non smoker",
    # 2 - "Ex smoker", 3 - "Smoker"
    dplyr::select(-c(SMOKINGSTATUS)) %>>%
    dplyr::rename(SmokingStatus = SMOKINGTEXT) %>>%
    dplyr::left_join(self$db$ALCOHOLSTATUS,
      by = c("ALCOHOLSTATUS" = "ALCOHOLCODE")
    ) %>>%
    dplyr::select(-c(ALCOHOLSTATUS)) %>>%
    dplyr::rename(
      AlcoholStatus = ALCOHOLTEXT,
      Created = CREATED,
      Updated = UPDATED
    )
  # 0 - no entry, 1 = Nil, 2 = Occasional
  # 3 - Moderate, 4 = Heavy
  # note that '0' in the CLINICAL will be the case
  # if either current or **Past** alcohol consumption not entered

  # two tables providing 'decoding' for social history elements in db$clinical
  self$db$accomodation <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "ACCOMODATION")) %>>%
    dplyr::rename(
      AccomodationCode = ACCOMODATIONCODE,
      AccomodationText = ACCOMODATIONTEXT
    ) %>>%
    dplyr::mutate(AccomodationText = trimws(AccomodationText))
  # currently 0 '', 1 'Own home', 2 "Relative's home", 3 'Other private house'
  # 4 'Hostel', 5 'Nursing home (RACF), 6 'Homeless', 7 'Rental home'
  self$db$liveswith <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "LIVESWITH")) %>>%
    dplyr::rename(
      LivesWithCode = LIVESWITHCODE,
      LivesWithText = LIVESWITHTEXT
    ) %>>%
    dplyr::mutate(LivesWithText = trimws(LivesWithText))
  # currently 0 '', 1 'Spouse', 2 'Relative', 3 'Friend', 4 'Alone'
  self$db$carer <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "CARER")) %>>%
    dplyr::select(
      INTERNALID, TITLECODE, SURNAME, FIRSTNAME,
      ADDRESS, CITY, POSTCODE, CONTACTPHONE, CONTACTPHONE2, RELATIONSHIP,
      CREATED, UPDATED
    ) %>>%
    dplyr::rename(
      InternalID = INTERNALID,
      TitleCode = TITLECODE,
      Surname = SURNAME, Firstname = FIRSTNAME,
      Address = ADDRESS, City = CITY, Postcode = POSTCODE,
      ContactPhone = CONTACTPHONE,
      ContactPhone2 = CONTACTPHONE2,
      Relationship = RELATIONSHIP,
      Created = CREATED, Updated = UPDATED
    ) %>>%
    dplyr::mutate(
      Surname = trimws(Surname), Firstname = trimws(Firstname),
      Address = trimws(Address), City = trimws(City),
      Postcode = trimws(Postcode), ContactPhone = trimws(ContactPhone),
      ContactPhone2 = trimws(ContactPhone2),
      Relationship = trimws(Relationship)
    )

  self$db$titles <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "TITLES")) %>>%
    dplyr::select(TITLECODE, TITLE) %>>%
    dplyr::rename(TitleCode = TITLECODE, Title = TITLE) %>>%
    dplyr::mutate(Title = trimws(Title))

  self$db$reactions <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "BPS_Reactions")) %>>%
    dplyr::select(InternalID, ItemName, Reaction, Severity, Comment) %>>%
    # for some reason, error when selecting 'Created'
    dplyr::mutate(
      ItemName = trimws(ItemName),
      Reaction = trimws(Reaction),
      Severity = trimws(Severity)
    )

  self$db$alcohol <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "BPS_Alcohol")) %>>%
    dplyr::select(
      InternalID,
      NonDrinker, DaysPerweek, DrinksPerday, Description,
      # NonDrinker - 'Yes' or 'No'
      PastAlcoholLevel, YearStarted, YearStopped, Comment
    ) %>>%
    dplyr::mutate(NonDrinker = trimws(Nondrinker)) %>>%
    dplyr::left_join(self$db$clinical %>>%
      dplyr::select(InternalID, Updated),
    by = c("InternalID" = "InternalID")
    )
  # strangely 'by' needs to be explicit, perhaps because of lazy eval?
  # to tell if the patient has a alcohol history requires...
  # NonDrinker = 'Yes' OR DaysPerweek/DrinksPerday to be non-zero
  # unfortunately, no date is attached to this alcohol history
  #
  # this table appears to have one entry per patient
  #
  # there IS a date attached to AlcoholStatus in 'clinical' table,
  # but this requires entries in both Present and Past alcohol intake.
  # the 'UPDATED' field in clinical appears to have the correct update
  # date for BPS_Alcohol

  self$db$investigations <- emr_db$conn() %>>%
    # output - InternalID, Collected (Date), TestName
    dplyr::tbl(dbplyr::in_schema("dbo", "BPS_Investigations")) %>>%
    dplyr::select(
      InternalID, ReportID,
      TestName,
      Reported, Checked, Actioned,
      # three dates
      CheckedBy,
      # a name of the provider who checked
      Notation, Action,
      # Action includes 'Urgent Appointment' and 'Non-urgent Appointment'
      Comment
    ) %>>%
    # as of Jan/2019, the odbc engine for MSSQL can't handle the
    # full ('Select *') Investigations table
    # due to some type of bug/standards non-compliance.
    # also can handle the History table. need to
    # 'Select' out just a few columns.
    dplyr::mutate(TestName = trimws(TestName))

  self$db$papsmears <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "BPS_PapSmears")) %>>%
    dplyr::select(
      InternalID, PapDate, CSTType,
      HPV16, HPV18, HPVOther, Result,
      HPVChanges, EndocervicalCells, Comment
    ) %>>%
    dplyr::mutate(
      CSTType = trimws(CSTType),
      HPV16 = trimws(HPV16), HPV18 = trimws(HPV18), HPVOther = trimws(HPVOther),
      Result = trimws(Result), HPVChanges = trimws(HPVChanges),
      EndocervicalCells = trimws(EndocervicalCells)
    )
  # CSTType includes 'PAP'
  # Result includes 'Negative'

  self$db$appointments <- emr_db$conn() %>>%
    # Patient, InternalID, AppointmentDate, AppointmentTime, Provider, Status
    dplyr::tbl(dbplyr::in_schema("dbo", "BPS_Appointments")) %>>%
    dplyr::select(c(
      "Patient", "InternalID",
      "AppointmentDate", "AppointmentTime",
      "Provider", "Status"
    ))
  # Status : 'Booked', 'Completed', 'At billing', 'Waiting', 'With doctor'

  self$db$visits <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "BPS_Visits")) %>>%
    dplyr::select(InternalID, VisitType, VisitDate, UserID, DrName) %>>%
    dplyr::mutate(
      VisitType = trimws(VisitType),
      DrName = trimws(DrName)
    )
  # VisitType : 'Surgery', 'Home', "Non Visit', 'Hospital', 'RACF', 'Telephone'
  # ... 'SMS', 'Email', 'Locum Service', 'Out of Office', 'Other', 'Hostel'
  # ... 'Telehealth'

  self$db$immunizations <- emr_db$conn() %>>%
    # InternalID, GivenDate, VaccineName, VaccineID
    dplyr::tbl(dbplyr::in_schema("dbo", "BPS_Immunisations")) %>>%
    dplyr::select(c("InternalID", "GivenDate", "VaccineName", "VaccineID")) %>>%
    dplyr::mutate(
      GivenDate = as.Date(GivenDate),
      VaccineName = trimws(VaccineName)
    )

  self$db$vaccine_disease <- emr_db$conn() %>>%
    # vaccineIDs linked to diseases
    # e.g. diseasecode 7+30 are for influenza vaccines
    dplyr::tbl(dbplyr::in_schema("bpsdrugs.dbo", "VACCINE_DISEASE")) %>>%
    dplyr::select("VACCINEID", "DISEASECODE")

  self$db$vaccines <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("bpsdrugs.dbo", "VACCINES")) %>>%
    # there is also ACIRCODE, CHIDLHOOD, GENERIC
    dplyr::select("VACCINEID", "VACCINENAME") %>>%
    dplyr::rename(VaccineID = VACCINEID, VaccineName = VACCINENAME) %>>%
    dplyr::mutate(VaccineName = trimws(VACCINENAME))

  self$db$vaccine_disease <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("bpsdrugs.dbo", "VACCINE_DISEASE")) %>>%
    dplyr::select("VACCINEID", "DISEASECODE")

  self$db$vaxdiseases <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("bpsdrugs.dbo", "VAXDISEASES")) %>>%
    dplyr::select("DISEASECODE", "DISEASENAME") %>>%
    dplyr::rename(DiseaseCode = DISEASECODE, DiseaseName = DISEASENAME) %>>%
    dplyr::mutate(DiseaseName = trimws(DISEASENAME))

  self$db$preventive_health <- emr_db$conn() %>>%
    # INTERNALID, ITEMID (e.g. not for Zostavax remindders)
    dplyr::tbl(dbplyr::in_schema("dbo", "PreventiveHealth")) %>>%
    dplyr::select("InternalID" = "INTERNALID", "ITEMID")

  self$db$correspondenceIn <- emr_db$conn() %>>%
    # InternalID, CorrespondenceDate, Subject, Detail
    dplyr::tbl(dbplyr::in_schema("dbo", "BPS_CorrespondenceIn")) %>>%
    dplyr::select(
      InternalID, DocumentID,
      CorrespondenceDate,
      Subject, Detail, Comment
    )

  self$db$correspondenceInRaw <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "CORRESPONDENCEIN")) %>>%
    dplyr::select(
      DOCUMENTID, INTERNALID,
      USERID, CHECKEDBY,
      # both USERID and CHECKEDBY are numbers, not names
      CORRESPONDENCEDATE, CHECKDATE, ACTIONDATE,
      # three dates
      CATEGORY, SUBJECT, DETAIL, COMMENT,
      NOTATION, ACTION
    )
  # Action includes 6 for Non-urgent appointment,
  # and 7 for Urgent appointment

  self$db$reportValues <- emr_db$conn() %>>%
    # InternalID, ReportDate, ResultName, LoincCode
    dplyr::tbl(dbplyr::in_schema("dbo", "BPS_ReportValues")) %>>%
    dplyr::select(
      InternalID, ReportID, ReportDate, LoincCode, BPCode, ResultName,
      ResultValue, Units, Range
    ) %>>%
    dplyr::mutate(
      ReportDate = as_datetime(ReportDate),
      LoincCode = trimws(LoincCode),
      ResultName = trimws(ResultName),
      Range = trimws(Range),
      BPCode = as.numeric(BPCode),
      Units = trimws(Units),
      # "ResultValue = as.numeric(ResultValue),"
      # this coercion only works in my modified version of dbplyr 1.4.2
      # (if there are non-numeric characters in ResultValue)
      # modified dbplyr translates as.numeric (and as.Date) as a 'try_cast'
      # the default sample database contains ResultValue which are
      # not purely numeric e.g. "<0.5", and results in an error
      # when just trying to 'cast', if those characters are not removed
      ResultValue = trimws(ResultValue),
      # get rid of leading "<" or ">", this will result in an 'assigned' value
      # equal to the limit of the test
      ResultValue = dplyr::case_when(
        substr(ResultValue, 1, 1) %LIKE% "%[<>]%" ~
        substr(ResultValue, 2, 100), # assume nchar <= 100
        # doesn't accept nchar(ResultValue)
        TRUE ~ ResultValue
      )
    ) %>>%
    # need separate mutate to work after trimming "<" and ">" from ResultValue
    dplyr::mutate(ResultValue = as.double(ResultValue))
  # modified version of dbplyr also required for 'as.double' to cast as a FLOAT
  # the default NUMERIC rounds/?truncates ResultValue to an integer

  # BPCode
  #  1 - HbA1C
  #  2- Cholesterol, 3 - HDL cholesterol, 4 - LDL cholesterol, 5 - triglycerides
  #  6 - Creatinine, 7 - Urine Albumin, 12 - INR, 14 - Glucose (Serum)
  #  16 - eGFR
  #  17 - Albumin/Creatinine ratio, 18 - UAE, 19 - HbA1C (SI)
  #
  #  16 - Diabetes Cycle of Care page records in "mL/min" units
  #
  #  17 variously labelled 'ACR' or 'Albumin/Creat Ratio' in SAMPLES database
  #   units will be recorded e.g. mg/mmol
  #
  #  18 "UAE"
  #  units:
  #   "mcg/min"
  #
  #  7 "Microalbuminuria"
  #   units can be "g/day" "mg/L" "mg/mmol" "mcg/min"
  #  this might be simultaneously recorded (from the Diabetes Cycle of Care Page)
  #   as BPCode 18, with the same ReportDate and ReportID!, if units are "mcg/min"

  self$db$services <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "BPS_Services")) %>>%
    dplyr::select(
      "InternalID" = "INTERNALID", "ServiceDate" = "SERVICEDATE",
      "MBSItem" = "MBSITEM", "Description" = "DESCRIPTION"
    )

  self$db$servicesRaw <- emr_db$conn() %>>%
    # PAYERCODE = 0 unknown
    # PAYERCODE = 1 private (patient)
    # PAYERCODE = 2 bulk-billing (Medicare direct billing)
    # PAYERCODE = 3 DVA
    # PAYERCODE = 4 WorkCover
    # PAYERCODE = 5 private (head of family)
    # PAYERCODE = 8 private (other)
    dplyr::tbl(dbplyr::in_schema("dbo", "SERVICES")) %>>%
    dplyr::filter(SERVICESTATUS != 9, RECORDSTATUS != 2) %>>%
    # RECORDSTATUS 2 appears to be cancelled services
    # SERVICESTATUS 9 appears to be 'reversal' of services
    dplyr::select(
      "InvoiceID" = "INVOICEID", "ServiceDate" = "SERVICEDATE",
      "MBSItem" = "MBSITEM", "Description" = "DESCRIPTION",
      "PayerCode" = "PAYERCODE"
    )

  self$db$invoices <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "INVOICES")) %>>%
    dplyr::select(
      InvoiceID = INVOICEID, UserID = USERID, Total = TOTAL,
      InternalID = INTERNALID, SENTTOWORKCOVER
    )
  # some versions of BP appear to require an extraneous field
  # so that UserID/InternalID are not converted to zeros!
  # in this case, the extraneous field is 'SENTTOWORKCOVER'
  # Total is in cents

  self$db$history <- emr_db$conn() %>>%
    # InternalID, Year, Condition, ConditionID, Status
    dplyr::tbl(dbplyr::in_schema("dbo", "BPS_History")) %>>%
    dplyr::select(
      InternalID, Year,
      Condition, ConditionID, Status
    )

  self$db$currentRx <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "BPS_CurrentRx")) %>>%
    dplyr::select(
      InternalID, DrugName, Dose, Frequency, PRN,
      Route, Quantity, ProductUnit, Repeats, Indication,
      LastDate, ProductID
    ) %>>%
    dplyr::mutate(
      DrugName = trimws(DrugName),
      Dose = trimws(Dose),
      Frequency = trimws(Frequency),
      PRN = trimws(PRN),
      ProductUnit = trimws(ProductUnit),
      Indication = trimws(Indication),
      LastDate = as.Date(LastDate)
    )

  self$db$relationcode <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "RELATIONS")) %>>%
    dplyr::select(
      RelationCode = RELATIONCODE,
      RelationName = RELATIONNAME
    ) %>>%
    dplyr::mutate(RelationName = trimws(RelationName))

  self$db$familyhistorydetail <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "BPS_FamilyHistoryDetail")) %>>%
    dplyr::select(InternalID,
      Relation = RelationName,
      Condition, DiseaseCode,
      DiseaseComment = Comment
    ) %>>%
    dplyr::mutate(
      Relation = trimws(Relation),
      Condition = trimws(Condition),
      DiseaseComment = trimws(DiseaseComment)
    )

  self$db$familyhistory <- emr_db$conn() %>>%
    # incorporates db$familyhistorydetail
    dplyr::tbl(dbplyr::in_schema("dbo", "FAMILYHISTORY")) %>>%
    dplyr::select(
      InternalID = INTERNALID, Unknown = ADOPTED,
      # Unknown : 0 - FALSE, 1 = TRUE
      FatherAlive = PATALIVE, MotherAlive = MATALIVE,
      # 0 - unknown, 1 - No, 2 - Yes
      FatherAgeAtDeath = PATAGEATDEATH, MotherAgeAtDeath = MATAGEATDEATH,
      FatherCauseOfDeath = PATCAUSEOFDEATH,
      MotherCauseOfDeath = MATCAUSEOFDEATH,
      FatherCauseOfDeathCode = PATCAUSEOFDEATHCODE,
      MotherCauseOfDeathCode = MATCAUSEOFDEATHCODE,
      Comment = FHCOMMENT
    ) %>>%
    # unfortunately the CREATED column does not have the date of first entry
    # so there is no accurate date of first entry
    dplyr::mutate(
      FatherCauseOfDeath = trimws(FatherCauseOfDeath),
      MotherCauseOfDeath = trimws(MotherCauseOfDeath),
      Comment = trimws(Comment)
    ) %>>%
    dplyr::left_join(self$db$familyhistorydetail,
      by = "InternalID"
    )
  # after joining with db$familyhistorydetail,
  # there may be multiple rows per InternalID, each with a different relative


  self$db$observations <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "BPS_Observations")) %>>%
    dplyr::select(
      InternalID, RECORDID, ObservationCode, ObservationName,
      ObservationDate, ObservationTime, ObservationValue
    ) %>>%
    dplyr::mutate(
      ObservationDate = as.Date(ObservationDate),
      ObservationName = trimws(ObservationName),
      ObservationValue = trimws(ObservationValue)
    )

  # ObservationCode
  #  1 - temp, 2 - pulse (rate)
  #  3 - systolic blood pressure, 4 - diastolic blood pressure
  #  6 - BSL, 7 - Height, 8 - Weight, 9 - BMI
  #  10 - Head circumference
  #  17 - Waist, 18 - Hip
  #  21 - WHRatio, 26 - DiabRisk

  self$db$currentRx_raw <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "CURRENTRX")) %>>%
    dplyr::select(
      "InternalID" = "INTERNALID", "PRODUCTID",
      "DRUGNAME", "RXSTATUS"
    )
  # RXSTATUS appears to be 1 if 'long-term' and 2 if 'short-term'

  self$db$obgyndetail <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "OBSGYNDETAIL")) %>>%
    dplyr::select(
      "InternalID" = "INTERNALID", "NOMINALLMP",
      "LASTPAPDATE", "LASTPAPRESULT", "BREASTFEEDING",
      "MammogramStatus", "LastMammogramDate", "MammogramResult"
    )

  self$db$pregnancies <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "PREGNANCIES")) %>>%
    dplyr::select(
      InternalID = INTERNALID, EDCbyDate = EDCBYDATE, EDCbyScan = EDCBYSCAN,
      UseScan = USESCAN, ActualLMP = ACTUALLMP, NominalLMP = NOMINALLMP,
      EndDate = ENDDATE, OutcomeCode = OUTCOMECODE
    ) %>>%
    dplyr::mutate(
      EDCbyDate = as.Date(EDCbyDate), EDCbyScan = as.Date(EDCbyScan),
      ActualLMP = as.Date(ActualLMP), NominalLMP = as.Date(NominalLMP),
      EndDate = as.Date(EndDate)
    )
  #   OutcomeCode :0 = none recorded, 1 = "Live birth",
  #   2 = Miscarriage, 3 = Termination, 4 = Ectopic,
  #   5 = IUFD (intra-uterine fetal death), 6 = stillbirth
  #   7 = hydatiform mole
  #

  self$db$asthmaplan <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "ASTHMAPLAN")) %>>%
    dplyr::select(
      InternalID = INTERNALID,
      UserID = USERID,
      PlanDate = PLANDATE,
      BestPEFR = BESTPEFR,
      Mild = MILD, Moderate = MODERATE, Severe = SEVERE,
      Emergency = EMERGENCY, Exercise = EXERCISE
    ) %>>%
    dplyr::mutate(
      PlanDate = as.Date(PlanDate),
      Mild = trimws(Mild), Moderate = trimws(Moderate),
      Severe = trimws(Severe), Emergency = trimws(Emergency),
      Exercise = trimws(Exercise)
    )

  self$db$pcehrdocuments <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "PCEHRDOCUMENTS")) %>>%
    dplyr::select(
      InternalID = INTERNALID,
      UserID = USERID,
      DocumentType = DOCUMENTTYPE,
      # 0 = downloaded document
      # 1 = uploaded shared health summary
      # 2 = uploaded event
      DocumentDate = DOCUMENTDATE,
      Created = CREATED, CreatedBy = CREATEDBY,
      Updated = UPDATED, UpdatedBy = UPDATEDBY
    )

  self$db$dbversion <- self$db$dbversion + 1
  print(paste("dbversion:", self$db$dbversion))
  private$set_reactive(self$dbversion, self$db$dbversion)
})

##### other variables and methods #################

## fields
#' UserFullConfig
#'
#' Integrates the UserConfig in the SQLite configuration file
#' with the user information in the EMR database
#' (if the clinical database is not open, then only SQLite
#' information is returned)
#'
#' Contains user names attached to configuration information.
#' Contains ALL user names. Does NOT contain passwords.
#'
#' By contrast, private$.UserConfig() contains just names
#' who have been configured (in SQLite), including passwords
#'
#' @name UserFullConfig
.active(dMeasure, "UserFullConfig", function(value) {
  if (!missing(value)) {
    stop("Can't set `$UserFullConfig`", call. = FALSE)
    # read-only field
  }

  if (is.null(self$db$users)) {
    UserFullConfig <- self$UserConfig %>>%
      dplyr::mutate(
        Identifier = as.character(NA),
        LicenseDate = as.Date(NA, origin = "1970-01-01")
      )
    # just the .UserConfig except the passwords
    # mutate to the same shape even if database is not open
  } else {
    PracticeName <- self$db$practice %>>%
      dplyr::pull(PracticeName)
    PracticeName <- PracticeName[[1]] # just pull out the first entry
    UserFullConfig <- self$db$users %>>% dplyr::collect() %>>%
      # forces database to be read
      # (instead of subsequent 'lazy' read)
      # collect() required for mutation and left_join
      dplyr::mutate(
        Fullname =
          trimws(paste(Title, Firstname, Surname, sep = " "))
      ) %>>%
      # include 'Fullname'
      dplyr::left_join(self$UserConfig, by = "Fullname") %>>%
      # add user details including practice locations
      # next section decodes the LicenseDate
      # the Identifier is used in $read_subscription_db to interrogate the
      # license database
      # and is also used to help 'decode' the LicenseDate
      dplyr::mutate(Identifier = paste0(
        vapply(ProviderNo,
          # create verification string
          function(n) if (is.na(n) || nchar(n) == 0) {
              # practice name if no provider number
              PracticeName
            }
            else {
              n # the provider number
            },
          FUN.VALUE = character(1),
          USE.NAMES = FALSE
        ), "::",
        Fullname, "::"
      )) %>>%
      dplyr::mutate(
        LicenseDate =
          # decrypt License
        as.Date(mapply(function(y, z) {
          dMeasure::verify_license(y, z)
        }, License, Identifier, USE.NAMES = FALSE),
        origin = "1970-01-01"
        )
      )
  }

  return(UserFullConfig)
})

#' verify license/subscription
#'
#' @param License an encoded character string
#' @param Identifier a character string
#'  Identifier is converted to upper case
#'
#' @return a date object 'LicenseDate'. returns NA if not valid
#'
#' @export
verify_license <- function(License, Identifier) {
  if (is.na(License)) { # if NA for License
    LicenseDate <- NA # remain unchanged
  } else { # otherwise decode
    Identifier <- toupper(Identifier) # convert to upper-case
    zzz <- simple_decode(License, "karibuni") # this could return NULL if not valid
    if (!is.na(zzz) && substr(zzz, 1, nchar(Identifier)) == Identifier) {
      # left side of decrypted license must equal the Identifier
      LicenseDate <- substring(zzz, nchar(Identifier) + 1)
      # converts decrypted License (right side of string)
      # keep remainder of string, and convert to date
    } else {
      LicenseDate <- NA
      # invalid new license
    }
  }

  return(as.Date(LicenseDate, origin = "1970-01-01"))
}

##### location #####################################

## fields

.public(dMeasure, "location", "All") # location/group. by default, it is 'All'

## methods

#' Show list of locations
#'
#' This includes 'All'
#'
#' @param dMeasure_obj dMeasure R6 object
#'
#' @return the list of locations, including 'All'
#' @export
location_list <- function(dMeasure_obj) {
  dMeasure_obj$location_list
}

.active(dMeasure, "location_list", function(value) {
  if (!missing(value)) {
    stop("$location_list is read-only!")
  }
  locations <- c("All") # 'everyone', but not itself a group
  if (!is.null(private$PracticeLocations)) {
    locations <- c(
      locations,
      private$PracticeLocations %>>%
        dplyr::pull(Name)
    )
  }
  # set reactive versions, only if shiny is available
  private$set_reactive(self$location_listR, locations)
  private$set_reactive(self$location_groupR, locations[!locations %in% "All"])
  # exclude "All" in $locations_groupR
  private$set_reactive(
    self$PracticeLocationsR,
    as.data.frame(private$PracticeLocations)
  )

  return(locations)
})
.reactive(dMeasure, "location_listR", quote("All")) # includes 'All'
.reactive(dMeasure, "location_groupR", quote("")) # does not include 'All'
.reactive(dMeasure, "PracticeLocationsR", quote(data.frame(
  id = numeric(),
  Name = character(),
  Description = character()
)))
#' Choose location
#'
#' Location is used in subsequent list of clinicians available
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param location any of the practice locations/groups. can also be 'All'
#'
#' @return the location (after being updated, if possible)
#'
#' returns an error, and does not update the location, if trying to
#' set to an unavailable location
choose_location <- function(dMeasure_obj,
                            location = dMeasure_obj$location) {
  dMeasure_obj$choose_location(location)
}

.public(dMeasure, "choose_location", function(location = self$location) {
  locations <- self$location_list
  if (!(location %in% locations)) {
    stop(paste0("'", location, "' is not in the list of locations."))
  } else {
    self$location <- location
  }

  return(self$location)
})

##### date setting ####################################################

## fields

.public_init(dMeasure, "date_a", quote(Sys.Date())) # 'from' date. by default, it is 'today'
.public_init(dMeasure, "date_b", quote(Sys.Date())) # 'to' date

## methods

#' Choose date
#'
#' Sets 'from' and 'to' dates used in subsequent searches
#'
#' @param dMeasure_obj dateContact R6 object
#' @param date_from 'From' date. default is current date_from
#' @param date_to 'To' date. default is current date_to
#'
#' @return list(date_a, date_b)
#'
#' if date_a is later than date_b, a warning is returned,
#' and the dates are NOT changed
#' @export
choose_date <- function(dMeasure_obj,
                        date_from = dMeasure_obj$date_a,
                        date_to = dMeasure_obj$date_b) {
  dMeasure_obj$choose_date(date_from, date_to)
}

.public(dMeasure, "choose_date", function(date_from = self$date_a,
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
.reactive(dMeasure, "date_aR", quote(self$date_a))
.reactive(dMeasure, "date_bR", quote(self$date_b))
