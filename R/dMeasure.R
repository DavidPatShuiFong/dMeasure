##### dMeasure ###########################################
#' @include r6_helpers.R
#' @include dateContact.R
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
                initialize = function () {
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
  if (requireNamespace("shiny", quietly = TRUE)) {
    myreactive(value)
  }
})
.private(dMeasure, "trigger", function(myreactive) {
  # toggles a reactive between (usually) 0 and 1
  myreactive(1 - shiny::isolate(myreactive()))
})


##### close and finalize object ##########################

.public(dMeasure, "close", function() {
  # close any open database connections
  if (!is.null(private$.identified_user)) {
    self$user_logout()
  }
  if (private$config_db$is_open()) {
    if (private$config_db$keep_log) { # if currently logging
      log_id <- private$config_db$write_log_db(
        query = "Closing databases")
      private$config_db$close_log_db() # close logging database
    }
    private$config_db$close()

    # empty the configuration fields
    private$.BPdatabase <- private$BPdatabase[0,]
    private$.BPdatabaseChoice <- "None"
    private$PracticeLocations <- private$PracticeLocation[0,]
    private$.UserConfig <- private$.UserConfig[0,]
    private$.UserRestrictions <- private$.UserRestrictions[0,]
  }
  if (private$emr_db$is_open()) {
    if (private$emr_db$keep_log) { # if currently logging
      private$emr_db$close_log_db() # close logging database
    }
    private$emr_db$close()
    private$db$users <- NULL
    private$db$patients <- NULL
    private$db$investigations <- NULL
    private$db$appointments <- NULL
    private$db$immunizations <- NULL
    private$db$preventive_health <- NULL
    private$db$correspondenceIn <- NULL
    private$db$reportValues <- NULL
    private$db$services <- NULL
    private$db$history <- NULL
    self$clinician_choice_list <- NULL
  }
  self$authenticated = FALSE

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

#' read configuration filepaths
#'
#' By default, the YAML configuration is either in the working
#' directory (where a local installation of R lives),
#' or the user's home directory
#'
#' this method will set $yaml_config_filepath and $sql_config_filepath
#' it will read the YAML configuration filepath, which if already
#' existing might contain the 'real' location of the $sql_config_filepath
#'
#' returns the SQL filepath
#'
#' @name configuration_file_path
#'
#' @return SQL filepath
#'
#' @examples
#' dMeasure_obj <- dMeasure$new()
#' dMeasure_obj$configuration_file_path # read filepath
#' dMeasure_obj$configuration_file_path <- "c:/config.sqlite"
#'  # sets filepath
.active(dMeasure, "configuration_file_path", function (filepath) {

  self$yaml_config_filepath <- "~/.DailyMeasure_cfg.yaml"
  # the location the of the '.yaml' configuration file
  # always in the user's home directory
  if (!missing(filepath)) {
    # the 'new' configuration filepath is being set
    self$close() # close any open database connections
    self$sql_config_filepath <- filepath # set the new config filepath

    private$local_config <- list()
    private$local_config$config_file <- self$sql_config_filepath
    # main configuration file, could (potentially) be set to 'common location'
  } else {
    # no configuration filepath has been provided
    # read the old configuration filepath, or create one
    if (configr::is.yaml.file(self$yaml_config_filepath)) {
      # if config file exists and is a YAML-type file
      private$local_config <- configr::read.config(self$yaml_config_filepath)
      self$sql_config_filepath <- private$local_config$config_file
      # config in local location
    } else {
      # local config file does not exist. possibly first-run
      if (grepl("Program Files", normalizePath(R.home()))) {
        # this is a system-wide install
        self$sql_config_filepath <- "~/.DailyMeasure_cfg.sqlite"
        # store in user's home directory
      } else {
        # this is a 'local' user install, not a system-wide install
        # e.g. C:/Users/MyName/AppData/Programs/...
        # as opposed to 'C:/Program Files/...'
        self$sql_config_filepath <- "./.DailyMeasure_cfg.sqlite"
        # this file can be stored in the AppData folder, out of sight of the user
      }
      private$local_config <- list()
      private$local_config$config_file <- self$sql_config_filepath
      # main configuration file, could (potentially) be set to 'common location'
    }
  }

  # write the (minimalist) local config file
  if (!configr::is.yaml.file(self$yaml_config_filepath) | !missing(filepath)) {
    # either there is no .YAML configuration file,
    # or the .sqlite filepath has been changed
    configr::write.config(
      private$local_config,
      file.path = self$yaml_config_filepath,
      write.type = "yaml"
    )
  }

  private$set_reactive(self$configuration_file_pathR, self$sql_config_filepath)

  return(self$sql_config_filepath)
})
.reactive(dMeasure, "configuration_file_pathR", NULL)

##### Configuration details - databases, locations, users ###########

## Fields
.private_init(dMeasure, "config_db", quote(dbConnection::dbConnection$new()))
# R6 connection to database
# using either DBI or pool
.reactive(dMeasure, "config_db_trigR", 0)
# $config_db_trigR will trigger (0/1) with each configuration
# database change

.private(dMeasure, ".BPdatabase", data.frame(id = integer(),
                                             Name = character(),
                                             Address = character(),
                                             Database = character(),
                                             UserID = character(),
                                             dbPassword = character(),
                                             stringsAsFactors = FALSE))
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
.reactive(dMeasure, "BPdatabaseR", quote(data.frame(id = integer(),
                                                    Name = character(),
                                                    Address = character(),
                                                    Database = character(),
                                                    UserID = character(),
                                                    dbPassword = character(),
                                                    stringsAsFactors = FALSE)))
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
.private(dMeasure, "PracticeLocations", data.frame(id = integer(),
                                                   Name = character(),
                                                   Description = character(),
                                                   stringsAsFactors = FALSE))
# id needed for editing this dataframe later
# need default value for practice location filter
# interface initialization
.private(dMeasure, ".UserConfig", data.frame(id = integer(),
                                             Fullname = character(), AuthIdentity = character(),
                                             Location = character(),
                                             Attributes = character(),
                                             Password = character(),
                                             stringsAsFactors = FALSE))
.active(dMeasure, "UserConfig", function(value) {
  if (!missing(value)) {
    stop("self$UserConfig is read-only!")
  }

  userconfig <- private$.UserConfig %>>%
    dplyr::select(-Password) # same as $.UserConfig, except the password

  private$set_reactive(self$UserConfigR, userconfig) # set reactive version
  return(userconfig)
})
.reactive(dMeasure, "UserConfigR", quote(data.frame(id = integer(), Fullname = character(),
                                                    AuthIdentity = character(),
                                                    Location = character(),
                                                    Attributes = character())))

.private(dMeasure, ".UserRestrictions", data.frame(uid = integer(),
                                                   Restriction = character(),
                                                   stringsAsFactors = FALSE))

.reactive(dMeasure, "UserRestrictions", quote(data.frame(uid = integer(),
                                                         Restriction = character(),
                                                         stringsAsFactors = FALSE)))

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
#' Tries to open the databse. If fails, will be set to 'None'.
#'
#' Sets $BPdatabasechoiceR reactive, if shiny/reactive
#' environment available
#'
#' (Stored in private$.BPdatabaseChoice)
#'
#' @name BPdatabaseChoice
#'
#' @return the current database choice
#'
#' @examples
#' dMeasure_obj$BPdatabaseChoice # returns the current choice
#' dMeasure_obj$BPdatabaseChoice <- "None" # sets database to none
.active(dMeasure, "BPdatabaseChoice", function(choice) {
  if (missing(choice)) {
    return(private$.BPdatabaseChoice)
  } else {
    if (!(choice %in% c("None", private$.BPdatabase$Name))) {
      stop(paste0("Database choice must be one of ",
                  paste0("'", private$.BPdatabase$Name, "'", collapse = ", "),
                  " or 'None'."))
    }

    # close existing database connection
    # safe to call $close() if no database is open

    if (choice == private$.BPdatabaseChoice) {
      # do nothing
      return(private$.BPdatabaseChoice)
    }
    # the chosen database is not the current database,
    # so close the current database
    private$emr_db$close()

    if (choice == "None") {
      # do nothing
    } else if (!is.null(choice)) {
      server <- private$.BPdatabase %>>%
        dplyr::filter(Name == choice) %>>%
        dplyr::collect()
      print("Opening EMR database")
      private$emr_db$connect(odbc::odbc(), driver = "SQL Server",
                             server = server$Address, database = server$Database,
                             uid = server$UserID,
                             pwd = dMeasure::simple_decode(server$dbPassword))
    }

    if (!private$emr_db$is_open() || !DBI::dbIsValid(private$emr_db$conn())) {
      # || 'short-circuits' the evaluation, so if not an environment,
      # then dbIsValid() is not evaluated (will return an error if emr_db$conn() is NULL)

      # either database not opened, or has just been closed, including set to 'None'
      private$db$users <- NULL
      private$db$patients <- NULL
      private$db$investigations <- NULL
      private$db$appointments <- NULL
      private$db$immunizations <- NULL
      private$db$preventive_health <- NULL
      private$db$correspondenceIn <- NULL
      private$db$reportValues <- NULL
      private$db$services <- NULL
      private$db$servicesRaw <- NULL
      private$db$invoices <- NULL
      private$db$history <- NULL
      self$clinician_choice_list <- NULL
      choice <- "None" # set choice of database to 'None'
    } else {
      if (self$Log) {
        log_id <- private$config_db$write_log_db(
          query = "opened EMR database",
          data = choice)
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

    if (nrow(private$config_db$conn() %>>% dplyr::tbl("ServerChoice") %>>%
             dplyr::filter(id == 1) %>>% dplyr::collect()) == 0) {
      # create a new entry
      query <- "INSERT INTO ServerChoice (id, Name) VALUES (?, ?)"
      data_for_sql <- as.list.data.frame(c(1, private$.BPdatabaseChoice))
      private$config_db$dbSendQuery(query, data_for_sql)
      # write to SQLite configuration database
    }

    if ((private$config_db$conn() %>>% dplyr::tbl("ServerChoice") %>>%
         dplyr::filter(id == 1) %>>%
         dplyr::pull(Name)) != private$.BPdatabaseChoice) {
      # if new choice is not recorded in current configuration database
      # already an entry in the ServerChoice table
      query <- "UPDATE ServerChoice SET Name = ? WHERE id = ?"
      data_for_sql <- as.list.data.frame(c(private$.BPdatabaseChoice, 1))
      private$config_db$dbSendQuery(query, data_for_sql)
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
#' both these parameters have defaults, which may have
#' been set by previous calls
open_configuration_db <-
  function(dMeasure_obj,
           configuration_file_path = dMeasure_obj$configuration_file_path) {
    dMeasure_obj$open_configuration_db(configuration_file_path)
  }

.public(dMeasure, "open_configuration_db", function (
  configuration_file_path = self$configuration_file_path) {

  # if no configuration filepath is defined, then try to read one
  if (length(configuration_file_path) == 0) {
    configuration_file_path <- self$configuration_file_path
  }

  config_db <- private$config_db # for convenience

  if (file.exists(configuration_file_path)) {
    # open config database file
    config_db$connect(RSQLite::SQLite(),
                      dbname = self$configuration_file_path)
  } else {
    # if the config database doesn't exist,
    # then create it (note create = TRUE option)
    config_db$connect(RSQLite::SQLite(),
                      dbname = self$configuration_file_path)
    # create = TRUE not a valid option?
    # always tries to create file if it doesn't exist
  }

  initialize_data_table = function(config_db, tablename, variable_list ) {
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
      columns <- config_db$conn() %>>% dplyr::tbl(tablename) %>>% colnames()
      # list of column (variable) names
      data <- config_db$conn() %>>% dplyr::tbl(tablename) %>>% dplyr::collect()
      # get a copy of the table's data
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
    initialize_data_table(config_db, "Server",
                          list(c("id", "integer"),
                               c("Name", "character"),
                               c("Address", "character"),
                               c("Database", "character"),
                               c("UserID", "character"),
                               c("dbPassword", "character")))
    # initialize_data_table will create table and/or
    # ADD 'missing' columns to existing table

    initialize_data_table(config_db, "ServerChoice",
                          list(c("id", "integer"),
                               c("Name", "character")))
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

    initialize_data_table(config_db, "LogSettings",
                          list(c("id", "integer"), # will always be '1'
                               c("Log", "integer"),
                               c("Filename", "character")))
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

    initialize_data_table(config_db, "Location",
                          list(c("id", "integer"),
                               c("Name", "character"),
                               c("Description", "character")))

    initialize_data_table(config_db, "Users",
                          list(c("id", "integer"),
                               c("Fullname", "character"),
                               c("AuthIdentity", "character"),
                               c("Location", "character"),
                               c("Password", "character"),
                               c("Attributes", "character")))

    initialize_data_table(config_db, "UserRestrictions",
                          list(c("uid", "integer"),
                               c("Restriction", "character")))
    # list of restrictions for users
    # use of 'uid' rather than 'id'
    # (this relates to the 'Attributes' field in "Users")
  }
  invisible(self)
})

#' read the SQL configuration database
#'
#' @param dMeasure_obj dMeasure object
#' @param config_db R6 object to open SQL database - default is the internally stored value
read_configuration_db <- function(dMeasure_obj,
                                  config_db = dMeasure_obj$config_db) {
  dMeasure_obj$read_configuration_db(config_db)
}

.public(dMeasure, "read_configuration_db", function(config_db = private$config_db) {

  if (!config_db$is_open()) {
    # if config_db is not yet opened/defined
    # then try to open configuration database
    self$open_configuration_db()
    config_db <- private$config_db
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
    dplyr::tbl("Location")
  invisible(self$location_list)
  # $location_list() will refresh the reactive location_listR if available

  private$.UserConfig <- config_db$conn() %>>%
    dplyr::tbl("Users") %>>%
    # in .UserConfig, there can be multiple Locations/Attributes per user
    dplyr::collect() %>>%
    dplyr::mutate(Location = stringi::stri_split(Location, regex = ";"),
                  Attributes = stringi::stri_split(Attributes, regex = ";"))
  invisible(self$UserConfig) # this will also set $userConfigR reactive version

  private$.UserRestrictions <- config_db$conn() %>>%
    dplyr::tbl("UserRestrictions") %>>% dplyr::collect()
  self$UserRestrictions(private$.UserRestrictions)

  self$match_user()
  # see if 'identified' system user is matched with a configured user

  private$trigger(self$config_db_trigR)
  # notification of configuration database change

  invisible(self)
})
.public(dMeasure, "BPdatabaseChoice_new", function() {
  if (private$config_db$is_open()) {
    # config database is open
    new <- private$config_db$conn() %>>% dplyr::tbl("ServerChoice") %>>%
      dplyr::filter(id == 1) %>>% dplyr::pull(Name)
  } else {
    # config database is not open
    new <- self$BPdatabaseChoice # the current choice
  }
  return(new)
})


##### User login ##################################################

## fields
.private(dMeasure, ".identified_user", data.frame(id = integer(), Fullname = character(),
                                                  AuthIdentity = character(), Location = character(),
                                                  Password = character(), Attributes = character()))
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
  private$.identified_user <-
    private$.UserConfig[private$.UserConfig$AuthIdentity == Sys.info()[["user"]],]
  private$set_reactive(self$identified_user,
                       private$.identified_user %>>%
                         dplyr::select(Fullname, AuthIdentity, Location, Attributes)
  )
  # set reactive version if reactive (shiny) environment available
  # does not include password

  if ("RequirePasswords" %in% unlist(private$.UserRestrictions$Restriction)) {
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
  list(restriction = "GlobalActionView",
       view_to_hide = list("immunization", "cancerscreen")),
  list(restriction = "GlobalBillView",
       view_to_hide = list("billings")),
  list(restriction = "GlobalCDMView",
       view_to_hide = list("cdm"))
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
  if (self$location == 'All') {
    # note that 'ifelse' only returns result in the
    # same 'shape' as the comparison statement
    clinician_list <- self$UserFullConfig$Fullname
  } else {
    clinician_list <- subset(private$.UserConfig$Fullname,
                             sapply(private$.UserConfig$Location,
                                    function (y) self$location %in% y))
    # filter clinicians by location choice
    # it is possible for a clinician to have multiple locations
    # initially, $Location might include a lot of NA
    #
    # if filtered, then only configured users can be in the list
  }

  for (restriction in self$view_restrictions) {
    # go through list of view restrictions
    if (restriction$restriction %in% unlist(private$.UserRestrictions$Restriction)) {
      # if the restriction has been activated
      if (view_name %in% restriction$view_to_hide) {
        # if the relevant view is being shown
        if (self$authenticated == FALSE |
            !(restriction$restriction %in% unlist(private$.identified_user$Attributes))) {
          # if user is not authenticated or
          # if the current user does not have this 'Global' attribute
          # then can only view one's own appointments
          clinician_list <- subset(clinician_list,
                                   clinician_list == private$.identified_user$Fullname)
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
.private_init(dMeasure, "emr_db", quote(dbConnection::dbConnection$new()))
# R6 object containing database object
.private(dMeasure, "db", list(dbversion = 0)) # later will be the EMR databases.
# $db$dbversion is number of EMR database openings
# there is also a 'public reactive' version if shiny is available
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
open_emr_db <- function(dMeasure_obj,
                        BPdatabaseChoice = dMeasure_obj$BPdatabaseChoice) {
  dMeasure_obj$open_emr_db(BPdatabaseChoice)
}

.public(dMeasure, "open_emr_db",function(BPdatabaseChoice = NULL) {

  if (!private$config_db$is_open() || length(self$BPdatabaseChoice) == 0) {
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
#' @param dMeasure_obj dMeasure object
#' @param emr_db R6 object connecting to EMR database
initialize_emr_tables <- function(dMeasure_obj,
                                  emr_db = dMeasure_obj$emr_db) {
  dMeasure_obj$initialize_emr_tables(emr_db)
}

.public(dMeasure, "initialize_emr_tables", function(emr_db = private$emr_db) {

  print("Re-initializing databases")

  private$db$users <- emr_db$conn() %>>%
    # this is a function! a collect() is later called prior to mutate/join,
    # (as a result is no longer a 'lazy eval') and cannot be evaluated just once.
    # output - Fullname, UserID, Surname, Firstname, LocationName, Title, ProviderNo
    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_Users')) %>>%
    dplyr::select(c('UserID', 'Surname', 'Firstname',
                    'LocationName', 'Title', 'ProviderNo'))
  invisible(self$UserConfig)
  # will also set $UserConfigR reactive
  # does not include password in public/reactive

  private$db$patients <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_Patients'))

  private$db$investigations <- emr_db$conn() %>>%
    # output - InternalID, Collected (Date), TestName
    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_Investigations')) %>>%
    dplyr::select(InternalID, ReportID,
                  TestName,
                  Reported, Checked, Actioned,
                  # three dates
                  CheckedBy,
                  # a name of the provider who checked
                  Notation, Action,
                  # Action includes 'Urgent Appointment' and 'Non-urgent Appointment'
                  Comment)
  # as of Jan/2019, the odbc engine for MSSQL can't handle the
  # full ('Select *') Investigations table
  # due to some type of bug/standards non-compliance.
  # also can handle the History table. need to
  # 'Select' out just a few columns.

  private$db$appointments <- emr_db$conn() %>>%
    # Patient, InternalID, AppointmentDate, AppointmentTime, Provider, Status
    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_Appointments')) %>>%
    dplyr::select(c('Patient', 'InternalID',
                    'AppointmentDate', 'AppointmentTime',
                    'Provider', 'Status'))
  # Status : 'Booked', 'Completed', 'At billing', 'Waiting', 'With doctor'

  private$db$visits <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_Visits')) %>>%
    dplyr::select(InternalID, VisitType, VisitDate, UserID, DrName) %>>%
    dplyr::mutate(VisitType = trimws(VisitType),
                  DrName = trimws(DrName))
  # VisitType : 'Surgery', 'Home', "Non Visit', 'Hospital', 'RACF', 'Telephone'
  # ... 'SMS', 'Email', 'Locum Service', 'Out of Office', 'Other', 'Hostel'
  # ... 'Telehealth'

  private$db$immunizations <- emr_db$conn() %>>%
    # InternalID, GivenDate, VaccineName, VaccineID
    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_Immunisations')) %>>%
    dplyr::select(c('InternalID', 'GivenDate', 'VaccineName', 'VaccineID'))

  private$db$vaccine_disease <- emr_db$conn() %>>%
    # vaccineIDs linked to diseases
    # e.g. diseasecode 7+30 are for influenza vaccines
    dplyr::tbl(dbplyr::in_schema("bpsdrugs.dbo", "VACCINE_DISEASE")) %>>%
    dplyr::select("VACCINEID", "DISEASECODE")

  private$db$preventive_health <- emr_db$conn() %>>%
    # INTERNALID, ITEMID (e.g. not for Zostavax remindders)
    dplyr::tbl(dbplyr::in_schema('dbo', 'PreventiveHealth')) %>>%
    dplyr::select('InternalID' = 'INTERNALID', 'ITEMID')

  private$db$correspondenceIn <- emr_db$conn() %>>%
    # InternalID, CorrespondenceDate, Subject, Detail
    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_CorrespondenceIn')) %>>%
    dplyr::select(InternalID, DocumentID,
                  CorrespondenceDate,
                  Subject, Detail, Comment)

  private$db$correspondenceInRaw <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "CORRESPONDENCEIN")) %>>%
    dplyr::select(DOCUMENTID, INTERNALID,
                  USERID, CHECKEDBY,
                  # both USERID and CHECKEDBY are numbers, not names
                  CORRESPONDENCEDATE, CHECKDATE, ACTIONDATE,
                  # three dates
                  CATEGORY, SUBJECT, DETAIL, COMMENT,
                  NOTATION, ACTION)
  # Action includes 6 for Non-urgent appointment,
  # and 7 for Urgent appointment

  private$db$reportValues <- emr_db$conn() %>>%
    # InternalID, ReportDate, ResultName, LoincCode
    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_ReportValues')) %>>%
    dplyr::select('InternalID', 'ReportDate', 'ResultName', 'LoincCode')

  private$db$services <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_SERVICES')) %>>%
    dplyr::select('InternalID' = 'INTERNALID', 'ServiceDate' = 'SERVICEDATE',
                  'MBSItem' = 'MBSITEM', 'Description' = 'DESCRIPTION')

  private$db$servicesRaw <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema('dbo', 'SERVICES')) %>>%
    dplyr::select('InvoiceID' = 'INVOICEID', 'ServiceDate' = 'SERVICEDATE',
                  'MBSItem' = 'MBSITEM', 'Description' = 'DESCRIPTION')

  private$db$invoices <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema('dbo', 'INVOICES')) %>>%
    dplyr::select('InvoiceID' = 'INVOICEID', 'UserID' = 'USERID',
                  'InternalID' = 'INTERNALID')

  private$db$history <- emr_db$conn() %>>%
    # InternalID, Year, Condition, ConditionID, Status
    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_History')) %>>%
    dplyr::select('InternalID', 'Year',
                  'Condition', 'ConditionID', 'Status')

  private$db$observations <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "OBSERVATIONS")) %>>%
    dplyr::select('InternalID' = 'INTERNALID', 'DATANAME',
                  'DATACODE', 'DATAVALUE', 'OBSDATE')

  private$db$currentrx <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "CURRENTRX")) %>>%
    dplyr::select('InternalID' = 'INTERNALID', 'PRODUCTID',
                  'DRUGNAME', 'RXSTATUS')
  # RXSTATUS appears to be 1 if 'long-term' and 2 if 'short-term'

  private$db$obgyndetail <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "OBSGYNDETAIL")) %>>%
    dplyr::select('InternalID' = 'INTERNALID', 'NOMINALLMP',
                  'LASTPAPDATE', 'LASTPAPRESULT', 'BREASTFEEDING',
                  'MammogramStatus', 'LastMammogramDate', 'MammogramResult')

  private$db$pregnancies <- emr_db$conn() %>>%
    dplyr::tbl(dbplyr::in_schema("dbo", "PREGNANCIES")) %>>%
    dplyr::select('InternalID' = 'INTERNALID', 'EDCBYDATE',
                  'ACTUALLMP', 'NOMINALLMP', 'ENDDATE')

  private$db$dbversion <- private$db$dbversion + 1
  print(paste("dbversion:", private$db$dbversion))
  private$set_reactive(self$dbversion, private$db$dbversion)
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
#'
.active(dMeasure, "UserFullConfig", function(value) {
  if (!missing(value)) {
    stop("Can't set `$UserFullConfig`", call. = FALSE)
    # read-only field
  }

  if (is.null(private$db$users)) {
    UserFullConfig <- private$.UserConfig %>>%
      dplyr::select(-Password)
    # just the .UserConfig except the passwords
  } else {
    UserFullConfig <- private$db$users %>>% dplyr::collect() %>>%
      # forces database to be read
      # (instead of subsequent 'lazy' read)
      # collect() required for mutation and left_join
      dplyr::mutate(Title = trimws(Title),
                    Firstname = trimws(Firstname),
                    Surname = trimws(Surname)) %>>%
      dplyr::mutate(Fullname =
                      paste(Title, Firstname, Surname, sep = ' ')) %>>%
      # include 'Fullname'
      dplyr::left_join(private$.UserConfig, by = 'Fullname') %>>%
      # add user details including practice locations
      dplyr::select(-Password) # removes the password field
  }

  return(UserFullConfig)
})

##### location #####################################

## fields

.public(dMeasure, "location", "All")    # location/group. by default, it is 'All'

## methods

#' Show list of locations
#'
#' This includes 'All'
#'
#' @param dMeasure_obj dMeasure R6 object
#'
#' @return the list of locations, including 'All'
location_list <- function(dMeasure_obj) {
  dMeasure_obj$location_list()
}

.active(dMeasure, "location_list", function(value) {
  if (!missing(value)) {
    stop("$location_list is read-only!")
  }
  locations <- data.frame(Name = "All", stringsAsFactors = FALSE)
  if (!is.null(private$PracticeLocations)) {
    locations <-
      rbind(locations,
            as.data.frame(private$PracticeLocations %>>%
                            dplyr::select(Name))) %>>%
      dplyr::mutate(Name = trimws(Name)) %>>% # remove whitespace
      unlist(use.names = FALSE)
  }
  # set reactive versions, only if shiny is available
  private$set_reactive(self$location_listR, locations)
  private$set_reactive(self$location_groupR, locations[!locations %in% "All"])
  # exclude "All" in $locations_groupR
  private$set_reactive(self$PracticeLocationsR,
                       as.data.frame(private$PracticeLocations))

  return(locations)

})
.reactive(dMeasure, "location_listR", quote("All")) # includes 'All'
.reactive(dMeasure, "location_groupR", quote("")) # does not include 'All'
.reactive(dMeasure, "PracticeLocationsR", quote(data.frame(id = numeric(),
                                                           Name = character(),
                                                           Description = character())))
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

##### date and contact ####################################################
.public_init(dMeasure, "dateContact", dateContact$new())

.public(dMeasure, "choose_date", function(date_from = self$dateContact$date_a,
                                          date_to = self$dateContact$date_b) {
  # just a 'dummy' for the dateContact$choose_date method

  return(self$dateContact$choose_date(date_from = date_from, date_to = date_to))
})
