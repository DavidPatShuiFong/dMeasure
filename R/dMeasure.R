#' dMeasure class
#' @title dMeasure class
#' @description case-finding in EMR (Best Practice)
#' @field yaml_config_filepath - filepath of YAML configuration
#' @field sql_config_filepath - filepath of SQL configuration (NULL is not connected)
#' @field local_config - in-memory copy of YAML configuration
#'
#' @section Methods:
#' \itemize{
#' \item{\code{\link{read_configuration_filepaths}} : read (or initiate) YAML/SQL DB filepaths}
#' \item{\code{\link{open_configuration_db}} : open SQLite configuration database}
#' \item{\code{\link{read_configuration_db}} : read SQLite configuration database}
#' \item{\code{\link{open_emr_db}} : open Best Practice database}
#' \item{\code{\link{initialize_emr_tables}} : configure Best Practice datatables}
#' \item{\code{\link{update_UserFullConfig}} : create 'full' user configuratio table}
#' \item{\code{\link{update_date}} : change, or read, search date range}
#' \item{\code{\link{location_list}} : list practice locations/groups}
#' \item{\code{\link{update_location}} : change, or read, current location}
#' }
#'
#' @examples
#'
#' @include calculation_definitions.R
#'
#' @export
dMeasure <-
  R6::R6Class("dMeasure",
              public = list()
              # this is a 'skeleton' class
              # it is filled in the with the '.public' function
  )

##### function to fill in the class ######################
.public <- function(...) dMeasure$set("public", ...)


##### close and finalize object ##########################

.public("close", function() {
  # close any open database connections
  if (!is.null(self$config_db)) {
    self$config_db$close()
    self$config_db <- NULL
  }
  if (!is.null(self$emr_db)) {
    self$emr_db$close()
    self$emr_db <- NULL
  }
  invisible(self)
})

.public("finalize", function() {
  # object being destroyed/removed
  # close all open connections
  self$close()
})

##### Configuration file location ########################
## fields

.public("yaml_config_filepath", NULL)
.public("sql_config_filepath", NULL)
.public("local_config", NULL)

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
#' finally sets $configuration_file_path to 'real' SQL filepath
#'
#' @param dMeasure_obj dMeasure object
#'
#' @return nothing, modifies \code{dMeasure_obj}
#'
#' @examples
#' dMeasure_obj <- dMeasure$new()
#' dMeasure_obj$read_configuration_filepaths()
#'
#' read_configuration_filepaths(dMeasure_obj)
read_configuration_filepaths <- function(dMeasure_obj) {
  dMeasure_obj$read_configuration_filepaths()
  invisible()
}

# methods
.public("read_configuration_filepaths", function () {
  if (grepl("Program Files", normalizePath(R.home()))) {
    # this is a system-wide install
    self$yaml_config_filepath <- "~/.DailyMeasure_cfg.yaml"
    self$sql_config_filepath <- "~/.DailyMeasure_cfg.sqlite"
    # store in user's home directory
  } else {
    # this is a 'local' user install, not a system-wide install
    # e.g. C:/Users/MyName/AppData/Programs/...
    # as opposed to 'C:/Program Files/...'
    self$yaml_config_filepath <- "./DailyMeasure_cfg.yaml"
    self$sql_config_filepath <- "./DailyMeasure_cfg.sqlite"
    # this file can be stored in the AppData folder, out of sight of the user
  }

  if (configr::is.yaml.file(self$yaml_config_filepath)) {
    # if config file exists and is a YAML-type file
    self$local_config <- configr::read.config(self$yaml_config_filepath)
    # config in local location
  } else {
    # local config file does not exist. possibly first-run
    self$local_config <- list()
    self$local_config$config_file <- self$sql_config_filepath
    # main configuration file, could be set to 'common location'
    # write the (minimalist) local config file
    configr::write.config(
      self$local_config,
      file.path = self$yaml_config_filepath,
      write.type = "yaml"
    )
  }
  self$configuration_file_path <- self$local_config$config_file
  invisible(self)
})


##### Configuration details - databases, locations, users ###########

## Fields
.public("config_db", NULL)
# later dbConnection::dbConnection$new() connection to database
# using either DBI or pool
.public("configuration_file_path", character())
.public("BPdatabase", data.frame(id = integer(),
                                 Name = character(),
                                 Address = character(),
                                 Database = character(),
                                 UserID = character(),
                                 dbPassword = character(),
                                 stringsAsFactors = FALSE))
.public("BPdatabaseChoice", character())
# database choice will be the same as the 'Name' of
# the chosen entry in BPdatabase
.public("PracticeLocations", data.frame(id = integer(),
                                        Name = character(),
                                        Description = character(),
                                        stringsAsFactors = FALSE))
# id needed for editing this dataframe later
# need default value for practice location filter
# interface initialization
.public("UserConfig", data.frame(id = integer(),
                                 Fullname = character(), AuthIdentity = character(),
                                 Location = character(),
                                 Attributes = character(),
                                 Password = character(),
                                 stringsAsFactors = FALSE))

.public("UserRestrictions", data.frame(uid = integer(),
                                       Restriction = character(),
                                       stringsAsFactors = FALSE))
# this lists the 'enabled' restrictions,
#  relevant to the 'Attributes' field of 'UserConfig'
# without the restriction, all users have the 'permission'
#  for the 'non-specified' action
# use 'uid' rather than 'id', because 'id' is
# later used to identify the restrictions...

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

.public("open_configuration_db", function (
  configuration_file_path = self$configuration_file_path) {

  # if no configuration filepath is defined, then try to read one
  if (length(configuration_file_path) == 0) {
    self$read_configuration_filepaths()
    configuration_file_path <- self$configuration_file_path
  }

  # on first call, self$config_db could be NULL
  if (is.null(self$config_db)) {
    self$config_db <- dbConnection::dbConnection$new()
    # new R6 object which generalizes database connections
  }

  config_db <- self$config_db # for convenience

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

    tablenames <- config_db$conn() %>% DBI::dbListTables()

    if (tablename %in% tablenames) {
      # if table exists in config_db database
      columns <- config_db$conn() %>% dplyr::tbl(tablename) %>% colnames()
      # list of column (variable) names
      data <- config_db$conn() %>% dplyr::tbl(tablename) %>% dplyr::collect()
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
        data <- data %>%
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


  if (!is.null(self$config_db$conn())) {
    # check that tables exist in the config file
    # also create new columns (variables) as necessary
    initialize_data_table(config_db, "Server",
                          list(c("id", "integer"),
                               c("Name", "character"),
                               c("Address", "character"),
                               c("Database", "character"),
                               c("UserID", "character"),
                               c("dbPassword", "character")))
    # initialize_data_table will create table and/or ADD 'missing' columns to existing table

    initialize_data_table(config_db, "ServerChoice",
                          list(c("id", "integer"),
                               c("Name", "character")))
    # there should only be (at most) one entry in this table!
    # with id '1', and a 'Name' the same as the chosen entry in table "Server"

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

.public("read_configuration_db", function(config_db = self$config_db) {

  if (is.null(config_db)) {
    # if config_db is not yet opened/defined
    # then try to open configuration database
    self$open_configuration_db()
    config_db <- self$config_db
  }

  self$BPdatabase <- config_db$conn() %>%
    dplyr::tbl("Server") %>% dplyr::collect()
  self$BPdatabaseChoice <-
    (config_db$conn() %>% dplyr::tbl("ServerChoice") %>%
       dplyr::filter(id == 1) %>% dplyr::select("Name") %>%
       dplyr::collect())[[1]]
  self$PracticeLocations <- config_db$conn() %>%
    dplyr::tbl("Location")
  self$UserConfig <- config_db$conn() %>%
    dplyr::tbl("Users") %>%
    # in UserConfig, there can be multiple Locations/Attributes per user
    dplyr::collect() %>%
    dplyr::mutate(Location = stringi::stri_split(Location, regex = ";"),
                  Attributes = stringi::stri_split(Attributes, regex = ";"))
  self$UserRestrictions <- config_db$conn() %>%
    dplyr::tbl("UserRestrictions") %>% dplyr::collect()

  self$match_user()
  # see if 'identified' system user is matched with a configured user

  invisible(self)
})

##### User login ##################################################

## fields
.public("identified_user", NULL)
# user information for just the identified user
.public("authenticated", FALSE)
# has the current 'identified' user been authenticated yet?

## methods

#' Match user with current 'identified' system user
#'
#' Matches 'dMeasure_obj$Userconfig$AuthIdentity' with Sys.info()[["user"]]
#'
#' @param dMeasure_obj dMeasure object
#'
#' @return self
match_user <- function(dMeasure_obj) {
  dMeasure_obj$match_user()
}

.public("match_user", function() {
  self$identified_user <-
    self$UserConfig[self$UserConfig$AuthIdentity == Sys.info()[["user"]],]

  invisible(self)
})

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
  if (is.null(self$identified_user)) {
    stop("No user identified!")
  }
  if (is.na(self$identified_user$Password) || nchar(self$identified_user$Password) == 0) {
    stop("No password set for currently identified user!")
  }
  if (!simple_tag_compare(password, self$identified_user$Password)) {
    stop("Wrong password")
  } else {
    self$authenticated <- TRUE
  }
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
  if (is.null(self$identified_user)) {
    stop("No user identified!")
  }

  if (is.na(self$identified_user$Password) || nchar(self$identified_user$Password) == 0) {
    # no password yet set for currentl identified user, so just accept the 'newpassword'
    setPassword(newpassword, self$UserConfig, self$identified_user, self$config_db)
    self$authenticated <- TRUE
  } else {
    # there is an old password, which needs to be compared with 'oldpassword'
    if (!simple_tag_compare(oldpassword, self$identified_user$Password)) {
      stop("Wrong password")
    } else {
      # old password specified correctly
      setPassword(newpassword, self, self$config_db)
      self$authenticated <- TRUE
    }
  }
  self$match_user() # re-read 'identified user' configuration, as password has changed

  return(self$authenticated)
})

##### clinician choice list #######################################

## fields
.public("clinician_choice_list", NULL)
# available clinicians appointments
.public("clinicians", NULL)

## constants
view_restrictions <- list(
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
)

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
#'
#' @return the clinician choice list
clinician_list <- function(dMeasure_obj,
                           view_name = "All") {
  dMeasure_obj$clinician_list(view_name)
}

.public("clinician_list", function(view_name = "All") {
  if (self$location == 'All') {
    # note that 'ifelse' only returns result in the
    # same 'shape' as the comparison statement
    clinician_list <- self$UserFullConfig$Fullname
  } else {
    clinician_list <- subset(UserConfig$Fullname,
                             sapply(UserConfig$Location,
                                    function (y) self$location %in% y))
    # filter clinicians by location choice
    # it is possible for a clinician to have multiple locations
    # initially, $Location might include a lot of NA
  }

  for (restriction in view_restrictions) {
    # go through list of view restrictions
    if (restriction$restriction %in% unlist(self$UserRestrictions$Restriction)) {
      # if the restriction has been activated
      if (view_name %in% restriction$view_to_hide) {
        # if the relevant view is being shown
        if (self$authenticated == FALSE |
            !(restriction$restriction %in% unlist(self$identified_user$Attributes))) {
          # if user is not authenticated or
          # if the current user does not have this 'Global' attribute
          # then can only view one's own appointments
          clinician_list <- subset(clinician_list,
                                   clinician_list == self$identified_user$Fullname)
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
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param choices="" list of clinicians chosen
#' @param view_name="All" view
#'
#' @return list of clinicians chosen
#' this will be 'checked' against actual available clinicians ($clinicians_list)
chosen_clinicians <- function(dMeasure_obj, choices = "", view_name = "All") {
  dMeasure_obj$chosen_clinicians(choices, view_name)
}

.public("chosen_clinicians", function(choices = "", view_name = "All") {
  choices <- intersect(choices, self$clinician_list(view_name))
  # can only actually choose clinicians available in chosen view

  self$clinicians <- choices
  return(choices)
})

##### Electronic Medical Record (EMR) database configuration ######

## fields
.public("emr_db", NULL) # later will be R6 object containing database object
.public("db", list(dbversion = 0)) # later will be the EMR databases.
# $dbversion is number of EMR database openings

## methods

#' opens the EMR database
#'
#' @param dMeasure_obj dMeasure object
#' @param BPdatabaseChoice the chosen database from the config_db list
#' @param emr_db' R6 database object. this might not be initially defined
#'
#' if no arguments passed, the defaults are what is stored in
#' the  object
open_emr_db <- function(dMeasure_obj,
                        BPdatabaseChoice = dMeasure_obj$BPdatabaseChoice,
                        emr_db = dMeasure_obj$emr_db) {
  dMeasure_obj$open_emr_db(BPdatabaseChoice, emr_db)
}

.public("open_emr_db",function(BPdatabaseChoice = NULL,
                               emr_db = NULL) {

  if (is.null(self$config_db) || length(a$BPdatabaseChoice) == 0) {
    # no BPdatabase has been defined, or the current configuration is not valid
    # try to define the current configuration and open the BP database
    self$read_configuration_db()
    BPdatabaseChoice <- self$BPdatabaseChoice
    emr_db <- self$emr_db
  }

  if (is.null(BPdatabaseChoice)) {
    BPdatabaseChoice = self$BPdatabaseChoice
  }
  if (is.null(emr_db)) {
    emr_db = self$emr_db
  }

  print(paste("ChosenServerName:", BPdatabaseChoice))

  if (is.null(emr_db)) {
    self$emr_db <- dbConnection::dbConnection$new()
    emr_db <- self$emr_db
    # on first run, self$emr_db may be NULL
  }
  # close existing database connection
  # safe to call $close() if no database is open
  emr_db$close()

  if (BPdatabaseChoice == "None") {
    # do nothing
  } else if (!is.null(BPdatabaseChoice)) {
    server <- self$BPdatabase %>%
      dplyr::filter(Name == BPdatabaseChoice) %>%
      dplyr::collect()
    print("Opening EMR database")
    emr_db$connect(odbc::odbc(), driver = "SQL Server",
                   server = server$Address, database = server$Database,
                   uid = server$UserID, pwd = simple_decode(server$dbPassword))
  }

  if (is.null(emr_db$conn()) || !DBI::dbIsValid(emr_db$conn())) {
    # || 'short-circuits' the evaluation, so if not an environment,
    # then dbIsValid() is not evaluated (will return an error if emr_db$conn() is NULL)

    # either database not opened, or has just been closed
    self$db$users <- NULL
    self$db$patients <- NULL
    self$db$investigations <- NULL
    self$db$appointments <- NULL
    self$db$immunizations <- NULL
    self$db$preventive_health <- NULL
    self$db$correspondenceIn <- NULL
    self$db$reportValues <- NULL
    self$db$services <- NULL
    self$db$history <- NULL
    self$clinician_choice_list <- NULL
    self$BPdatabaseChoice <- "None" # set choice of database to 'None'

  } else {
    # successfully opened database
    self$initialize_emr_tables(emr_db)
    dummy <- self$update_UserFullConfig() # match UserConfig to EMR user configs
    dummy <- self$clinician_list() # and list all 'available' clinicians
  }
})

#' initialize the tables of the EMR database
#'
#' @param dMeasure_obj dMeasure object
#' @param emr_db R6 object connecting to EMR database
initialize_emr_tables <- function(dMeasure_obj,
                                  emr_db = dMeasure_obj$emr_db) {
  dMeasure_obj$initialize_emr_tables(emr_db)
}

.public("initialize_emr_tables", function(emr_db = self$emr_db) {

  print("Re-initializing databases")

  self$db$users <- emr_db$conn() %>%
    # this is a function! a collect() is later called prior to mutate/join,
    # (as a result is no longer a 'lazy eval') and cannot be evaluated just once.
    # output - Fullname, UserID, Surname, Firstname, LocationName, Title, ProviderNo
    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_Users')) %>%
    dplyr::select(c('UserID', 'Surname', 'Firstname',
                    'LocationName', 'Title', 'ProviderNo'))

  self$db$patients <- emr_db$conn() %>%
    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_Patients'))

  self$db$investigations <- emr_db$conn() %>%
    # output - InternalID, Collected (Date), TestName
    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_Investigations')) %>%
    dplyr::select(c('InternalID', 'Collected', 'TestName'))
  # as of Jan/2019, the odbc engine for MSSQL can't handle the
  # full ('Select *') Investigations table
  # due to some type of bug/standards non-compliance.
  # also can handle the History table. need to
  # 'Select' out just a few columns.

  self$db$appointments <- emr_db$conn() %>%
    # Patient, InternalID, AppointmentDate, AppointmentTime, Provider, Status
    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_Appointments')) %>%
    dplyr::select(c('Patient', 'InternalID',
                    'AppointmentDate', 'AppointmentTime',
                    'Provider', 'Status'))

  self$db$immunizations <- emr_db$conn() %>%
    # InternalID, GivenDate, VaccineName, VaccineID
    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_Immunisations')) %>%
    dplyr::select(c('InternalID', 'GivenDate', 'VaccineName', 'VaccineID'))

  self$db$vaccine_disease <- emr_db$conn() %>%
    # vaccineIDs linked to diseases
    # e.g. diseasecode 7+30 are for influenza vaccines
    dplyr::tbl(dbplyr::in_schema("bpsdrugs.dbo", "VACCINE_DISEASE")) %>%
    dplyr::select("VACCINEID", "DISEASECODE")

  self$db$preventive_health <- emr_db$conn() %>%
    # INTERNALID, ITEMID (e.g. not for Zostavax remindders)
    dplyr::tbl(dbplyr::in_schema('dbo', 'PreventiveHealth')) %>%
    dplyr::select('InternalID' = 'INTERNALID', 'ITEMID')

  self$db$correspondenceIn <- emr_db$conn() %>%
    # InternalID, CorrespondenceDate, Subject, Detail
    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_CorrespondenceIn')) %>%
    dplyr::select('InternalID', 'CorrespondenceDate', 'Subject', 'Detail')

  self$db$reportValues <- emr_db$conn() %>%
    # InternalID, ReportDate, ResultName, LoincCode
    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_ReportValues')) %>%
    dplyr::select('InternalID', 'ReportDate', 'ResultName', 'LoincCode')

  self$db$services <- emr_db$conn() %>%
    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_SERVICES')) %>%
    dplyr::select('InternalID' = 'INTERNALID', 'ServiceDate' = 'SERVICEDATE',
                  'MBSItem' = 'MBSITEM', 'Description' = 'DESCRIPTION')

  self$db$history <- emr_db$conn() %>%
    # InternalID, Year, Condition, ConditionID, Status
    dplyr::tbl(dbplyr::in_schema('dbo', 'BPS_History')) %>%
    dplyr::select('InternalID', 'Year',
                  'Condition', 'ConditionID', 'Status')

  self$db$observations <- emr_db$conn() %>%
    dplyr::tbl(dbplyr::in_schema("dbo", "OBSERVATIONS")) %>%
    dplyr::select('InternalID' = 'INTERNALID', 'DATANAME',
                  'DATACODE', 'DATAVALUE', 'OBSDATE')

  self$db$currentrx <- emr_db$conn() %>%
    dplyr::tbl(dbplyr::in_schema("dbo", "CURRENTRX")) %>%
    dplyr::select('InternalID' = 'INTERNALID', 'PRODUCTID',
                  'DRUGNAME', 'RXSTATUS')
  # RXSTATUS appears to be 1 if 'long-term' and 2 if 'short-term'

  self$db$obgyndetail <- emr_db$conn() %>%
    dplyr::tbl(dbplyr::in_schema("dbo", "OBSGYNDETAIL")) %>%
    dplyr::select('InternalID' = 'INTERNALID', 'NOMINALLMP',
                  'LASTPAPDATE', 'LASTPAPRESULT', 'BREASTFEEDING',
                  'MammogramStatus', 'LastMammogramDate', 'MammogramResult')

  self$db$pregnancies <- emr_db$conn() %>%
    dplyr::tbl(dbplyr::in_schema("dbo", "PREGNANCIES")) %>%
    dplyr::select('InternalID' = 'INTERNALID', 'EDCBYDATE',
                  'ACTUALLMP', 'NOMINALLMP', 'ENDDATE')

  self$db$dbversion <- self$db$dbversion + 1
  print(paste("dbversion:", self$db$dbversion))
})

##### other variables and methods #################

## fields
.public("UserFullConfig", NULL)
# contains user names attached to configuration information
# contains ALL user names
# UserConfig() contains just names who have been configured

## methods

#' update the UserFullConfig
#'
#' integrates the UserConfig in the SQLite configuration file
#' with the user information in the EMR database
#'
#' @param dMeasure_obj dMeasure object
#' @param db tables in the EMR database
#' @param UserConfig UserConfig stored in configuration file
#'
#' @return UserFullConfig
#' also updates this object's copy
update_UserFullConfig <- function(dMeasure_obj,
                                  db = dMeasure_obj$db,
                                  UserConfig = dMeasure_obj$UserConfig) {
  dMeasure_obj$update_UserFullConfig(db, UserConfig)
}

.public("update_UserFullConfig", function (db = NULL,
                                           UserConfig = NULL) {
  if (is.null(db)) {
    db <- self$db
    # the default
  }
  if (is.null(UserConfig)) {
    UserConfig <- self$UserConfig
    # the default
  }

  if (is.null(db$users)) {
    UserFullConfig <- NULL
  } else {
    UserFullConfig <- db$users %>% dplyr::collect() %>%
      # forces database to be read
      # (instead of subsequent 'lazy' read)
      # collect() required for mutation and left_join
      dplyr::mutate(Title = trimws(Title),
                    Firstname = trimws(Firstname),
                    Surname = trimws(Surname)) %>%
      dplyr::mutate(Fullname =
                      paste(Title, Firstname, Surname, sep = ' ')) %>%
      # include 'Fullname'
      dplyr::left_join(UserConfig, by = 'Fullname')
    # add user details including practice locations
  }

  self$UserFullConfig <- UserFullConfig # update object's copy
  return(UserFullConfig) # and return a copy to caller
})

##### date variables and location #####################################

## fields

.public("date_a", Sys.Date()) # 'from' date. by default, it is 'today'
.public("date_b", Sys.Date()) # 'to' date
.public("location", "All")    # location/group. by default, it is 'All'

## methods

#' Update date
#'
#' Sets 'from' and 'to' dates used in subsequent searches
#'
#' @param date_from 'From' date. default is current date_from
#' @param date_to 'To' date. default is current date_to
#'
#' @return list(date_a, date_b)
#'
#' if date_a is later than date_b, an error is returned
update_date <- function(dMeasure_obj,
                        date_from = dMeasure_obj$date_a,
                        date_to = dMeasure_obj$date_b) {
  dMeasure_obj$update_date(date_from, date_to)
}

.public("update_date", function(date_from = self$date_a,
                                date_to = self$date_b) {
  if (date_from > date_to) {
    stop("'From' date cannot be later than 'To' date")
  }
  self$date_a <- date_from
  self$date_b <- date_to

  return(list(self$date_a, self$date_b))
})

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

.public("location_list", function() {
  locations <- data.frame(Name = "All")
  if (!is.null(self$PracticeLocations)) {
    locations <-
      rbind(locations,
            as.data.frame(self$PracticeLocations %>%
                            dplyr::select(Name))) %>%
      unlist(use.names = FALSE)
  }
  return(locations)
})

#' Update location
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
update_location <- function(dMeasure_obj,
                            location = dMeasure_obj$location) {
  dMeasure_obj$update_location(location)
}

.public("update_location", function(location = self$location) {
  locations <- self$location_list()
  if (!location %in% locations) {
    stop(paste0("'", location, "' is not in the list of locations."))
  } else {
    self$location <- location
  }

  return(self$location)
})
