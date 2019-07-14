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
#' @param description list object : $Name, $Address, $Database, $UserID, $dbPassword
#'
#' @return dataframe - full list of database descriptions
#'  can also return error (stop) if description is invalid
server.insert <- function(dMeasure_obj, description) {
  dMeasure_obj$server.insert(description)
}

.public("server.insert", function(description) {

  tryCatch(permission <- self$server.permission(),
           warning = function(w)
             stop(paste(w,
                        "'ServerAdmin' permission required to modify server list.")))

  if (toupper(description$Name) %in% toupper(append(private$.BPdatabase$Name, "None"))) {
    # if the proposed server is the same as one that already exists
    # (ignoring case)
    stop("New server name cannot be the same as existing names, or 'None'")
  } else if (is.null(description$Name) |
             is.null(description$Address) |
             is.null(description$Database) |
             is.null(description$UserID) |
             is.null(description$dbPassword)) {
    stop(paste("All entries ($id, $Name, $Address, $Database, $UserID, $dbPassword)",
               "must be described"))
  } else if (stringi::stri_length(description$Name) == 0 |
             stringi::stri_length(description$Address) == 0 |
             stringi::stri_length(description$Database) == 0 |
             stringi::stri_length(description$UserID) == 0 |
             stringi::stri_length(description$dbPassword) == 0) {
    stop(paste("All entries ($id, $Name, $Address, $Database, $UserID, $dbPassword)",
               "must be described"))
  } else {
    newid <- max(c(as.data.frame(private$.BPdatabase)$id, 0)) + 1
    # initially, private$.BPdatabase$id might be an empty set, so need to append a '0'
    description$id <- newid
    description$dbPassword <- simple_encode(description$dbPassword)
    # immediately encode password.
    # stored encrypted both in memory and in configuration file

    query <- paste("INSERT INTO Server",
                   "(id, Name, Address, Database, UserID, dbPassword)",
                   "VALUES (?, ?, ?, ?, ?, ?)")
    data_for_sql <- as.list.data.frame(c(newid, description$Name, description$Address,
                                         description$Database, description$UserID,
                                         description$dbPassword))

    private$config_db$dbSendQuery(query, data_for_sql)
    # if the connection is a pool, can't send write query (a statement) directly
    # so use the object's method
    private$trigger(self$config_db_trigR)

    private$.BPdatabase <- rbind(private$.BPdatabase, description,
                                 stringsAsFactors = FALSE)
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
#' @param description list $id, $Name, $Address, $Database, $UserID, $dbPassword
#'
#' @return dataframe - full list of database descriptions
#'  can also return error (stop) if description is invalid
server.update <- function(dMeasure_obj, description) {
  dMeasure_obj$server.update(description)
}

.public("server.update", function(description) {

  tryCatch(permission <- self$server.permission(),
           warning = function(w)
             stop(paste(w,
                        "'ServerAdmin' permission required to modify server list.")))

  if (is.null(description$id)) {
    stop("Server to change is to be identified by $id")
  }
  if (!description$id %in% (private$.BPdatabase %>>% dplyr::pull(id))) {
    stop(paste("No server definition with id = ", description$id), "!", sep = "")
  }
  if (self$BPdatabaseChoice != "None") { # only if there is a chosen database
    if (private$.BPdatabase %>>% dplyr::filter(Name == self$BPdatabaseChoice) %>>%
        dplyr::pull(id) == description$id) {
      stop(paste("Cannot update server definition id = ", description$id,
                 ", currently in use!"))
    }
  }

  # if definition is not provided, then 'fill it in' from the current definition
  if (is.null(description$Name)) {
    description$Name <- private$.BPdatabase %>>%
      dplyr::filter(id == description$id) %>>%
      dplyr::pull(Name)
  } else {
    if (toupper(description$Name) %in%
        toupper(append(private$.BPdatabase[-(id = description$id),]$Name, "None"))) {
      # if the proposed server is the same as one that already exists
      # (ignoring case, and removing the 'id' which is specified in the description)
      stop("New server name cannot be the same as existing names, or 'None'")
    }
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
  if (is.null(description$dbPassword)) {
    description$dbPassword <- private$.BPdatabase %>>%
      dplyr::filter(id == description$id) %>>%
      dplyr::pull(dbPassword)
  } else {
    description$dbPassword <- simple_encode(description$dbPassword)
    # immediately encode password.
    # stored encrypted both in memory and in configuration file
  }

  query <- paste("UPDATE Server SET Name = ?, Address = ?, Database = ?,",
                 "UserID = ?, dbPassword = ? WHERE id = ?")
  data_for_sql <- as.list.data.frame(c(description$Name, description$Address,
                                       description$Database, description$UserID,
                                       description$dbPassword, description$id))

  private$config_db$dbSendQuery(query, data_for_sql)
  # if the connection is a pool, can't send write query (a statement) directly
  # so use the object's method
  private$trigger(self$config_db_trigR)

  private$.BPdatabase <- rbind(private$.BPdatabase[-(id = description$id),], description,
                               stringsAsFactors = FALSE)
  # store new values in copy of settings in memory
  return(private$.BPdatabase %>>%
           dplyr::select(-dbPassword))
})

#' server.delete
#'
#' remove a server description
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param description $id
#'
#' @return dataframe - full list of database descriptions
#'  can also return error (stop) if description is invalid
server.delete <- function(dMeasure_obj, description) {
  dMeasure_obj$server.delete(description)
}

.public("server.delete", function(description) {
  # delete a server description

  tryCatch(permission <- self$server.permission(),
           warning = function(w)
             stop(paste(w,
                        "'ServerAdmin' permission required to modify server list.")))

  name <- private$.BPdatabase[private$.BPdatabase$id == description$id,]$Name
  if (length(name) == 0) { # id not found
    stop(paste0("Cannot remove id = '", description$id, "', not defined!"))
  }
  if (toupper(name) == toupper(self$BPdatabaseChoice)) {
    stop(paste0("Cannot remove '", name, "', currently in use!"))
  }

  # remove from config SQLite database
  query <- "DELETE FROM Server WHERE id = ?"
  data_for_sql <- as.list.data.frame(c(description$id))

  private$config_db$dbSendQuery(query, data_for_sql)
  # if the connection is a pool, can't send write query (a statement) directly
  # so use the object's method
  private$trigger(self$config_db_trigR) # send a trigger signal

  private$.BPdatabase <- private$.BPdatabase[-c(id = description$id),]

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
server.list <- function(dMeasure_obj) {
  dMeasure_obj$server.list()
}

.public("server.list", function() {
  # list server descriptions

  tryCatch(permission <- self$server.permission(),
           warning = function(w)
             warning(paste(w,
                           "'ServerAdmin' permission required to view server list.")))

  if (permission) {
    description <- private$.BPdatabase %>>%
      dplyr::select(-dbPassword)}
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
server.permission <- function(dMeasure_obj) {
  dMeasure_obj$server.permission()
}

.public("server.permission", function() {
  if ("ServerAdmin" %in% unlist(private$UserRestrictions$Restriction)) {
    # only some users allowed to see/change server settings
    if ("ServerAdmin" %in% unlist(private$.identified_user$Attributes) &
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
