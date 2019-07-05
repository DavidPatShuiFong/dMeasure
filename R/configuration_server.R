##### Configuration - server ###########################################
#' Configuration - server
#'
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
#' @param description $Name, $Address, $Database, $UserID, $dbPassword
#'
#' @return dataframe - full list of database descriptions
#'  can also return error (stop) if description is invalid
server.insert <- function(description) {
  dMeasure_obj$server.insert(description)
}

.public("server.insert", function(description) {
  if (toupper(description$Name) %in% toupper(append(self$BPdatabase$Name, "None"))) {
    # if the proposed server is the same as one that already exists
    # (ignoring case)
    stop("New server name cannot be the same as existing names, or 'None'")
  } else if (stringi::stri_length(description$Name) == 0 |
             stringi::stri_length(description$Address) == 0 |
             stringi::stri_length(description$Database) == 0 |
             stringi::stri_length(description$UserID) == 0 |
             stringi::stri_length(description$dbPassword) == 0) {
    stop("All entries must be described")
  } else {
    newid <- max(c(as.data.frame(self$BPdatabase)$id, 0)) + 1
    # initially, self$BPdatabase$id might be an empty set, so need to append a '0'
    description$id <- newid
    description$dbPassword <- simple_encode(description$dbPassword)
    # immediately encode password.
    # stored encrypted both in memory and in configuration file

    query <- "INSERT INTO Server (id, Name, Address, Database, UserID, dbPassword) VALUES (?, ?, ?, ?, ?, ?)"
    data_for_sql <- as.list.data.frame(c(newid, description$Name, description$Address,
                                         description$Database, description$UserID,
                                         description$dbPassword))

    self$config_db$dbSendQuery(query, data_for_sql)
    # if the connection is a pool, can't send write query (a statement) directly
    # so use the object's method

    self$BPdatabase <- rbind(self$BPdatabase, description)
    # update the dataframe in memory
    return(self$BPdatabase)
  }
})

#' server.update
#'
#' change (update) a server descriptioin
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param description $id, $Name, $Address, $Database, $UserID, $dbPassword
#'
#' @return dataframe - full list of database descriptions
#'  can also return error (stop) if description is invalid
server.update <- function(description) {
  dMeasure_obj$server.update(description)
}

.public("server.update", function(description) {

  if (toupper(description$Name) %in%
      toupper(append(self$BPdatabase[-(id = description$id),]$Name, "None"))) {
    # if the proposed server is the same as one that already exists
    # (ignoring case, and removing the 'id' which is specified in the description)
    stop("New server name cannot be the same as existing names, or 'None'")
  } else if (toupper(description$Name) == toupper(self$BPdatabaseChoice)) {
    stop(paste0("Cannot edit '", description$Name, "', currently in use!"))
  } else if (toupper(description$Name == "NONE")) {
    stop("New server name cannot be 'None'!")
  } else if (stringi::stri_length(description$Name) == 0 |
             stringi::stri_length(description$Address) == 0 |
             stringi::stri_length(description$Database) == 0 |
             stringi::stri_length(description$UserID) == 0 |
             stringi::stri_length(description$dbPassword) == 0) {
    stop("All entries must be described")
  } else {
    description$dbPassword <- simple_encode(description$dbPassword)
    # immediately encode password.
    # stored encrypted both in memory and in configuration file

    query <- "UPDATE Server SET Name = ?, Address = ?, Database = ?, UserID = ?, dbPassword = ? WHERE id = ?"
    data_for_sql <- as.list.data.frame(c(description$Name, description$Address,
                                         description$Database, description$UserID,
                                         description$dbPassword, description$id))

    self$config_db$dbSendQuery(query, data_for_sql)
    # if the connection is a pool, can't send write query (a statement) directly
    # so use the object's method

    self$BPdatabase <- rbind(self$BPdatabase[-(id = description$id),], description)
    # store new values in copy of settings in memory
    return(self$BPdatabase)
  }
})


#' servers.delete
#'
#' remove a server descriptioin
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param description $Name
#'
#' @return dataframe - full list of database descriptions
#'  can also return error (stop) if description is invalid
server.delete <- function(description) {
  dMeasure_obj$server.delete(description)
}

.public("server.delete", function(description) {
  # delete a server description

  if (toupper(description$Name) == toupper(self$BPdatabaseChoice)) {
    stop(paste0("Cannot remove '", description$Name, "', currently in use!"))
  } else {
    id <- self$BPdatabase[self$BPdatabase$Name == description$Name,]$id

    if(length(id) == 0) { # $Name was not found
      stop(paste0("Cannot remove '", description$Name, "', not defined!"))
    } else {
      # remove from config SQLite database
      query <- "DELETE FROM Server WHERE id = ?"
      data_for_sql <- as.list.data.frame(c(id))

      self$config_db$dbSendQuery(query, data_for_sql)
      # if the connection is a pool, can't send write query (a statement) directly
      # so use the object's method

      self$BPdatabase <- self$BPdatabase[-c(id = id),]
    }
  }
  return(self$BPdatabase)
})
