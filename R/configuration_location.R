##### Configuration - location ###########################################
#' Configuration - location
#'
#' @name configuration_location
#' @include dMeasure.R
#' needs access to dMeasure and methods and fields
#' @include calculation_definitions.R
#' needs access to functions
NULL

#' location.insert
#'
#' insert a new location description
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param description list object : $Name, $Description
#'
#' @return dataframe - full list of location descriptions
#'  can also return error (stop) if description is invalid
location.insert <- function(dMeasure_obj, description) {
  dMeasure_obj$location.insert(description)
}

.public("location.insert", function(description) {
  # insert a practice location

  tryCatch(permission <- self$location.permission(),
           warning = function(w)
             stop(paste(w,
                        "'UserAdmin' permission required to view or edit location list.")))

  if (length(grep(toupper(description$Name),
                  toupper(as.data.frame(self$PracticeLocations %>>%
                                        dplyr::select(Name)))
                  ))) {
    # if the proposed new name is the same as one that already exists
    # (ignoring case). grep returns empty integer list if no match
    stop("New practice location name cannot be the same as existing names")
  } else if (is.null(description$Name)){
    stop("New practice location name cannot be 'empty'!")
  } else {

    newid <- max(c(as.data.frame(self$PracticeLocations)$id, 0)) + 1
    # initially, PracticeLocations$id might be an empty set
    # so need to append a '0'
    descriptionid <- newid

    query <- "INSERT INTO Location (id, Name, Description) VALUES (?, ?, ?)"
    data_for_sql <- as.list.data.frame(c(newid, description$Name, description$Description))

    self$config_db$dbSendQuery(query, data_for_sql)
    # if the connection is a pool, can't send write query (a statement) directly
    # so use the object's method

    # self$PracticeLocations is 'lazy' evaluated directly from SQLite,
    # so does not need to be modified manually

    return(self$PracticeLocations)
  }
})

#' location.update
#'
#' update a location description
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param description list object : $id, $Name, $Description
#'
#' @return dataframe - full list of location descriptions
#'  can also return error (stop) if description is invalid
location.update <- function(dMeasure_obj, description) {
  dMeasure_obj$location.update(description)
}

.public("location.update", function(description) {
  # change (update) a practice location

  tryCatch(permission <- self$location.permission(),
           warning = function(w)
             stop(paste(w,
                        "'UserAdmin' permission required to view or edit location list.")))

  olddescription <- self$PracticeLocations %>>%
    dplyr::filter(id == description$id) %>>% dplyr::collect()
  # the description before modificatioin


  if (length(grep(toupper(description$Name),
                  toupper(as.data.frame(self$PracticeLocations %>>%
                                        dplyr::filter(id != description$id))$Name)
  ))) {
    # if the proposed new name is the same as one that already exists
    # (ignoring case). grep returns empty integer list if no match
    stop("New practice location name cannot be the same as existing names, or 'None'")
  } else if (is.null(description$Name)){
    stop("New practice location name cannot be 'empty'!")
  } else if ((olddescription$Name %in% self$UserConfig$Location) &
             (olddescription$Name != description$Name)) {
    stop(paste0("Cannot change the name of '", olddescription$Name,
                "', this location is assigned to a user."))
  } else {
    query <- "UPDATE Location SET Name = ?, Description = ? WHERE id = ?"
    data_for_sql <- as.list.data.frame(c(description$Name, description$Description,
                                         description$id))

    self$config_db$dbSendQuery(query, data_for_sql)
    # if the connection is a pool, can't send write query (a statement) directly
    # so use the object's method

    # self$PracticeLocations is 'lazy' evaluated directly from SQLite,
    # so does not need to be modified manually

    return(self$PracticeLocations)
  }
})

#' location.delete
#'
#' delete a location description
#'
#' @param dMeasure_obj dMeasure R6 object
#' @param description list object : $Name
#'
#' @return dataframe - full list of location descriptions
#'  can also return error (stop) if deletion if cannot delete
location.delete <- function(dMeasure_obj, description) {
  dMeasure_obj$location.delete(description)
}

.public("location.delete", function(description) {
  # delete a practice location

  tryCatch(permission <- self$location.permission(),
           warning = function(w)
             stop(paste(w,
                        "'UserAdmin' permission required to view or edit location list.")))

  if (description$Name %in% self$UserConfig$Location) {
    stop(paste0("Cannot remove '", description$Name,
                "', this location is assigned to a user."))
  } else {
    description$id <- self$PracticeLocations %>>%
      dplyr::filter(Name == description$Name) %>>%
      dplyr::select(id) %>>%
      dplyr::collect() %>>%
      unlist(use.names = FALSE)

    if (length(description$id) == 0) {
      stop("$Name not found in list of server names.")
    }

    query <- "DELETE FROM Location WHERE id = ?"
    data_for_sql <- as.list.data.frame(c(description$id))

    config_db$dbSendQuery(query, data_for_sql)
    # if the connection is a pool, can't send write query (a statement) directly
    # so use the object's method

    # self$PracticeLocations is 'lazy' evaluated directly from SQLite,
    # so does not need to be modified manually

  }
  return(self$PracticeLocations)
})

#' location.list
#'
#' list location description
#'
#' @param dMeasure_obj dMeasure R6 object
#'
#' @return dataframe - full list of location descriptions
location.list <- function(dMeasure_obj) {
  dMeasure_obj$location.list()
}

.public("location.list", function() {

  tryCatch(permission <- self$location.permission(),
           warning = function(w)
             stop(paste(w,
                        "'UserAdmin' permission required to view or edit location list.")))

  return(self$PracticeLocations)
})

#' location.permission
#'
#' Does the current user have location access permission?
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
location.permission <- function(dMeasure_obj) {
  dMeasure_obj$location.permission()
}

.public("location.permission", function() {
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
