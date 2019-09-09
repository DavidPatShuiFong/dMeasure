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
#' @export
location.insert <- function(dMeasure_obj, description) {
  dMeasure_obj$location.insert(description)
}
.public(dMeasure, "location.insert", function(description) {
  # insert a practice location

  tryCatch(permission <- self$location.permission(),
           warning = function(w)
             stop(paste(w,
                        "'UserAdmin' permission required to view or edit location list.")))

  if (length(grep(toupper(description$Name),
                  toupper(as.data.frame(private$PracticeLocations %>>%
                                        dplyr::select(Name)))
  ))) {
    # if the proposed new name is the same as one that already exists
    # (ignoring case). grep returns empty integer list if no match
    stop("New practice location name cannot be the same as existing names")
  } else if (is.null(description$Name)){
    stop("New practice location name cannot be 'empty'!")
  } else {

    newid <- max(c(as.data.frame(private$PracticeLocations)$id, 0)) + 1
    # initially, PracticeLocations$id might be an empty set
    # so need to append a '0'
    descriptionid <- newid

    query <- "INSERT INTO Location (id, Name, Description) VALUES (?, ?, ?)"
    data_for_sql <- as.list.data.frame(c(newid, description$Name, description$Description))

    private$config_db$dbSendQuery(query, data_for_sql)
    # if the connection is a pool, can't send write query (a statement) directly
    # so use the object's method
    private$trigger(self$config_db_trigR) # send a trigger signal

    # private$PracticeLocations is 'lazy' evaluated directly from SQLite,
    # so does not need to be modified manually

    invisible(self$location_list) # will also trigger change in $location_listR
    # don't need to explicitly set private$PracticeLocations, since it
    # it is calculated automatically upon database change
    return(private$PracticeLocations)
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
#' @export
location.update <- function(dMeasure_obj, description) {
  dMeasure_obj$location.update(description)
}

.public(dMeasure, "location.update", function(description) {
  # change (update) a practice location

  tryCatch(permission <- self$location.permission(),
           warning = function(w)
             stop(paste(w,
                        "'UserAdmin' permission required to view or edit location list.")))

  olddescription <- private$PracticeLocations %>>%
    dplyr::filter(id == description$id) %>>% dplyr::collect()
  # the description before modificatioin


  if (length(grep(toupper(description$Name),
                  toupper(as.data.frame(private$PracticeLocations %>>%
                                        dplyr::filter(id != description$id))$Name)
  ))) {
    # if the proposed new name is the same as one that already exists
    # (ignoring case). grep returns empty integer list if no match
    stop("New practice location name cannot be the same as existing names, or 'None'")
  } else if (is.null(description$Name)){
    stop("New practice location name cannot be 'empty'!")
  } else if ((olddescription$Name %in% private$.UserConfig$Location) &
             (olddescription$Name != description$Name)) {
    stop(paste0("Cannot change the name of '", olddescription$Name,
                "', this location is assigned to a user."))
  } else {
    query <- "UPDATE Location SET Name = ?, Description = ? WHERE id = ?"
    data_for_sql <- as.list.data.frame(c(description$Name, description$Description,
                                         description$id))

    private$config_db$dbSendQuery(query, data_for_sql)
    # if the connection is a pool, can't send write query (a statement) directly
    # so use the object's method
    private$trigger(self$config_db_trigR) # send a trigger signal

    invisible(self$location_list) # will also trigger change in $location_listR
    # don't need to explicitly set private$PracticeLocations, since
    # private$PracticeLocations is 'lazy' evaluated directly from SQLite,
    # so does not need to be modified manually
    return(private$PracticeLocations)
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
#' @export
location.delete <- function(dMeasure_obj, description) {
  dMeasure_obj$location.delete(description)
}

.public(dMeasure, "location.delete", function(description) {
  # delete a practice location

  tryCatch(permission <- self$location.permission(),
           warning = function(w)
             stop(paste(w,
                        "'UserAdmin' permission required to view or edit location list.")))

  if (description$Name %in% private$.UserConfig$Location) {
    stop(paste0("Cannot remove '", description$Name,
                "', this location is assigned to a user."))
  } else {
    description$id <- private$PracticeLocations %>>%
      dplyr::filter(Name == description$Name) %>>%
      dplyr::select(id) %>>%
      dplyr::collect() %>>%
      unlist(use.names = FALSE)

    if (length(description$id) == 0) {
      stop("$Name not found in list of server names.")
    }

    query <- "DELETE FROM Location WHERE id = ?"
    data_for_sql <- as.list.data.frame(c(description$id))

    private$config_db$dbSendQuery(query, data_for_sql)
    # if the connection is a pool, can't send write query (a statement) directly
    # so use the object's method
    private$trigger(self$config_db_trigR) # send a trigger signal

  }
  invisible(self$location_list) # will also trigger change in $location_listR
  # don't need to explicitly set private$PracticeLocations, since
  # private$PracticeLocations is 'lazy' evaluated directly from SQLite,
  # so does not need to be modified manually
  return(private$PracticeLocations)
})

#' location.list
#'
#' list location description
#'
#' @param dMeasure_obj dMeasure R6 object
#'
#' @return dataframe - full list of location descriptions
#' @export
location.list <- function(dMeasure_obj) {
  dMeasure_obj$location.list()
}

.public(dMeasure, "location.list", function() {

  tryCatch(permission <- self$location.permission(),
           warning = function(w)
             stop(paste(w,
                        "'UserAdmin' permission required to view or edit location list.")))

  return(private$PracticeLocations)
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
#' @export
location.permission <- function(dMeasure_obj) {
  dMeasure_obj$location.permission()
}

.public(dMeasure, "location.permission", function() {
  if ("UserAdmin" %in% unlist(private$.UserRestrictions$Restriction)) {
    # only some users allowed to see/change server settings
    if ("UserAdmin" %in% unlist(private$.identified_user$Attributes) &
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
