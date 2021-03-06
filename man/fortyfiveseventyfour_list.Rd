% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conditions.R
\name{fortyfiveseventyfour_list}
\alias{fortyfiveseventyfour_list}
\title{list of patients who are 45 to 74 years age at time of $Date}
\usage{
fortyfiveseventyfour_list(dMeasure_obj, appointments = NULL)
}
\arguments{
\item{dMeasure_obj}{dMeasure R6 object}

\item{appointments}{dataframe of appointments $InternalID and $Date

 if no parameter provided, derives from $appointments_filtered}
}
\value{
a vector of numbers, which are the InternalIDs
}
\description{
list of patients who are 45 to 74 years age at time of $Date
}
