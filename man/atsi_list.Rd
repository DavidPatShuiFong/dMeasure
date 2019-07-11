% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conditions.R
\name{atsi_list}
\alias{atsi_list}
\title{list of patients recorded ATSI ethnicity}
\usage{
atsi_list(dMeasure_obj, appointments = NULL)
}
\arguments{
\item{dMeasure_obj}{dMeasure R6 object}

\item{appointments}{list. requires $InternalID
by default, the appointments in $appointments_filtered}
}
\value{
a list of numbers, which are the InternalIDs
}
\description{
list of patients recorded ATSI ethnicity
}