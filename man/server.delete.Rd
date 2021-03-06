% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/configuration_server.R
\name{server.delete}
\alias{server.delete}
\title{server.delete}
\usage{
server.delete(dMeasure_obj, description)
}
\arguments{
\item{dMeasure_obj}{dMeasure R6 object}

\item{description}{list, with '$id'
the server description to be deleted identified by '$id'}
}
\value{
dataframe - full list of database descriptions
 can also return error (stop) if description is invalid
}
\description{
remove a server description
}
