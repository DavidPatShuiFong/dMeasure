% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dMeasure.R
\name{BPdatabaseChoice}
\alias{BPdatabaseChoice}
\title{choose (or read) database choice}
\usage{
BPdatabaseChoice(dMeasure_obj, choice)
}
\arguments{
\item{dMeasure_obj}{dMeasure R6 object}

\item{choice}{(optional) name of database choice

 possible value includes "None", which will close any current database}
}
\value{
the current database choice, if choice not provided
}
\description{
This must be one of 'None' or one of the defined databases.
Tries to open the database. If fails, will be set to 'None'.
}
\details{
Sets $BPdatabasechoiceR reactive, if shiny/reactive
environment available

(Stored in private$.BPdatabaseChoice)
}
\examples{
dMeasure_obj$BPdatabaseChoice # returns the current choice
dMeasure_obj$BPdatabaseChoice <- "None" # sets database to none
}
