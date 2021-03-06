% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculation_definitions.R
\name{add_age}
\alias{add_age}
\title{Add age to a given reference date}
\usage{
add_age(birthDate, age, by = "year")
}
\arguments{
\item{age}{numeric}

\item{by}{default is "year", but can be, for example "-1 month"}

\item{birthdate}{vector of dates}
}
\value{
vector of dates
}
\description{
Adds an interval (years) to a birthDate
}
