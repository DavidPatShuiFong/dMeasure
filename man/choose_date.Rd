% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dMeasure.R
\name{choose_date}
\alias{choose_date}
\title{Choose date}
\usage{
choose_date(
  dMeasure_obj,
  date_from = dMeasure_obj$date_a,
  date_to = dMeasure_obj$date_b
)
}
\arguments{
\item{dMeasure_obj}{dateContact R6 object}

\item{date_from}{'From' date. default is current date_from}

\item{date_to}{'To' date. default is current date_to}
}
\value{
list(date_a, date_b)

if date_a is later than date_b, a warning is returned,
and the dates are NOT changed
}
\description{
Sets 'from' and 'to' dates used in subsequent searches
}
