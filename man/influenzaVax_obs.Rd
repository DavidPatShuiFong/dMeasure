% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/observations.R
\name{influenzaVax_obs}
\alias{influenzaVax_obs}
\title{List of influenza immunization observations/recordings}
\usage{
influenzaVax_obs(dMeasure_obj, intID, date_from = NA, date_to = NA)
}
\arguments{
\item{dMeasure_obj}{dMeasure R6 object}

\item{intID}{vector of InternalID}

\item{date_from}{start date. default is $date_b minus 15 months}

\item{date_to}{end date (inclusive). default is $date_b}
}
\value{
dataframe of InternalID, FluvaxName, FluvaxDate
}
\description{
Filtered by InternalID (vector patient numbers) and dates
}
\details{
the reference date for 'most recent' measurement is 'date_to'
}
