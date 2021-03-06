% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/observations.R
\name{PersistentProteinuria_obs}
\alias{PersistentProteinuria_obs}
\title{List of patients with persistent proteinuria}
\usage{
PersistentProteinuria_obs(dMeasure_obj, intID, date_from = NA, date_to = NA)
}
\arguments{
\item{dMeasure_obj}{dMeasure R6 object}

\item{intID}{vector of InternalID}

\item{date_from}{start date. default is "1900-01-01" (accepts -Inf)}

\item{date_to}{end date (inclusive). default is $date_b}
}
\value{
dataframe of InternalID, PersistentProteinuria
}
\description{
Filtered by InternalID (vector patient numbers) and dates
}
\details{
From http://cvdcheck.org.au/pdf/Absolute_CVD_Risk_Full_Guidelines.pdf

Proteinuria is defined as
  urinary albumin:creatinine ratio (UACR)
  > 35 mg/mmol in females
  > 25 mg/mmol in males.

Persistent proteinuria is defined as 2 positive measurements, 3 months apart.
}
