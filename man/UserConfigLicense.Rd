% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dMeasure.R
\name{UserConfigLicense}
\alias{UserConfigLicense}
\title{show user configurations with license}
\usage{
UserConfigLicense(dMeasure_obj)
}
\arguments{
\item{dMeasure_obj}{dMeasure R6 object}
}
\value{
dataframe of user configuration descriptions
 id, Fullname, AuthIdentity, Location, Attributes, License

 Fullname - Best Practice full user name
 AuthIdentity - Windows login identity
 Location - vector of groups/locations
 Attributes - vector of user's attributes/permissions
 License - undecoded license
 Identifier - identifier used to interrogate subscription database
 LicenseDate - date of license
}
\description{
show user configurations with license
}
\examples{
dMeasure_obj <- dMeasure$new()
dMeasure_obj$open_configuration_db()
dMeasure_obj$read_configuration_db()
dMeasure_obj$UserConfigLicense
}
