% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/configuration_password.R
\name{password.reset}
\alias{password.reset}
\title{password.reset}
\usage{
password.reset(dMeasure_obj, user, newpassword = "")
}
\arguments{
\item{dMeasure_obj}{dMeasure R6 object}

\item{user}{Full name of user (not the current identified user)}

\item{newpassword}{the new password ("" empty string by default)

No return value
 Error (stop) if trying to reset a user which
 doesn't exist, or trying to reset own password}
}
\description{
Reset pass of (another) user to empty string
}
