% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dMeasure.R
\name{read_dMeasureModules}
\alias{read_dMeasureModules}
\alias{dMeasureModules}
\title{read_dMeasureModules}
\usage{
dMeasureModules(dMeasure_obj)
}
\arguments{
\item{none}{}
}
\value{
a dataframe providing module details e.g. Package name
}
\description{
discover available dMeasure module packages
}
\details{
dMeasure's function is extended by modules, such as \code{dMeasureAppointments},
\code{dMeasureMedication}, \code{dMeasureQIM} and \code{dMeasureBilling}

\code{read_dMeasureModules} detects which of these modules (R packages) have
been installed and reads the details.
\itemize{
\item dMeasure modules all start with the prefix \code{dMeasure}
\item dMeasure modules all export the function \code{dMeasureIntegration}
\itemize{
\item the function \code{dMeasureIntegration} (and others) can return details of the module package
\itemize{
\item typically \code{Package}, \code{Provides}, \code{Requires}, \code{moduleID}
\item \code{Package} - the module package name
\item \code{Provides} - the functionality the module package provides
\item \code{Requires} - the dependencies of the module package. Can be just \code{dMeasure}, but also can be other modules
\item \code{moduleID} - the ID when the server component of the module is executed. can be multiple IDs
\itemize{
\item can be a list of IDs with optional \verb{$extraargs} extra arguments to pass to server component of module
}
\item \code{configID} - optional ID of server component of configuration panel
}
}
\item dMeasure modules can provide additional information
\itemize{
\item optional \code{sidebarmenuPriority} function helps arrange position of module in left sidebar.
}
}

\code{read_dMeasureModules} stores the results in \verb{$dMeasureModules} and returns the dataframe
+ \verb{$dMeasureModules} is used in methods such as \verb{$open_configuration_db}, and so
\code{read_dMeasureModules} may need to be called early in the work-flow if the configuration
information is required.
}
