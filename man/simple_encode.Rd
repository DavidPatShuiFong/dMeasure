% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculation_definitions.R
\name{simple_encode}
\alias{simple_encode}
\title{Simple encoder}
\usage{
simple_encode(msg, key = NULL, nonce = NULL)
}
\arguments{
\item{msg}{the text to encode}

\item{key}{the cipher, which can be set manually, otherwise will read from env}

\item{nonce}{a non-secret unique data value used to randomize the cipher}
}
\value{
- the encrypted text.

  returns NA for elements of msg which are NA
}
\description{
Simple encode of text strings, will output a text string.
Uses sodium library and base64_enc/dec from jsonlite. Has some defaults, but
will also take command-line arguments or read from environment
}
