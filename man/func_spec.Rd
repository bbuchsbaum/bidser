% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/specs.R
\name{func_spec}
\alias{func_spec}
\title{Create a spec table for "func" files}
\usage{
func_spec()
}
\description{
The \code{spec} object describes pattern rules for matching BIDS filenames.
It consists of a \code{keystruc} table and a \code{kinds} table. The \code{keystruc} table
defines keys, their regex patterns, their optionality, etc. The \code{kinds} table
defines allowed file kinds and their suffixes.
}
\value{
A list containing \code{keystruc}, \code{kinds}, and \code{type} for functional files.
}
\examples{
func_spec()
}
\keyword{internal}
