% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match_helpers.R
\name{mandatory_key}
\alias{mandatory_key}
\title{Create a parser for a mandatory BIDS key}
\usage{
mandatory_key(label, regex = "[A-Za-z0-9]+")
}
\arguments{
\item{label}{The label of the key to parse.}

\item{regex}{The regex for the key's value. Must be a single character string.}
}
\value{
A parser that must match one occurrence of \verb{_<label>-<id>}
}
\description{
Matches a pattern \verb{_<label>-<id>} where \code{id} matches a given regex.
}
\keyword{internal}
