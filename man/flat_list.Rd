% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_generic.R
\name{flat_list}
\alias{flat_list}
\title{Get "flat" representation of BIDS Project}
\usage{
flat_list(x, ...)
}
\arguments{
\item{x}{the \code{bids_project} object}

\item{...}{extra args}
}
\description{
get a flattened (non-hiearchical) representation of project formatted as a \code{data.frame}
}
\examples{
p <- system.file("inst/extdata/ds001", package="bidser")
flat_list(bids_project(p))
}
