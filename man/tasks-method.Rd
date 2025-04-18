% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_generic.R, R/bids.R
\name{tasks}
\alias{tasks}
\alias{tasks.bids_project}
\title{Get tasks from a BIDS project}
\usage{
tasks(x, ...)

\method{tasks}{bids_project}(x)
}
\arguments{
\item{x}{the object to extract tasks from}

\item{...}{extra args passed to methods}
}
\value{
A character vector of unique, sorted task names found in the BIDS project
}
\description{
This function retrieves a sorted vector of unique task names from a BIDS project.
Tasks in BIDS are typically represented in filenames with the pattern 'task-XX'.
This function extracts and returns the unique task identifiers, filtering out
any NULL or NA values.
}
\examples{
# Get tasks from a BIDS project
p <- system.file("extdata/ds001", package="bidser")
tasks(bids_project(p))

# Create a BIDS structure with specific tasks and check them
bs <- bids_structure(name="Test", subjects=c("01", "02"), 
                    tasks=c("rest", "memory", "working"))
tasks(bs)
}
