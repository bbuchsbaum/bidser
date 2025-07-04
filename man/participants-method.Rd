% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_generic.R, R/bids.R
\name{participants}
\alias{participants}
\alias{participants.bids_project}
\title{Get participants from a BIDS project}
\usage{
participants(x, ...)

\method{participants}{bids_project}(x, ...)
}
\arguments{
\item{x}{the \code{bids_project} object}

\item{...}{extra args passed to methods}
}
\value{
A character vector of unique participant IDs found in the BIDS project.
If no participants are found or the 'subid' column doesn't exist in the project's
data table, returns an empty character vector.
}
\description{
This function retrieves a vector of unique participant IDs from a BIDS project.
It extracts the subject identifiers from the project's data table, filtering out
any NA values. Participant IDs in BIDS typically follow the format 'sub-XX'.
}
\examples{
\donttest{
# Get participants from a BIDS project
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  participants(proj)
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
}
}
