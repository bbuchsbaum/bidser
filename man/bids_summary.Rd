% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_generic.R, R/bids.R
\name{bids_summary}
\alias{bids_summary}
\title{Summarize a BIDS dataset}
\usage{
bids_summary(x)

bids_summary(x)
}
\arguments{
\item{x}{A \code{bids_project} object.}
}
\value{
A list containing summary statistics about the BIDS dataset

A list with summary information:
\itemize{
\item \code{n_subjects}: number of participants
\item \code{n_sessions}: number of sessions (if any), otherwise NULL
\item \code{tasks}: a data frame with \code{task} and \code{n_runs} columns
\item \code{total_runs}: total number of runs across the dataset
}
}
\description{
Provides a quick summary of dataset statistics, including:
\itemize{
\item Number of subjects
\item Number of sessions (if applicable)
\item Available tasks and the number of runs per task
\item Total number of runs
}
}
\examples{
\donttest{
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  summary <- bids_summary(proj)
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
}
}
