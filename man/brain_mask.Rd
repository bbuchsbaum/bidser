% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_generic.R, R/bidsio.R
\name{brain_mask}
\alias{brain_mask}
\alias{brain_mask.bids_project}
\title{Retrieve a brain mask for a subject}
\usage{
brain_mask(x, subid, ...)

\method{brain_mask}{bids_project}(x, subid, ...)
}
\arguments{
\item{x}{A bids_project object}

\item{subid}{A regular expression pattern to match subject IDs}

\item{...}{Additional arguments passed to methods}
}
\value{
A logical mask volume
}
\description{
This convenience function wraps \code{\link[=create_preproc_mask]{create_preproc_mask()}} and
returns a brain mask volume for a given subject.
}
\examples{
\donttest{
# Download and load a BIDS project with fMRIPrep derivatives
tryCatch({
  ds001_deriv_path <- get_example_bids_dataset("ds000001-fmriprep")
  proj <- bids_project(ds001_deriv_path, fmriprep=TRUE)
  mask <- brain_mask(proj, subid="01")
  
  # Create mask for multiple subjects
  multi_mask <- brain_mask(proj, subid=".*")
  
  # Clean up
  unlink(ds001_deriv_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires derivatives dataset: ", e$message)
})
}
}
