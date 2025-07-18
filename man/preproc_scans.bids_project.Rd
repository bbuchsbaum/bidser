% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bids.R
\name{preproc_scans.bids_project}
\alias{preproc_scans.bids_project}
\title{Get preprocessed scans from a BIDS project}
\usage{
\method{preproc_scans}{bids_project}(
  x,
  subid = ".*",
  task = ".*",
  run = ".*",
  session = ".*",
  variant = NULL,
  space = ".*",
  modality = "bold",
  kind = ".*",
  full_path = FALSE,
  ...
)
}
\arguments{
\item{x}{A \code{bids_project} object.}

\item{subid}{A regex pattern for matching subjects. Default is ".*".}

\item{task}{A regex pattern for matching tasks. Default is ".*".}

\item{run}{A regex pattern for matching runs. Default is ".*".}

\item{session}{A regex pattern for matching sessions. Default is ".*".}

\item{variant}{A regex pattern for matching preprocessing variants. Default is NULL
(no variant filtering).}

\item{space}{A regex pattern for matching spaces (e.g., "MNI152NLin2009cAsym").
Default is ".*".}

\item{modality}{A regex pattern for matching modality. Default is "bold".
Set this to something else if you need a different modality.}

\item{kind}{The kind of preprocessed data to return. Default is ".*" to match any kind.}

\item{full_path}{If TRUE, return full file paths. Otherwise return relative paths.
Default is FALSE.}

\item{...}{Additional arguments passed to internal functions.}
}
\value{
A character vector of file paths to preprocessed scans matching the criteria.
Returns NULL if:
\itemize{
\item No matching files are found
\item The project doesn't have fMRIPrep derivatives
\item The specified criteria don't match any files
}
}
\description{
This function retrieves paths to preprocessed functional MRI scans from a BIDS project's
fMRIPrep derivatives. It allows filtering by various BIDS entities such as subject,
task, run, session, and space. The function is particularly useful for accessing
preprocessed data for analysis pipelines.
}
\examples{
\donttest{
# Create a BIDS project with fMRIPrep derivatives
tryCatch({
  ds_path <- get_example_bids_dataset("phoneme_stripped")
  proj <- bids_project(ds_path, fmriprep=TRUE)
  
  # Get all preprocessed BOLD scans
  all_scans <- preproc_scans(proj)
  
  # Get preprocessed scans for specific subjects
  sub_scans <- preproc_scans(proj, subid="0[12]")
  
  # Get scans in MNI space
  mni_scans <- preproc_scans(proj, space="MNI152NLin2009cAsym")
  
  # Get scans for a specific task with full paths
  task_scans <- preproc_scans(proj,
                             task="phoneme",
                             full_path=TRUE)
  
  # Get scans from a specific session
  session_scans <- preproc_scans(proj, session="test")
  
  # Combine multiple filters
  filtered_scans <- preproc_scans(proj,
                                 subid="01",
                                 task="phoneme",
                                 run="01",
                                 space="MNI152NLin2009cAsym")
  
  # Clean up
  unlink(ds_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
}

}
