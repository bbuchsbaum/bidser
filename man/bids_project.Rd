% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bids.R
\name{bids_project}
\alias{bids_project}
\title{Create a BIDS Project Object}
\usage{
bids_project(path = ".", fmriprep = FALSE, prep_dir = "derivatives/fmriprep")
}
\arguments{
\item{path}{Character string. The file path to the root of the BIDS project.
Defaults to the current directory (".").}

\item{fmriprep}{Logical. Whether to load the fMRIPrep derivatives folder hierarchy.
Defaults to FALSE.}

\item{prep_dir}{Character string. The location of the fMRIPrep subfolder relative
to the derivatives directory. Defaults to "derivatives/fmriprep".}
}
\value{
A \code{bids_project} object representing the BIDS project structure. The object
provides methods for:
\itemize{
\item Accessing raw and preprocessed data files
\item Querying subjects, sessions, and tasks
\item Reading event files and confound regressors
\item Checking BIDS compliance
\item Extracting metadata from file names
Returns NULL if the directory does not contain a valid BIDS dataset.
}
}
\description{
This function creates a BIDS project object from a directory containing BIDS-formatted
neuroimaging data. It can optionally load preprocessed derivatives from fMRIPrep.
The function validates the basic BIDS structure and provides methods for accessing
raw and preprocessed data, querying subjects, sessions, and tasks, reading event
files, and checking BIDS compliance.
}
\examples{
\donttest{
# Create a BIDS project
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  
  # Get all functional scans
  all_scans <- func_scans(proj)
  
  # Get scans for specific subjects
  sub_scans <- func_scans(proj, subid="0[123]")
  
  # Get scans for a specific task
  task_scans <- func_scans(proj, task="rest")
  
  # Get scans from specific runs
  run_scans <- func_scans(proj, run="0[123]")
  
  # Combine multiple filters
  filtered_scans <- func_scans(proj,
                              subid="01",
                              task="rest",
                              run="01")
  
  # Get relative paths instead of full paths
  rel_scans <- func_scans(proj, full_path=FALSE)
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
}

}
