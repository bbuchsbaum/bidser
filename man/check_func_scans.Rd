% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.R
\name{check_func_scans}
\alias{check_func_scans}
\title{Check Functional Scans in a BIDS Project}
\usage{
check_func_scans(x)
}
\arguments{
\item{x}{A \code{bids_project} object created by \code{bids_project()}.}
}
\value{
A list containing:
\itemize{
\item \code{scans}: A tibble with details of all functional scans, including:
\itemize{
\item Subject ID
\item Task name
\item Run number
\item File size
\item Full file path
}
\item \code{tasklist}: A vector of unique tasks found in the project
\item \code{scans_per_subject}: A summary tibble showing the number of scans per subject
}

If multiple tasks are present, also includes:
\itemize{
\item \code{scans_per_task}: Summary of scan counts by task
\item \code{scans_per_task_subject}: Summary of scan counts by subject and task
\item \code{size_per_task}: Tibble with file size statistics by task
}

If only one task is present:
\itemize{
\item \code{size_per_subject}: Tibble with file size statistics by subject
}
}
\description{
This function performs a comprehensive inspection of functional scans within a BIDS project,
providing detailed summaries of scan counts and file sizes per subject and task. It helps
identify potential issues such as missing scans, inconsistent file sizes, or unexpected
variations in the data.
}
\examples{
# Create a BIDS project object
proj <- bids_project(system.file("extdata/ds001", package="bidser"))

# Check functional scans
scan_check <- check_func_scans(proj)

# View available tasks
print(scan_check$tasklist)

# Check scan counts per subject
print(scan_check$scans_per_subject)

# Example with multiple tasks
ds007 <- bids_project(system.file("extdata/ds007", package="bidser"))
multi_check <- check_func_scans(ds007)

# View scan distribution across tasks
print(multi_check$scans_per_task)

# Check for potential issues
if (nrow(multi_check$scans) > 0) {
  # Look for subjects with fewer scans than expected
  expected_scans <- 4  # Example: expecting 4 scans per subject
  missing <- multi_check$scans_per_subject[
    multi_check$scans_per_subject$n < expected_scans,
  ]
  if (nrow(missing) > 0) {
    print("Subjects with missing scans:")
    print(missing)
  }
}

}
