# Get functional scans from a BIDS project

This function extracts functional scan files from a BIDS project that
match specified criteria such as subject ID, task name, run number, and
session. It can return either full paths or relative paths to the files.

## Usage

``` r
func_scans(x, ...)

# S3 method for class 'mock_bids_project'
func_scans(
  x,
  subid = ".*",
  task = ".*",
  run = ".*",
  session = ".*",
  kind = "bold",
  suffix = "nii(\\.gz)?$",
  full_path = TRUE,
  ...
)
```

## Arguments

- x:

  A mock_bids_project object

- ...:

  Additional arguments passed to search_files

- subid:

  Regex to match subject IDs (default: ".\*")

- task:

  Regex to match tasks (default: ".\*")

- run:

  Regex to match runs (default: ".\*")

- session:

  Regex to match sessions (default: ".\*")

- kind:

  Type of functional data (default: "bold")

- suffix:

  Regex pattern for file suffix (default: "nii(\\gz)?\$")

- full_path:

  If TRUE, return full file paths (default: TRUE)

## Value

A character vector of file paths to functional scans matching the
criteria. Returns NULL if no matching files are found.

## Examples

``` r
# \donttest{
# Create a BIDS project object
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  
  # Get all functional scans
  all_scans <- func_scans(proj)
  
  # Get scans for specific subjects
  if (length(participants(proj)) > 0) {
    sub_scans <- func_scans(proj, subid=participants(proj)[1])
  }
  
  # Get scans for a specific task and run
  if (length(tasks(proj)) > 0) {
    task_scans <- func_scans(proj, task=tasks(proj)[1], run="01")
  }
  
  # Get scans with relative paths
  rel_scans <- func_scans(proj, full_path=FALSE)
  
  # Also try with a dataset that has sessions
  ds007_path <- get_example_bids_dataset("ds007")
  ds007_proj <- bids_project(ds007_path)
  if (length(sessions(ds007_proj)) > 0) {
    session_scans <- func_scans(ds007_proj, session=sessions(ds007_proj)[1])
  }
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
  unlink(ds007_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
# }
```
