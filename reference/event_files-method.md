# Get event files from a BIDS project

This function retrieves a vector of event files (events.tsv) from a BIDS
project that match specified criteria. Event files in BIDS contain trial
information for task-based functional MRI data, including onset times,
durations, and trial types.

Finds event files matching the given subject, task, run, and session
criteria.

## Usage

``` r
event_files(x, ...)

# S3 method for class 'bids_project'
event_files(
  x,
  subid = ".*",
  task = ".*",
  run = ".*",
  session = ".*",
  full_path = TRUE,
  ...
)

# S3 method for class 'mock_bids_project'
event_files(
  x,
  subid = ".*",
  task = ".*",
  run = ".*",
  session = ".*",
  full_path = TRUE,
  ...
)
```

## Arguments

- x:

  A mock_bids_project object

- ...:

  Additional arguments passed to internal functions

- subid:

  Regex to match subject IDs (default: ".\*")

- task:

  Regex to match tasks (default: ".\*")

- run:

  Regex to match runs (default: ".\*")

- session:

  Regex to match sessions (default: ".\*")

- full_path:

  If TRUE, return full paths of files (default: TRUE)

## Value

A character vector of file paths to event files matching the specified
criteria. If no matching files are found, returns NULL.

A character vector of file paths to event files. If no matching files
are found, returns an empty character vector.

## Examples

``` r
# \donttest{
# Get all event files from a BIDS project
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  event_files(proj)
  
  # Get event files for specific subjects and tasks
  if (length(participants(proj)) > 0) {
    event_files(proj, subid=participants(proj)[1], task="balloonanalogrisktask")
  }
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
# }
# \donttest{
# Get event files for a specific subject and task
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  x <- bids_project(ds001_path)
  files <- event_files(x, subid="01", task="balloonanalogrisktask")
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
# }
```
