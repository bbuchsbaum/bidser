# Get Repetition Time (TR) from a sidecar JSON

This function attempts to find and return the repetition time (TR) for a
given subject, task, and run (and optionally session) by locating the
associated BOLD sidecar JSON file and extracting the 'RepetitionTime'
field. If not found, returns NA.

## Usage

``` r
get_repetition_time(x, subid, task, run = ".*", session = ".*", ...)
```

## Arguments

- x:

  A `bids_project` object.

- subid:

  Subject ID (exact or regex).

- task:

  Task name (exact or regex).

- run:

  Run number (exact or regex). Default is ".\*" to allow flexible
  matching.

- session:

  Session ID (exact or regex). Default is ".\*".

- ...:

  Additional arguments passed to
  [`read_sidecar()`](https://bbuchsbaum.github.io/bidser/reference/read_sidecar.md).

## Value

A numeric value representing the RepetitionTime in seconds, or NA if not
found.

## Examples

``` r
# \donttest{
# Download and get TR for a specific subject and task
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  
  if (length(participants(proj)) > 0 && length(tasks(proj)) > 0) {
    tr <- get_repetition_time(proj, 
                             subid=participants(proj)[1], 
                             task=tasks(proj)[1])
    cat("TR:", tr, "seconds\n")
  }
  
  # Try with a dataset that has sessions
  ds007_path <- get_example_bids_dataset("ds007")
  ds007_proj <- bids_project(ds007_path)
  if (length(participants(ds007_proj)) > 0 && length(sessions(ds007_proj)) > 0) {
    tr_session <- get_repetition_time(ds007_proj,
                                     subid=participants(ds007_proj)[1],
                                     session=sessions(ds007_proj)[1])
    cat("TR with session:", tr_session, "seconds\n")
  }
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
  unlink(ds007_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
#> No matching JSON sidecar files found.
#> No matching sidecar JSON file found for the specified criteria.
#> TR: NA seconds
# }
```
