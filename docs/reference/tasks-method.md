# Get tasks from a BIDS project

This function retrieves a sorted vector of unique task names from a BIDS
project. Tasks in BIDS are typically represented in filenames with the
pattern 'task-XX'. This function extracts and returns the unique task
identifiers, filtering out any NULL or NA values.

## Usage

``` r
tasks(x, ...)

# S3 method for class 'bids_project'
tasks(x, ...)
```

## Arguments

- x:

  the object to extract tasks from

- ...:

  extra args passed to methods

## Value

A character vector of unique, sorted task names found in the BIDS
project

## Examples

``` r
# \donttest{
# Get tasks from a BIDS project
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  tasks(proj)
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
# }
```
