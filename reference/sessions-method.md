# Get sessions from a BIDS project

This function retrieves a vector of session IDs from a BIDS project.
Sessions in BIDS are typically represented as directories named 'ses-XX'
within subject directories. This function extracts and returns the
unique session identifiers.

## Usage

``` r
sessions(x, ...)

# S3 method for class 'bids_project'
sessions(x, ...)
```

## Arguments

- x:

  the object to extract sessions from

- ...:

  extra args passed to methods

## Value

A character vector of unique session IDs if the project has sessions, or
NULL if the project does not have sessions

## Examples

``` r
# \donttest{
# Get sessions from a BIDS project
tryCatch({
  ds007_path <- get_example_bids_dataset("ds007")
  proj <- bids_project(ds007_path)
  sessions(proj)
  
  # Clean up (disabled for performance - cached dataset)
  # unlink(ds007_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
#> NULL
# }
```
