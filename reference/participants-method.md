# Get participants from a BIDS project

This function retrieves a vector of unique participant IDs from a BIDS
project. It extracts the subject identifiers from the project's data
table, filtering out any NA values. Participant IDs in BIDS typically
follow the format 'sub-XX'.

## Usage

``` r
participants(x, ...)

# S3 method for class 'bids_project'
participants(x, ...)
```

## Arguments

- x:

  the `bids_project` object

- ...:

  extra args passed to methods

## Value

A character vector of unique participant IDs found in the BIDS project.
If no participants are found or the 'subid' column doesn't exist in the
project's data table, returns an empty character vector.

## Examples

``` r
# \donttest{
# Get participants from a BIDS project
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  participants(proj)
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
# }
```
