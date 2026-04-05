# Get "flat" representation of BIDS Project

This function returns a flattened (non-hierarchical) representation of a
BIDS project formatted as a data frame. It extracts file paths or file
names from the BIDS tree structure, filtering for entries that start
with "sub-" to focus on subject-level data.

## Usage

``` r
flat_list(x, ...)

# S3 method for class 'bids_project'
flat_list(x, full_path = TRUE, ...)
```

## Arguments

- x:

  the `bids_project` object

- ...:

  extra args passed to methods

- full_path:

  If TRUE, return full paths to files; if FALSE, return just file names
  (default: TRUE)

## Value

A data frame containing either full paths to files (if `full_path=TRUE`)
or just the file names (if `full_path=FALSE`). Each row represents one
file in the BIDS project.

## Examples

``` r
# \donttest{
# Get flat representation with full paths
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  flat_list(proj)
  
  # Get flat representation with just file names
  flat_list(proj, full_path=FALSE)
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
# }
```
