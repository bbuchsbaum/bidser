# Search files in BIDS structure

This function searches for files in a BIDS project that match a
specified pattern and optional key-value criteria. It can be used to
find files in both raw data and preprocessed derivatives based on
filename patterns and BIDS metadata.

This function searches for files in a BIDS project that match a
specified pattern and optional key-value criteria. It can search in both
raw data and preprocessed derivatives (if available).

Finds files in the mock BIDS tree by matching file names and BIDS
entities.

## Usage

``` r
search_files(x, ...)

# S3 method for class 'bids_project'
search_files(x, regex = ".*", full_path = FALSE, strict = TRUE, ...)

# S3 method for class 'mock_bids_project'
search_files(x, regex = ".*", full_path = FALSE, strict = TRUE, ...)
```

## Arguments

- x:

  A `mock_bids_project` object.

- ...:

  Additional BIDS entities to match (e.g., `subid = "01"`,
  `task = "rest"`). Values are treated as regex patterns unless they are
  simple strings without regex characters.

- regex:

  A regular expression to match filenames (node names). Default `".*"`.

- full_path:

  If `TRUE`, return full paths (prefixed with `x$path`). If `FALSE`,
  return relative paths within the BIDS structure. Default `FALSE`.

- strict:

  If `TRUE` (default), queries for a BIDS entity (e.g., `task="X"`)
  require the entity to exist on the file node and match the pattern. If
  `FALSE`, files lacking the queried entity are not automatically
  excluded (though they won't match if the pattern isn't `.*`).

## Value

A character vector of file paths matching the criteria, or NULL if no
matches found.

A character vector of file paths matching the criteria, or NULL if no
matches found.

A character vector of matching file paths, or `NULL` if no matches.

## Examples

``` r
# \donttest{
# Search for event files in a BIDS dataset  
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path, fmriprep=FALSE)
  event_files <- search_files(proj, regex="events\\.tsv$")
  
  # Search with additional criteria
  sub01_files <- search_files(proj, regex="bold\\.nii\\.gz$", subid="01", 
                              task="balloonanalogrisktask")
  
  # Get full paths
  full_paths <- search_files(proj, regex="events\\.tsv$", full_path=TRUE)
  
  # Search with strict matching
  strict_matches <- search_files(proj, regex="\\.tsv$", strict=TRUE, 
                                 task="balloonanalogrisktask")
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
# }
# \donttest{
# Search for event files in a BIDS dataset
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path, fmriprep=FALSE)
  event_files <- search_files(proj, regex="events\\.tsv$")
  
  # Search with additional criteria (note: ds001 only has one subject '01')
  sub01_files <- search_files(proj, regex="bold\\.nii\\.gz$", subid="01", 
                              task="balloonanalogrisktask")
  
  # Get full paths
  full_paths <- search_files(proj, regex="events\\.tsv$", full_path=TRUE)
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
# }
```
