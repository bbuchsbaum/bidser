# Build Subject Graph Structure

Creates a structured list or tibble containing all available data for a
single subject, organized by data type. This provides a comprehensive
view of all available files for a subject, useful for batch processing
and pipeline ingestion.

## Usage

``` r
build_subject_graph(x, subid, session = ".*", flatten = FALSE, ...)

# S3 method for class 'bids_project'
build_subject_graph(x, subid, session = ".*", flatten = FALSE, ...)

# S3 method for class 'mock_bids_project'
build_subject_graph(x, subid, session = ".*", flatten = FALSE, ...)
```

## Arguments

- x:

  A `bids_project` or `mock_bids_project` object.

- subid:

  Subject identifier (with or without `sub-` prefix).

- session:

  Optional session filter. Default `".*"` matches all sessions.

- flatten:

  Logical. If `FALSE` (default), return a nested list structure. If
  `TRUE`, return a flat tibble with columns for file_type and metadata.

- ...:

  Additional arguments passed to underlying query functions.

## Value

If `flatten = FALSE` (default), a named list with class
`bids_subject_graph`:

- subid:

  Subject identifier (without "sub-" prefix)

- sessions:

  Character vector of available sessions

- epi:

  Named list of preprocessed EPI file paths, keyed by task.run

- anat:

  List with t1w and masks sublists

- transforms:

  Named list of transform files, keyed by from_to_to format

- surfaces:

  Nested list by space, then hemisphere (L/R)

- confounds:

  Character vector of confound file paths

If `flatten = TRUE`, a tibble with columns:

- file_type:

  Type of file (epi, anat, transform, surface, confound)

- path:

  File path

- subid, session, task, run, space, hemi, from, to:

  BIDS metadata

## Examples

``` r
# \donttest{
# Build subject graph
tryCatch({
  ds_path <- get_example_bids_dataset("ds000001-fmriprep")
  proj <- bids_project(ds_path, fmriprep = TRUE)

  # Get nested structure
  graph <- build_subject_graph(proj, "01")
  names(graph)

  # Get flat tibble
  flat <- build_subject_graph(proj, "01", flatten = TRUE)
  head(flat)

  # Clean up
  unlink(ds_path, recursive = TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
#> Example requires internet connection: participants.tsv is missing
# }
```
