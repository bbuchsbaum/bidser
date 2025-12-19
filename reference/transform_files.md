# Query Transform Files from a BIDS Project

Retrieves paths to transformation files (xfm, warp, affine) from a BIDS
project, optionally filtered by source and target coordinate space.
Transform files are typically found in fMRIPrep derivatives and encode
spatial transformations between different coordinate spaces (e.g., T1w
to MNI, boldref to T1w).

## Usage

``` r
transform_files(
  x,
  subid = ".*",
  session = ".*",
  from = ".*",
  to = ".*",
  mode = ".*",
  kind = ".*",
  full_path = TRUE,
  ...
)

# S3 method for class 'bids_project'
transform_files(
  x,
  subid = ".*",
  session = ".*",
  from = ".*",
  to = ".*",
  mode = ".*",
  kind = ".*",
  full_path = TRUE,
  ...
)

# S3 method for class 'mock_bids_project'
transform_files(
  x,
  subid = ".*",
  session = ".*",
  from = ".*",
  to = ".*",
  mode = ".*",
  kind = ".*",
  full_path = TRUE,
  ...
)
```

## Arguments

- x:

  A `bids_project` or `mock_bids_project` object.

- subid:

  Regex pattern to match subject IDs (without "sub-" prefix). Default
  `".*"` matches all subjects.

- session:

  Regex pattern to match session IDs (without "ses-" prefix). Default
  `".*"` matches all sessions.

- from:

  Regex pattern to match source space (the "from" BIDS entity). Common
  values: "T1w", "boldref", "fsnative". Default `".*"` matches all.

- to:

  Regex pattern to match target space (the "to" BIDS entity). Common
  values: "MNI152NLin2009cAsym", "T1w", "fsnative". Default `".*"`
  matches all.

- mode:

  Regex pattern to match transform mode entity. Default `".*"`.

- kind:

  Transform type: `"xfm"`, `"warp"`, `"affine"`, or `".*"` for all
  types. Default `".*"` matches all transform types.

- full_path:

  Logical. If `TRUE` (default), return absolute file paths. If `FALSE`,
  return paths relative to project root.

- ...:

  Additional arguments passed to `search_files`.

## Value

Character vector of file paths matching the criteria, or `NULL` if no
matching files are found.

## Examples

``` r
# \donttest{
# Get all transform files
tryCatch({
  ds_path <- get_example_bids_dataset("ds000001-fmriprep")
  proj <- bids_project(ds_path, fmriprep = TRUE)

  # All transforms
  all_xfms <- transform_files(proj)

  # Transforms from T1w to MNI space
  t1_to_mni <- transform_files(proj, from = "T1w", to = "MNI152")

  # All transforms for a specific subject
  sub01_xfms <- transform_files(proj, subid = "01")

  # Clean up
  unlink(ds_path, recursive = TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
#> Example requires internet connection: participants.tsv is missing
# }
```
