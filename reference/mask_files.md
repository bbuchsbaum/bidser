# Query Mask Files from a BIDS Project

Retrieves paths to brain mask files from a BIDS project, optionally
filtered by subject, session, and coordinate space. Mask files are
typically found in fMRIPrep derivatives and include brain masks and
tissue segmentation masks.

## Usage

``` r
mask_files(
  x,
  subid = ".*",
  session = ".*",
  space = ".*",
  full_path = TRUE,
  ...
)

# S3 method for class 'bids_project'
mask_files(
  x,
  subid = ".*",
  session = ".*",
  space = ".*",
  full_path = TRUE,
  ...
)

# S3 method for class 'mock_bids_project'
mask_files(
  x,
  subid = ".*",
  session = ".*",
  space = ".*",
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

- space:

  Regex pattern to match coordinate space (e.g.,
  `"MNI152NLin2009cAsym"`, `"T1w"`). Default `".*"` matches all spaces.

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
# Get all mask files
tryCatch({
  ds_path <- get_example_bids_dataset("ds000001-fmriprep")
  proj <- bids_project(ds_path, fmriprep = TRUE)

  # All masks
  all_masks <- mask_files(proj)

  # Masks in MNI space
  mni_masks <- mask_files(proj, space = "MNI152")

  # Masks for specific subject
  sub01_masks <- mask_files(proj, subid = "01")

  # Clean up
  unlink(ds_path, recursive = TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
#> Example requires internet connection: participants.tsv is missing
# }
```
