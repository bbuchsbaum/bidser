# Query Surface Files from a BIDS Project

Retrieves paths to surface mesh files (GIFTI format, .gii) from a BIDS
project, optionally filtered by hemisphere and surface type. Surface
files are typically found in fMRIPrep derivatives and represent cortical
surface reconstructions in various coordinate spaces.

## Usage

``` r
surface_files(
  x,
  subid = ".*",
  session = ".*",
  hemi = ".*",
  surf_type = ".*",
  space = ".*",
  full_path = TRUE,
  ...
)

# S3 method for class 'bids_project'
surface_files(
  x,
  subid = ".*",
  session = ".*",
  hemi = ".*",
  surf_type = ".*",
  space = ".*",
  full_path = TRUE,
  ...
)

# S3 method for class 'mock_bids_project'
surface_files(
  x,
  subid = ".*",
  session = ".*",
  hemi = ".*",
  surf_type = ".*",
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

- hemi:

  Hemisphere filter: `"L"` for left, `"R"` for right, or `".*"` for
  both. Default `".*"` matches both hemispheres.

- surf_type:

  Surface type filter: `"pial"`, `"inflated"`, `"midthickness"`,
  `"smoothwm"`, `"white"`, `"sphere"`, `"spherereg"`, or `".*"` for all
  types. Default `".*"` matches all surface types.

- space:

  Regex pattern to match coordinate space (e.g., `"fsnative"`,
  `"fsaverage"`). Default `".*"` matches all spaces.

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
# Get all surface files
tryCatch({
  ds_path <- get_example_bids_dataset("ds000001-fmriprep")
  proj <- bids_project(ds_path, fmriprep = TRUE)

  # All surfaces
  all_surfs <- surface_files(proj)

  # Left hemisphere pial surfaces only
  left_pial <- surface_files(proj, hemi = "L", surf_type = "pial")

  # All surfaces in fsnative space
  fsnative_surfs <- surface_files(proj, space = "fsnative")

  # Clean up
  unlink(ds_path, recursive = TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
#> Example requires internet connection: participants.tsv is missing
# }
```
