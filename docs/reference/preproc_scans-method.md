# Get preprocessed functional MRI scans

This function retrieves paths to preprocessed functional MRI scans from
a BIDS project. It searches for files in the fMRIPrep derivatives
directory that match specified criteria, such as subject ID, task, run,
and other BIDS metadata. Preprocessed scans are identified by having
either 'desc-preproc' or 'kind-preproc' in their filename.

## Usage

``` r
preproc_scans(
  x,
  subid = ".*",
  task = ".*",
  run = ".*",
  session = ".*",
  variant = NULL,
  space = ".*",
  modality = "bold",
  kind = ".*",
  full_path = FALSE,
  ...
)
```

## Arguments

- x:

  A `bids_project` object

- subid:

  Subject ID regex to match specific subjects (default: ".\*" for all
  subjects)

- task:

  Task regex to match specific tasks (default: ".\*" for all tasks)

- run:

  Run regex to match specific runs (default: ".\*" for all runs)

- session:

  Session regex to match specific sessions (default: ".\*" for all
  sessions)

- variant:

  Preprocessing variant to match (default: NULL, which matches files
  without a variant)

- space:

  Space regex to match specific spaces (default: ".\*" for all spaces)

- modality:

  Image modality to match (default: "bold" for functional MRI)

- kind:

  Kind regex to match specific kinds (default: ".\*" for all kinds)

- full_path:

  If TRUE, return full file paths; if FALSE, return paths relative to
  the project root (default: FALSE)

- ...:

  Additional arguments passed to internal functions

## Value

A character vector of file paths to preprocessed functional scans
matching the criteria. If no matching files are found, returns NULL.

## Examples

``` r
# Get all preprocessed scans from a BIDS project with fMRIPrep derivatives
# \donttest{
# Download and load a BIDS project with fMRIPrep derivatives
tryCatch({
  ds001_deriv_path <- get_example_bids_dataset("ds000001-fmriprep")
  proj <- bids_project(ds001_deriv_path, fmriprep=TRUE)
  
  # Get all preprocessed scans
  scans <- preproc_scans(proj)
  
  # Get preprocessed scans for a specific subject
  if (!is.null(scans) && length(scans) > 0) {
    sub01_scans <- preproc_scans(proj, subid="01")
  }
  
  # Clean up
  unlink(ds001_deriv_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires derivatives dataset: ", e$message)
})
#> Example requires derivatives dataset: participants.tsv is missing
# }
```
