# Create a preprocessing mask from BIDS data

Create a preprocessing mask from BIDS data

## Usage

``` r
create_preproc_mask(x, subid, thresh = 0.99, ...)
```

## Arguments

- x:

  A bids_project object

- subid:

  A regular expression pattern to match subject IDs

- thresh:

  Threshold value for mask creation (default: 0.99)

- ...:

  Additional arguments passed to methods

## Value

A logical mask volume

## Examples

``` r
# \donttest{
# Download and load a BIDS project with fMRIPrep derivatives
tryCatch({
  ds001_deriv_path <- get_example_bids_dataset("ds000001-fmriprep")
  proj <- bids_project(ds001_deriv_path, fmriprep=TRUE)
  mask <- create_preproc_mask(proj, subid=".*")
  
  # Create mask for single subject
  sub01_mask <- create_preproc_mask(proj, subid="01")
  
  # Clean up
  unlink(ds001_deriv_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires derivatives dataset: ", e$message)
})
#> Example requires derivatives dataset: participants.tsv is missing
# }
```
