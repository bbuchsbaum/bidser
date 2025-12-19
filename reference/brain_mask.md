# Retrieve a brain mask for a subject

This convenience function wraps
[`create_preproc_mask()`](https://bbuchsbaum.github.io/bidser/reference/create_preproc_mask.md)
and returns a brain mask volume for a given subject.

## Usage

``` r
brain_mask(x, subid, ...)

# S3 method for class 'bids_project'
brain_mask(x, subid, ...)
```

## Arguments

- x:

  A bids_project object

- subid:

  A regular expression pattern to match subject IDs

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
  mask <- brain_mask(proj, subid="01")
  
  # Create mask for multiple subjects
  multi_mask <- brain_mask(proj, subid=".*")
  
  # Clean up
  unlink(ds001_deriv_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires derivatives dataset: ", e$message)
})
#> Example requires derivatives dataset: participants.tsv is missing
# }
```
