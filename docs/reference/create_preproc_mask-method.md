# Create Preprocessing Mask (Mock Implementation)

This function is not implemented for `mock_bids_project` objects as they
do not contain actual image data required to create a mask.

## Usage

``` r
# S3 method for class 'mock_bids_project'
create_preproc_mask(x, ...)
```

## Arguments

- x:

  A `mock_bids_project` object.

- ...:

  Arguments (ignored).

## Value

Throws an error indicating the function is not applicable to mock
objects.

## Examples

``` r
mock <- create_mock_bids("Test", c("01"), tibble::tibble(
  subid = "01", datatype = "func",
  suffix = "bold.nii.gz", fmriprep = FALSE
))
#> Warning: Encoding failed for: sub-01_bold.nii.gz - skipping this file in mock tree.
try(create_preproc_mask(mock))
#> Error in create_preproc_mask(mock) : 
#>   `create_preproc_mask` requires actual image data and cannot be used with `mock_bids_project` objects.
```
