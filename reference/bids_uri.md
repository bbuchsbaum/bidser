# Construct a BIDS URI object

Parses a BIDS URI string of the form
`bids:<dataset_name>:<relative_path>` and returns an S3 object of class
`bids_uri`.

## Usage

``` r
bids_uri(uri)
```

## Arguments

- uri:

  A character scalar BIDS URI, e.g.
  `"bids::sub-01/fmap/sub-01_epi.nii.gz"` or
  `"bids:deriv1:sub-01/anat/T1w.nii.gz"`.

## Value

An object of class `bids_uri` with fields `dataset_name`,
`relative_path`, and `uri`.

## Examples

``` r
u <- bids_uri("bids::sub-01/func/sub-01_task-rest_bold.nii.gz")
u$dataset_name   # ""
#> [1] ""
u$relative_path  # "sub-01/func/sub-01_task-rest_bold.nii.gz"
#> [1] "sub-01/func/sub-01_task-rest_bold.nii.gz"

# URI with a named dataset link
u2 <- bids_uri("bids:deriv1:sub-01/anat/sub-01_T1w.nii.gz")
u2$dataset_name  # "deriv1"
#> [1] "deriv1"
```
