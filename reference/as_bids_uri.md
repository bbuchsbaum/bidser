# Coerce to a BIDS URI object

Coerce to a BIDS URI object

## Usage

``` r
as_bids_uri(x, ...)

# S3 method for class 'character'
as_bids_uri(x, ...)

# S3 method for class 'bids_uri'
as_bids_uri(x, ...)
```

## Arguments

- x:

  A character scalar BIDS URI string, or a `bids_uri` object.

- ...:

  Additional arguments passed to methods.

## Value

A `bids_uri` object.

## Examples

``` r
u <- as_bids_uri("bids::sub-01/anat/sub-01_T1w.nii.gz")
u$dataset_name   # ""
#> [1] ""
u$relative_path  # "sub-01/anat/sub-01_T1w.nii.gz"
#> [1] "sub-01/anat/sub-01_T1w.nii.gz"
```
