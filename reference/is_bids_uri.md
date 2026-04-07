# Test whether an object is a BIDS URI

Test whether an object is a BIDS URI

## Usage

``` r
is_bids_uri(x)
```

## Arguments

- x:

  Any object.

## Value

`TRUE` if `x` inherits from `"bids_uri"`, `FALSE` otherwise.

## Examples

``` r
u <- bids_uri("bids::sub-01/anat/sub-01_T1w.nii.gz")
is_bids_uri(u)      # TRUE
#> [1] TRUE
is_bids_uri("bids::sub-01/anat/sub-01_T1w.nii.gz")  # FALSE
#> [1] FALSE
```
