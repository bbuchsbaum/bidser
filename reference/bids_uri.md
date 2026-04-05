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
