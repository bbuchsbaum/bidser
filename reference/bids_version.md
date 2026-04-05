# Get the BIDS version of a dataset

Get the BIDS version of a dataset

## Usage

``` r
bids_version(x, ...)

# S3 method for class 'bids_project'
bids_version(x, ...)

# S3 method for class 'mock_bids_project'
bids_version(x, ...)

# S3 method for class 'bids_dataset_description'
bids_version(x, ...)
```

## Arguments

- x:

  A `bids_dataset_description` or `bids_project` object.

- ...:

  Additional arguments passed to methods.

## Value

A character scalar BIDS version string, or `NA_character_`.
