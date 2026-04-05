# Resolve a BIDS URI to a local or remote path

Resolve a BIDS URI to a local or remote path

## Usage

``` r
resolve_bids_uri(uri, description, ..., must_exist = FALSE)

# S3 method for class 'bids_uri'
resolve_bids_uri(uri, description, ..., must_exist = FALSE)

# S3 method for class 'character'
resolve_bids_uri(uri, description, ..., must_exist = FALSE)
```

## Arguments

- uri:

  A `bids_uri` object or a character scalar BIDS URI string.

- description:

  A `bids_dataset_description` or `bids_project` object.

- ...:

  Additional arguments passed to methods.

- must_exist:

  Logical. If `TRUE`, the resolved path must exist on disk.

## Value

A character scalar path (or URL for remote links).
