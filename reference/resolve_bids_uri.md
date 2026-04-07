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

## Examples

``` r
# \donttest{
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  desc <- read_dataset_description(ds001_path)
  uri <- bids_uri("bids::sub-01/anat/sub-01_T1w.nii.gz")
  path <- resolve_bids_uri(uri, desc)
  unlink(ds001_path, recursive = TRUE)
}, error = function(e) message("Example requires internet: ", e$message))
# }
```
