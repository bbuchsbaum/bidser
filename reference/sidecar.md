# Get the sidecar metadata attached to a tabular BIDS object

Get the sidecar metadata attached to a tabular BIDS object

## Usage

``` r
sidecar(x, ...)

# S3 method for class 'bids_tabular'
sidecar(x, ...)
```

## Arguments

- x:

  A `bids_tabular` object.

- ...:

  Additional arguments passed to methods.

## Value

A named list of sidecar metadata, or an empty list if none.

## Examples

``` r
# \donttest{
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  pt <- read_participants(ds001_path)
  sc <- sidecar(pt)
  unlink(ds001_path, recursive = TRUE)
}, error = function(e) message("Example requires internet: ", e$message))
# }
```
