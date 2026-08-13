# Get fields from a BIDS dataset description

Get fields from a BIDS dataset description

## Usage

``` r
dataset_name(x, ...)

dataset_type(x, ...)
```

## Arguments

- x:

  A `bids_dataset_description` object.

- ...:

  Additional arguments passed to methods.

## Value

A character scalar.

## Examples

``` r
desc_dir <- tempfile("bids-description-")
dir.create(desc_dir)
writeLines(
  '{"Name":"Example Dataset","BIDSVersion":"1.9.0","DatasetType":"raw"}',
  file.path(desc_dir, "dataset_description.json")
)
desc <- read_dataset_description(desc_dir)
dataset_name(desc)
#> [1] "Example Dataset"
dataset_type(desc)
#> [1] "raw"
unlink(desc_dir, recursive = TRUE)
```
