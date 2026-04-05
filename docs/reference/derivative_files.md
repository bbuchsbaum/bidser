# Query derivative files from a BIDS project

Convenience wrapper around
[`query_files()`](https://bbuchsbaum.github.io/bidser/reference/query_files.md)
that restricts results to derivative pipelines.

## Usage

``` r
derivative_files(x, pipeline = NULL, ...)
```

## Arguments

- x:

  A `bids_project` object.

- pipeline:

  Optional character vector of pipeline names to include. When `NULL`
  (default), files from all discovered pipelines are returned.

- ...:

  Additional arguments passed to
  [`query_files()`](https://bbuchsbaum.github.io/bidser/reference/query_files.md).

## Value

Character vector of file paths (or tibble when `return = "tibble"`).
