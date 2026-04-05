# Build or retrieve a persistent file index for a BIDS project

Creates a lightweight on-disk index of files and parsed BIDS entities.
The index is stored as an RDS file and does not change user-facing query
semantics; it simply provides a cached tabular representation of the
project.

## Usage

``` r
bids_index(x, rebuild = FALSE, persist = TRUE)
```

## Arguments

- x:

  A `bids_project` object.

- rebuild:

  If `TRUE`, rebuild the index even if one is already available.

- persist:

  If `TRUE`, save the index to `x$index_path`.

## Value

A tibble describing indexed files.
