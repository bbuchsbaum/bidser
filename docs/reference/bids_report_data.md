# Assemble lightweight report data for a BIDS project

Produces a structured report payload composed from dataset summary,
compliance checks, derivative pipeline discovery, and run-level
variables coverage.

## Usage

``` r
bids_report_data(x, ...)
```

## Arguments

- x:

  A `bids_project` object.

- ...:

  Additional arguments passed to
  [`variables_table()`](https://bbuchsbaum.github.io/bidser/reference/variables_table.md).

## Value

A list with report-ready summary tables and metadata.
