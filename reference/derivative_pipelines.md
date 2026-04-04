# List derivative pipelines in a BIDS project

Returns a tibble describing the derivative pipelines currently attached
to a `bids_project`. This is the preferred inspection entry point for
new code; legacy `fmriprep` / `prep_dir` fields remain available for
compatibility.

## Usage

``` r
derivative_pipelines(x)
```

## Arguments

- x:

  A `bids_project` object.

## Value

A tibble with one row per derivative pipeline and columns `pipeline`,
`root`, `description`, and `source`.
