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

## Examples

``` r
# \donttest{
tryCatch({
  ds_path <- get_example_bids_dataset("ds000001-fmriprep")
  proj <- bids_project(ds_path, fmriprep = TRUE)
  derivative_pipelines(proj)
  unlink(ds_path, recursive = TRUE)
}, error = function(e) message("Example requires internet: ", e$message))
#> Example requires internet: participants.tsv is missing
# }
```
