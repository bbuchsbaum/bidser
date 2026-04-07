# Read a sessions.tsv file for a subject

Read a sessions.tsv file for a subject

## Usage

``` r
read_sessions_tsv(x, subid, ...)

# S3 method for class 'bids_project'
read_sessions_tsv(x, subid, ...)
```

## Arguments

- x:

  A `bids_project` object.

- subid:

  Subject ID (without `sub-` prefix).

- ...:

  Additional arguments passed to methods.

## Value

A `bids_sessions_tsv` object inheriting from `tbl_df`, or NULL.

## Examples

``` r
# \donttest{
tryCatch({
  ds007_path <- get_example_bids_dataset("ds007")
  proj <- bids_project(ds007_path)
  sess <- read_sessions_tsv(proj, subid = "01")
  unlink(ds007_path, recursive = TRUE)
}, error = function(e) message("Example requires internet: ", e$message))
#> No sessions.tsv found for sub-01
# }
```
