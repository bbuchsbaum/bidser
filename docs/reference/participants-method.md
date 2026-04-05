# Get participants from a BIDS project

Retrieves participant information from a BIDS project. By default
returns a sorted character vector of unique participant IDs (without the
`"sub-"` prefix).

## Usage

``` r
participants(x, ...)

# S3 method for class 'bids_project'
participants(x, as_tibble = FALSE, ...)
```

## Arguments

- x:

  the `bids_project` object

- ...:

  extra args passed to methods

- as_tibble:

  Logical. If `FALSE` (default), return a character vector of
  participant IDs. If `TRUE`, return a tibble with all
  `participants.tsv` columns plus a `source` column (`"table"` or
  `"filesystem"`).

## Value

A character vector of unique participant IDs, or a tibble when
`as_tibble = TRUE`.

## Details

When `as_tibble = TRUE`, a tibble is returned instead containing the
full `participants.tsv` data (or inferred IDs when the file is missing)
plus a `source` column indicating whether each ID came from the
`"table"` or the `"filesystem"`.

## Examples

``` r
# \donttest{
# Get participants from a BIDS project
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  participants(proj)

  # Get full tibble with provenance
  participants(proj, as_tibble = TRUE)

  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
# }
```
