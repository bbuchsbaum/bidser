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
  # Example datasets are cached; leave the cache in place.
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
#> # A tibble: 16 × 4
#>    participant_id sex     age source
#>    <chr>          <chr> <int> <chr> 
#>  1 01             F        26 file  
#>  2 02             M        24 file  
#>  3 03             F        27 file  
#>  4 04             F        20 file  
#>  5 05             M        22 file  
#>  6 06             F        26 file  
#>  7 07             M        24 file  
#>  8 08             M        21 file  
#>  9 09             M        26 file  
#> 10 10             F        21 file  
#> 11 11             F        24 file  
#> 12 12             F        22 file  
#> 13 13             F        21 file  
#> 14 14             F        30 file  
#> 15 15             F        24 file  
#> 16 16             M        19 file  
# }
```
