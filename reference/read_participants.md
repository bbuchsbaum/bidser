# Read participants.tsv as a typed tabular object

Read participants.tsv as a typed tabular object

## Usage

``` r
read_participants(x, ...)

# S3 method for class 'bids_project'
read_participants(x, ...)

# S3 method for class 'character'
read_participants(x, ...)
```

## Arguments

- x:

  A `bids_project` object or a character path.

- ...:

  Additional arguments passed to methods.

## Value

A `bids_participants` object inheriting from `tbl_df`, or NULL.

## Examples

``` r
# \donttest{
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  pt <- read_participants(ds001_path)
  print(pt)
  unlink(ds001_path, recursive = TRUE)
}, error = function(e) message("Example requires internet: ", e$message))
#> <bids_participants> /tmp/Rtmpz8fxYy/bids_example_ds001/participants.tsv (16 rows x 3 cols, sidecar: yes)
#> # A tibble: 16 × 3
#>    participant_id sex     age
#>  * <chr>          <chr> <dbl>
#>  1 sub-01         F        26
#>  2 sub-02         M        24
#>  3 sub-03         F        27
#>  4 sub-04         F        20
#>  5 sub-05         M        22
#>  6 sub-06         F        26
#>  7 sub-07         M        24
#>  8 sub-08         M        21
#>  9 sub-09         M        26
#> 10 sub-10         F        21
#> 11 sub-11         F        24
#> 12 sub-12         F        22
#> 13 sub-13         F        21
#> 14 sub-14         F        30
#> 15 sub-15         F        24
#> 16 sub-16         M        19
# }
```
