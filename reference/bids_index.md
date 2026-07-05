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

## Examples

``` r
# \donttest{
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  idx <- bids_index(proj, persist = FALSE)
  print(idx)
  # Example datasets are cached; leave the cache in place.
}, error = function(e) message("Example requires internet: ", e$message))
#> # A tibble: 135 × 33
#>    path   file  scope pipeline extension datatype  size file_mtime subid session
#>    <chr>  <chr> <chr> <chr>    <chr>     <chr>    <dbl>      <dbl> <chr> <chr>  
#>  1 CHANG… CHAN… raw   NA       ""        NA         141     1.78e9 NA    NA     
#>  2 CITAT… CITA… raw   NA       ".cff"    NA        1176     1.78e9 NA    NA     
#>  3 README READ… raw   NA       ""        NA        1172     1.78e9 NA    NA     
#>  4 datas… data… raw   NA       ".json"   NA         134     1.78e9 NA    NA     
#>  5 parti… part… raw   NA       ".json"   NA         246     1.78e9 NA    NA     
#>  6 parti… part… raw   NA       ".tsv"    NA         215     1.78e9 NA    NA     
#>  7 sub-0… sub-… raw   NA       ".nii.gz" anat         0     1.78e9 01    NA     
#>  8 sub-0… sub-… raw   NA       ".nii.gz" anat         0     1.78e9 01    NA     
#>  9 sub-0… sub-… raw   NA       ".nii.gz" func         0     1.78e9 01    NA     
#> 10 sub-0… sub-… raw   NA       ".tsv"    func      8610     1.78e9 01    NA     
#> # ℹ 125 more rows
#> # ℹ 23 more variables: task <chr>, run <chr>, kind <chr>, suffix <chr>,
#> #   type <chr>, modality <chr>, acq <chr>, ce <chr>, dir <chr>, rec <chr>,
#> #   echo <chr>, space <chr>, res <chr>, desc <chr>, label <chr>, variant <chr>,
#> #   from <chr>, to <chr>, target <chr>, class <chr>, mod <chr>, hemi <chr>,
#> #   mode <chr>
# }
```
