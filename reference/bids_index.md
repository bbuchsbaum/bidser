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
  unlink(ds001_path, recursive = TRUE)
}, error = function(e) message("Example requires internet: ", e$message))
#> # A tibble: 128 × 33
#>    path   file  scope pipeline extension datatype  size file_mtime subid session
#>    <chr>  <chr> <chr> <chr>    <chr>     <chr>    <dbl>      <dbl> <chr> <chr>  
#>  1 sub-0… sub-… raw   NA       .nii.gz   anat         0     1.78e9 01    NA     
#>  2 sub-0… sub-… raw   NA       .nii.gz   anat         0     1.78e9 01    NA     
#>  3 sub-0… sub-… raw   NA       .nii.gz   func         0     1.78e9 01    NA     
#>  4 sub-0… sub-… raw   NA       .tsv      func      8610     1.78e9 01    NA     
#>  5 sub-0… sub-… raw   NA       .nii.gz   func         0     1.78e9 01    NA     
#>  6 sub-0… sub-… raw   NA       .tsv      func      8568     1.78e9 01    NA     
#>  7 sub-0… sub-… raw   NA       .nii.gz   func         0     1.78e9 01    NA     
#>  8 sub-0… sub-… raw   NA       .tsv      func      8132     1.78e9 01    NA     
#>  9 sub-0… sub-… raw   NA       .nii.gz   anat         0     1.78e9 02    NA     
#> 10 sub-0… sub-… raw   NA       .nii.gz   anat         0     1.78e9 02    NA     
#> # ℹ 118 more rows
#> # ℹ 23 more variables: task <chr>, run <chr>, kind <chr>, suffix <chr>,
#> #   type <chr>, modality <chr>, acq <chr>, ce <chr>, dir <chr>, rec <chr>,
#> #   echo <chr>, space <chr>, res <chr>, desc <chr>, label <chr>, variant <chr>,
#> #   from <chr>, to <chr>, target <chr>, class <chr>, mod <chr>, hemi <chr>,
#> #   mode <chr>
# }
```
