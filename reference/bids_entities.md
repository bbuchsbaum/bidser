# Parse BIDS entities from paths into a tibble

`bids_entities()` is the vectorized counterpart to
[`encode()`](https://bbuchsbaum.github.io/bidser/reference/encode.md).
It accepts a character vector of full paths or bare filenames and
returns one row per input path with the union of parsed BIDS entity
columns.

## Usage

``` r
bids_entities(paths, include_path = TRUE, coerce = TRUE)
```

## Arguments

- paths:

  Character vector of paths or filenames.

- include_path:

  Logical. If `TRUE` (default), include a `.path` column containing the
  original input.

- coerce:

  Logical. If `TRUE` (default), coerce numeric BIDS entities such as
  `run` and `echo` to integer when possible.

## Value

A tibble with one row per input path. Missing entities are `NA`.

## Examples

``` r
bids_entities(c(
  "sub-01_task-rest_run-01_bold.nii.gz",
  "sub-01_task-rest_run-02_bold.nii.gz"
))
#> # A tibble: 2 × 7
#>   .path                               kind  subid task    run suffix type 
#>   <chr>                               <chr> <chr> <chr> <int> <chr>  <chr>
#> 1 sub-01_task-rest_run-01_bold.nii.gz bold  01    rest      1 nii.gz func 
#> 2 sub-01_task-rest_run-02_bold.nii.gz bold  01    rest      2 nii.gz func 
```
