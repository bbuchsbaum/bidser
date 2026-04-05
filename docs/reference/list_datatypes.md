# List registered BIDS datatypes

Returns the names of all datatypes currently registered in the bidser
runtime registry.

## Usage

``` r
list_datatypes(scope = c("all", "raw", "derivative", "both"))
```

## Arguments

- scope:

  One of `"all"` (default), `"raw"`, `"derivative"`, or `"both"`.
  Filters by scope.

## Value

A character vector of datatype names.

## Examples

``` r
list_datatypes()
#> [1] "func"     "anat"     "fmap"     "funcprep" "anatprep"
list_datatypes(scope = "raw")
#> [1] "func" "anat" "fmap"
```
