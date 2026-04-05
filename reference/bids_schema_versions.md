# List available BIDS schema versions

Returns the version strings for all BIDS schema JSON files shipped with
this installation of bidser.

## Usage

``` r
bids_schema_versions()
```

## Value

Character vector of available version strings (e.g. `"1.10.1"`).

## Examples

``` r
# \donttest{
bids_schema_versions()
#> [1] "1.10.0"
# }
```
