# Load and cache the BIDS schema

Loads the vendored BIDS JSON schema and caches the result in the package
environment so repeated calls within a session are free.

## Usage

``` r
bids_schema(version = "1.10.0")
```

## Arguments

- version:

  Character. BIDS schema version to load (default `"1.10.0"`).

## Value

A named list representing the compiled schema with top-level keys
`objects`, `rules`, `meta`, etc.

## See also

[`bids_schema_versions`](https://bbuchsbaum.github.io/bidser/reference/bids_schema_versions.md)

## Examples

``` r
# \donttest{
s <- bids_schema()
names(s)
#> [1] "schema_version" "bids_version"   "meta"           "objects"       
#> [5] "rules"         
# }
```
