# Extract valid suffixes from schema

The schema stores suffixes as a named list (keyed by long name) where
each entry contains a `value` field with the actual suffix string used
in filenames (e.g. `"bold"`, `"T1w"`, `"events"`).

## Usage

``` r
.bids_schema_suffixes(schema, datatype = NULL)
```

## Arguments

- schema:

  Result of
  [`bids_schema()`](https://bbuchsbaum.github.io/bidser/reference/bids_schema.md).

- datatype:

  Not used currently; reserved for future datatype filtering.

## Value

Character vector of valid suffix strings.
