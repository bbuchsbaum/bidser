# Extract canonical BIDS entity keys from schema

The schema stores entities as a named list (keyed by long name) where
each entry contains a `name` field with the short key used in filenames
(e.g. `"sub"`, `"ses"`, `"task"`, `"run"`).

## Usage

``` r
.bids_schema_entities(schema)
```

## Arguments

- schema:

  Result of
  [`bids_schema()`](https://bbuchsbaum.github.io/bidser/reference/bids_schema.md).

## Value

Character vector of entity keys.
