# Validate a BIDS filename against the schema

Checks that the entities and suffix in a BIDS filename are all
recognised by the schema. Unknown entities and suffixes produce
warnings; truly unparseable filenames produce issues (hard failures).

## Usage

``` r
.bids_schema_validate_filename(filename, schema)
```

## Arguments

- filename:

  Character. Basename of a BIDS file (not the full path).

- schema:

  Result of
  [`bids_schema()`](https://bbuchsbaum.github.io/bidser/reference/bids_schema.md).

## Value

A list with fields:

- valid:

  logical — `TRUE` when there are no hard issues.

- issues:

  character vector of hard issues.

- warnings:

  character vector of soft warnings.
