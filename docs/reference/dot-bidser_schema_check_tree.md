# Run schema validation across all files in a bids_project tree

Walks all leaf nodes of the project's `bids_tree`, validates each
BIDS-like filename against the schema, and returns any warnings or
issues found. Files under `derivatives/` sub-trees are skipped because
the BIDS schema's raw-file rules do not apply to preprocessed outputs.

## Usage

``` r
.bidser_schema_check_tree(x, schema)
```

## Arguments

- x:

  A `bids_project` object.

- schema:

  Result of
  [`bids_schema()`](https://bbuchsbaum.github.io/bidser/reference/bids_schema.md).

## Value

Character vector of warning/issue strings (empty if all files pass).
