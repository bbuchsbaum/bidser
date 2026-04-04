# Create a tidy run-level variables table

Aggregates event tables, confound tables, and scan inventory into a
single run-level tibble with normalized identifiers and nested
list-columns. This is a lightweight variables layer for downstream R
workflows rather than a full model-spec engine.

## Usage

``` r
variables_table(
  x,
  subid = ".*",
  task = ".*",
  run = ".*",
  session = ".*",
  include = c("events", "confounds"),
  scope = c("all", "raw", "derivatives"),
  pipeline = NULL
)
```

## Arguments

- x:

  A `bids_project` object.

- subid:

  Regex pattern to match subject IDs.

- task:

  Regex pattern to match tasks.

- run:

  Regex pattern to match runs.

- session:

  Regex pattern to match sessions.

- include:

  Which variable sources to include. Supported values are `"events"` and
  `"confounds"`.

- scope:

  Query scope used for scan inventory. One of `"all"`, `"raw"`, or
  `"derivatives"`.

- pipeline:

  Optional derivative pipeline name to restrict derivative scan
  inventory.

## Value

A tibble with one row per run and list-columns `scans`, `events`, and
`confounds` when available.
