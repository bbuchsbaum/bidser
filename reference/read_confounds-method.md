# Read Confound Files (Mock Implementation)

Read Confound Files (Mock Implementation)

## Usage

``` r
# S3 method for class 'mock_bids_project'
read_confounds(
  x,
  subid = ".*",
  task = ".*",
  session = ".*",
  run = ".*",
  cvars = NULL,
  npcs = -1,
  perc_var = -1,
  nest = TRUE,
  ...
)
```

## Arguments

- x:

  A `mock_bids_project` object.

- subid:

  Regex pattern for subject IDs. Default `".*"`.

- task:

  Regex pattern for task names. Default `".*"`.

- session:

  Regex pattern for session IDs. Default `".*"`.

- run:

  Regex pattern for run indices. Default `".*"`.

- cvars:

  Variables to select (ignored in mock).

- npcs:

  PCA components (ignored in mock).

- perc_var:

  PCA variance (ignored in mock).

- nest:

  If `TRUE`, returns a nested tibble keyed by subject, session and run.

- ...:

  Additional BIDS entities (passed to `search_files`).

## Value

A tibble of confound data (nested if `nest = TRUE`).
