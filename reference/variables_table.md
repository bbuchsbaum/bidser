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

## Examples

``` r
# \donttest{
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  vt <- variables_table(proj)
  print(vt)
  unlink(ds001_path, recursive = TRUE)
}, error = function(e) message("Example requires internet: ", e$message))
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> # A tibble: 48 × 8
#>    .subid .session .task                .run  scans    n_scans events   n_events
#>    <chr>  <chr>    <chr>                <chr> <list>     <int> <list>      <int>
#>  1 01     ""       balloonanalogriskta… 01    <tibble>       1 <tibble>      158
#>  2 01     ""       balloonanalogriskta… 02    <tibble>       1 <tibble>      156
#>  3 01     ""       balloonanalogriskta… 03    <tibble>       1 <tibble>      149
#>  4 02     ""       balloonanalogriskta… 01    <tibble>       1 <tibble>      185
#>  5 02     ""       balloonanalogriskta… 02    <tibble>       1 <tibble>      184
#>  6 02     ""       balloonanalogriskta… 03    <tibble>       1 <tibble>      186
#>  7 03     ""       balloonanalogriskta… 01    <tibble>       1 <tibble>      150
#>  8 03     ""       balloonanalogriskta… 02    <tibble>       1 <tibble>      169
#>  9 03     ""       balloonanalogriskta… 03    <tibble>       1 <tibble>      175
#> 10 04     ""       balloonanalogriskta… 01    <tibble>       1 <tibble>      166
#> # ℹ 38 more rows
# }
```
