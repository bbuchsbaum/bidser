# Query files in BIDS structure with explicit semantics

This function provides a stricter, more explicit querying interface than
[`search_files()`](https://bbuchsbaum.github.io/bidser/reference/search_files.md),
with support for exact-vs-regex entity matching, optional
entity-existence requirements, and raw/derivatives scope selection.

## Usage

``` r
query_files(x, ...)

# S3 method for class 'bids_project'
query_files(
  x,
  regex = ".*",
  full_path = FALSE,
  match_mode = c("regex", "exact", "glob"),
  require_entity = FALSE,
  scope = c("all", "raw", "derivatives"),
  pipeline = NULL,
  return = c("paths", "tibble"),
  use_index = c("auto", "never"),
  strict = TRUE,
  ...
)

# S3 method for class 'mock_bids_project'
query_files(
  x,
  regex = ".*",
  full_path = FALSE,
  match_mode = c("regex", "exact", "glob"),
  require_entity = FALSE,
  scope = c("all", "raw", "derivatives"),
  pipeline = NULL,
  return = c("paths", "tibble"),
  use_index = c("auto", "never"),
  strict = TRUE,
  ...
)
```

## Arguments

- x:

  A `bids_project` or `mock_bids_project` object.

- ...:

  Additional entity filters (e.g., `subid = "01"`, `task = "rest"`,
  `extension = ".nii.gz"`, `datatype = "func"`). The special filters
  `extension` and `datatype` are handled post-hoc and support the same
  matching modes as other entities.

- regex:

  A regular expression applied to filenames. Default is `".*"`.

- full_path:

  If `TRUE`, return full paths. If `FALSE`, return paths relative to the
  dataset root.

- match_mode:

  Matching mode for entity filters in `...`:

  - `"regex"`: values are treated as regex patterns (default)

  - `"exact"`: values are treated as exact string matches

  - `"glob"`: values are shell-style globs (e.g., `"sub-0*"`)

- require_entity:

  If `TRUE`, queried entity keys must be present on a file for it to
  match. If `FALSE`, wildcard patterns can match files where that entity
  is missing.

- scope:

  Which dataset scope to query:

  - `"all"`: raw + derivatives

  - `"raw"`: raw data only

  - `"derivatives"`: derivatives only

- pipeline:

  Optional derivative pipeline name(s) used when `scope = "derivatives"`
  or `scope = "all"`.

- return:

  Whether to return matching file paths (`"paths"`) or a tibble with
  parsed entities (`"tibble"`).

- use_index:

  Whether to use a persisted file index when available:

  - `"auto"`: use a cached index if present

  - `"never"`: always query the in-memory tree

- strict:

  Passed through to search methods. If `TRUE`, missing queried entities
  typically fail matching (except wildcard behavior in legacy paths).

## Value

A character vector of matching files, a tibble of indexed rows (sorted
by subid, session, task, run, path), or `NULL` if no matches are found.

## Examples

``` r
# \donttest{
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path, fmriprep = FALSE)

  # Exact entity matching for reproducible filters in new workflows
  exact_bold <- query_files(
    proj,
    regex = "bold\\.nii\\.gz$",
    subid = "01",
    task = "balloonanalogrisktask",
    match_mode = "exact"
  )

  # Regex entity matching when selecting multiple runs or tasks
  regex_bold <- query_files(
    proj,
    regex = "bold\\.nii\\.gz$",
    subid = "0[12]",
    task = "balloon.*|mixedgamblestask",
    match_mode = "regex"
  )

  # Require task labels to actually exist on the matched files
  task_annotated <- query_files(
    proj,
    regex = "\\.nii\\.gz$",
    task = ".*",
    require_entity = TRUE,
    scope = "raw"
  )

  unlink(ds001_path, recursive = TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
# }
# \donttest{
tryCatch({
  deriv_path <- get_example_bids_dataset("ds000001-fmriprep")
  proj_deriv <- bids_project(deriv_path, fmriprep = TRUE)

  deriv_only <- query_files(
    proj_deriv,
    regex = "bold\\.nii\\.gz$",
    desc = "preproc",
    scope = "derivatives",
    pipeline = "fmriprep",
    match_mode = "exact"
  )

  all_scopes <- query_files(
    proj_deriv,
    regex = "bold\\.nii\\.gz$",
    scope = "all",
    return = "tibble"
  )

  unlink(deriv_path, recursive = TRUE)
}, error = function(e) {
  message("Example requires derivatives dataset: ", e$message)
})
#> Example requires derivatives dataset: participants.tsv is missing
# }
```
