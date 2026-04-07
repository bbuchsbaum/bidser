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
#> Rows: 158 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 156 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 149 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 185 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 184 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 186 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 150 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 169 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 175 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 166 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 175 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 169 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 135 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 138 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 146 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 177 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 187 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 172 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 170 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 162 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 160 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 163 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 166 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 165 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 172 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 167 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 158 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 175 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 173 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 173 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 150 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 153 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 168 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 149 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 156 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 148 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 151 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 172 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 162 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 168 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 173 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 162 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 149 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 127 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 135 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 121 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 141 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 157 Columns: 8
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (1): trial_type
#> dbl (7): onset, duration, cash_demean, control_pumps_demean, explode_demean,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
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
#> No confound data found for the given selection.
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
