# Load All Event Files

Searches for and reads event files (`events.tsv`) from a BIDS project,
combining them into a single (potentially nested) tibble.

This function searches for all `events.tsv` files that match the
provided filters (subid, task, run, session) and loads them into a
single tibble. If `full_path=TRUE`, full file paths are returned;
otherwise relative paths.

## Usage

``` r
load_all_events(x, ...)

# S3 method for class 'bids_project'
load_all_events(
  x,
  subid = ".*",
  task = ".*",
  run = ".*",
  session = ".*",
  full_path = TRUE,
  ...
)
```

## Arguments

- x:

  A `bids_project` object.

- ...:

  Additional arguments passed on to `search_files`.

- subid:

  A regex for matching participant IDs. Default is `".*"`.

- task:

  A regex for matching tasks. Default is `".*"`.

- run:

  A regex for matching runs. Default is `".*"`.

- session:

  A regex for matching sessions. Default is `".*"`.

- full_path:

  If TRUE, return full file paths before reading. Default is TRUE.

## Value

A tibble containing the combined event data.

A tibble combining all matched event files, with columns `.subid`,
`.task`, `.run`, `.session` and all event columns. If no events are
found, returns an empty tibble.

## Examples

``` r
# \donttest{
# Example with a bids_project (assuming events exist)
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  all_events <- load_all_events(proj)
  print(all_events)
  
  # Load specific subject/task
  if (length(participants(proj)) > 0) {
    sub01_events <- load_all_events(proj, subid=participants(proj)[1], task="balloonanalogrisktask")
    print(sub01_events)
  }
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
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
#> # A tibble: 7,723 × 14
#>    .subid .session .task      .run  file   onset duration trial_type cash_demean
#>    <chr>  <chr>    <chr>      <chr> <chr>  <dbl>    <dbl> <chr>            <dbl>
#>  1 01     NA       balloonan… 01    /tmp…  0.061    0.772 pumps_dem…          NA
#>  2 01     NA       balloonan… 01    /tmp…  4.96     0.772 pumps_dem…          NA
#>  3 01     NA       balloonan… 01    /tmp…  7.18     0.772 pumps_dem…          NA
#>  4 01     NA       balloonan… 01    /tmp… 10.4      0.772 pumps_dem…          NA
#>  5 01     NA       balloonan… 01    /tmp… 13.4      0.772 pumps_dem…          NA
#>  6 01     NA       balloonan… 01    /tmp… 16.8      0.772 explode_d…          NA
#>  7 01     NA       balloonan… 01    /tmp… 24.9      0.772 pumps_dem…          NA
#>  8 01     NA       balloonan… 01    /tmp… 27.5      0.772 pumps_dem…          NA
#>  9 01     NA       balloonan… 01    /tmp… 30.1      0.772 cash_deme…          -4
#> 10 01     NA       balloonan… 01    /tmp… 38.4      0.772 pumps_dem…          NA
#> # ℹ 7,713 more rows
#> # ℹ 5 more variables: control_pumps_demean <dbl>, explode_demean <dbl>,
#> #   pumps_demean <dbl>, response_time <dbl>, .file <chr>
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
#> # A tibble: 463 × 14
#>    .subid .session .task      .run  file   onset duration trial_type cash_demean
#>    <chr>  <chr>    <chr>      <chr> <chr>  <dbl>    <dbl> <chr>            <dbl>
#>  1 01     NA       balloonan… 01    /tmp…  0.061    0.772 pumps_dem…          NA
#>  2 01     NA       balloonan… 01    /tmp…  4.96     0.772 pumps_dem…          NA
#>  3 01     NA       balloonan… 01    /tmp…  7.18     0.772 pumps_dem…          NA
#>  4 01     NA       balloonan… 01    /tmp… 10.4      0.772 pumps_dem…          NA
#>  5 01     NA       balloonan… 01    /tmp… 13.4      0.772 pumps_dem…          NA
#>  6 01     NA       balloonan… 01    /tmp… 16.8      0.772 explode_d…          NA
#>  7 01     NA       balloonan… 01    /tmp… 24.9      0.772 pumps_dem…          NA
#>  8 01     NA       balloonan… 01    /tmp… 27.5      0.772 pumps_dem…          NA
#>  9 01     NA       balloonan… 01    /tmp… 30.1      0.772 cash_deme…          -4
#> 10 01     NA       balloonan… 01    /tmp… 38.4      0.772 pumps_dem…          NA
#> # ℹ 453 more rows
#> # ℹ 5 more variables: control_pumps_demean <dbl>, explode_demean <dbl>,
#> #   pumps_demean <dbl>, response_time <dbl>, .file <chr>
# }
```
