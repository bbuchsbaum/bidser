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
#> Rows: 158 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 156 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 149 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 185 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 184 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 186 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 150 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 169 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 175 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 166 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 175 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 169 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 135 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 138 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 146 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 177 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 187 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 172 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 170 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 162 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 160 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 163 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 166 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 165 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 172 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 167 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 158 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 175 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 173 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 173 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 150 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 153 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 168 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 149 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 156 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 148 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 151 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 172 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 162 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 168 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 173 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 162 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 149 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 127 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 135 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 121 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 141 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 157 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> # A tibble: 7,723 × 7
#>    .subid .session .task                .run  file  onset\tduration\ttri…¹ .file
#>    <chr>  <chr>    <chr>                <chr> <chr> <chr>                  <chr>
#>  1 01     NA       balloonanalogriskta… 01    /tmp… "0.061\t0.772\tpumps_… /tmp…
#>  2 01     NA       balloonanalogriskta… 01    /tmp… "4.958\t0.772\tpumps_… /tmp…
#>  3 01     NA       balloonanalogriskta… 01    /tmp… "7.179\t0.772\tpumps_… /tmp…
#>  4 01     NA       balloonanalogriskta… 01    /tmp… "10.416\t0.772\tpumps… /tmp…
#>  5 01     NA       balloonanalogriskta… 01    /tmp… "13.419\t0.772\tpumps… /tmp…
#>  6 01     NA       balloonanalogriskta… 01    /tmp… "16.754\t0.772\texplo… /tmp…
#>  7 01     NA       balloonanalogriskta… 01    /tmp… "24.905\t0.772\tpumps… /tmp…
#>  8 01     NA       balloonanalogriskta… 01    /tmp… "27.454\t0.772\tpumps… /tmp…
#>  9 01     NA       balloonanalogriskta… 01    /tmp… "30.111\t0.772\tcash_… /tmp…
#> 10 01     NA       balloonanalogriskta… 01    /tmp… "38.449\t0.772\tpumps… /tmp…
#> # ℹ 7,713 more rows
#> # ℹ abbreviated name:
#> #   ¹​`onset\tduration\ttrial_type\tcash_demean\tcontrol_pumps_demean\texplode_demean\tpumps_demean\tresponse_time`
#> Rows: 158 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 156 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 149 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> # A tibble: 463 × 7
#>    .subid .session .task                .run  file  onset\tduration\ttri…¹ .file
#>    <chr>  <chr>    <chr>                <chr> <chr> <chr>                  <chr>
#>  1 01     NA       balloonanalogriskta… 01    /tmp… "0.061\t0.772\tpumps_… /tmp…
#>  2 01     NA       balloonanalogriskta… 01    /tmp… "4.958\t0.772\tpumps_… /tmp…
#>  3 01     NA       balloonanalogriskta… 01    /tmp… "7.179\t0.772\tpumps_… /tmp…
#>  4 01     NA       balloonanalogriskta… 01    /tmp… "10.416\t0.772\tpumps… /tmp…
#>  5 01     NA       balloonanalogriskta… 01    /tmp… "13.419\t0.772\tpumps… /tmp…
#>  6 01     NA       balloonanalogriskta… 01    /tmp… "16.754\t0.772\texplo… /tmp…
#>  7 01     NA       balloonanalogriskta… 01    /tmp… "24.905\t0.772\tpumps… /tmp…
#>  8 01     NA       balloonanalogriskta… 01    /tmp… "27.454\t0.772\tpumps… /tmp…
#>  9 01     NA       balloonanalogriskta… 01    /tmp… "30.111\t0.772\tcash_… /tmp…
#> 10 01     NA       balloonanalogriskta… 01    /tmp… "38.449\t0.772\tpumps… /tmp…
#> # ℹ 453 more rows
#> # ℹ abbreviated name:
#> #   ¹​`onset\tduration\ttrial_type\tcash_demean\tcontrol_pumps_demean\texplode_demean\tpumps_demean\tresponse_time`
# }
```
