# Read Event Files from a BIDS Project

This generic function reads and nests event files from a BIDS project.
Event files contain timing information about task events, conditions,
and responses during functional MRI scans. The function can filter
events by subject and task, and returns a nested tibble for easy data
manipulation.

## Usage

``` r
read_events(x, ...)
```

## Arguments

- x:

  The object to read events from (typically a `bids_project`).

- ...:

  Additional arguments passed to methods.

## Value

A nested tibble with columns:

- `.task`: Task name

- `.run`: Run number

- `.subid`: Subject ID

- `data`: Nested column containing the event data If no matching data is
  found, returns an empty tibble with appropriate columns.

## Examples

``` r
# \donttest{
# Create a BIDS project
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  
  # Read all event files
  all_events <- read_events(proj)
  
  # Read events for specific subjects
  sub_events <- read_events(proj, subid="0[123]")
  
  # Read events for a specific task
  task_events <- read_events(proj, task="balloonanalogrisktask")
  
  # Combine multiple filters
  filtered_events <- read_events(proj,
                                subid="01",
                                task="balloonanalogrisktask")
  
  # Access nested data
  if (nrow(filtered_events) > 0) {
    first_run <- filtered_events$data[[1]]
    print(head(first_run))
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
#> # A tibble: 6 × 2
#>   onset\tduration\ttrial_type\tcash_demean\tcontrol_pumps_demean\texplod…¹ .file
#>   <chr>                                                                    <chr>
#> 1 "0.061\t0.772\tpumps_demean\tn/a\tn/a\tn/a\t-2.000\t2.420"               /tmp…
#> 2 "4.958\t0.772\tpumps_demean\tn/a\tn/a\tn/a\t-1.000\t0.578"               /tmp…
#> 3 "7.179\t0.772\tpumps_demean\tn/a\tn/a\tn/a\t0.000\t0.766"                /tmp…
#> 4 "10.416\t0.772\tpumps_demean\tn/a\tn/a\tn/a\t1.000\t0.840"               /tmp…
#> 5 "13.419\t0.772\tpumps_demean\tn/a\tn/a\tn/a\t2.000\t1.462"               /tmp…
#> 6 "16.754\t0.772\texplode_demean\tn/a\tn/a\t1.700\tn/a\tn/a"               /tmp…
#> # ℹ abbreviated name:
#> #   ¹​`onset\tduration\ttrial_type\tcash_demean\tcontrol_pumps_demean\texplode_demean\tpumps_demean\tresponse_time`
# }
```
