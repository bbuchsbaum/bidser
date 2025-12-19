# Read event files from a BIDS project

Reads and nests event files for given subjects and tasks from a
`bids_project` object. Returns a nested tibble with event data grouped
by task, session, run, and subject. Event files typically contain
trial-by-trial information for task-based fMRI data, including onset
times, durations, trial types, and other task-specific variables.

## Usage

``` r
# S3 method for class 'bids_project'
read_events(x, subid = ".*", task = ".*", run = ".*", session = ".*", ...)
```

## Arguments

- x:

  A `bids_project` object.

- subid:

  Regex pattern to match subject IDs. Default is ".\*" (all subjects).

- task:

  Regex pattern to match tasks. Default is ".\*" (all tasks).

- run:

  Regex pattern to match runs. Default is ".\*" (all runs).

- session:

  Regex pattern to match sessions. Default is ".\*" (all sessions).

- ...:

  Additional arguments passed to `event_files`.

## Value

A nested tibble with columns:

- `.task`: Task name

- `.session`: Session ID (if present)

- `.run`: Run number

- `.subid`: Subject ID

- `data`: A nested tibble containing the event data with columns:

  - `onset`: Event onset time in seconds

  - `duration`: Event duration in seconds

  - Additional task-specific columns (e.g., trial type, response,
    accuracy) If no matching data is found, returns an empty tibble with
    appropriate columns. Run and session identifiers are parsed from
    filenames using
    [`func_parser()`](https://bbuchsbaum.github.io/bidser/reference/func_parser.md).

## Examples

``` r
# \donttest{
# Create a BIDS project object
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  
  # Read all event files
  all_events <- read_events(proj)
  
  # Read events for a specific subject and task
  sub01_events <- read_events(proj, 
                            subid="01", 
                            task="balloonanalogrisktask")
  
  # Read events for multiple subjects and a specific run
  multi_sub_events <- read_events(proj, 
                                subid="0[1-3]", 
                                run="01")
  
  # Access nested data for analysis
  if (nrow(sub01_events) > 0) {
    # Get first subject's data
    first_sub_data <- sub01_events$data[[1]]
    
    # Calculate mean trial duration
    mean_duration <- mean(first_sub_data$duration)
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
#> Rows: 158 Columns: 1
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
#> Rows: 150 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset  duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Warning: Unknown or uninitialised column: `duration`.
#> Warning: argument is not numeric or logical: returning NA
# }
```
