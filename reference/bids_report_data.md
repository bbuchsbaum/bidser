# Assemble lightweight report data for a BIDS project

Produces a structured report payload composed from dataset summary,
compliance checks, derivative pipeline discovery, and run-level
variables coverage.

## Usage

``` r
bids_report_data(x, ...)
```

## Arguments

- x:

  A `bids_project` object.

- ...:

  Additional arguments passed to
  [`variables_table()`](https://bbuchsbaum.github.io/bidser/reference/variables_table.md).

## Value

A list with report-ready summary tables and metadata.

## Examples

``` r
# \donttest{
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  rd <- bids_report_data(proj)
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
# }
```
