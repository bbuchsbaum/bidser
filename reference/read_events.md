# Read Event Files from a BIDS Project

This generic function reads and nests event files from a BIDS project.
Event files contain timing information about task events, conditions,
and responses during functional MRI scans. The function can filter
events by subject and task, and returns a nested tibble for easy data
manipulation.

## Usage

``` r
read_events(x, subid = ".*", task = ".*", run = ".*", session = ".*", ...)
```

## Arguments

- x:

  The object to read events from (typically a `bids_project`).

- subid:

  Regex pattern to match subject IDs. Default is `".*"`.

- task:

  Regex pattern to match tasks. Default is `".*"`.

- run:

  Regex pattern to match runs. Default is `".*"`.

- session:

  Regex pattern to match sessions. Default is `".*"`.

- ...:

  Additional arguments passed to methods.

## Value

A nested tibble with columns:

- `.task`: Task name

- `.session`: Session ID

- `.run`: Run number

- `.subid`: Subject ID

- `task`, `session`, `run`, `participant_id`: bare aliases for the
  legacy dotted metadata columns

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
  # Example datasets are cached; leave the cache in place.
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
#> # A tibble: 6 × 9
#>    onset duration trial_type     cash_demean control_pumps_demean explode_demean
#>    <dbl>    <dbl> <chr>                <dbl>                <dbl>          <dbl>
#> 1  0.061    0.772 pumps_demean            NA                   NA           NA  
#> 2  4.96     0.772 pumps_demean            NA                   NA           NA  
#> 3  7.18     0.772 pumps_demean            NA                   NA           NA  
#> 4 10.4      0.772 pumps_demean            NA                   NA           NA  
#> 5 13.4      0.772 pumps_demean            NA                   NA           NA  
#> 6 16.8      0.772 explode_demean          NA                   NA            1.7
#> # ℹ 3 more variables: pumps_demean <dbl>, response_time <dbl>, .file <chr>
# }
```
