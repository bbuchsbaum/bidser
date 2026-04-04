# Read Event Files from Mock BIDS Project

Retrieves and formats event data stored within the mock project object.

## Usage

``` r
# S3 method for class 'mock_bids_project'
read_events(x, subid = ".*", task = ".*", run = ".*", session = ".*", ...)
```

## Arguments

- x:

  A `mock_bids_project` object.

- subid:

  Regex pattern for subject IDs. Default `".*"`.

- task:

  Regex pattern for task names. Default `".*"`.

- run:

  Regex pattern for run indices. Default `".*"`.

- session:

  Regex pattern for session IDs. Default `".*"`.

- ...:

  Additional arguments passed to `event_files`.

## Value

A nested tibble with columns `.subid`, `.task`, `.run`, `.session` (if
applicable), and `data` (containing the event tibbles), or an empty
tibble if no matching data.

## Examples

``` r
parts <- c("01")
fs <- tibble::tibble(
  subid = "01", datatype = "func",
  suffix = c("bold.nii.gz", "events.tsv"),
  task = "rest", run = "01", fmriprep = FALSE
)
evt_data <- list()
evt_data[["sub-01/func/sub-01_task-rest_run-01_events.tsv"]] <-
  tibble::tibble(onset = c(1, 5, 10), duration = c(0.5, 0.5, 0.5),
                 trial_type = c("go", "stop", "go"))
mock <- create_mock_bids("EventTest", parts, fs, event_data = evt_data)
events <- read_events(mock)
print(events)
#> # A tibble: 1 × 5
#> # Groups:   .subid, .task, .run, .session [1]
#>   .subid .task .run  .session data            
#>   <chr>  <chr> <chr> <chr>    <list>          
#> 1 01     rest  01    NA       <tibble [3 × 3]>
```
