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
