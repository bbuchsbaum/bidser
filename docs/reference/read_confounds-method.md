# Read Confound Files (Mock Implementation)

Read Confound Files (Mock Implementation)

## Usage

``` r
# S3 method for class 'mock_bids_project'
read_confounds(
  x,
  subid = ".*",
  task = ".*",
  session = ".*",
  run = ".*",
  cvars = NULL,
  npcs = -1,
  perc_var = -1,
  nest = TRUE,
  ...
)
```

## Arguments

- x:

  A `mock_bids_project` object.

- subid:

  Regex pattern for subject IDs. Default `".*"`.

- task:

  Regex pattern for task names. Default `".*"`.

- session:

  Regex pattern for session IDs. Default `".*"`.

- run:

  Regex pattern for run indices. Default `".*"`.

- cvars:

  Variables to select (ignored in mock).

- npcs:

  PCA components (applied when requested).

- perc_var:

  PCA variance (applied when requested).

- nest:

  If `TRUE`, returns a nested tibble keyed by subject, task, session and
  run.

- ...:

  Additional BIDS entities (passed to `search_files`).

## Value

A `bids_confounds` tibble of confound data (nested if `nest = TRUE`).

## Examples

``` r
parts <- c("01")
fs <- tibble::tibble(
  subid = "01", datatype = "func",
  suffix = c("bold.nii.gz", "desc-confounds_timeseries.tsv"),
  task = "rest", fmriprep = c(TRUE, TRUE)
)
conf_data <- list()
key <- "derivatives/fmriprep/sub-01/func/sub-01_task-rest_desc-confounds_timeseries.tsv"
conf_data[[key]] <- data.frame(
  csf = rnorm(50), white_matter = rnorm(50),
  trans_x = rnorm(50), trans_y = rnorm(50)
)
mock <- create_mock_bids("ConfTest", parts, fs, confound_data = conf_data)
conf <- read_confounds(mock)
print(conf)
#> # A tibble: 1 × 6
#> # Groups:   .subid, .task, .run, .session, .desc [1]
#>   .subid .task .run  .session .desc     data             
#>   <chr>  <chr> <chr> <chr>    <chr>     <list>           
#> 1 01     rest  NA    NA       confounds <tibble [50 × 4]>
```
