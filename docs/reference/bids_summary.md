# Summarize a BIDS dataset

Provides a quick summary of dataset statistics, including:

- Number of subjects

- Number of sessions (if applicable)

- Available tasks and the number of runs per task

- Total number of runs

## Usage

``` r
bids_summary(x)

bids_summary(x)
```

## Arguments

- x:

  A `bids_project` object.

## Value

A list containing summary statistics about the BIDS dataset

A list with summary information:

- `n_subjects`: number of participants

- `n_sessions`: number of sessions (if any), otherwise NULL

- `tasks`: a data frame with `task` and `n_runs` columns

- `total_runs`: total number of runs across the dataset

## Examples

``` r
# \donttest{
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  summary <- bids_summary(proj)
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
# }
```
