# Summarize a BIDS dataset

Provides a quick summary of dataset statistics, including:

- Number of subjects

- Number of sessions (if applicable)

- Available tasks and the number of runs per task

- Total number of runs

## Usage

``` r
bids_summary(x)
```

## Arguments

- x:

  A `bids_project` object.

## Value

A list with summary information:

- `n_subjects`: number of participants

- `n_sessions`: number of sessions (if any), otherwise NULL

- `tasks`: a tibble with columns `task` and `n_runs`, where `n_runs` is
  the total number of runs of that task across all subjects and sessions

- `total_runs`: total number of runs across the dataset (the sum of
  `tasks$n_runs`)

## Details

A run is one raw BOLD acquisition (`*_bold.nii[.gz]`), identified by its
subject, session, task, `acq`, `ce`, `rec`, `dir`, and `run` entities.
Multi-echo or magnitude/phase files from the same acquisition count as
one run. Derivatives, events files, and sidecars are not counted.

## Examples

``` r
# \donttest{
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  summary <- bids_summary(proj)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
# }
```
