# Get confound files from a BIDS project

This function retrieves a vector of confound files from a BIDS project
that match specified criteria. Confound files in BIDS derivatives
(typically from fMRIPrep) contain nuisance variables that can be used
for denoising fMRI data, such as motion parameters, physiological
signals, and other noise components.

Searches the mock BIDS structure for files matching typical confound
file patterns (e.g., `*_confounds*.tsv`, `*_regressors*.tsv`,
`*_timeseries*.tsv`) within the derivatives directory.

## Usage

``` r
confound_files(x, ...)

# S3 method for class 'bids_project'
confound_files(x, subid = ".*", task = ".*", session = ".*", ...)

# S3 method for class 'mock_bids_project'
confound_files(
  x,
  subid = ".*",
  task = ".*",
  session = ".*",
  run = ".*",
  full_path = FALSE,
  ...
)
```

## Arguments

- x:

  A `mock_bids_project` object.

- ...:

  Additional arguments passed to `search_files`.

- subid:

  Regex pattern for subject IDs. Default `".*"`.

- task:

  Regex pattern for task names. Default `".*"`.

- session:

  Regex pattern for session IDs. Default `".*"`.

- run:

  Regex pattern for run indices. Default `".*"`.

- full_path:

  If `TRUE`, return full paths (prefixed with `x$path`). If `FALSE`
  (default), return relative paths.

## Value

A character vector of file paths to confound files matching the
specified criteria. If no matching files are found, returns NULL.

A character vector of file paths

A character vector of relative or full paths to potential confound
files, or `NULL` if none are found.

## Details

This function assumes confound files reside in the derivatives path
specified by `x$prep_dir` and were defined in the `file_structure`
passed to `create_mock_bids` with `fmriprep=TRUE`.

## Examples

``` r
# \donttest{
# Get all confound files from a BIDS project with fMRIPrep derivatives
tryCatch({
  ds_path <- get_example_bids_dataset("ds000001-fmriprep")
  proj <- bids_project(ds_path, fmriprep=TRUE)
  conf_files <- confound_files(proj)
  
  # Get confound files for specific subjects and tasks
  confound_files(proj, subid="sub-01", task="balloonanalogrisktask")
  
  # Clean up
  unlink(ds_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
#> Example requires internet connection: participants.tsv is missing
# }
# Setup mock project with a derivative confound file
participants_df <- tibble::tibble(participant_id = "01")
file_structure_df <- tibble::tribble(
  ~subid, ~session, ~datatype, ~task, ~run, ~suffix, ~fmriprep, ~desc,
  "01",   NA,       "func",    "taskA", "01", 
  "desc-confounds_timeseries.tsv", TRUE, "confounds"
)
mock_proj <- create_mock_bids("ConfoundMock", participants_df, file_structure_df)

# Find confound files
confound_files(mock_proj)
#> [1] "derivatives/fmriprep/sub-01/func/sub-01_task-taskA_run-01_desc-confounds_timeseries.tsv"

# Find for specific subject
confound_files(mock_proj, subid="01")
#> [1] "derivatives/fmriprep/sub-01/func/sub-01_task-taskA_run-01_desc-confounds_timeseries.tsv"
```
