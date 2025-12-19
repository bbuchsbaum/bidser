# Find File Pairs in a BIDS Project

This function matches pairs of related files (e.g., BOLD and event
files) in a BIDS project, returning a tibble with matched filenames.
It's useful for verifying that corresponding files exist for each
subject and task, such as ensuring every BOLD file has an associated
events file.

## Usage

``` r
file_pairs(
  x,
  pair = c("bold-events", "preproc-events"),
  task = ".*",
  matchon = c("run", "task"),
  ...
)
```

## Arguments

- x:

  A `bids_project` object.

- pair:

  A character string specifying which pair of files to match. Currently
  supported:

  - "bold-events": matches BOLD files with event files

  - "preproc-events": matches preprocessed BOLD files with event files

- task:

  A regex pattern to filter tasks. Default is ".\*" (no filter).

- matchon:

  A character vector of keys to match on, usually c("run", "task").

- ...:

  Additional arguments passed to internal functions.

## Value

A tibble with columns:

- `subid`: The subject ID

- `task`: The task name

- `[type1]`: The name of the first file type (e.g., "bold" or "preproc")

- `[type2]`: The matched file of the second type (e.g., "events"), or
  `NA` if no match found

- Additional columns for matched metadata (e.g., run, session)

## Examples

``` r
# \donttest{
# Create a BIDS project object
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  
  # Match BOLD files with their corresponding event files
  bold_pairs <- file_pairs(proj, pair="bold-events")
  
  # Check pairs for a specific task
  task_pairs <- file_pairs(proj, 
                          pair="bold-events",
                          task="balloonanalogrisktask")
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
#> Example requires internet connection: In argument: `stringr::str_detect(task, task)`.
# }
```
