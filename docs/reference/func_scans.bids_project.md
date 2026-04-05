# Get Functional Scans from a BIDS Project

This method extracts functional scan files from a BIDS project based on
specified criteria such as subject ID, task name, run number, and
session. It can return either full or relative file paths to the
functional scans.

## Usage

``` r
# S3 method for class 'bids_project'
func_scans(
  x,
  subid = ".*",
  task = ".*",
  run = ".*",
  session = ".*",
  kind = "bold",
  full_path = TRUE,
  ...
)
```

## Arguments

- x:

  A `bids_project` object.

- subid:

  Regular expression for matching subject IDs. Default is ".\*".

- task:

  Regular expression for matching task names. Default is ".\*".

- run:

  Regular expression for matching run numbers. Default is ".\*".

- session:

  Regular expression for matching session IDs. Default is ".\*".

- kind:

  Regular expression for matching scan type. Default is "bold".

- full_path:

  Logical. If TRUE, return full file paths. If FALSE, return relative
  paths. Default is TRUE.

- ...:

  Additional arguments (not currently used).

## Value

A character vector of file paths to functional scans matching the
criteria. Returns NULL if:

- No matching files are found

- The project doesn't contain functional data

- The specified criteria don't match any files

## Examples

``` r
# \donttest{
# Create a BIDS project
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  
  # Get all functional scans
  all_scans <- func_scans(proj)
  
  # Get scans for specific subjects
  sub_scans <- func_scans(proj, subid="0[123]")
  
  # Get scans for a specific task
  task_scans <- func_scans(proj, task="rest")
  
  # Get scans from specific runs
  run_scans <- func_scans(proj, run="0[123]")
  
  # Combine multiple filters
  filtered_scans <- func_scans(proj,
                              subid="01",
                              task="rest",
                              run="01")
  
  # Get relative paths instead of full paths
  rel_scans <- func_scans(proj, full_path=FALSE)
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
# }
```
