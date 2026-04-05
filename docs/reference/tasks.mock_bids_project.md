# Get Tasks from Mock BIDS Project

Extracts the unique task names found in the mock project's file
structure. Note: Returns names *without* the "task-" prefix.

## Usage

``` r
# S3 method for class 'mock_bids_project'
tasks(x, ...)
```

## Arguments

- x:

  A `mock_bids_project` object.

- ...:

  Extra arguments (ignored).

## Value

Character vector of unique task names (e.g., c("rest", "nback")),
sorted.

## Examples

``` r
# Create a mock project with tasks
parts <- data.frame(participant_id = "01")
fs <- data.frame(subid="01", task="taskA", run="01", datatype="func", 
                 suffix="bold.nii.gz", fmriprep=FALSE)
fs <- rbind(fs, data.frame(subid="01", task="taskB", run="01", datatype="func", 
                          suffix="bold.nii.gz", fmriprep=FALSE))
mock_proj <- create_mock_bids("TaskMock", parts, fs)

# Get task names
tasks(mock_proj)
#> [1] "taskA" "taskB"
```
