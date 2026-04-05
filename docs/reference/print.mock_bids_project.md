# Print Mock BIDS Project Summary

Provides a console summary of the mock BIDS project, displaying key
information like participant count, tasks, sessions, derivatives status,
and discovered BIDS entities.

## Usage

``` r
# S3 method for class 'mock_bids_project'
print(x, ...)
```

## Arguments

- x:

  A `mock_bids_project` object.

- ...:

  Extra arguments (ignored).

## Value

The `mock_bids_project` object `x` invisibly.

## Examples

``` r
# Create a simple mock project
parts <- data.frame(participant_id = "01")
fs <- data.frame(subid = "01", datatype="func", suffix="bold.nii.gz", fmriprep=FALSE)
mock_proj <- create_mock_bids("SimpleMock", parts, fs)
#> Warning: Encoding failed for: sub-01_bold.nii.gz - skipping this file in mock tree.

# Print the summary
print(mock_proj)
#> Mock BIDS Project Summary 
#> Project Name:  SimpleMock 
#> Participants (n):  1 
#> Tasks:  (none) 
#> Datatypes:  (none) 
#> Suffixes:  (none) 
#> BIDS Keys:  (none) 
#> Path:  mock://SimpleMock 
```
