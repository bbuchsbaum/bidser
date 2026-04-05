# Encode a string into a BIDS key-value list

This function parses a BIDS filename and extracts its components into a
key-value list. It understands standard BIDS entities like subject,
session, task, run, etc.

## Usage

``` r
encode(x, ...)

# S3 method for class 'character'
encode(x, ...)
```

## Arguments

- x:

  The filename string to encode

- ...:

  Additional arguments passed to methods

## Value

A list of key-value pairs extracted from the filename

## Examples

``` r
# Encode an anatomical file
encode("sub-01_T1w.nii.gz")
#> $subid
#> [1] "01"
#> 
#> $kind
#> [1] "T1w"
#> 
#> $suffix
#> [1] "nii.gz"
#> 
#> $type
#> [1] "anatprep"
#> 

# Encode a functional file
encode("sub-01_task-rest_run-01_bold.nii.gz")
#> $subid
#> [1] "01"
#> 
#> $task
#> [1] "rest"
#> 
#> $run
#> [1] "01"
#> 
#> $kind
#> [1] "bold"
#> 
#> $suffix
#> [1] "nii.gz"
#> 
#> $type
#> [1] "funcprep"
#> 

# Encode a file with session information
encode("sub-01_ses-pre_task-rest_run-01_bold.nii.gz")
#> $subid
#> [1] "01"
#> 
#> $session
#> [1] "pre"
#> 
#> $task
#> [1] "rest"
#> 
#> $run
#> [1] "01"
#> 
#> $kind
#> [1] "bold"
#> 
#> $suffix
#> [1] "nii.gz"
#> 
#> $type
#> [1] "funcprep"
#> 
```
