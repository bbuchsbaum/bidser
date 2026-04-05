# Parse a file-name into BIDS components

This generic function parses a BIDS filename into its component parts.
It uses a parser combinator approach to match the filename against known
BIDS patterns and extract relevant metadata such as subject ID, session,
task, run, and modality.

## Usage

``` r
parse(x, fname, ...)
```

## Arguments

- x:

  the parser object to use for parsing

- fname:

  the string (filename) to parse

- ...:

  extra args passed to methods

## Value

A parsed representation of the BIDS filename, typically a list with
extracted components

## Examples

``` r
# Parse an anatomical file
parser <- anat_parser()
parse(parser, "sub-01_T1w.nii.gz")
#> $result
#> $result$subid
#> [1] "01"
#> 
#> $result$kind
#> [1] "T1w"
#> 
#> $result$suffix
#> [1] "nii.gz"
#> 
#> $result$type
#> [1] "anat"
#> 
#> 
#> $remaining
#> [1] ""
#> 

# Parse a functional file
parser <- func_parser()
parse(parser, "sub-01_task-rest_run-01_bold.nii.gz")
#> $result
#> $result$subid
#> [1] "01"
#> 
#> $result$task
#> [1] "rest"
#> 
#> $result$run
#> [1] "01"
#> 
#> $result$kind
#> [1] "bold"
#> 
#> $result$suffix
#> [1] "nii.gz"
#> 
#> $result$type
#> [1] "func"
#> 
#> 
#> $remaining
#> [1] ""
#> 

# Use the generic BIDS parser
parser <- bids_parser()
parse(parser, "sub-01_ses-pre_task-rest_run-01_bold.nii.gz")
#> $result
#> $result$subid
#> [1] "01"
#> 
#> $result$session
#> [1] "pre"
#> 
#> $result$task
#> [1] "rest"
#> 
#> $result$run
#> [1] "01"
#> 
#> $result$kind
#> [1] "bold"
#> 
#> $result$suffix
#> [1] "nii.gz"
#> 
#> $result$type
#> [1] "funcprep"
#> 
#> 
#> $remaining
#> [1] ""
#> 
```
