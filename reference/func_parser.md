# Functional parser constructor

Functional parser constructor

## Usage

``` r
func_parser()
```

## Value

A functional BIDS parser object for parsing functional MRI files

## Examples

``` r
# Create a functional parser
parser <- func_parser()

# Parse a functional file
result <- parse(parser, "sub-01_task-rest_run-01_bold.nii.gz")
```
