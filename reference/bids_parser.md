# BIDS filename parsers using regex

These functions create parsers for different types of BIDS files using
regex-based pattern matching instead of parser combinators. Create a
parser for a generic BIDS file

## Usage

``` r
bids_parser()
```

## Value

A BIDS parser object that can parse various types of BIDS files

## Details

This parser tries to match against various known parsers (anat, func,
fmriprep anat/func).

## Examples

``` r
# Create a generic BIDS parser
parser <- bids_parser()

# Parse different types of files
anat_result <- parse(parser, "sub-01_T1w.nii.gz")
func_result <- parse(parser, "sub-01_task-rest_bold.nii.gz")
prep_result <- parse(parser, "sub-01_task-rest_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz")
```
