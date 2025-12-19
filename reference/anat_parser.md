# Anatomical parser constructor

Anatomical parser constructor

## Usage

``` r
anat_parser()
```

## Value

An anatomical BIDS parser object for parsing anatomical files

## Examples

``` r
# Create an anatomical parser
parser <- anat_parser()

# Parse an anatomical file
result <- parse(parser, "sub-01_T1w.nii.gz")
```
