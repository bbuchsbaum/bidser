# Fieldmap parser constructor

Fieldmap parser constructor

## Usage

``` r
fmap_parser()
```

## Value

A fieldmap BIDS parser object for parsing fieldmap files

## Examples

``` r
# Create a fieldmap parser
parser <- fmap_parser()

# Parse a fieldmap file
result <- parse(parser, "sub-01_magnitude1.nii.gz")
```
