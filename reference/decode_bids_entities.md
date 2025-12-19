# Decode BIDS entities back into a filename

This function reconstructs a BIDS filename from parsed entities, using
the standard BIDS entity ordering.

## Usage

``` r
decode_bids_entities(entities)
```

## Arguments

- entities:

  A named list of BIDS entities (from
  [`encode`](https://bbuchsbaum.github.io/bidser/reference/encode.md)).

## Value

A character string representing the BIDS filename.

## Examples

``` r
# Parse a filename and reconstruct it
entities <- encode("sub-01_task-rest_run-01_bold.nii.gz")
filename <- decode_bids_entities(entities)
print(filename)
#> [1] "sub-01_task-rest_run-01_bold_nii.gz"

# Modify entities and create new filename
entities$desc <- "smooth6mm"
new_filename <- decode_bids_entities(entities)
print(new_filename)
#> [1] "sub-01_task-rest_run-01_desc-smooth6mm_bold_nii.gz"
```
