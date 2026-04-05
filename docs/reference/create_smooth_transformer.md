# Create a simple smoothing transformer

This creates a transformer function that adds a smoothing description to
BIDS filenames. This is a lightweight example - real implementations
would perform actual image processing.

## Usage

``` r
create_smooth_transformer(fwhm, suffix_pattern = "bold\\.nii")
```

## Arguments

- fwhm:

  The smoothing FWHM to add to the description.

- suffix_pattern:

  Optional regex pattern to match specific file types.

## Value

A transformer function for use with
[`bids_transform`](https://bbuchsbaum.github.io/bidser/reference/bids_transform.md).

## Examples

``` r
# \donttest{
# Create a smoothing transformer
smooth_6mm <- create_smooth_transformer(6)

# Apply it to a toy BIDS-like file path
in_dir <- tempdir()
out_dir <- tempdir()
infile <- file.path(in_dir, "sub-01_task-rest_bold.nii.gz")
file.create(infile)
#> [1] TRUE
new_file <- smooth_6mm(infile, out_dir)
#> Processing: sub-01_task-rest_bold.nii.gz -> sub-01_task-rest_desc-smooth6mm_bold_nii.gz
basename(new_file)
#> [1] "sub-01_task-rest_desc-smooth6mm_bold_nii.gz"
unlink(infile)
unlink(new_file)
# }
```
