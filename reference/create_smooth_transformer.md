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

# Use with bids_transform (example)
# ds_path <- get_example_bids_dataset("ds001")
# proj <- bids_project(ds_path)
# new_files <- bids_transform(proj, smooth_6mm, "smoothed", 
#                             subid = "01", suffix = "bold.nii.gz")
# }
```
