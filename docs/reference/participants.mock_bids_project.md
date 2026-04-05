# Get Participants from Mock BIDS Project

Extracts the unique participant IDs from the mock project definition.
Note: Returns IDs *without* the "sub-" prefix for consistency with
`bids_project` methods.

## Usage

``` r
# S3 method for class 'mock_bids_project'
participants(x, as_tibble = FALSE, ...)
```

## Arguments

- x:

  A `mock_bids_project` object.

- as_tibble:

  If `TRUE`, return a tibble with participant metadata instead of a
  character vector.

- ...:

  Extra arguments (ignored).

## Value

Character vector of unique participant IDs (e.g., c("01", "02")),
sorted. If `as_tibble = TRUE`, a tibble with participant metadata.

## Examples

``` r
# Create a mock project
parts <- data.frame(participant_id = c("sub-01", "sub-02"))
fs <- data.frame(subid=c("01", "02"), datatype="func", suffix="bold.nii.gz", fmriprep=FALSE)
mock_proj <- create_mock_bids("SimpleMock", parts, fs)
#> Warning: Encoding failed for: sub-01_bold.nii.gz - skipping this file in mock tree.
#> Warning: Encoding failed for: sub-02_bold.nii.gz - skipping this file in mock tree.

# Get participant IDs
participants(mock_proj)
#> [1] "01" "02"
```
