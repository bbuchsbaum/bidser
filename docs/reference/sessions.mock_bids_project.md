# Get Sessions from Mock BIDS Project

Extracts the unique session IDs found in the mock project's file
structure. Note: Returns IDs *without* the "ses-" prefix.

## Usage

``` r
# S3 method for class 'mock_bids_project'
sessions(x, ...)
```

## Arguments

- x:

  A `mock_bids_project` object.

- ...:

  Extra arguments (ignored).

## Value

Character vector of unique session IDs (e.g., c("pre", "post")), sorted,
or `NULL` if the project does not have sessions.

## Examples

``` r
# Create a mock project with sessions
parts <- data.frame(participant_id = "01")
fs <- data.frame(subid="01", session="test", datatype="func", suffix="bold.nii.gz", fmriprep=FALSE)
mock_proj <- create_mock_bids("SessionMock", parts, fs)
#> Warning: Encoding failed for: sub-01_ses-test_bold.nii.gz - skipping this file in mock tree.

# Get session IDs
sessions(mock_proj)
#> NULL

# Project without sessions
fs_no_session <- data.frame(subid="01", datatype="func", suffix="bold.nii.gz", fmriprep=FALSE)
mock_proj_no_sess <- create_mock_bids("NoSessionMock", parts, fs_no_session)
#> Warning: Encoding failed for: sub-01_bold.nii.gz - skipping this file in mock tree.
sessions(mock_proj_no_sess) # Returns NULL
#> NULL
```
