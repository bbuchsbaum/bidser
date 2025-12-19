# Basic BIDS Compliance Checks

This function performs a simple, lightweight check of common BIDS
requirements:

- Checks that `participants.tsv` and `dataset_description.json` exist at
  the root.

- Ensures all subject directories begin with `sub-`.

- If sessions are present, ensures that session directories begin with
  `ses-`.

## Usage

``` r
bids_check_compliance(x)

bids_check_compliance(x)
```

## Arguments

- x:

  A `bids_project` object.

## Value

A list with compliance check results

A list with:

- `passed` (logical): TRUE if all checks passed, FALSE otherwise.

- `issues` (character vector): Descriptions of any issues found.

## Details

Note: This is not a full BIDS validator. For complete validation, use
the official BIDS validator.

## Examples

``` r
# \donttest{
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  compliance <- bids_check_compliance(proj)
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
# }
```
