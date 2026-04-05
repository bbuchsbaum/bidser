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
bids_check_compliance(x, schema_check = TRUE, schema_version = "1.10.0")

bids_check_compliance(x, schema_check = TRUE, schema_version = "1.10.0")
```

## Arguments

- x:

  A `bids_project` object.

- schema_check:

  Logical. Whether to run schema validation (default `TRUE`).

- schema_version:

  Character. BIDS schema version to use (default `"1.10.1"`).

## Value

A list with compliance check results including `passed`, `issues`,
`warnings`, `participants_source`, and `schema_checked`.

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
