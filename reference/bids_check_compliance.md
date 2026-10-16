# Basic BIDS Compliance Checks

Performs a lightweight check of common BIDS requirements:

- `dataset_description.json` must exist at the dataset root (an issue if
  missing).

- `participants.tsv`, `README`/`README.md`, and `CHANGES` are
  recommended (a warning if missing).

- Every participant must have a `sub-<label>` directory.

- If sessions are present, session directories must be named
  `ses-<label>`.

- Optionally, file names are checked against the vendored BIDS schema
  (see
  [`bids_schema()`](https://bbuchsbaum.github.io/bidser/reference/bids_schema.md));
  mismatches are reported as warnings.

## Usage

``` r
bids_check_compliance(x, schema_check = TRUE, schema_version = "1.10.0")
```

## Arguments

- x:

  A `bids_project` object.

- schema_check:

  Logical. Whether to run schema validation (default `TRUE`). If the
  requested schema version is not available, schema validation is
  skipped and `schema_checked` is `FALSE`.

- schema_version:

  Character. BIDS schema version to use (default `"1.10.0"`, the version
  vendored with bidser; see
  [`bids_schema_versions()`](https://bbuchsbaum.github.io/bidser/reference/bids_schema_versions.md)).

## Value

A list with:

- `passed` (logical): `TRUE` if no issues were found.

- `issues` (character): descriptions of failed requirements.

- `warnings` (character): missing recommended files and schema warnings.

- `participants_source` (character): where participant IDs came from.

- `schema_checked` (logical): whether schema validation ran.

## Details

This is not a full BIDS validator. For complete validation, use the
official BIDS validator.

## Examples

``` r
# \donttest{
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  compliance <- bids_check_compliance(proj)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
# }
```
