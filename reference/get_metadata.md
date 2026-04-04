# Resolve metadata for a BIDS file

Retrieves JSON metadata for a target BIDS file. When `inherit = TRUE`,
this method applies BIDS-style inheritance by merging matching sidecars
from less specific to more specific locations.

## Usage

``` r
get_metadata(
  x,
  file,
  inherit = TRUE,
  scope = c("auto", "raw", "derivatives", "all"),
  ...
)

# S3 method for class 'bids_project'
get_metadata(
  x,
  file,
  inherit = TRUE,
  scope = c("auto", "raw", "derivatives", "all"),
  ...
)
```

## Arguments

- x:

  A `bids_project` object.

- file:

  Target file path (relative to project root or absolute path).

- inherit:

  If `TRUE`, merge inherited metadata from parent sidecars.

- scope:

  Scope used when applying inheritance:

  - `"auto"`: infer from file location

  - `"raw"`: raw data inheritance

  - `"derivatives"`: derivatives inheritance

  - `"all"`: allow full project ancestry

- ...:

  Additional arguments for methods.

## Value

A named list of metadata fields.

## Details

If multiple sidecars are equally specific at the same directory depth,
they are merged in deterministic path order after less specific
ancestors, so the final values are reproducible.
