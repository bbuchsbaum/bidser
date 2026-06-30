# Get the number of volumes in functional scans

Reads the NIfTI header for one or more BOLD files and returns the 4th
data dimension, i.e. the number of time points/volumes in each scan.

## Usage

``` r
n_volumes(x, ...)

# S3 method for class 'character'
n_volumes(x, ...)

# S3 method for class 'bids_project'
n_volumes(
  x,
  subid = ".*",
  task = ".*",
  run = ".*",
  session = ".*",
  scope = c("raw", "derivatives"),
  as_tibble = FALSE,
  ...
)
```

## Arguments

- x:

  A character vector of NIfTI paths, or a `bids_project` object.

- ...:

  Additional arguments passed to methods.

- subid:

  Regex pattern to match subject IDs.

- task:

  Regex pattern to match task names.

- run:

  Regex pattern to match run IDs.

- session:

  Regex pattern to match session IDs.

- scope:

  Scan source. `"raw"` uses
  [`func_scans()`](https://bbuchsbaum.github.io/bidser/reference/func_scans.md),
  while `"derivatives"` uses
  [`preproc_scans()`](https://bbuchsbaum.github.io/bidser/reference/preproc_scans-method.md).

- as_tibble:

  If `TRUE`, return `.path`, parsed entities, and `nvols`.

## Value

For character input, a named integer vector with one value per path. For
`bids_project` input, a named integer vector by default, or a tibble
when `as_tibble = TRUE`.

## Examples

``` r
if (FALSE) { # \dontrun{
n_volumes("sub-01_task-rest_bold.nii.gz")
n_volumes(proj, subid = "01", task = "rest")
} # }
```
