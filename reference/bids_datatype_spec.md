# Build a custom BIDS datatype spec

Constructs a `parser_spec` list suitable for passing to
[`register_datatype`](https://bbuchsbaum.github.io/bidser/reference/register_datatype.md).
Standard BIDS entity keys are recognised automatically and filled with
canonical regex patterns and ordering; unknown keys fall back to
`[A-Za-z0-9]+` and are appended after standard ones.

## Usage

``` r
bids_datatype_spec(
  type,
  entities = c("sub", "ses"),
  required = "sub",
  suffixes = list()
)
```

## Arguments

- type:

  Non-empty character string naming the datatype (e.g. `"dwi"`).

- entities:

  Character vector of BIDS entity keys to include (e.g.
  `c("sub", "ses", "acq", "run")`). `"sub"` is always present.

- required:

  Character vector of entity keys that are required (non-optional).
  Defaults to `"sub"`.

- suffixes:

  Named list mapping kind strings to one or more file extensions. Each
  value may be a single string or a character vector (e.g.
  `list(dwi = c(".nii.gz", ".nii", ".bvec", ".bval", ".json"))`).

## Value

A `parser_spec` list with elements `keystruc`, `kinds`, and `type`,
ready for
[`gen_parser`](https://bbuchsbaum.github.io/bidser/reference/gen_parser.md)
and
[`register_datatype`](https://bbuchsbaum.github.io/bidser/reference/register_datatype.md).

## Examples

``` r
spec <- bids_datatype_spec(
  type     = "dwi",
  entities = c("sub", "ses", "acq", "run"),
  suffixes = list(
    dwi   = c(".nii.gz", ".nii", ".bvec", ".bval", ".json"),
    sbref = c(".nii.gz", ".nii")
  )
)
if (FALSE) { # \dontrun{
register_datatype("dwi", spec = spec, parser_fn = gen_parser(spec),
                  folder = "dwi", scope = "raw")
} # }
```
