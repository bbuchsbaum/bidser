# Register a custom BIDS datatype

Adds a new datatype to the bidser runtime registry so that
[`bids_project`](https://bbuchsbaum.github.io/bidser/reference/bids_project.md)
will scan and parse its folder. The five built-in datatypes (`"func"`,
`"anat"`, `"fmap"`, `"funcprep"`, `"anatprep"`) can only be overwritten
by passing `overwrite = TRUE`.

## Usage

``` r
register_datatype(
  name,
  spec,
  parser_fn,
  folder = name,
  scope = c("raw", "derivative", "both"),
  overwrite = FALSE
)
```

## Arguments

- name:

  A non-empty character string naming the datatype.

- spec:

  A spec list as returned by `func_spec()` etc.

- parser_fn:

  A parser object as returned by
  [`func_parser()`](https://bbuchsbaum.github.io/bidser/reference/func_parser.md)
  etc.

- folder:

  The folder name to scan inside each subject/session directory.
  Defaults to `name`.

- scope:

  One of `"raw"`, `"derivative"`, or `"both"`.

- overwrite:

  Logical. If `FALSE` (default) an error is thrown when attempting to
  overwrite an existing registration.

## Value

The `name` string, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
my_spec   <- func_spec()   # placeholder
my_parser <- func_parser() # placeholder
register_datatype("dwi", spec = my_spec, parser_fn = my_parser,
                  folder = "dwi", scope = "raw")
list_datatypes()
} # }
```
