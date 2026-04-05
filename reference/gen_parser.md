# Generate a parser from a spec

Converts a `parser_spec` list (as returned by
[`bids_datatype_spec`](https://bbuchsbaum.github.io/bidser/reference/bids_datatype_spec.md)
or the internal `func_spec()` etc.) into a parser object suitable for
passing to
[`register_datatype`](https://bbuchsbaum.github.io/bidser/reference/register_datatype.md).

## Usage

``` r
gen_parser(spec, typename = "kind")
```

## Arguments

- spec:

  A `parser_spec` list with elements `keystruc`, `kinds`, and `type`, as
  returned by
  [`bids_datatype_spec`](https://bbuchsbaum.github.io/bidser/reference/bids_datatype_spec.md).

- typename:

  The name given to the final type element. Default is `"kind"`.

## Value

A regex-based parser object.

## See also

[`bids_datatype_spec`](https://bbuchsbaum.github.io/bidser/reference/bids_datatype_spec.md),
[`register_datatype`](https://bbuchsbaum.github.io/bidser/reference/register_datatype.md)
