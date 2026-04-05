# Unregister a custom BIDS datatype

Removes a previously registered custom datatype from the registry.
Built-in datatypes cannot be removed.

## Usage

``` r
unregister_datatype(name)
```

## Arguments

- name:

  Character string naming the datatype to remove.

## Value

The `name` string, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
register_datatype("dwi", spec = func_spec(), parser_fn = func_parser())
unregister_datatype("dwi")
} # }
```
