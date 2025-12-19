# Resolve canonical confound variable names

Given a set of desired confound variables, returns the matching column
names present in a dataset, taking into account aliases across fmriprep
versions.

## Usage

``` r
resolve_cvars(cvars, col_names, rename = FALSE)
```

## Arguments

- cvars:

  Character vector of canonical or alias confound names.

- col_names:

  Character vector of available column names.

- rename:

  If TRUE, a named vector is returned where names are canonical
  variables and values are the matching column names. When FALSE the
  result is an unnamed vector of column names to select.

## Value

Character vector of resolved column names.
