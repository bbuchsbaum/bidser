# Read a sessions.tsv file for a subject

Read a sessions.tsv file for a subject

## Usage

``` r
read_sessions_tsv(x, subid, ...)

# S3 method for class 'bids_project'
read_sessions_tsv(x, subid, ...)
```

## Arguments

- x:

  A `bids_project` object.

- subid:

  Subject ID (without `sub-` prefix).

- ...:

  Additional arguments passed to methods.

## Value

A `bids_sessions_tsv` object inheriting from `tbl_df`, or NULL.
