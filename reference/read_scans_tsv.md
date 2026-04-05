# Read a scans.tsv file for a subject

Read a scans.tsv file for a subject

## Usage

``` r
read_scans_tsv(x, subid, session = NULL, ...)

# S3 method for class 'bids_project'
read_scans_tsv(x, subid, session = NULL, ...)
```

## Arguments

- x:

  A `bids_project` object.

- subid:

  Subject ID (without `sub-` prefix).

- session:

  Optional session ID (without `ses-` prefix).

- ...:

  Additional arguments passed to methods.

## Value

A `bids_scans_tsv` object inheriting from `tbl_df`, or NULL.
