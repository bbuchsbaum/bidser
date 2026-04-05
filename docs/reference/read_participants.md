# Read participants.tsv as a typed tabular object

Read participants.tsv as a typed tabular object

## Usage

``` r
read_participants(x, ...)

# S3 method for class 'bids_project'
read_participants(x, ...)

# S3 method for class 'character'
read_participants(x, ...)
```

## Arguments

- x:

  A `bids_project` object or a character path.

- ...:

  Additional arguments passed to methods.

## Value

A `bids_participants` object inheriting from `tbl_df`, or NULL.
