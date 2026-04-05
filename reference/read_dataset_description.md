# Read the dataset_description.json for a BIDS project

Read the dataset_description.json for a BIDS project

## Usage

``` r
read_dataset_description(x, ...)

# S3 method for class 'character'
read_dataset_description(x, ...)

# S3 method for class 'bids_project'
read_dataset_description(x, ...)
```

## Arguments

- x:

  A `bids_project` object or a character path.

- ...:

  Additional arguments passed to methods.

## Value

A `bids_dataset_description` object, or NULL if the file is absent.
