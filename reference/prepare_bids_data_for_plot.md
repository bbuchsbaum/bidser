# Prepare BIDS data for visualization

This internal function processes a bids_project object and extracts the
necessary data for visualization, including project info and formatted
data.

## Usage

``` r
prepare_bids_data_for_plot(x, include_derivatives = TRUE)
```

## Arguments

- x:

  A bids_project object

- include_derivatives:

  Logical. Whether to include derivatives data

## Value

A list containing project info and formatted data
