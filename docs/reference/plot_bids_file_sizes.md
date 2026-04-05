# Plot BIDS file size distribution

Creates a plot showing the distribution of file sizes across types.

## Usage

``` r
plot_bids_file_sizes(data, color_scheme = "viridis", scale = "log")
```

## Arguments

- data:

  Preprocessed BIDS data from prepare_bids_data_for_plot

- color_scheme:

  Color scheme to use

- scale:

  Scale to use for file sizes ("log", "sqrt", or "linear")

## Value

A ggplot object
