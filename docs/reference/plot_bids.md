# Plot a comprehensive visual overview of a BIDS project

This function creates a multi-panel visualization of a BIDS project
structure, showing file distributions, completeness, and data
characteristics.

## Usage

``` r
plot_bids(
  x,
  interactive = TRUE,
  color_scheme = "viridis",
  include_derivatives = TRUE,
  file_size_scale = "log",
  highlight_missing = TRUE,
  visualization_mode = "standard",
  debug = FALSE
)
```

## Arguments

- x:

  A `bids_project` object

- interactive:

  Logical. Whether to create an interactive plot (default TRUE)

- color_scheme:

  Character. Name of the color palette to use (default "viridis")

- include_derivatives:

  Logical. Whether to include derivatives data in the visualization
  (default TRUE)

- file_size_scale:

  Character. Whether to scale file sizes ("log", "sqrt", or "linear",
  default "log")

- highlight_missing:

  Logical. Whether to highlight missing data points (default TRUE)

- visualization_mode:

  Character. The mode of visualization to use ("standard", "heatmap", or
  "complete")

- debug:

  Logical. Whether to print debugging information (default FALSE)

## Value

A plot object (ggplot2, plotly, or other depending on settings)

## Examples

``` r
# \donttest{
# Create a basic BIDS project and plot it
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  plot_bids(proj)
  
  # Create an interactive plot
  plot_bids(proj, interactive=TRUE)
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
# }
```
