# Create a specialized heatmap visualization of BIDS data

This function creates a heatmap visualization of a BIDS project, where
the x-axis represents subjects and the y-axis represents tasks by run.
Each cell in the heatmap is colored by file size, providing an intuitive
view of data completeness and size distribution across the project. This
is particularly useful for quality control and identifying missing data.

## Usage

``` r
bids_heatmap(
  x,
  interactive = TRUE,
  color_scheme = "viridis",
  file_type = "func",
  highlight_missing = TRUE,
  text_size = 2.5,
  rotate_labels = TRUE
)
```

## Arguments

- x:

  A `bids_project` object

- interactive:

  Logical. Whether to create an interactive plot (default TRUE)

- color_scheme:

  Character. Name of the color palette to use (default "viridis")

- file_type:

  Character. Type of files to visualize (default "func")

- highlight_missing:

  Logical. Whether to highlight missing data points (default TRUE)

- text_size:

  Numeric. Size of text labels (default 2.5)

- rotate_labels:

  Logical. Whether to rotate the axis labels (default TRUE)

## Value

A plot object (ggplot2 or plotly depending on interactive parameter)

## Examples

``` r
# \donttest{
# Create a basic interactive heatmap for a BIDS dataset
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  bids_heatmap(proj)
  
  # Create a static heatmap with custom settings
  bids_heatmap(proj, 
               interactive = FALSE,
               color_scheme = "plasma",
               text_size = 3,
               rotate_labels = FALSE)
  
  # Visualize anatomical data with missing data highlighted
  bids_heatmap(proj, 
               file_type = "anat",
               highlight_missing = TRUE,
               color_scheme = "magma")
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
# }
```
