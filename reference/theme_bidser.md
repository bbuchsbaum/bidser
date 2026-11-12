# A quiet ggplot2 theme for BIDS figures

`theme_bidser()` is the house style used by
[`plot_bids()`](https://bbuchsbaum.github.io/bidser/reference/plot_bids.md),
[`plot_bids_tree()`](https://bbuchsbaum.github.io/bidser/reference/plot_bids_tree.md),
[`bids_heatmap()`](https://bbuchsbaum.github.io/bidser/reference/bids_heatmap.md)
and [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on confound
objects. It removes non-data ink (panel fills, boxes, most grid lines),
left-aligns titles, and uses a single warm-neutral ink colour so that
the data marks carry the colour.

## Usage

``` r
theme_bidser(base_size = 11, base_family = "", grid = "none")
```

## Arguments

- base_size:

  Base font size in points.

- base_family:

  Base font family.

- grid:

  Which major grid lines to keep: `"none"`, `"x"`, `"y"` or `"xy"`.

## Value

A ggplot2 theme object.

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    theme_bidser(grid = "y")
}
```
