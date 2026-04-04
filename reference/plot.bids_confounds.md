# Plot PCA confounds

Visualize principal component scores and loadings returned by
`read_confounds(..., npcs = ...)`. When multiple runs are present, the
default view facets per run for scores (up to `max_panels`) and
aggregates loadings across runs.

## Usage

``` r
# S3 method for class 'bids_confounds'
plot(
  x,
  view = c("auto", "run", "aggregate"),
  pcs = NULL,
  top_n = 8,
  max_panels = 6,
  ...
)
```

## Arguments

- x:

  A `bids_confounds` object returned by
  [`read_confounds()`](https://bbuchsbaum.github.io/bidser/reference/read_confounds.md).

- view:

  Character. One of `"auto"`, `"run"`, or `"aggregate"`.

- pcs:

  Integer or character vector of PCs to plot (e.g., `1:5` or
  `c("PC1", "PC2")`).

- top_n:

  Integer. Keep the top `top_n` variables per PC based on absolute
  loading. Set to `NULL` to keep all variables.

- max_panels:

  Integer. In `view = "auto"`, facet score plots only when the number of
  runs is at most `max_panels`.

- ...:

  Unused.

## Value

A ggplot object, or a list of ggplot objects when patchwork is not
available.

## Examples

``` r
# \donttest{
parts <- c("01", "02")
fs <- tibble::tibble(
  subid = rep(c("01", "02"), each = 2),
  datatype = "func",
  suffix = rep(c("bold.nii.gz", "desc-confounds_timeseries.tsv"), 2),
  task = "rest", fmriprep = TRUE
)
conf_data <- list()
for (p in parts) {
  key <- paste0("derivatives/fmriprep/sub-", p,
                "/func/sub-", p, "_task-rest_desc-confounds_timeseries.tsv")
  conf_data[[key]] <- data.frame(
    csf = rnorm(100), white_matter = rnorm(100),
    global_signal = rnorm(100), framewise_displacement = abs(rnorm(100)),
    trans_x = rnorm(100), trans_y = rnorm(100), trans_z = rnorm(100),
    rot_x = rnorm(100), rot_y = rnorm(100), rot_z = rnorm(100)
  )
}
mock <- create_mock_bids("ConfPlot", parts, fs, confound_data = conf_data)
conf <- read_confounds(mock, npcs = 3)
if (requireNamespace("ggplot2", quietly = TRUE)) {
  plot(conf)
}

# }
```
