# Plot PCA confounds

Visualize principal component scores and loadings returned by
`read_confounds(..., npcs = ...)`. When multiple runs are present, the
default view facets per run for scores (up to `max_panels`) and
aggregates loadings across runs.

## Usage

``` r
# S3 method for class 'bids_confounds'
plot(x, view = c("auto", "run", "aggregate"), pcs = NULL, top_n = 8, max_panels = 6, ...)
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
tryCatch({
  ds_path <- get_example_bids_dataset("ds000001-fmriprep")
  proj <- bids_project(ds_path, fmriprep = TRUE)
  conf <- read_confounds(proj, npcs = 5)
  plot(conf)
  unlink(ds_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires derivatives dataset with confounds: ", e$message)
})
#> Example requires derivatives dataset with confounds: participants.tsv is missing
# }
```
