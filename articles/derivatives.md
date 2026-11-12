# Discovering and Querying Derivatives

When a BIDS project already has derivatives, the first job is usually
not data loading. It is figuring out which pipeline produced which
files, whether the expected confounds are present, and whether each run
is analysis-ready.

This vignette uses a tiny local project so the full workflow is runnable
without downloading example data. For raw-project discovery, start with
[`vignette("quickstart")`](https://bbuchsbaum.github.io/bidser/articles/quickstart.md).

## Where are the derivatives?

Use
[`plot_bids()`](https://bbuchsbaum.github.io/bidser/reference/plot_bids.md)
to get a fast overview of the raw project plus attached derivative
folders. Each pipeline gets its own column group, drawn in outline, and
a derivative that is absent where raw data exist is flagged as missing.
A derivative whose raw input is itself missing is not counted against
the pipeline.

``` r

plot_bids(proj, interactive = FALSE)
```

![](derivatives_files/figure-html/overview-plot-1.png)

[`plot_bids_tree()`](https://bbuchsbaum.github.io/bidser/reference/plot_bids_tree.md)
shows where each pipeline wrote its files:

``` r

plot_bids_tree(proj)
```

![](derivatives_files/figure-html/overview-tree-1.png)

For code,
[`derivative_pipelines()`](https://bbuchsbaum.github.io/bidser/reference/derivative_pipelines.md)
is the clearest inspection entry point.

``` r

pipes <- derivative_pipelines(proj)
pipes
#> # A tibble: 2 × 4
#>   pipeline root                 description source    
#>   <chr>    <chr>                <list>      <chr>     
#> 1 fmriprep derivatives/fmriprep <bds_dts_>  discovered
#> 2 qsiprep  derivatives/qsiprep  <bds_dts_>  discovered

stopifnot(
  nrow(pipes) == 2L,
  all(c("fmriprep", "qsiprep") %in% pipes$pipeline)
)
```

The fixture has two derivative roots, but only one of them contains
functional preprocessed BOLD data. That is the pipeline you want to
target in the next query.

## How do you pull one derivative run?

Use
[`query_files()`](https://bbuchsbaum.github.io/bidser/reference/query_files.md)
when you want explicit scope and pipeline control.

``` r

prep_bold <- query_files(
  proj,
  regex = "bold\\.nii\\.gz$",
  scope = "derivatives",
  pipeline = "fmriprep",
  desc = "preproc",
  return = "tibble"
)

prep_bold[, c("path", "scope", "pipeline")]
#> # A tibble: 1 × 3
#>   path                                                            scope pipeline
#>   <chr>                                                           <chr> <chr>   
#> 1 derivatives/fmriprep/sub-01/func/sub-01_task-rest_run-01_space… deri… fmriprep

stopifnot(
  nrow(prep_bold) == 1L,
  identical(prep_bold$scope[[1]], "derivatives"),
  identical(prep_bold$pipeline[[1]], "fmriprep")
)
```

That query stays readable because each filter answers one question:
which file suffix, which scope, and which pipeline.

## How do you read and check confounds?

[`read_confounds()`](https://bbuchsbaum.github.io/bidser/reference/read_confounds.md)
reads the derivative table and returns a tibble that stays indexed by
subject, task, run, and session. This vignette shows just enough to
sanity-check a pipeline; for confound *sets*, PCA *strategies*, and the
run-level
[`variables_table()`](https://bbuchsbaum.github.io/bidser/reference/variables_table.md)
bridge into modeling, see
[`vignette("confounds-and-variables")`](https://bbuchsbaum.github.io/bidser/articles/confounds-and-variables.md).

``` r

confounds_flat <- read_confounds(
  proj,
  subid = "01",
  task = "rest",
  nest = FALSE
)

confounds_flat
#> # A tibble: 200 × 11
#>        CSF WhiteMatter GlobalSignal FramewiseDisplacement  trans_x trans_y
#>      <dbl>       <dbl>        <dbl>                 <dbl>    <dbl>   <dbl>
#>  1  0.373       0.0814        100.0                0.153  -0.00803 -0.0415
#>  2 -0.340      -0.262         100.                 0.203  -0.0155  -0.0240
#>  3  0.251       0.0425         99.7                0.0572 -0.0240   0.0268
#>  4 -1.01       -0.377         100.                 0.164  -0.0151   0.0347
#>  5  0.199      -0.738          99.9                0.157  -0.0286   0.0425
#>  6 -0.0946     -0.0280        100.                 0.120  -0.0203   0.0364
#>  7  0.0950      0.234          99.9                0.102  -0.0482   0.0348
#>  8 -0.350       0.148          99.7                0.0935 -0.0651   0.0375
#>  9 -0.561      -0.0191         99.9                0.0679 -0.0777   0.0598
#> 10  0.506       0.238          99.8                0.0563 -0.0637   0.0297
#> # ℹ 190 more rows
#> # ℹ 5 more variables: trans_z <dbl>, participant_id <chr>, task <chr>,
#> #   run <chr>, session <chr>

stopifnot(
  nrow(confounds_flat) == 200L,
  all(is.finite(confounds_flat$FramewiseDisplacement)),
  mean(confounds_flat$FramewiseDisplacement) < 0.2
)
```

If you want a compact diagnostic view, ask for principal components and
plot them. Keep framewise displacement as a raw column and the plot adds
a head-motion panel with a 0.5 mm threshold (`fd_threshold`) above the
components.

``` r

confounds_pca <- read_confounds(
  proj,
  subid = "01",
  task = "rest",
  cvars = confound_strategy(
    pca_vars = c("CSF", "WhiteMatter", "GlobalSignal", "trans_*"),
    raw_vars = "FramewiseDisplacement",
    npcs = 3
  )
)

plot(confounds_pca)
```

![](derivatives_files/figure-html/plot-confounds-1.png)

The plot is most useful as a quick failure check: if the confounds are
missing, degenerate, or wildly scaled, the PCA summary becomes obviously
suspicious.

## How do you verify run-level coverage?

[`variables_table()`](https://bbuchsbaum.github.io/bidser/reference/variables_table.md)
pulls scans, events, and confounds into one run-level tibble without
forcing you to manage separate joins.

``` r

run_variables <- variables_table(
  proj,
  scope = "all",
  pipeline = "fmriprep"
)

run_variables[, c(".subid", ".task", ".run", "n_scans", "n_events", "n_confound_rows")]
#> # A tibble: 1 × 6
#>   .subid .task .run  n_scans n_events n_confound_rows
#>   <chr>  <chr> <chr>   <int>    <int>           <int>
#> 1 01     rest  01          1        1             200

stopifnot(
  run_variables$n_scans[[1]] == 1L,
  run_variables$n_events[[1]] == 1L,
  run_variables$n_confound_rows[[1]] == 200L
)
```

That table is a practical checkpoint before model fitting because it
shows whether each run has the pieces you expect.

## How do you turn that into a report?

[`bids_report()`](https://bbuchsbaum.github.io/bidser/reference/bids_report.md)
wraps the same run-level coverage into a compact text report.

``` r

report <- bids_report(
  proj,
  scope = "all",
  pipeline = "fmriprep"
)

report
#> BIDS Report
#> Project: bidser-deriv-6e54156ede2d 
#> Participants source: file 
#> Subjects: 1 
#> Sessions: 0 
#> Tasks: rest 
#> Total runs: 1 
#> Compliance: passed 
#> Index: available 
#> Pipelines: fmriprep, qsiprep 
#> Indexed runs: 1
```

## Next Steps

Use
[`vignette("quickstart")`](https://bbuchsbaum.github.io/bidser/articles/quickstart.md)
for raw BIDS inspection,
[`vignette("confounds-and-variables")`](https://bbuchsbaum.github.io/bidser/articles/confounds-and-variables.md)
for confound sets, PCA strategies, and run-level modeling tables, and
[`vignette("mock-bids")`](https://bbuchsbaum.github.io/bidser/articles/mock-bids.md)
when you need a fully local project for tests, demos, or packaging
workflows.
