# Working with Confounds and Variables

``` r
library(bidser)
library(dplyr)
library(tidyr)
library(tibble)
```

After loading a BIDS dataset and finding your scans, the next step in
most fMRI workflows is extracting **confound regressors** and **event
tables** for modeling. bidser gives you tidy tibbles at every level –
per-run, per-subject, or dataset-wide – so you can go straight from BIDS
into [`lm()`](https://rdrr.io/r/stats/lm.html), `lme4`, or whatever
modeling framework you prefer.

## Building a test dataset

We’ll create a small mock dataset with two subjects, two tasks, and
realistic fMRIPrep-style confound files so that every code chunk in this
vignette runs without network access.

``` r
proj <- bids_project(temp_dir, fmriprep = TRUE)
proj
#> BIDS Project Summary 
#> Project Name:  bids_confounds_vignette_45824ee31fd4 
#> Participants (n):  2 
#> Participants Source:  file 
#> Tasks:  nback, rest 
#> fMRIPrep Derivatives:  derivatives/fmriprep 
#> Derivative Pipelines:  fmriprep 
#> Index:  enabled 
#> Image Types:  func, funcprep 
#> Modalities:  (none) 
#> Keys:  folder, kind, relative_path, run, subid, suffix, task, type, desc, space
```

Our demo has 2 subjects, each with a resting-state and an n-back task
run, plus fMRIPrep derivatives containing confound timeseries.

## Confound sets: predefined recipes

Before reading any files, it helps to know what confound sets bidser
ships with. These are curated variable lists that match common denoising
strategies from the fMRI literature.

``` r
list_confound_sets()
#>         set                                                        description
#> 1   motion6                                       Rigid-body motion (6 params)
#> 2  motion12                                    Motion + first derivatives (12)
#> 3  motion24                             Friston 24-parameter motion model (24)
#> 4   global3                                    CSF, WM, and global signals (3)
#> 5        9p           9-parameter model: motion6 + CSF + WM + GlobalSignal (9)
#> 6       36p 36-parameter model: motion24 + globals with derivs/quadratics (36)
#> 7  acompcor                     Anatomical CompCor components (use n to limit)
#> 8  tcompcor                       Temporal CompCor components (use n to limit)
#> 9   compcor              Both anatomical and temporal CompCor (use n to limit)
#> 10   cosine                                   Discrete cosine basis regressors
#> 11 outliers     FD/RMSD, motion spike regressors, and nonsteady-state outliers
#> 12    dvars   DVARS family (dvars, std_dvars, non_std_dvars, vx_wisestd_dvars)
#> 13       fd                                        Framewise displacement only
```

Each set is a named collection of confound variable names. You can
inspect what a set contains:

``` r
# 6 rigid-body motion parameters
confound_set("motion6")
#> [1] "trans_x" "trans_y" "trans_z" "rot_x"   "rot_y"   "rot_z"

# Friston 24-parameter expansion (motion + derivatives + squares)
length(confound_set("motion24"))
#> [1] 24
```

The sets compose naturally – `"36p"` is `motion24` plus tissue signals
and their expansions, while `"9p"` is `motion6` plus the three global
signals.

## Reading confounds

[`read_confounds()`](https://bbuchsbaum.github.io/bidser/reference/read_confounds.md)
reads fMRIPrep confound TSV files and returns tidy tibbles. Pass a
confound set name to select variables:

``` r
conf_nested <- read_confounds(proj, cvars = confound_set("motion6"))
conf_nested
#> # A tibble: 4 × 5
#> # Groups:   participant_id, task, run, session [4]
#>   participant_id task  run   session data              
#>   <chr>          <chr> <chr> <chr>   <list>            
#> 1 01             nback 01    1       <tibble [100 × 6]>
#> 2 01             rest  01    1       <tibble [100 × 6]>
#> 3 02             nback 01    1       <tibble [100 × 6]>
#> 4 02             rest  01    1       <tibble [100 × 6]>
```

The result is a nested tibble – one row per run, with the actual
timeseries tucked inside the `data` list column. This structure makes it
easy to iterate over runs in a modeling pipeline:

``` r
conf_nested |>
  unnest(data) |>
  select(participant_id, task, run, trans_x, rot_x) |>
  head()
#> # A tibble: 6 × 6
#> # Groups:   participant_id, task, run, session [1]
#>   session participant_id task  run   trans_x   rot_x
#>   <chr>   <chr>          <chr> <chr>   <dbl>   <dbl>
#> 1 1       01             nback 01     0.0133 0.00121
#> 2 1       01             nback 01    -0.0172 0.00366
#> 3 1       01             nback 01    -0.0146 0.00413
#> 4 1       01             nback 01    -0.0373 0.00403
#> 5 1       01             nback 01    -0.0468 0.00313
#> 6 1       01             nback 01    -0.0688 0.00309
```

### Selecting variables by name or wildcard

You can also pass a character vector of specific variable names, or use
wildcards to match patterns:

``` r
# All CompCor components
compcor_conf <- read_confounds(
  proj,
  subid = "01", task = "rest",
  cvars = c("a_comp_cor_*", "t_comp_cor_*")
)
names(compcor_conf$data[[1]])
#> [1] "a_comp_cor_00" "a_comp_cor_01" "a_comp_cor_02" "a_comp_cor_03"
#> [5] "a_comp_cor_04" "a_comp_cor_05" "t_comp_cor_00" "t_comp_cor_01"
#> [9] "t_comp_cor_02"
```

### Flat output for quick modeling

When you just want a single table for modeling, use `nest = FALSE`:

``` r
conf_flat <- read_confounds(
  proj,
  cvars = confound_set("motion6"),
  nest = FALSE
)
dim(conf_flat)
#> [1] 400  10
head(conf_flat)
#> # A tibble: 6 × 10
#>   trans_x  trans_y trans_z   rot_x     rot_y    rot_z participant_id task  run  
#>     <dbl>    <dbl>   <dbl>   <dbl>     <dbl>    <dbl> <chr>          <chr> <chr>
#> 1  0.0133  0.00456 -0.0187 0.00121  -5.84e-6  2.30e-4 01             nback 01   
#> 2 -0.0172 -0.00769  0.0118 0.00366  -1.26e-3  1.62e-4 01             nback 01   
#> 3 -0.0146  0.00699  0.0117 0.00413  -1.05e-3 -8.40e-4 01             nback 01   
#> 4 -0.0373 -0.0300  -0.0219 0.00403  -1.60e-3 -1.40e-3 01             nback 01   
#> 5 -0.0468 -0.0494  -0.0229 0.00313  -2.23e-3 -1.16e-3 01             nback 01   
#> 6 -0.0688 -0.0368  -0.0220 0.00309  -3.83e-3  5.46e-5 01             nback 01   
#> # ℹ 1 more variable: session <chr>
```

Each row is one volume, with subject/task/run identifiers alongside the
confound values – ready to join with your design matrix.

## Confound strategies: PCA reduction

For high-dimensional confound sets, you often want to reduce the
dimensionality rather than include all regressors directly. A
[`confound_strategy()`](https://bbuchsbaum.github.io/bidser/reference/confound_strategy.md)
splits variables into two groups: those reduced via PCA and those kept
as-is.

``` r
list_confound_strategies()
#>     strategy                                                      description
#> 1 pcabasic80 PCA(motion24 + aCompCor + tCompCor + CSF + WM, 80% var) + cosine
```

The built-in `"pcabasic80"` strategy applies PCA to motion and CompCor
variables (retaining 80% of variance) while keeping cosine regressors
unchanged:

``` r
strat <- confound_strategy("pcabasic80")
conf_pca <- read_confounds(proj, subid = "01", task = "rest", cvars = strat)
names(conf_pca$data[[1]])
#>  [1] "PC1"       "PC2"       "PC3"       "PC4"       "PC5"       "PC6"      
#>  [7] "PC7"       "PC8"       "PC9"       "PC10"      "PC11"      "PC12"     
#> [13] "PC13"      "PC14"      "PC15"      "PC16"      "cosine_00" "cosine_01"
#> [19] "cosine_02"
```

The motion and CompCor variables have been replaced by a handful of
principal components, while the cosine regressors pass through
untouched.

### Custom strategies

You can build your own strategy by specifying which variables get
PCA-reduced and which are kept raw:

``` r
my_strat <- confound_strategy(
  pca_vars = c(confound_set("motion24"), confound_set("acompcor")),
  raw_vars = c("framewise_displacement", confound_set("cosine")),
  npcs = 5
)

conf_custom <- read_confounds(proj, subid = "01", task = "nback", cvars = my_strat)
names(conf_custom$data[[1]])
#> [1] "PC1"                    "PC2"                    "PC3"                   
#> [4] "PC4"                    "PC5"                    "framewise_displacement"
#> [7] "cosine_00"              "cosine_01"              "cosine_02"
```

Five PCs from the motion + aCompCor space, plus FD and cosine regressors
kept in their original form.

## Reading event files

Event files describe the experimental design – trial onsets, durations,
and conditions.
[`read_events()`](https://bbuchsbaum.github.io/bidser/reference/read_events.md)
returns them as nested tibbles:

``` r
events <- read_events(proj, task = "nback")
events
#> # A tibble: 2 × 5
#> # Groups:   .task, .session, .run, .subid [2]
#>   .subid .session .run  .task data             
#>   <chr>  <chr>    <chr> <chr> <list>           
#> 1 01     NA       01    nback <tibble [40 × 5]>
#> 2 02     NA       01    nback <tibble [40 × 5]>
```

Unnest to get a flat trial table:

``` r
trials <- events |>
  unnest(data) |>
  select(.subid, .task, .run, onset, duration, trial_type, response_time)

head(trials)
#> # A tibble: 6 × 8
#> # Groups:   .task, .session, .run, .subid [1]
#>   .session .subid .task .run  onset duration trial_type response_time
#>   <chr>    <chr>  <chr> <chr> <dbl>    <dbl> <chr>              <dbl>
#> 1 NA       01     nback 01     6.87        2 0back              0.806
#> 2 NA       01     nback 01     7.11        2 0back              0.705
#> 3 NA       01     nback 01     7.93        2 0back              0.805
#> 4 NA       01     nback 01     8.50        2 2back              0.781
#> 5 NA       01     nback 01    10.8         2 0back              0.624
#> 6 NA       01     nback 01    17.3         2 2back              0.529
```

``` r
trials |>
  group_by(.subid, trial_type) |>
  summarise(
    n_trials = n(),
    mean_rt = mean(response_time, na.rm = TRUE),
    .groups = "drop"
  )
#> # A tibble: 4 × 4
#>   .subid trial_type n_trials mean_rt
#>   <chr>  <chr>         <int>   <dbl>
#> 1 01     0back            19   0.772
#> 2 01     2back            21   0.809
#> 3 02     0back            18   0.778
#> 4 02     2back            22   0.777
```

If you want everything in one flat table directly, use
[`load_all_events()`](https://bbuchsbaum.github.io/bidser/reference/load_all_events-method.md):

``` r
all_events <- load_all_events(proj, task = "nback")
nrow(all_events)
#> [1] 80
```

## The variables table: one tibble per run

[`variables_table()`](https://bbuchsbaum.github.io/bidser/reference/variables_table.md)
pulls together scans, events, and confounds into a single run-level
tibble. This is the tidy bridge between BIDS and your analysis code:

``` r
vars <- variables_table(proj)
vars |> select(.subid, .task, .run, any_of(c("n_scans", "n_events", "n_confound_rows")))
#> # A tibble: 4 × 5
#>   .subid .task .run  n_scans n_confound_rows
#>   <chr>  <chr> <chr>   <int>           <int>
#> 1 01     nback 01          2             100
#> 2 01     rest  01          2             100
#> 3 02     nback 01          2             100
#> 4 02     rest  01          2             100
```

Each row is one run. The `scans`, `events`, and `confounds` list columns
hold the nested data. You can selectively include only events or only
confounds:

``` r
vars_events <- variables_table(proj, task = "nback", include = "events")
vars_events |> select(.subid, .task, .run, n_events)
#> # A tibble: 2 × 4
#>   .subid .task .run  n_events
#>   <chr>  <chr> <chr>    <int>
#> 1 01     nback 01          40
#> 2 02     nback 01          40
```

This structure makes it straightforward to write a per-run analysis
loop:

``` r
# Use the nback-only table which has both events and confounds columns
vars_nback <- variables_table(proj, task = "nback")

vars_nback |>
  filter(n_events > 0, n_confound_rows > 0) |>
  rowwise() |>
  mutate(
    n_conditions = length(unique(events$trial_type)),
    mean_fd = mean(confounds$framewise_displacement, na.rm = TRUE)
  ) |>
  ungroup() |>
  select(.subid, .task, n_conditions, mean_fd)
#> # A tibble: 2 × 4
#>   .subid .task n_conditions mean_fd
#>   <chr>  <chr>        <int>   <dbl>
#> 1 01     nback            2   0.192
#> 2 02     nback            2   0.196
```

## Dataset QA with bids_report()

[`bids_report()`](https://bbuchsbaum.github.io/bidser/reference/bids_report.md)
assembles a lightweight QA summary covering project metadata, compliance
checks, pipeline discovery, and run-level coverage:

``` r
report <- bids_report(proj)
report
#> BIDS Report
#> Project: bids_confounds_vignette_45824ee31fd4 
#> Participants source: file 
#> Subjects: 2 
#> Sessions: 0 
#> Tasks: nback, rest 
#> Total runs: 2 
#> Compliance: passed 
#> Index: available 
#> Pipelines: fmriprep 
#> Indexed runs: 4
```

The underlying data is fully accessible for custom reporting:

``` r
rdata <- bids_report_data(proj)
rdata$summary
#> $n_subjects
#> [1] 2
#> 
#> $n_sessions
#> NULL
#> 
#> $tasks
#> # A tibble: 2 × 2
#>   task  n_runs
#>   <chr>  <int>
#> 1 nback      1
#> 2 rest       1
#> 
#> $total_runs
#> [1] 2
rdata$run_coverage
#> # A tibble: 4 × 7
#>   .subid .session .task .run  n_scans n_events n_confound_rows
#>   <chr>  <chr>    <chr> <chr>   <int>    <int>           <int>
#> 1 01     ""       nback 01          2        0             100
#> 2 01     ""       rest  01          2        0             100
#> 3 02     ""       nback 01          2        0             100
#> 4 02     ""       rest  01          2        0             100
```

You can use the coverage table to spot missing data at a glance:

``` r
rdata$run_coverage |>
  mutate(
    has_scans = n_scans > 0,
    has_events = if ("n_events" %in% names(rdata$run_coverage)) n_events > 0 else FALSE,
    has_confounds = n_confound_rows > 0
  )
#> # A tibble: 4 × 10
#>   .subid .session .task .run  n_scans n_events n_confound_rows has_scans
#>   <chr>  <chr>    <chr> <chr>   <int>    <int>           <int> <lgl>    
#> 1 01     ""       nback 01          2        0             100 TRUE     
#> 2 01     ""       rest  01          2        0             100 TRUE     
#> 3 02     ""       nback 01          2        0             100 TRUE     
#> 4 02     ""       rest  01          2        0             100 TRUE     
#> # ℹ 2 more variables: has_events <lgl>, has_confounds <lgl>
```

## Putting it together

A typical analysis script combines these pieces into a pipeline. Here is
a sketch that extracts design-ready data for each run:

``` r
analysis_data <- variables_table(proj, task = "nback") |>
  filter(n_events > 0, n_confound_rows > 0) |>
  rowwise() |>
  mutate(
    n_trials = nrow(events),
    n_vols = nrow(confounds)
  ) |>
  ungroup() |>
  select(.subid, .task, .run, n_trials, n_vols)

analysis_data
#> # A tibble: 2 × 5
#>   .subid .task .run  n_trials n_vols
#>   <chr>  <chr> <chr>    <int>  <int>
#> 1 01     nback 01          40    100
#> 2 02     nback 01          40    100
```

From here you would unnest the events and confounds columns, build your
design matrix with
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html), and fit
your model – all in native R tibbles with no intermediate file-path
bookkeeping.
