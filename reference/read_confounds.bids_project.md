# Read confound files

Reads in fmriprep confound tables for one or more subjects.

## Usage

``` r
# S3 method for class 'bids_project'
read_confounds(
  x,
  subid = ".*",
  task = ".*",
  session = ".*",
  run = ".*",
  cvars = DEFAULT_CVARS,
  npcs = -1,
  perc_var = -1,
  nest = TRUE,
  ...
)
```

## Arguments

- x:

  A `bids_project` object

- subid:

  Subject ID regex

- task:

  Task regex

- session:

  Session regex

- run:

  Run regex. If the run identifier cannot be extracted from the
  filename, the run value defaults to "1".

- cvars:

  The names of the confound variables to select. Defaults to
  `DEFAULT_CVARS`. Canonical names such as `"csf"` are automatically
  mapped to any matching column names found in the dataset using
  `CVARS_ALIASES`. You can also pass convenience sets from
  [`confound_set()`](https://bbuchsbaum.github.io/bidser/reference/confound_set.md),
  e.g., `confound_set("motion24")`, or wildcard patterns like
  `"cosine_*"`, `"motion_outlier_*"`, or `"a_comp_cor_*[6]"`.

- npcs:

  Perform PCA reduction on confounds and return `npcs` PCs.

- perc_var:

  Perform PCA reduction to retain `perc_var`% variance.

- nest:

  If TRUE, nests confound tables by subject/task/session/run.

- ...:

  Additional arguments (not currently used)

## Value

A `bids_confounds` tibble (nested if nest=TRUE) with identifier columns
for participant_id, task, session, and run. When PCA is requested, the
object includes a `pca` attribute with per-run loadings and variance
used for plotting.

## Examples

``` r
# \donttest{
# Try to load a BIDS project with fMRIPrep derivatives
tryCatch({
  ds_path <- get_example_bids_dataset("ds000001-fmriprep")
  proj <- bids_project(ds_path, fmriprep=TRUE)
  
  # Read confounds with canonical names (automatically resolve to actual columns)
  conf <- read_confounds(proj, cvars = c("csf", "framewise_displacement"))

  # Use convenience sets
  conf_36p <- read_confounds(proj, cvars = confound_set("36p"))
  conf_compcor6 <- read_confounds(proj, cvars = confound_set("acompcor", n = 6))
  
  # Read confounds for specific subjects and tasks
  conf_sub <- read_confounds(proj, subid="01", task="balloonanalogrisktask")
  
  # Get confounds as flat tibble
  conf_flat <- read_confounds(proj, nest=FALSE)
  
  # Clean up
  unlink(ds_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires derivatives dataset with confounds: ", e$message)
})
#> Example requires derivatives dataset with confounds: participants.tsv is missing
# }
```
