# bidser

**bidser** reads and queries [BIDS](https://bids.neuroimaging.io/)
(Brain Imaging Data Structure) neuroimaging projects in R: locate
subjects, sessions, tasks, and files; resolve sidecar metadata; discover
derivative pipelines; and extract fMRIPrep confounds.

[Documentation](https://bbuchsbaum.github.io/bidser/) · [Getting
started](https://bbuchsbaum.github.io/bidser/articles/quickstart.html) ·
[Derivatives](https://bbuchsbaum.github.io/bidser/articles/derivatives.html)
·
[Confounds](https://bbuchsbaum.github.io/bidser/articles/confounds-and-variables.html)
· [API reference](https://bbuchsbaum.github.io/bidser/reference/) ·
[Changelog](https://bbuchsbaum.github.io/bidser/NEWS.md)

## Installation

Install the released version from CRAN:

``` r

install.packages("bidser")
```

Or the development version from GitHub:

``` r

# install.packages("remotes")
remotes::install_github("bbuchsbaum/bidser")
```

CRAN currently publishes 0.5.0; this repository is at 0.5.1.

## Quick start

Build an offline mock project (no Suggests packages or downloads), then
query it with the public entry points
[`participants()`](https://bbuchsbaum.github.io/bidser/reference/participants-method.md),
[`tasks()`](https://bbuchsbaum.github.io/bidser/reference/tasks-method.md),
and
[`func_scans()`](https://bbuchsbaum.github.io/bidser/reference/func_scans.md).
Point the same helpers at a real tree via
[`bids_project()`](https://bbuchsbaum.github.io/bidser/reference/bids_project.md):

``` r

library(bidser)

proj <- create_mock_bids(
  project_name = "demo",
  participants = c("01", "02"),
  file_structure = data.frame(
    subid = c("01", "02"),
    datatype = "func",
    task = "rest",
    run = "01",
    suffix = "bold.nii.gz",
    fmriprep = FALSE,
    stringsAsFactors = FALSE
  )
)

participants(proj)
#> [1] "01" "02"
tasks(proj)
#> [1] "rest"
func_scans(proj, full_path = FALSE)
#> [1] "sub-01/func/sub-01_task-rest_run-01_bold.nii.gz"
#> [2] "sub-02/func/sub-02_task-rest_run-01_bold.nii.gz"
```

Related APIs include
[`read_events()`](https://bbuchsbaum.github.io/bidser/reference/read_events.md),
[`query_files()`](https://bbuchsbaum.github.io/bidser/reference/query_files.md),
[`get_metadata()`](https://bbuchsbaum.github.io/bidser/reference/get_metadata.md),
and
[`derivative_pipelines()`](https://bbuchsbaum.github.io/bidser/reference/derivative_pipelines.md).
For a downloaded example dataset see [Getting
started](https://bbuchsbaum.github.io/bidser/articles/quickstart.html).

## fMRIPrep confounds

[`read_confounds()`](https://bbuchsbaum.github.io/bidser/reference/read_confounds.md)
selects nuisance regressors from fMRIPrep confound tables. Prefer the
public, version-robust helpers over hand-listed column names:

``` r

# Named, version-robust sets (resolve to whatever columns your dataset has)
read_confounds(proj, cvars = confound_set("motion24"))
read_confounds(proj, cvars = confound_set("36p"))

# PCA + raw denoising strategies (recommended modern default)
read_confounds(proj, cvars = confound_strategy("pcabasic80"))

# Discover what is available
list_confound_sets()
list_confound_strategies()
```

Code that previously reached into the unexported
`bidser:::DEFAULT_CVARS2` should switch to the stable public handle
`confound_set("legacy_default")`, which returns the identical 26-name
set. See
[`?read_confounds`](https://bbuchsbaum.github.io/bidser/reference/read_confounds.md),
[`?confound_set`](https://bbuchsbaum.github.io/bidser/reference/confound_set.md),
and the [confounds
vignette](https://bbuchsbaum.github.io/bidser/articles/confounds-and-variables.html).
