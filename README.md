
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bidser

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/bidser)](https://CRAN.R-project.org/package=bidser)
[![R-CMD-check](https://github.com/bbuchsbaum/bidser/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bbuchsbaum/bidser/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/bbuchsbaum/bidser/actions/workflows/pkgdown.yaml/badge.svg)](https://bbuchsbaum.github.io/bidser/)
[![Codecov test
coverage](https://codecov.io/gh/bbuchsbaum/bidser/branch/master/graph/badge.svg)](https://app.codecov.io/gh/bbuchsbaum/bidser?branch=master)
<!-- badges: end -->

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
[Changelog](NEWS.md)

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
query it with the public entry points `participants()`, `tasks()`, and
`func_scans()`. Point the same helpers at a real tree via
`bids_project()`:

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

Related APIs include `read_events()`, `query_files()`, `get_metadata()`,
and `derivative_pipelines()`. For a downloaded example dataset see
[Getting
started](https://bbuchsbaum.github.io/bidser/articles/quickstart.html).

## fMRIPrep confounds

`read_confounds()` selects nuisance regressors from fMRIPrep confound
tables. Prefer the public, version-robust helpers over hand-listed
column names:

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
set. See `?read_confounds`, `?confound_set`, and the [confounds
vignette](https://bbuchsbaum.github.io/bidser/articles/confounds-and-variables.html).
