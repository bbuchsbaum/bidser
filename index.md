# bidser

[BIDS](https://bids.neuroimaging.io/) in R – (it’s a start!)

The goal of bidser is to make working with the BIDS neuroimaging format
convenient in R. Current support is strongest for MRI datasets, with
explicit query helpers, metadata inheritance, derivative pipeline
discovery, and compatibility-oriented support for
[fmriprep](https://fmriprep.org/en/stable/) workflows.

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r

# install.packages("devtools")
devtools::install_github("bbuchsbaum/bidser")
```

## Example

See <https://bbuchsbaum.github.io/bidser/articles/quickstart.html>

## fMRIPrep confounds

[`read_confounds()`](https://bbuchsbaum.github.io/bidser/reference/read_confounds.md)
selects nuisance regressors from fMRIPrep confound tables. Rather than
hand-listing version-specific column names, use the high-level,
version-robust helpers:

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
and the *confounds-and-variables* vignette for details.
