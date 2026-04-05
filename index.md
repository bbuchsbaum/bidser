# bidser

[BIDS](https://bids.neuroimaging.io/) in R – (it’s a start!)

The goal of bidser is to make working with the BIDS neuroimaging format
convenient in R. Currently there is support for MRI data and some
support for some [fmriprep](https://fmriprep.org/en/stable/)
derivatives.

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bbuchsbaum/bidser")
```

## Example

See <https://bbuchsbaum.github.io/bidser/articles/quickstart.html>

## Citation

If you use bidser in published work, please cite:

Buchsbaum, B. (2026). *bidser: Work with Brain Imaging Data Structure
(BIDS) Projects*. R package version 0.4.0.
<https://github.com/bbuchsbaum/bidser>

## Albers theme

This package uses the albersdown theme. Existing vignette theme hooks
are replaced so `albers.css` and local `albers.js` render consistently
on CRAN and GitHub Pages. The defaults are configured via
`params$family` and `params$preset` (family = ‘red’, preset = ‘homage’).
The pkgdown site uses `template: { package: albersdown }` together with
generated `pkgdown/extra.css` and `pkgdown/extra.js` so the theme is
linked and activated on site pages.
