# Create a lightweight text report for a BIDS project

Create a lightweight text report for a BIDS project

## Usage

``` r
bids_report(x, ...)
```

## Arguments

- x:

  A `bids_project` object.

- ...:

  Additional arguments passed to
  [`bids_report_data()`](https://bbuchsbaum.github.io/bidser/reference/bids_report_data.md).

## Value

An object of class `bids_report`.

## Examples

``` r
# \donttest{
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  rpt <- bids_report(proj)
  print(rpt)
  unlink(ds001_path, recursive = TRUE)
}, error = function(e) message("Example requires internet: ", e$message))
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> Project does not have fmriprep derivatives enabled. Cannot search for confound files.
#> BIDS Report
#> Project: bids_example_ds001 
#> Participants source: file 
#> Subjects: 16 
#> Sessions: 0 
#> Tasks: balloonanalogrisktask 
#> Total runs: 3 
#> Compliance: passed 
#> Index: available 
#> Indexed runs: 48 
# }
```
