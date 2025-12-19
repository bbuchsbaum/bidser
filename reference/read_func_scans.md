# Read in a set of four-dimensional functional scans

Read in a set of four-dimensional functional scans

## Usage

``` r
read_func_scans.bids_project(
  x,
  mask,
  mode = c("normal", "bigvec"),
  subid = "^sub-.*",
  task = ".*",
  run = ".*",
  modality = "bold",
  ...
)
```

## Arguments

- x:

  A `bids_project` object

- mask:

  A brain mask of type `LogicalNeuroVol`

- mode:

  The file mode: 'normal' for in-memory files or 'bigvec' for on-disk
  files

- subid:

  One or more subject IDs (regex)

- task:

  An optional task regex

- run:

  An optional run regex

- modality:

  The image modality (usually "bold")

- ...:

  Extra arguments passed to
  [`neuroim2::read_vec`](https://bbuchsbaum.github.io/neuroim2/reference/read_vec.html)

## Value

An instance of type `NeuroVec`

## Examples

``` r
# \donttest{
tryCatch({
  ds_path <- get_example_bids_dataset("ds000001-fmriprep")
  proj <- bids_project(ds_path, fmriprep=TRUE)
  mask <- brain_mask(proj, subid="01")
  vec <- read_func_scans.bids_project(proj, mask,
                                     subid="01",
                                     task="balloonanalogrisktask",
                                     run="01")
  unlink(ds_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires derivatives dataset: ", e$message)
})
#> Example requires derivatives dataset: participants.tsv is missing
# }
```
