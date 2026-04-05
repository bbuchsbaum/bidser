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
