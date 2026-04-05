# Create a binary brain mask from preprocessed scans

This function creates a binary brain mask from preprocessed functional
scans in a BIDS project. It searches for BOLD brain mask files in the
fMRIPrep derivatives directory (i.e., files in the `func/` folder
matching the pattern `*_desc-brain_mask.nii.gz` or the older
`*_brainmask.nii.gz`), reads them with neuroim2, averages them, and
thresholds the result to produce a consensus binary mask.

## Usage

``` r
# S3 method for class 'bids_project'
create_preproc_mask(
  x,
  subid,
  thresh = 0.99,
  task = ".*",
  space = ".*",
  mask_kinds = c("brainmask", "mask"),
  ...
)
```

## Arguments

- x:

  A `bids_project` object with fMRIPrep derivatives.

- subid:

  Regular expression to match subject IDs (e.g., `"01"` for subject 01,
  `".*"` for all subjects).

- thresh:

  Threshold value between 0 and 1 (default 0.99). Voxels below this
  value in the averaged mask are excluded. Higher values produce more
  conservative masks.

- task:

  Regular expression for task filtering. Defaults to `".*"` (any task).
  Because functional masks always carry a `task` entity, this also
  implicitly excludes anatomical masks which lack it.

- space:

  Regular expression for output-space filtering (e.g.,
  `"MNI152NLin2009cAsym"`). Defaults to `".*"` (all spaces). When masks
  from multiple spaces are found the function stops with an error
  because their dimensions are incompatible.

- mask_kinds:

  Character vector of BIDS suffixes to search. Defaults to both
  `"brainmask"` (older fMRIPrep) and `"mask"` with `desc="brain"`
  (fMRIPrep \>= 21).

- ...:

  Additional arguments passed to `search_files` for finding mask files
  (e.g., `session`, `run`).

## Value

A logical mask volume (`LogicalNeuroVol`) suitable for use with
preprocessed functional data.

## Details

The search is restricted to **functional** brain masks by requiring the
`task` BIDS entity (anatomical masks do not carry `task`). When masks
from multiple output spaces are discovered the function raises an error;
pass a specific `space` value to disambiguate.

## Examples

``` r
# \donttest{
tryCatch({
  ds_path <- get_example_bids_dataset("ds000001-fmriprep")
  proj <- bids_project(ds_path, fmriprep=TRUE)

  # Mask for one subject in a specific space
  mask <- create_preproc_mask(proj, subid="01",
                              space="MNI152NLin2009cAsym")

  # Consensus mask across all subjects / runs
  all_mask <- create_preproc_mask(proj, subid=".*",
                                  space="MNI152NLin2009cAsym")

  # Restrict to a single task
  task_mask <- create_preproc_mask(proj, subid=".*",
                                  task="balloonanalogrisktask",
                                  space="MNI152NLin2009cAsym")

  unlink(ds_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires derivatives dataset: ", e$message)
})
#> Example requires derivatives dataset: participants.tsv is missing
# }
```
