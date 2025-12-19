# Create a binary brain mask from preprocessed scans

This function creates a binary brain mask from preprocessed functional
scans in a BIDS project. It searches for brainmask files in the fMRIPrep
derivatives directory, reads them using the neuroim2 package, and
averages them to create a single mask. The resulting mask can be used
for subsequent analyses with preprocessed functional data.

## Usage

``` r
# S3 method for class 'bids_project'
create_preproc_mask(
  x,
  subid,
  thresh = 0.99,
  mask_kinds = c("brainmask", "mask"),
  ...
)
```

## Arguments

- x:

  A `bids_project` object with fMRIPrep derivatives

- subid:

  Regular expression to match subject IDs (e.g., "01" for subject 01,
  ".\*" for all subjects)

- thresh:

  Threshold value between 0 and 1 (default 0.99). Values outside this
  range will trigger an error. Voxels with values below the threshold
  are excluded from the mask.

- mask_kinds:

  Character vector of BIDS file types to search when locating mask
  files. Defaults to both "brainmask" (older fMRIPrep versions) and
  "mask" with `desc="brain"` (BIDS 1.6+).

- ...:

  Additional arguments passed to `search_files` for finding mask files

## Value

A logical mask volume (`LogicalNeuroVol`) that can be used for
subsequent analyses with preprocessed functional data.

## Details

The function works by finding all brainmask files that match the subject
ID pattern, reading them into memory, averaging them, and then
thresholding the result to create a binary mask. This is useful when you
want to analyze multiple runs or subjects together and need a common
mask that covers the brain areas present in all scans.

The threshold parameter controls how conservative the mask is. Higher
values (closer to 1) result in a more conservative mask that includes
only voxels that are consistently marked as brain across all
subjects/runs. Lower values create a more inclusive mask.

## Examples

``` r
# \donttest{
# Load a BIDS project with fMRIPrep derivatives
tryCatch({
  ds_path <- get_example_bids_dataset("ds000001-fmriprep")
  proj <- bids_project(ds_path, fmriprep=TRUE)
  
  # Create a mask for all subjects (conservative threshold)
  all_subj_mask <- create_preproc_mask(proj, subid=".*")
  
  # Create a mask for a specific subject
  sub01_mask <- create_preproc_mask(proj, subid="01")
  
  # Create a more inclusive mask with a lower threshold
  inclusive_mask <- create_preproc_mask(proj, subid=".*", thresh=0.8)
  
  # Use additional search criteria
  task_mask <- create_preproc_mask(proj, subid=".*", task="balloonanalogrisktask")
  
  # Clean up
  unlink(ds_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires derivatives dataset: ", e$message)
})
#> Example requires derivatives dataset: participants.tsv is missing
# }
```
