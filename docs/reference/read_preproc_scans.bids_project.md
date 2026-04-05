# Read preprocessed functional MRI scans from a BIDS project

This function reads preprocessed functional MRI scans from a BIDS
project's fMRIPrep derivatives directory. It uses the `preproc_scans`
function to locate the files and then reads them into a `NeuroVec`
object using the neuroim2 package. If a mask is not provided, one will
be automatically created from available brainmask files.

## Usage

``` r
read_preproc_scans.bids_project(
  x,
  mask = NULL,
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

  A `bids_project` object with fMRIPrep derivatives

- mask:

  A brain mask of type `LogicalNeuroVol`, or NULL (if NULL, a mask will
  be created automatically)

- mode:

  The file mode: 'normal' for in-memory files or 'bigvec' for on-disk
  files

- subid:

  Regular expression to match subject IDs (default: "^sub-.\*" to match
  all subjects)

- task:

  Regular expression to match tasks (default: ".\*" to match all tasks)

- run:

  Regular expression to match runs (default: ".\*" to match all runs)

- modality:

  Image modality to match (default: "bold" for functional MRI)

- ...:

  Extra arguments passed to
  [`neuroim2::read_vec`](https://bbuchsbaum.github.io/neuroim2/reference/read_vec.html)

## Value

An instance of type `NeuroVec` containing the preprocessed functional
data.

## Details

This function requires the `neuroim2` package to be installed. It will
throw an error if the package is not available or if fMRIPrep
derivatives are not found in the BIDS project. If no mask is provided,
it will create one using the `create_preproc_mask` function.

## Examples

``` r
# \donttest{
# Load a BIDS project with fMRIPrep derivatives
tryCatch({
  ds_path <- get_example_bids_dataset("ds000001-fmriprep")
  proj <- bids_project(ds_path, fmriprep=TRUE)
  
  # Read preprocessed scans for all subjects
  # (mask will be created automatically)
  all_scans <- read_preproc_scans(proj)
  
  # Read preprocessed scans for a specific subject
  sub01_scans <- read_preproc_scans(proj, subid="01")
  
  # Read preprocessed scans for a specific task and run
  task_scans <- read_preproc_scans(proj, 
                                  task="balloonanalogrisktask",
                                  run="01")
  
  # Specify mode for large datasets
  bigvec_scans <- read_preproc_scans(proj, mode="bigvec")
  
  # Provide a custom mask
  mask <- create_preproc_mask(proj, thresh=0.95)
  masked_scans <- read_preproc_scans(proj, mask=mask)
  
  # Clean up
  unlink(ds_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires derivatives dataset: ", e$message)
})
#> Example requires derivatives dataset: participants.tsv is missing
# }
```
