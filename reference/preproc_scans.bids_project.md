# Get preprocessed scans from a BIDS project

This function retrieves paths to preprocessed functional MRI scans from
a BIDS project's fMRIPrep derivatives. It allows filtering by various
BIDS entities such as subject, task, run, session, and space. The
function is particularly useful for accessing preprocessed data for
analysis pipelines.

## Usage

``` r
# S3 method for class 'bids_project'
preproc_scans(
  x,
  subid = ".*",
  task = ".*",
  run = ".*",
  session = ".*",
  variant = NULL,
  space = ".*",
  modality = "bold",
  kind = ".*",
  full_path = FALSE,
  ...
)
```

## Arguments

- x:

  A `bids_project` object.

- subid:

  A regex pattern for matching subjects. Default is ".\*".

- task:

  A regex pattern for matching tasks. Default is ".\*".

- run:

  A regex pattern for matching runs. Default is ".\*".

- session:

  A regex pattern for matching sessions. Default is ".\*".

- variant:

  A regex pattern for matching preprocessing variants. Default is NULL
  (no variant filtering).

- space:

  A regex pattern for matching spaces (e.g., "MNI152NLin2009cAsym").
  Default is ".\*".

- modality:

  A regex pattern for matching modality. Default is "bold". Set this to
  something else if you need a different modality.

- kind:

  The kind of preprocessed data to return. Default is ".\*" to match any
  kind.

- full_path:

  If TRUE, return full file paths. Otherwise return relative paths.
  Default is FALSE.

- ...:

  Additional key-value filters for BIDS entities. These are matched
  against parsed file entities in the derivatives tree. Common examples:
  `space = "MNI152NLin2009cAsym"`, `res = "2"`, `acq = "ap"`,
  `echo = "1"`. Values are treated as regex. Keys already covered by
  explicit arguments (`subid`, `task`, `run`, `session`, `space`,
  `variant`, `modality`, `kind`) are ignored in `...`.

## Value

A character vector of file paths to preprocessed scans matching the
criteria. Returns NULL if:

- No matching files are found

- The project doesn't have fMRIPrep derivatives

- The specified criteria don't match any files

## Examples

``` r
# \donttest{
# Create a BIDS project with fMRIPrep derivatives
tryCatch({
  ds_path <- get_example_bids_dataset("phoneme_stripped")
  proj <- bids_project(ds_path, fmriprep=TRUE)
  
  # Get all preprocessed BOLD scans
  all_scans <- preproc_scans(proj)
  
  # Get preprocessed scans for specific subjects
  sub_scans <- preproc_scans(proj, subid="0[12]")
  
  # Get scans in MNI space
  mni_scans <- preproc_scans(proj, space="MNI152NLin2009cAsym")
  
  # Get scans for a specific task with full paths
  task_scans <- preproc_scans(proj,
                             task="phoneme",
                             full_path=TRUE)
  
  # Get scans from a specific session
  session_scans <- preproc_scans(proj, session="test")
  
  # Combine multiple filters
  filtered_scans <- preproc_scans(proj,
                                 subid="01",
                                 task="phoneme",
                                 run="01",
                                 space="MNI152NLin2009cAsym")
  
  # Filter by resolution (BIDS entity 'res')
  res2_scans <- preproc_scans(proj, res = "2")
  
  # Clean up
  unlink(ds_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
#> Example requires internet connection: Failed to download BIDS example data: Dataset 'phoneme_stripped' not found in BIDS examples
# }
```
