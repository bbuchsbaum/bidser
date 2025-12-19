# Read Confound Files from a BIDS Project

This function reads in fMRIPrep confound tables for one or more subjects
from a BIDS project. Confound files contain nuisance variables that can
be used for denoising fMRI data, such as motion parameters,
physiological signals, and other noise components. The function can
optionally perform PCA reduction on the confounds and return either
nested or flat tibbles.

## Usage

``` r
read_confounds(x, ...)
```

## Arguments

- x:

  The object to read confounds from (typically a `bids_project`).

- ...:

  Additional arguments passed to methods, including:

  - `subid`: Regex to match subject IDs (default: ".\*")

  - `task`: Regex to match tasks (default: ".\*")

  - `session`: Regex to match sessions (default: ".\*")

  - `run`: Regex to match runs (default: ".\*")

  - `cvars`: Character vector of confound variable names to select

  - `npcs`: Integer. Perform PCA reduction and return this many PCs

  - `perc_var`: Numeric. Perform PCA reduction to retain this percentage
    of variance

  - `nest`: Logical. If TRUE, nests confound tables by
    subject/session/run (default: TRUE)

## Value

A tibble containing confound data. If `nest=TRUE` (default), returns a
nested tibble with columns for subject, session, run, and a nested
`data` column containing the confound variables. If `nest=FALSE`,
returns a flat tibble with all confound variables. Returns NULL if no
matching files are found.

## Examples

``` r
# \donttest{
# Create a BIDS project with fMRIPrep derivatives
tryCatch({
  ds_path <- get_example_bids_dataset("ds000001-fmriprep")
  proj <- bids_project(ds_path, fmriprep=TRUE)
  
  # Read all confound files
  all_conf <- read_confounds(proj)
  
  # Read confounds for specific subjects and tasks
  sub_conf <- read_confounds(proj,
                            subid="01",
                            task="balloonanalogrisktask")
  
  # Select specific confound variables
  motion_conf <- read_confounds(proj,
                               cvars=c("framewise_displacement",
                                      "trans_x", "trans_y", "trans_z",
                                      "rot_x", "rot_y", "rot_z"))
  
  # Perform PCA reduction
  pca_conf <- read_confounds(proj, npcs=5)
  
  # Get confounds as a flat tibble
  flat_conf <- read_confounds(proj, nest=FALSE)
  
  # Combine multiple options
  custom_conf <- read_confounds(proj,
                               subid="01",
                               task="balloonanalogrisktask",
                               cvars=c("framewise_displacement",
                                      "trans_x", "trans_y", "trans_z"),
                               npcs=3,
                               nest=FALSE)
  
  # Clean up
  unlink(ds_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
#> Example requires internet connection: participants.tsv is missing
# }
```
