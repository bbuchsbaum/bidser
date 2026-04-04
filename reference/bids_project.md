# Create a BIDS Project Object

This function creates a BIDS project object from a directory containing
BIDS-formatted neuroimaging data. It can optionally load preprocessed
derivatives from fMRIPrep. The function validates the basic BIDS
structure and provides methods for accessing raw and preprocessed data,
querying subjects, sessions, and tasks, reading event files, and
checking BIDS compliance.

## Usage

``` r
bids_project(
  path = ".",
  fmriprep = FALSE,
  prep_dir = "derivatives/fmriprep",
  strict_participants = TRUE,
  derivatives = c("auto", "legacy", "none"),
  pipelines = NULL,
  index = c("auto", "none"),
  index_path = NULL
)
```

## Arguments

- path:

  Character string. The file path to the root of the BIDS project.
  Defaults to the current directory (".").

- fmriprep:

  Logical. Whether to load the fMRIPrep derivatives folder hierarchy.
  Defaults to FALSE. This remains available as a legacy compatibility
  switch for existing fMRIPrep-oriented workflows.

- prep_dir:

  Character string. The location of the fMRIPrep subfolder relative to
  the derivatives directory. Defaults to "derivatives/fmriprep". New
  code should prefer `derivatives = "auto"` plus
  [`derivative_pipelines()`](https://bbuchsbaum.github.io/bidser/reference/derivative_pipelines.md).

- strict_participants:

  Logical. If TRUE (default), require `participants.tsv`. If FALSE,
  infer participants from `sub-*` directories when the file is missing
  or incomplete.

- derivatives:

  Derivatives loading mode:

  - `"auto"` discovers available pipelines under `derivatives/`
    (default)

  - `"legacy"` keeps the older `fmriprep`/`prep_dir` behavior

  - `"none"` disables derivative discovery

- pipelines:

  Optional character vector of derivative pipeline names (or relative
  roots) to include when `derivatives = "auto"`.

- index:

  Whether to use an on-disk file index:

  - `"auto"` loads an existing index or creates one on first load
    (default). The index is automatically rebuilt when the dataset
    directory mtime changes.

  - `"none"` disables indexing

- index_path:

  Optional path for the persisted index file. Defaults to
  `file.path(path, ".bidser_index.rds")`.

## Value

A `bids_project` object representing the BIDS project structure. The
object provides methods for:

- Accessing raw and preprocessed data files

- Querying subjects, sessions, and tasks

- Reading event files and confound regressors

- Checking BIDS compliance

- Extracting metadata from file names Returns NULL if the directory does
  not contain a valid BIDS dataset.

## Examples

``` r
# \donttest{
# Create a BIDS project
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  
  # Get all functional scans
  all_scans <- func_scans(proj)
  
  # Get scans for specific subjects
  sub_scans <- func_scans(proj, subid="0[123]")
  
  # Get scans for a specific task
  task_scans <- func_scans(proj, task="rest")
  
  # Get scans from specific runs
  run_scans <- func_scans(proj, run="0[123]")
  
  # Combine multiple filters
  filtered_scans <- func_scans(proj,
                              subid="01",
                              task="rest",
                              run="01")
  
  # Get relative paths instead of full paths
  rel_scans <- func_scans(proj, full_path=FALSE)
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
# }
```
