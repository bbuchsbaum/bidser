# Read sidecar JSON files and return metadata as a tidy tibble

This function searches for JSON sidecar files matching the given
criteria (subject, task, run, session), reads the JSON content, and
converts all top-level fields into columns of a tibble. Each file's
metadata becomes one row in the returned tibble. This is particularly
useful for extracting metadata about BIDS imaging files, such as
acquisition parameters, task descriptions, and other relevant
information.

## Usage

``` r
read_sidecar(
  x,
  subid = ".*",
  task = ".*",
  run = ".*",
  session = ".*",
  modality = "bold",
  full_path = TRUE,
  inherit = FALSE,
  inherit_scope = c("auto", "raw", "derivatives", "all"),
  ...
)
```

## Arguments

- x:

  A `bids_project` object.

- subid:

  A regex for matching subject IDs. Default is `".*"`.

- task:

  A regex for matching tasks. Default is `".*"`.

- run:

  A regex for matching runs. Default is `".*"`.

- session:

  A regex for matching sessions. Default is `".*"`.

- modality:

  A regex for matching modality/kind (e.g. "bold"). Default is `"bold"`.
  This is matched against the 'kind' field in parsed BIDS filenames.

- full_path:

  If TRUE, return full file paths in the `file` column. Default is TRUE.

- inherit:

  Controls what the function anchors on:

  - `FALSE` (default): read each matching JSON sidecar *file* directly.
    One row per JSON file; `file` is the JSON path.

  - `TRUE`: resolve *effective* metadata per matching imaging *scan*
    using BIDS inheritance
    ([`get_metadata()`](https://bbuchsbaum.github.io/bidser/reference/get_metadata.md)).
    One row per matching data file; `file` is the scan path. This
    surfaces task- and dataset-level sidecars that apply to a scan even
    when it has no file-level JSON of its own.

- inherit_scope:

  Scope used when `inherit = TRUE`:

  - `"auto"` (default) infers raw/derivatives from file location

  - `"raw"`, `"derivatives"`, or `"all"` override scope explicitly

- ...:

  Additional arguments passed to
  [`search_files()`](https://bbuchsbaum.github.io/bidser/reference/search_files.md).

## Value

A tibble with one row per matched file (a JSON sidecar when
`inherit = FALSE`, an imaging scan when `inherit = TRUE`). Columns
include:

- `file`: the sidecar (or scan) file path

- `.subid`: subject ID extracted from filename

- `.session`: session ID extracted from filename (if present)

- `.task`: task name extracted from filename (if present)

- `.run`: run number extracted from filename (if present)

- Additional columns for each resolved top-level metadata key If no
  files are found, returns an empty tibble.

## Examples

``` r
# \donttest{
# Read all BOLD sidecar files from a BIDS dataset
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  metadata <- read_sidecar(proj)
  
  # Read sidecar files for a specific subject and task
  sub01_meta <- read_sidecar(proj, 
                            subid="01", 
                            task="balloonanalogrisktask")
  
  # Read sidecar files for anatomical data
  anat_meta <- read_sidecar(proj, 
                           modality="T1w",
                           full_path=FALSE)
  
  # Clean up
  # Example datasets are cached; leave the cache in place.
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
#> No matching JSON sidecar files found.
#> No matching JSON sidecar files found.
# }
```
