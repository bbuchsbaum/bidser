# Create a Mock BIDS Project Object

Generates an in-memory representation of a BIDS project, suitable for
testing and demonstration without requiring actual data files. Can
optionally create a "stub" directory structure on disk.

## Usage

``` r
create_mock_bids(
  project_name,
  participants,
  file_structure,
  dataset_description = NULL,
  event_data = list(),
  confound_data = list(),
  create_stub = FALSE,
  stub_path = NULL,
  prep_dir = "derivatives/fmriprep"
)
```

## Arguments

- project_name:

  A character string for the project name.

- participants:

  Either a `data.frame` mirroring `participants.tsv` content (must
  include 'participant_id') or a character vector of participant IDs
  (e.g., `c("01", "02")`). If IDs are given, a minimal `part_df` is
  created.

- file_structure:

  A `data.frame` or `tibble` defining the files in the mock structure.
  Each row represents a file. Required columns: `subid`, `datatype`,
  `suffix`. Optional BIDS entity columns: `session`, `task`, `run`,
  `acq`, `rec`, `dir`, `space`, `desc`, etc. Must also include a logical
  column `fmriprep` indicating if the file belongs in the derivatives
  directory specified by `prep_dir`.

- dataset_description:

  A list representing the `dataset_description.json` content. Defaults
  to a minimal valid description.

- event_data:

  A named list where names are the *relative paths* of `events.tsv`
  files (e.g., "sub-01/func/sub-01_task-A_run-1_events.tsv") and values
  are the corresponding `tibble` or `data.frame` content for those
  files. These paths must correspond to files defined in
  `file_structure` with a `suffix` like "events.tsv".

- confound_data:

  A named list where names are relative paths of confound TSV files
  within the derivatives directory and values are their `tibble` or
  `data.frame` content. Paths must match files defined in
  `file_structure`.

- create_stub:

  Logical (default `FALSE`). If `TRUE`, write a stub BIDS directory
  structure to disk at `stub_path`. Zero-byte files are created except
  for `participants.tsv`, `dataset_description.json`, and `events.tsv`
  files specified in `event_data`.

- stub_path:

  Character string, the path where the stub directory will be created.
  Required if `create_stub = TRUE`.

- prep_dir:

  Character string, the path relative to the root for derivatives
  (default "derivatives/fmriprep"). This path structure will be used
  both in the internal `data.tree` and for stub creation.

## Value

An object of class `mock_bids_project`.

## Examples

``` r
# \donttest{
# --- Example Setup ---
participants_df <- tibble::tibble(participant_id = c("01", "02"), age = c(25, 30))

file_structure_df <- tibble::tribble(
  ~subid, ~session, ~datatype, ~task,   ~run, ~suffix,                  ~fmriprep, ~desc,
  "01",   NA,       "anat",    NA,      NA,   "T1w.nii.gz",             FALSE,     NA,
  "01",   NA,       "func",    "taskA", "01", "bold.nii.gz",            FALSE,     NA,
  "01",   NA,       "func",    "taskA", "01", "events.tsv",             FALSE,     NA,
  "02",   "test",   "anat",    NA,      NA,   "T1w.nii.gz",             FALSE,     NA,
  "02",   "test",   "func",    "taskA", "01", "bold.nii.gz",            FALSE,     NA,
  "02",   "test",   "func",    "taskA", "01", "events.tsv",             FALSE,     NA,
  # Example derivative
  "01",   NA,       "func",    "taskA", "01", "preproc_bold.nii.gz",    TRUE,      "preproc"
)

# Define event data (paths must match generated structure)
event_data_list <- list()
event_data_list[["sub-01/func/sub-01_task-taskA_run-01_events.tsv"]] <- tibble::tibble(
  onset = c(1.0, 5.0), duration = c(0.5, 0.5), trial_type = c("condA", "condB")
)
event_data_list[["sub-02/ses-test/func/sub-02_ses-test_task-taskA_run-01_events.tsv"]] <-
  tibble::tibble(
    onset = c(1.5, 5.5), duration = c(0.5, 0.5), trial_type = c("condC", "condD")
)

# Create the mock project (in memory only)
mock_proj <- create_mock_bids(
  project_name = "MockTaskA",
  participants = participants_df,
  file_structure = file_structure_df,
  event_data = event_data_list
)

# Create the mock project and write stubs
mock_proj_stub <- create_mock_bids(
  project_name = "MockTaskA_stub",
  participants = c("01", "02"), # Example using just IDs
  file_structure = file_structure_df,
  event_data = event_data_list,
  create_stub = TRUE,
  stub_path = tempdir() # Use a temporary directory for example
)

# --- Using the Mock Project ---
print(mock_proj)
#> Mock BIDS Project Summary 
#> Project Name:  MockTaskA 
#> Participants (n):  2 
#> Tasks:  taskA 
#> Sessions:  test 
#> Derivatives:  derivatives/fmriprep 
#> Datatypes:  anat, func 
#> Suffixes:  nii.gz, tsv 
#> BIDS Keys:  (none) 
#> Path:  mock://MockTaskA 
print(participants(mock_proj))
#> [1] "01" "02"
print(tasks(mock_proj))
#> [1] "taskA"
print(sessions(mock_proj)) # Should return "test"
#> [1] "test"

print(func_scans(mock_proj, subid = "01"))
#> [1] "MockTaskA/sub-01/func/sub-01_task-taskA_run-01_bold.nii.gz"
print(event_files(mock_proj, subid = "02", session = "test"))
#> [1] "MockTaskA/sub-02/ses-test/func/sub-02_ses-test_task-taskA_run-01_events.tsv"

# Read the injected event data
events_sub1 <- read_events(mock_proj, subid = "01")
print(events_sub1)
#> # A tibble: 1 × 5
#> # Groups:   .subid, .task, .run, .session [1]
#>   .subid .task .run  .session data            
#>   <chr>  <chr> <chr> <chr>    <list>          
#> 1 01     taskA 01    NA       <tibble [2 × 3]>
if (nrow(events_sub1) > 0) print(tidyr::unnest(events_sub1, cols = data))
#> # A tibble: 2 × 7
#> # Groups:   .subid, .task, .run, .session [1]
#>   .subid .task .run  .session onset duration trial_type
#>   <chr>  <chr> <chr> <chr>    <dbl>    <dbl> <chr>     
#> 1 01     taskA 01    NA           1      0.5 condA     
#> 2 01     taskA 01    NA           5      0.5 condB     

# Search for derivatives
print(search_files(mock_proj, suffix = "preproc_bold.nii.gz"))
#> NULL

# Check stub directory (if created)
# list.files(mock_proj_stub$path, recursive = TRUE)
# if (file.exists(file.path(mock_proj_stub$path, names(event_data_list)[1]))) {
#   print(readLines(file.path(mock_proj_stub$path, names(event_data_list)[1])))
# }

# Clean up stub directory if created in temp
# unlink(mock_proj_stub$path, recursive = TRUE)
# }
```
