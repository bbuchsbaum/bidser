# Access a single subject from a BIDS project

`bids_subject` returns a lightweight interface with helper functions for
retrieving data associated with one subject.

`bids_subject` returns a lightweight facade that exposes convenience
functions to work with all data associated with one subject within a
BIDS project.

This function extracts a single subject's data from a BIDS project,
creating a new BIDS project object containing only that subject's files
and metadata.

## Usage

``` r
bids_subject(x, subid, ...)

bids_subject.bids_project(x, subid, ...)

bids_subject(x, subid, ...)
```

## Arguments

- x:

  A `bids_project` object.

- subid:

  Character string. The subject ID to extract (without the "sub-"
  prefix).

- ...:

  Additional arguments (not currently used).

## Value

A list of helper functions for the subject.

A list containing subject-specific helper functions. Each function
automatically filters results for the specified subject. The returned
object contains the following callable functions:

- `events(...)`:

  Returns nested tibble with event data for this subject. Equivalent to
  `read_events(project, subid = "XX", ...)`. Additional arguments (task,
  session, run, nest, etc.) can be passed.

- `event_files(...)`:

  Returns character vector of event file paths for this subject.
  Equivalent to `event_files(project, subid = "XX", ...)`. Additional
  arguments (task, session, run, full_path, etc.) can be passed.

- `scans(...)`:

  Returns character vector of functional scan file paths for this
  subject. Equivalent to `func_scans(project, subid = "XX", ...)`.
  Additional arguments (task, session, run, kind, full_path, etc.) can
  be passed.

- `confounds(...)`:

  Returns confound data for this subject (requires fMRIPrep
  derivatives). Equivalent to
  `read_confounds(project, subid = "XX", ...)`. Additional arguments
  (task, session, run, cvars, npcs, etc.) can be passed.

- `preproc_scans(...)`:

  Returns preprocessed scan paths for this subject (requires fMRIPrep
  derivatives). Equivalent to
  `preproc_scans(project, subid = "XX", ...)`. Additional arguments
  (task, session, run, space, variant, etc.) can be passed.

- `brain_mask(...)`:

  Creates brain mask for this subject (requires fMRIPrep derivatives).
  Equivalent to `brain_mask(project, subid = "XX", ...)`. Additional
  arguments (thresh, etc.) can be passed.

A new `bids_project` object containing only the specified subject's
data. Returns NULL if the subject is not found in the project.

## Examples

``` r
# \donttest{
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  subj <- bids_subject(proj, "01")
  subj$events()
  subj$scans()
  
  # Clean up
  # Example datasets are cached; leave the cache in place.
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
#> [1] "/tmp/RtmpuRZsQl/bids_example_ds001/sub-01/func/sub-01_task-balloonanalogrisktask_run-01_bold.nii.gz"
#> [2] "/tmp/RtmpuRZsQl/bids_example_ds001/sub-01/func/sub-01_task-balloonanalogrisktask_run-02_bold.nii.gz"
#> [3] "/tmp/RtmpuRZsQl/bids_example_ds001/sub-01/func/sub-01_task-balloonanalogrisktask_run-03_bold.nii.gz"
# }
# \donttest{
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  
  # Create subject interface for subject 01
  subj <- bids_subject(proj, "01")
  
  # Get functional scan paths for this subject
  scan_paths <- subj$scans()
  print(paste("Subject 01 has", length(scan_paths), "functional scans"))
  
  # Get event file paths for this subject
  event_paths <- subj$event_files()
  print(paste("Subject 01 has", length(event_paths), "event files"))
  
  # Read event data for this subject
  event_data <- subj$events()
  print("Event data structure:")
  print(event_data)
  
  # You can still pass additional filtering arguments
  # For example, get only specific tasks:
  task_scans <- subj$scans(task = "balloonanalogrisktask")
  
  # Dataset cache is intentionally retained for performance.
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
#> [1] "Subject 01 has 3 functional scans"
#> [1] "Subject 01 has 3 event files"
#> [1] "Event data structure:"
#> # A tibble: 3 × 9
#> # Groups:   .task, .session, .run, .subid [3]
#>   .task        .session .run  .subid task  session run   participant_id data    
#>   <chr>        <chr>    <chr> <chr>  <chr> <chr>   <chr> <chr>          <list>  
#> 1 balloonanal… NA       01    01     ball… NA      01    01             <tibble>
#> 2 balloonanal… NA       02    01     ball… NA      02    01             <tibble>
#> 3 balloonanal… NA       03    01     ball… NA      03    01             <tibble>
# }
# \donttest{
# Create a subject interface
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  
  # Create subject interface for subject 01  
  subj <- bids_subject(proj, "01")
  
  # Use the helper functions
  scans <- subj$scans()
  events <- subj$event_files()
  print(paste("Subject 01:", length(scans), "scans,", length(events), "events"))
  
  # Dataset cache is intentionally retained for performance.
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
#> [1] "Subject 01: 3 scans, 3 events"
# }
```
