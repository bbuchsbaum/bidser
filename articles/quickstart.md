# Getting Started with bidser

## Introduction to bidser

`bidser` is an R package designed for working with neuroimaging data
organized according to the [Brain Imaging Data Structure
(BIDS)](https://bids.neuroimaging.io/) standard. BIDS is a specification
that describes how to organize and name neuroimaging and behavioral
data, making datasets more accessible, shareable, and easier to analyze.

### What is BIDS?

BIDS organizes data into a hierarchical folder structure with
standardized naming conventions:

- **Subjects** are identified by folders named `sub-XX`
- **Sessions** (optional) are identified by folders named `ses-XX`
- **Data types** are organized into modality-specific folders (`anat`,
  `func`, `dwi`, etc.)
- **Files** follow specific naming patterns that encode metadata
  (subject, session, task, run, etc.)

### What does bidser do?

`bidser` provides tools to:

- **Query and filter** files based on BIDS metadata (subject, task, run,
  etc.)
- **Read event files** that describe experimental paradigms
- **Work with fMRIPrep derivatives** for preprocessed data
- **Navigate complex BIDS hierarchies** without manually constructing
  file paths

Let’s explore these capabilities using a real BIDS dataset.

## Loading a BIDS Dataset

We’ll use the `ds001` dataset from the BIDS examples, which contains
data from a “Balloon Analog Risk Task” experiment with 16 subjects.

    #> 
    #> Attaching package: 'bidser'
    #> The following object is masked from 'package:base':
    #> 
    #>     parse

``` r
library(bidser)
library(tibble)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(gluedown)

# Download example BIDS dataset
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  
  print(proj)
}, error = function(e) {
  message("This vignette requires an internet connection to download example data.")
  message("Error: ", e$message)
  knitr::knit_exit()
})
#> BIDS Project Summary 
#> Project Name:  bids_example_ds001 
#> Participants (n):  16 
#> Tasks:  balloonanalogrisktask 
#> Image Types:  anat, func 
#> Modalities:  (none) 
#> Keys:  folder, kind, relative_path, subid, suffix, type, run, task
```

The `bids_project` object provides a high-level interface to the
dataset. We can see it contains 16 subjects with both anatomical and
functional data.

## Basic Dataset Queries

### Dataset Structure

Let’s explore the basic structure of this dataset:

``` r
# Check if the dataset has multiple sessions per subject
sessions(proj)
#> NULL

# Get all participant IDs
participants(proj)
#>  [1] "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" "13" "14" "15"
#> [16] "16"

# What tasks are included?
tasks(proj)
#> [1] "balloonanalogrisktask"

# Get a summary of the dataset
bids_summary(proj)
#> $n_subjects
#> [1] 16
#> 
#> $n_sessions
#> NULL
#> 
#> $tasks
#> # A tibble: 1 × 2
#>   task                  n_runs
#>   <chr>                  <int>
#> 1 balloonanalogrisktask      3
#> 
#> $total_runs
#> [1] 3
```

### Finding Files by Type

bidser provides several ways to find files. Let’s start with the most
common neuroimaging file types:

``` r
# Find all anatomical T1-weighted images
t1w_files <- search_files(proj, regex = "T1w\\.nii", full_path = FALSE)
print(paste("Found", length(t1w_files), "T1w images"))
#> [1] "Found 16 T1w images"
head(t1w_files)
#> [1] "sub-01/anat/sub-01_T1w.nii.gz" "sub-02/anat/sub-02_T1w.nii.gz"
#> [3] "sub-03/anat/sub-03_T1w.nii.gz" "sub-04/anat/sub-04_T1w.nii.gz"
#> [5] "sub-05/anat/sub-05_T1w.nii.gz" "sub-06/anat/sub-06_T1w.nii.gz"

# Find all functional BOLD scans
bold_files <- func_scans(proj, full_path = FALSE)
print(paste("Found", length(bold_files), "functional scans"))
#> [1] "Found 48 functional scans"
head(bold_files)
#> [1] "sub-01/func/sub-01_task-balloonanalogrisktask_run-01_bold.nii.gz"
#> [2] "sub-01/func/sub-01_task-balloonanalogrisktask_run-02_bold.nii.gz"
#> [3] "sub-01/func/sub-01_task-balloonanalogrisktask_run-03_bold.nii.gz"
#> [4] "sub-02/func/sub-02_task-balloonanalogrisktask_run-01_bold.nii.gz"
#> [5] "sub-02/func/sub-02_task-balloonanalogrisktask_run-02_bold.nii.gz"
#> [6] "sub-02/func/sub-02_task-balloonanalogrisktask_run-03_bold.nii.gz"
```

### Filtering by Subject and Task

One of bidser’s key strengths is filtering data by BIDS metadata:

``` r
# Get functional scans for specific subjects
sub01_scans <- func_scans(proj, subid = "01")
sub02_scans <- func_scans(proj, subid = "02")

print(paste("Subject 01 has", length(sub01_scans), "scans"))
#> [1] "Subject 01 has 3 scans"
print(paste("Subject 02 has", length(sub02_scans), "scans"))
#> [1] "Subject 02 has 3 scans"

# Filter by task (ds001 only has one task, but this shows the syntax)
task_scans <- func_scans(proj, task = "balloonanalogrisktask")
print(paste("Found", length(task_scans), "scans for the balloon task"))
#> [1] "Found 48 scans for the balloon task"

# Combine filters: specific subject AND task
sub01_task_scans <- func_scans(proj, subid = "01", task = "balloonanalogrisktask")
print(paste("Subject 01 has", length(sub01_task_scans), "balloon task scans"))
#> [1] "Subject 01 has 3 balloon task scans"
```

### Working with Multiple Subjects

You can use regular expressions to select multiple subjects at once:

``` r
# Get scans for subjects 01, 02, and 03
first_three_scans <- func_scans(proj, subid = "0[123]")
print(paste("First 3 subjects have", length(first_three_scans), "scans total"))
#> [1] "First 3 subjects have 9 scans total"

# Get scans for all subjects (equivalent to default)
all_scans <- func_scans(proj, subid = ".*")
print(paste("All subjects have", length(all_scans), "scans total"))
#> [1] "All subjects have 48 scans total"
```

## Working with Event Files

Event files describe the experimental paradigm - when stimuli were
presented, what responses occurred, etc. This is crucial for task-based
fMRI analysis.

``` r
# Find all event files
event_file_paths <- event_files(proj)
print(paste("Found", length(event_file_paths), "event files"))
#> [1] "Found 48 event files"

# Read event data into a nested data frame
events_data <- suppressMessages(read_events(proj))
head(events_data)
#> # A tibble: 6 × 5
#> # Groups:   .task, .session, .run, .subid [6]
#>   .subid .session .run  .task                 data              
#>   <chr>  <chr>    <chr> <chr>                 <list>            
#> 1 01     NA       01    balloonanalogrisktask <tibble [158 × 2]>
#> 2 01     NA       02    balloonanalogrisktask <tibble [156 × 2]>
#> 3 01     NA       03    balloonanalogrisktask <tibble [149 × 2]>
#> 4 02     NA       01    balloonanalogrisktask <tibble [185 × 2]>
#> 5 02     NA       02    balloonanalogrisktask <tibble [184 × 2]>
#> 6 02     NA       03    balloonanalogrisktask <tibble [186 × 2]>
```

Let’s explore the event data structure:

``` r
# Look at events for the first subject
library(tidyr)

first_subject_events <- suppressMessages(
  events_data %>% 
    filter(.subid == "01") %>% 
    unnest(cols = c(data))
)

print("Event structure for subject 01:")
#> [1] "Event structure for subject 01:"
print(head(first_subject_events))
#> # A tibble: 6 × 6
#> # Groups:   .task, .session, .run, .subid [1]
#>   .subid .session .run  .task                 onset\tduration\ttrial_typ…¹ .file
#>   <chr>  <chr>    <chr> <chr>                 <chr>                        <chr>
#> 1 01     NA       01    balloonanalogrisktask "0.061\t0.772\tpumps_demean… /tmp…
#> 2 01     NA       01    balloonanalogrisktask "4.958\t0.772\tpumps_demean… /tmp…
#> 3 01     NA       01    balloonanalogrisktask "7.179\t0.772\tpumps_demean… /tmp…
#> 4 01     NA       01    balloonanalogrisktask "10.416\t0.772\tpumps_demea… /tmp…
#> 5 01     NA       01    balloonanalogrisktask "13.419\t0.772\tpumps_demea… /tmp…
#> 6 01     NA       01    balloonanalogrisktask "16.754\t0.772\texplode_dem… /tmp…
#> # ℹ abbreviated name:
#> #   ¹​`onset\tduration\ttrial_type\tcash_demean\tcontrol_pumps_demean\texplode_demean\tpumps_demean\tresponse_time`

# What columns are in the event data?
print("Event file columns:")
#> [1] "Event file columns:"
print(names(first_subject_events))
#> [1] ".subid"                                                                                                     
#> [2] ".session"                                                                                                   
#> [3] ".run"                                                                                                       
#> [4] ".task"                                                                                                      
#> [5] "onset\tduration\ttrial_type\tcash_demean\tcontrol_pumps_demean\texplode_demean\tpumps_demean\tresponse_time"
#> [6] ".file"
```

### Analyzing Event Data

Let’s do some basic exploration of the experimental design:

``` r
# How many trials per subject?
trial_counts <- suppressMessages(
  events_data %>%
    unnest(cols = c(data)) %>%
    group_by(.subid) %>%
    summarise(n_trials = n(), .groups = "drop")
)

print("Trials per subject:")
#> [1] "Trials per subject:"
print(trial_counts)
#> # A tibble: 16 × 2
#>    .subid n_trials
#>    <chr>     <int>
#>  1 01          463
#>  2 02          555
#>  3 03          494
#>  4 04          510
#>  5 05          419
#>  6 06          536
#>  7 07          492
#>  8 08          494
#>  9 09          497
#> 10 10          521
#> 11 11          471
#> 12 12          453
#> 13 13          485
#> 14 14          503
#> 15 15          411
#> 16 16          419

# What trial types are there?
if ("trial_type" %in% names(first_subject_events)) {
  trial_types <- first_subject_events %>%
    count(trial_type, name = "frequency")
  print("Trial types in the experiment:")
  print(trial_types)
}
```

## Working with Individual Subjects

The
[`bids_subject()`](https://bbuchsbaum.github.io/bidser/reference/bids_subject.md)
function provides a convenient interface for working with data from a
single subject. It returns a lightweight object with helper functions
that automatically filter data for that subject.

``` r
# Create a subject-specific interface for subject 01
subject_01 <- bids_subject(proj, "01")

# The subject object provides several helper functions:
# Get all functional scans for this subject
sub01_scans <- subject_01$scans()
print(paste("Subject 01 has", length(sub01_scans), "functional scans"))
#> [1] "Subject 01 has 3 functional scans"

# Get event files for this subject
sub01_events <- subject_01$events()
#> Rows: 158 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 156 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 149 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
print(paste("Subject 01 has", length(sub01_events), "event files"))
#> [1] "Subject 01 has 5 event files"

# Read event data for this subject
sub01_event_data <- suppressMessages(subject_01$events())
print("Event data structure for subject 01:")
#> [1] "Event data structure for subject 01:"
print(sub01_event_data)
#> # A tibble: 3 × 5
#> # Groups:   .task, .session, .run, .subid [3]
#>   .subid .session .run  .task                 data              
#>   <chr>  <chr>    <chr> <chr>                 <list>            
#> 1 01     NA       01    balloonanalogrisktask <tibble [158 × 2]>
#> 2 01     NA       02    balloonanalogrisktask <tibble [156 × 2]>
#> 3 01     NA       03    balloonanalogrisktask <tibble [149 × 2]>
```

This approach is particularly useful when you’re doing subject-level
analyses:

``` r
# Example: Analyze data for multiple subjects using the subject interface
subjects_to_analyze <- c("01", "02", "03")

for (subj_id in subjects_to_analyze) {
  # Create subject interface
  subj <- bids_subject(proj, subj_id)
  
  # Get subject-specific data
  scans <- subj$scans()
  events <- subj$events()
  
  cat(sprintf("Subject %s: %d scans, %d event files\n", 
              subj_id, length(scans), length(events)))
}
#> Rows: 158 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 156 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 149 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Subject 01: 3 scans, 5 event files
#> Rows: 185 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 184 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 186 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Subject 02: 3 scans, 5 event files
#> Rows: 150 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 169 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 175 Columns: 1
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: " "
#> chr (1): onset duration    trial_type  cash_demean control_pumps_demean    explode_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Subject 03: 3 scans, 5 event files
```

The subject interface makes it easy to write analysis pipelines that
iterate over subjects without manually constructing filters:

``` r
# Compare trial counts across subjects using the subject interface
subject_trial_summary <- tibble(
  subject = character(),
  n_trials = numeric(),
  n_scans = numeric()
)

for (subj_id in participants(proj)[1:3]) {  # Just first 3 subjects for demo
  subj <- bids_subject(proj, subj_id)
  
  # Count trials
  event_data <- suppressMessages(subj$events())
  n_trials <- if (nrow(event_data) > 0) {
    event_data %>% 
      unnest(cols = c(data)) %>% 
      nrow()
  } else {
    0
  }
  
  # Count scans
  n_scans <- length(subj$scans())
  
  # Add to summary
  subject_trial_summary <- bind_rows(
    subject_trial_summary,
    tibble(subject = subj_id, n_trials = n_trials, n_scans = n_scans)
  )
}

print("Trial and scan summary by subject:")
#> [1] "Trial and scan summary by subject:"
print(subject_trial_summary)
#> # A tibble: 3 × 3
#>   subject n_trials n_scans
#>   <chr>      <dbl>   <dbl>
#> 1 01           463       3
#> 2 02           555       3
#> 3 03           494       3
```

## Advanced Querying

### Custom File Searches

The
[`search_files()`](https://bbuchsbaum.github.io/bidser/reference/search_files.md)
function is very flexible for custom queries:

``` r
# Find all JSON sidecar files
json_files <- search_files(proj, regex = "\\.json$")
print(paste("Found", length(json_files), "JSON files"))
#> [1] "Found 0 JSON files"

# Find files for specific runs
run1_files <- search_files(proj, regex = "bold", run = "01")
print(paste("Found", length(run1_files), "files from run 01"))
#> [1] "Found 16 files from run 01"

# Complex pattern matching
# Find T1w files for subjects 01-05
t1w_subset <- search_files(proj, regex = "T1w", subid = "0[1-5]")
print(paste("Found", length(t1w_subset), "T1w files for subjects 01-05"))
#> [1] "Found 5 T1w files for subjects 01-05"
```

### Getting Full File Paths

Sometimes you need the complete file paths for analysis:

``` r
# Get full paths to functional scans for analysis
full_paths <- func_scans(proj, subid = "01", full_path = TRUE)
print("Full paths to subject 01's functional scans:")
#> [1] "Full paths to subject 01's functional scans:"
print(full_paths)
#> [1] "/tmp/Rtmpk3HtqF/bids_example_ds001/sub-01/func/sub-01_task-balloonanalogrisktask_run-01_bold.nii.gz"
#> [2] "/tmp/Rtmpk3HtqF/bids_example_ds001/sub-01/func/sub-01_task-balloonanalogrisktask_run-02_bold.nii.gz"
#> [3] "/tmp/Rtmpk3HtqF/bids_example_ds001/sub-01/func/sub-01_task-balloonanalogrisktask_run-03_bold.nii.gz"

# Check that files actually exist
if (length(full_paths) > 0) {
  files_exist <- file.exists(full_paths)
  print(paste("All files exist:", all(files_exist)))
}
#> [1] "All files exist: TRUE"
```

## Next Steps

This quickstart covered the basic functionality of bidser for querying
BIDS datasets. For more advanced usage, see:

- **fMRIPrep integration**: Working with preprocessed derivatives
- **Data loading**: Reading neuroimaging data with `neurobase` or
  `RNifti`
- **Confound regression**: Using physiological and motion regressors
- **Group analysis**: Combining data across subjects efficiently

## Reading files produced by FMRIPrep

If you have processed a dataset with FMRIPrep, `bidser` can be used to
read in many of the resultant derivative files. If a project has an
FMRIPrep derivatives folder, then we can read in the BIDS hierarchy plus
derivatives as follows:

``` r
# Try to get a derivatives dataset - note this may not be available
tryCatch({
  # Some BIDS examples may have derivatives, or we can demonstrate with empty structure
  deriv_path <- get_example_bids_dataset("ds000001-fmriprep")
  proj_deriv <- bids_project(deriv_path, fmriprep=TRUE)
  
  print(proj_deriv)
  
  # Now we can access various derivative files with convenience functions.
  # For example, to read in "preproc" scans we can use the `preproc_scans` function.
  pscans <- preproc_scans(proj_deriv)
  if (!is.null(pscans) && length(pscans) > 0) {
    print(head(as.character(pscans)))
  } else {
    message("No preprocessed scans found in this example dataset")
  }
  
  # Clean up derivatives dataset
  unlink(deriv_path, recursive=TRUE)
  
}, error = function(e) {
  message("Derivatives example not available: ", e$message)
  message("This is normal - not all BIDS examples include fMRIPrep derivatives.")
})
#> Derivatives example not available: participants.tsv is missing
#> This is normal - not all BIDS examples include fMRIPrep derivatives.
```
