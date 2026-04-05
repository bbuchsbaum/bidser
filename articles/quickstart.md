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

``` r
proj
#> BIDS Project Summary 
#> Project Name:  bids_example_ds001 
#> Participants (n):  16 
#> Participants Source:  file 
#> Tasks:  balloonanalogrisktask 
#> Index:  enabled 
#> Image Types:  func, anat 
#> Modalities:  (none) 
#> Keys:  folder, kind, relative_path, run, subid, suffix, task, type
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

Let’s find the most common neuroimaging file types:

``` r
# Find all anatomical T1-weighted images
t1w_files <- query_files(proj, regex = "T1w\\.nii", full_path = FALSE)
head(t1w_files)
#> [1] "sub-01/anat/sub-01_T1w.nii.gz" "sub-02/anat/sub-02_T1w.nii.gz"
#> [3] "sub-03/anat/sub-03_T1w.nii.gz" "sub-04/anat/sub-04_T1w.nii.gz"
#> [5] "sub-05/anat/sub-05_T1w.nii.gz" "sub-06/anat/sub-06_T1w.nii.gz"

# Find all functional BOLD scans
bold_files <- func_scans(proj, full_path = FALSE)
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

cat("Subject 01:", length(sub01_scans), "scans\n")
#> Subject 01: 3 scans
cat("Subject 02:", length(sub02_scans), "scans\n")
#> Subject 02: 3 scans

# Filter by task (ds001 only has one task, but this shows the syntax)
task_scans <- func_scans(proj, task = "balloonanalogrisktask")
cat("Balloon task:", length(task_scans), "scans total\n")
#> Balloon task: 48 scans total

# Combine filters: specific subject AND task
sub01_task_scans <- func_scans(proj, subid = "01", task = "balloonanalogrisktask")
cat("Subject 01, balloon task:", length(sub01_task_scans), "scans\n")
#> Subject 01, balloon task: 3 scans
```

### Working with Multiple Subjects

You can use regular expressions to select multiple subjects at once:

``` r
# Get scans for subjects 01, 02, and 03
first_three_scans <- func_scans(proj, subid = "0[123]")
cat("First 3 subjects:", length(first_three_scans), "scans total\n")
#> First 3 subjects: 9 scans total

# Get scans for all subjects (equivalent to default)
all_scans <- func_scans(proj, subid = ".*")
cat("All subjects:", length(all_scans), "scans total\n")
#> All subjects: 48 scans total
```

## Working with Event Files

Event files describe the experimental paradigm - when stimuli were
presented, what responses occurred, etc. This is crucial for task-based
fMRI analysis.

``` r
# Find all event files
event_file_paths <- event_files(proj)
cat("Found", length(event_file_paths), "event files\n")
#> Found 48 event files

# Read event data into a nested data frame
events_data <- read_events(proj)
events_data
#> # A tibble: 48 × 5
#> # Groups:   .task, .session, .run, .subid [48]
#>    .subid .session .run  .task                 data              
#>    <chr>  <chr>    <chr> <chr>                 <list>            
#>  1 01     NA       01    balloonanalogrisktask <tibble [158 × 9]>
#>  2 01     NA       02    balloonanalogrisktask <tibble [156 × 9]>
#>  3 01     NA       03    balloonanalogrisktask <tibble [149 × 9]>
#>  4 02     NA       01    balloonanalogrisktask <tibble [185 × 9]>
#>  5 02     NA       02    balloonanalogrisktask <tibble [184 × 9]>
#>  6 02     NA       03    balloonanalogrisktask <tibble [186 × 9]>
#>  7 03     NA       01    balloonanalogrisktask <tibble [150 × 9]>
#>  8 03     NA       02    balloonanalogrisktask <tibble [169 × 9]>
#>  9 03     NA       03    balloonanalogrisktask <tibble [175 × 9]>
#> 10 04     NA       01    balloonanalogrisktask <tibble [166 × 9]>
#> # ℹ 38 more rows
```

Let’s explore the event data structure:

``` r
# Unnest events for subject 01
first_subject_events <- events_data %>%
  filter(.subid == "01") %>%
  unnest(cols = c(data))

head(first_subject_events)
#> # A tibble: 6 × 13
#> # Groups:   .task, .session, .run, .subid [1]
#>   .subid .session .run  .task              onset duration trial_type cash_demean
#>   <chr>  <chr>    <chr> <chr>              <dbl>    <dbl> <chr>            <dbl>
#> 1 01     NA       01    balloonanalogris…  0.061    0.772 pumps_dem…          NA
#> 2 01     NA       01    balloonanalogris…  4.96     0.772 pumps_dem…          NA
#> 3 01     NA       01    balloonanalogris…  7.18     0.772 pumps_dem…          NA
#> 4 01     NA       01    balloonanalogris… 10.4      0.772 pumps_dem…          NA
#> 5 01     NA       01    balloonanalogris… 13.4      0.772 pumps_dem…          NA
#> 6 01     NA       01    balloonanalogris… 16.8      0.772 explode_d…          NA
#> # ℹ 5 more variables: control_pumps_demean <dbl>, explode_demean <dbl>,
#> #   pumps_demean <dbl>, response_time <dbl>, .file <chr>
names(first_subject_events)
#>  [1] ".subid"               ".session"             ".run"                
#>  [4] ".task"                "onset"                "duration"            
#>  [7] "trial_type"           "cash_demean"          "control_pumps_demean"
#> [10] "explode_demean"       "pumps_demean"         "response_time"       
#> [13] ".file"
```

### Analyzing Event Data

Let’s do some basic exploration of the experimental design:

``` r
# How many trials per subject?
trial_counts <- events_data %>%
  unnest(cols = c(data)) %>%
  group_by(.subid) %>%
  summarise(n_trials = n(), .groups = "drop")

trial_counts
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
```

## Working with Metadata Sidecars

BIDS stores acquisition metadata in JSON sidecars. `bidser` now supports
both direct sidecar reads and inheritance-aware resolution following the
BIDS inheritance principle.

``` r
# Read sidecar rows directly
direct_sidecars <- read_sidecar(
  proj,
  subid = "01",
  task = "balloonanalogrisktask",
  inherit = FALSE
)

nrow(direct_sidecars)
#> [1] 0
names(direct_sidecars)
#> character(0)
```

If you want the effective metadata for a scan after applying inherited
sidecars from parent locations, use
[`get_metadata()`](https://bbuchsbaum.github.io/bidser/reference/get_metadata.md)
or set `inherit = TRUE` in
[`read_sidecar()`](https://bbuchsbaum.github.io/bidser/reference/read_sidecar.md):

``` r
# Resolve metadata for a specific BOLD file with inheritance
resolved_meta <- get_metadata(proj, bold_files[[1]], inherit = TRUE)

sort(names(resolved_meta))[1:8]
#> [1] "RepetitionTime" "TaskName"       NA               NA              
#> [5] NA               NA               NA               NA
resolved_meta$RepetitionTime
#> [1] 2

# Inheritance-aware sidecar table
inherited_sidecars <- read_sidecar(
  proj,
  subid = "01",
  task = "balloonanalogrisktask",
  inherit = TRUE
)

if (nrow(inherited_sidecars) > 0) {
  inherited_sidecars %>%
    select(any_of(c("file", "RepetitionTime")))
} else {
  inherited_sidecars
}
#> # A tibble: 0 × 0
```

This is useful when the metadata you need lives in a task- or
dataset-level JSON sidecar instead of the most specific file-level
sidecar.

## Working with Individual Subjects

The
[`bids_subject()`](https://bbuchsbaum.github.io/bidser/reference/bids_subject.md)
function provides a convenient interface for working with data from a
single subject. It returns a lightweight object with helper functions
that automatically filter data for that subject.

``` r
# Create a subject-specific interface for subject 01
subject_01 <- bids_subject(proj, "01")

# Get all functional scans for this subject
sub01_scans <- subject_01$scans()
cat("Subject 01:", length(sub01_scans), "functional scans\n")
#> Subject 01: 3 functional scans

# Get event files for this subject
sub01_events <- subject_01$events()
cat("Subject 01:", length(sub01_events), "event files\n")
#> Subject 01: 5 event files

# Read event data for this subject
sub01_event_data <- subject_01$events()
sub01_event_data
#> # A tibble: 3 × 5
#> # Groups:   .task, .session, .run, .subid [3]
#>   .subid .session .run  .task                 data              
#>   <chr>  <chr>    <chr> <chr>                 <list>            
#> 1 01     NA       01    balloonanalogrisktask <tibble [158 × 9]>
#> 2 01     NA       02    balloonanalogrisktask <tibble [156 × 9]>
#> 3 01     NA       03    balloonanalogrisktask <tibble [149 × 9]>
```

This approach is particularly useful when you’re doing subject-level
analyses:

``` r
subjects_to_analyze <- c("01", "02", "03")

for (subj_id in subjects_to_analyze) {
  subj <- bids_subject(proj, subj_id)
  scans <- subj$scans()
  events <- subj$events()
  cat(sprintf("Subject %s: %d scans, %d event files\n",
              subj_id, length(scans), length(events)))
}
#> Subject 01: 3 scans, 5 event files
#> Subject 02: 3 scans, 5 event files
#> Subject 03: 3 scans, 5 event files
```

The subject interface makes it easy to write analysis pipelines that
iterate over subjects without manually constructing filters:

``` r
subject_trial_summary <- lapply(participants(proj)[1:3], function(subj_id) {
  subj <- bids_subject(proj, subj_id)
  event_data <- subj$events()
  n_trials <- if (nrow(event_data) > 0) {
    event_data %>% unnest(cols = c(data)) %>% nrow()
  } else {
    0
  }
  tibble(subject = subj_id, n_trials = n_trials, n_scans = length(subj$scans()))
}) %>% bind_rows()

subject_trial_summary
#> # A tibble: 3 × 3
#>   subject n_trials n_scans
#>   <chr>      <int>   <int>
#> 1 01           463       3
#> 2 02           555       3
#> 3 03           494       3
```

## Advanced Querying with `query_files()`

[`query_files()`](https://bbuchsbaum.github.io/bidser/reference/query_files.md)
is the primary file-finding API in bidser. It supports exact, regex, and
glob matching modes, scoped searches across raw data and derivatives,
and can return either paths or a tibble with parsed entities.

### Match Modes

``` r
# Exact entity matching -- reproducible, no regex surprises
exact_bold <- query_files(
  proj,
  regex = "bold\\.nii\\.gz$",
  subid = "01",
  task = "balloonanalogrisktask",
  match_mode = "exact"
)
cat("Exact-match BOLD files:", length(exact_bold), "\n")
#> Exact-match BOLD files: 3

# Regex entity matching -- select multiple values with patterns
regex_bold <- query_files(
  proj,
  regex = "bold\\.nii\\.gz$",
  subid = "0[1-3]",
  task = "balloon.*",
  match_mode = "regex"
)
cat("Regex-match BOLD files:", length(regex_bold), "\n")
#> Regex-match BOLD files: 9

# Glob matching -- shell-style wildcards
glob_bold <- query_files(
  proj,
  regex = "bold\\.nii\\.gz$",
  subid = "0*",
  match_mode = "glob"
)
cat("Glob-match BOLD files:", length(glob_bold), "\n")
#> Glob-match BOLD files: 30
```

### Entity Presence, Extension, and Datatype Filters

``` r
# Require the queried entity to actually exist on returned files
task_annotated <- query_files(
  proj,
  regex = "\\.nii\\.gz$",
  task = ".*",
  require_entity = TRUE,
  scope = "raw"
)
cat("Files with an explicit task entity:", length(task_annotated), "\n")
#> Files with an explicit task entity: 48

# Filter by extension and datatype directly
json_files <- query_files(proj, extension = "\\.json$")
cat("JSON files:", length(json_files), "\n")
#> JSON files: 0

func_niftis <- query_files(proj, datatype = "func", extension = "\\.nii\\.gz$")
cat("Functional NIfTIs:", length(func_niftis), "\n")
#> Functional NIfTIs: 48
```

### Tibble Output with Parsed Entities

``` r
# Return a tibble instead of paths -- includes all parsed BIDS entities
bold_tbl <- query_files(
  proj,
  regex = "bold\\.nii\\.gz$",
  subid = "0[1-3]",
  return = "tibble"
)
bold_tbl |> select(path, subid, task, run)
#> # A tibble: 9 × 4
#>   path                                                         subid task  run  
#>   <chr>                                                        <chr> <chr> <chr>
#> 1 sub-01/func/sub-01_task-balloonanalogrisktask_run-01_bold.n… 01    ball… 01   
#> 2 sub-01/func/sub-01_task-balloonanalogrisktask_run-02_bold.n… 01    ball… 02   
#> 3 sub-01/func/sub-01_task-balloonanalogrisktask_run-03_bold.n… 01    ball… 03   
#> 4 sub-02/func/sub-02_task-balloonanalogrisktask_run-01_bold.n… 02    ball… 01   
#> 5 sub-02/func/sub-02_task-balloonanalogrisktask_run-02_bold.n… 02    ball… 02   
#> 6 sub-02/func/sub-02_task-balloonanalogrisktask_run-03_bold.n… 02    ball… 03   
#> 7 sub-03/func/sub-03_task-balloonanalogrisktask_run-01_bold.n… 03    ball… 01   
#> 8 sub-03/func/sub-03_task-balloonanalogrisktask_run-02_bold.n… 03    ball… 02   
#> 9 sub-03/func/sub-03_task-balloonanalogrisktask_run-03_bold.n… 03    ball… 03
```

The tibble is sorted deterministically by subject, session, task, run,
and path.

### Scoped Queries and Derivatives

When derivatives are present, `scope` controls where to search and
`pipeline` selects specific derivative pipelines:

``` r
deriv_path <- get_example_bids_dataset("ds000001-fmriprep")
proj_deriv <- bids_project(deriv_path)

# Search only derivatives from a specific pipeline
prep_bold <- query_files(
  proj_deriv,
  regex = "bold\\.nii\\.gz$",
  desc = "preproc",
  scope = "derivatives",
  pipeline = "fmriprep",
  match_mode = "exact"
)

# Or use the convenience wrapper
deriv_bold <- derivative_files(proj_deriv, pipeline = "fmriprep",
                               regex = "bold\\.nii\\.gz$")

# Search everywhere and get a tibble with scope/pipeline columns
all_bold <- query_files(
  proj_deriv,
  regex = "bold\\.nii\\.gz$",
  scope = "all",
  return = "tibble"
)
```

### Permissive Loading

[`bids_project()`](https://bbuchsbaum.github.io/bidser/reference/bids_project.md)
can handle real-world datasets missing `participants.tsv` – subjects are
inferred from the directory tree:

``` r
proj_relaxed <- bids_project(
  "/path/to/bids",
  strict_participants = FALSE
)

# Check where participant IDs came from
participants(proj_relaxed, as_tibble = TRUE)

# See which derivative pipelines were discovered
derivative_pipelines(proj_relaxed)
```

### Run-Level Variables and Report Data

[`variables_table()`](https://bbuchsbaum.github.io/bidser/reference/variables_table.md)
gives you a run-level tibble that nests scan inventory, events, and
confounds – ready for downstream R workflows:

``` r
vars <- variables_table(
  proj_deriv,
  scope = "all",
  pipeline = "fmriprep"
)

vars[, c(".subid", ".task", ".run", "n_scans", "n_events", "n_confound_rows")]

report <- bids_report(proj_deriv, scope = "all", pipeline = "fmriprep")
report
```

### Full File Paths

When you need absolute paths for analysis tools:

``` r
full_paths <- func_scans(proj, subid = "01", full_path = TRUE)
full_paths
#> [1] "/tmp/RtmpGZbqOe/bids_example_ds001/sub-01/func/sub-01_task-balloonanalogrisktask_run-01_bold.nii.gz"
#> [2] "/tmp/RtmpGZbqOe/bids_example_ds001/sub-01/func/sub-01_task-balloonanalogrisktask_run-02_bold.nii.gz"
#> [3] "/tmp/RtmpGZbqOe/bids_example_ds001/sub-01/func/sub-01_task-balloonanalogrisktask_run-03_bold.nii.gz"

all(file.exists(full_paths))
#> [1] TRUE
```

## Working with fMRIPrep Derivatives

`bidser` automatically discovers derivative pipelines under
`derivatives/`. You can query preprocessed scans, confounds, and masks
through
[`query_files()`](https://bbuchsbaum.github.io/bidser/reference/query_files.md):

``` r
deriv_path <- get_example_bids_dataset("ds000001-fmriprep")
proj_deriv <- bids_project(deriv_path)

# See which pipelines were discovered
derivative_pipelines(proj_deriv)

# Query preprocessed BOLD scans
preproc <- query_files(
  proj_deriv,
  regex = "bold\\.nii\\.gz$",
  desc = "preproc",
  scope = "derivatives",
  pipeline = "fmriprep",
  return = "tibble"
)
head(preproc$path)

# Read confound regressors
conf <- read_confounds(proj_deriv, subid = "01")
```
