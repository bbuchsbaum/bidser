#' Parse a file-name into BIDS components
#'
#' This generic function parses a BIDS filename into its component parts.
#' It uses a parser combinator approach to match the filename against known BIDS patterns
#' and extract relevant metadata such as subject ID, session, task, run, and modality.
#'
#' @param x the parser object to use for parsing
#' @param fname the string (filename) to parse
#' @param ... extra args passed to methods
#' @return A parsed representation of the BIDS filename, typically a list with extracted components
#' @export
#' @rdname parse-method
#' @examples
#' # Parse an anatomical file
#' parser <- anat_parser()
#' parse(parser, "sub-01_T1w.nii.gz")
#' 
#' # Parse a functional file
#' parser <- func_parser()
#' parse(parser, "sub-01_task-rest_run-01_bold.nii.gz")
#' 
#' # Use the generic BIDS parser
#' parser <- bids_parser()
#' parse(parser, "sub-01_ses-pre_task-rest_run-01_bold.nii.gz")
parse <- function (x, fname,...) {
  UseMethod("parse", x)
}

#' Encode a string into a BIDS key-value list
#'
#' This function parses a BIDS filename and extracts its components into a key-value list.
#' It understands standard BIDS entities like subject, session, task, run, etc.
#'
#' @param x The string (filename) to encode
#' @param ... Additional arguments passed to methods
#' @return A list of key-value pairs extracted from the filename
#' @export
#' @examples
#' # Encode an anatomical file
#' encode("sub-01_T1w.nii.gz")
#'
#' # Encode a functional file
#' encode("sub-01_task-rest_run-01_bold.nii.gz")
#'
#' # Encode a file with session information
#' encode("sub-01_ses-pre_task-rest_run-01_bold.nii.gz")
encode <- function(x, ...) {
  UseMethod("encode")
}


#' Decode a key-value list into a string
#' 
#' @param x the list to decode
#' @param ... extra args
#' @noRd
decode <- function(x,...) {
  UseMethod("decode")
}


#' Get sessions from a BIDS project
#' 
#' This function retrieves a vector of session IDs from a BIDS project.
#' Sessions in BIDS are typically represented as directories named 'ses-XX'
#' within subject directories. This function extracts and returns the unique
#' session identifiers.
#' 
#' @param x the object to extract sessions from
#' @param ... extra args passed to methods
#' @return A character vector of unique session IDs if the project has sessions,
#'   or NULL if the project does not have sessions
#' @export
#' @rdname sessions-method
#' @examples 
#' \donttest{
#' # Get sessions from a BIDS project
#' tryCatch({
#'   ds007_path <- get_example_bids_dataset("ds007")
#'   proj <- bids_project(ds007_path)
#'   sessions(proj)
#'   
#'   # Clean up (disabled for performance - cached dataset)
#'   # unlink(ds007_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
sessions <- function (x, ...) {
  UseMethod("sessions", x)
}


#' Get tasks from a BIDS project
#' 
#' This function retrieves a sorted vector of unique task names from a BIDS project.
#' Tasks in BIDS are typically represented in filenames with the pattern 'task-XX'.
#' This function extracts and returns the unique task identifiers, filtering out
#' any NULL or NA values.
#' 
#' @param x the object to extract tasks from
#' @param ... extra args passed to methods
#' @return A character vector of unique, sorted task names found in the BIDS project
#' @export
#' @rdname tasks-method
#' @examples 
#' \donttest{
#' # Get tasks from a BIDS project
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path)
#'   tasks(proj)
#'   
#'   # Clean up
#'   unlink(ds001_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
tasks <- function (x, ...) {
  UseMethod("tasks", x)
}

#' Get "flat" representation of BIDS Project
#' 
#' This function returns a flattened (non-hierarchical) representation of a BIDS project
#' formatted as a data frame. It extracts file paths or file names from the BIDS tree
#' structure, filtering for entries that start with "sub-" to focus on subject-level data.
#' 
#' @param x the `bids_project` object
#' @param full_path If TRUE, return full paths to files; if FALSE, return just file names (default: TRUE)
#' @param ... extra args passed to methods
#' 
#' @return A data frame containing either full paths to files (if `full_path=TRUE`) or 
#'   just the file names (if `full_path=FALSE`). Each row represents one file in the BIDS project.
#' @export
#' @rdname flat_list-method
#' @examples 
#' \donttest{
#' # Get flat representation with full paths
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path)
#'   flat_list(proj)
#'   
#'   # Get flat representation with just file names
#'   flat_list(proj, full_path=FALSE)
#'   
#'   # Clean up
#'   unlink(ds001_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
flat_list <- function(x, ...) {
  UseMethod("flat_list", x)
}

#' Get participants from a BIDS project
#' 
#' This function retrieves a vector of unique participant IDs from a BIDS project.
#' It extracts the subject identifiers from the project's data table, filtering out
#' any NA values. Participant IDs in BIDS typically follow the format 'sub-XX'.
#' 
#' @param x the `bids_project` object
#' @param ... extra args passed to methods
#' 
#' @return A character vector of unique participant IDs found in the BIDS project.
#'   If no participants are found or the 'subid' column doesn't exist in the project's
#'   data table, returns an empty character vector.
#' @export
#' @rdname participants-method
#' @examples 
#' \donttest{
#' # Get participants from a BIDS project
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path)
#'   participants(proj)
#'   
#'   # Clean up
#'   unlink(ds001_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
participants <- function (x, ...) {
  UseMethod("participants", x)
}

#' Get event files from a BIDS project
#' 
#' This function retrieves a vector of event files (events.tsv) from a BIDS project
#' that match specified criteria. Event files in BIDS contain trial information for
#' task-based functional MRI data, including onset times, durations, and trial types.
#' 
#' @param x the `bids_project` object
#' @param ... extra args passed to methods, including:
#'   \itemize{
#'     \item{subid}{Regex to match subject IDs (default: ".*")}
#'     \item{task}{Regex to match tasks (default: ".*")}
#'     \item{run}{Regex to match runs (default: ".*")}
#'     \item{session}{Regex to match sessions (default: ".*")}
#'     \item{full_path}{If TRUE, return full paths of files (default: TRUE)}
#'   }
#' 
#' @return A character vector of file paths to event files matching the specified criteria.
#'   If no matching files are found, returns NULL.
#' @export
#' @rdname event_files-method
#' @examples 
#' \donttest{
#' # Get all event files from a BIDS project
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path)
#'   event_files(proj)
#'   
#'   # Get event files for specific subjects and tasks
#'   if (length(participants(proj)) > 0) {
#'     event_files(proj, subid=participants(proj)[1], task="balloonanalogrisktask")
#'   }
#'   
#'   # Clean up
#'   unlink(ds001_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
event_files <- function (x, ...) {
  UseMethod("event_files", x)
}

#' Get confound files from a BIDS project
#' 
#' This function retrieves a vector of confound files from a BIDS project that match 
#' specified criteria. Confound files in BIDS derivatives (typically from fMRIPrep) 
#' contain nuisance variables that can be used for denoising fMRI data, such as 
#' motion parameters, physiological signals, and other noise components.
#' 
#' @param x the `bids_project` object
#' @param ... extra args passed to methods, including:
#'   \itemize{
#'     \item{subid}{Regex to match subject IDs (default: ".*")}
#'     \item{task}{Regex to match tasks (default: ".*")}
#'     \item{session}{Regex to match sessions (default: ".*")}
#'     \item{nest}{If TRUE, results are nested by subject/session/run (default: TRUE)}
#'   }
#' 
#' @return A character vector of file paths to confound files matching the specified criteria.
#'   If no matching files are found, returns NULL.
#' @export
#' @rdname confound_files-method
#' @examples 
#' \donttest{
#' # Get all confound files from a BIDS project with fMRIPrep derivatives
#' tryCatch({
#'   ds_path <- get_example_bids_dataset("ds000001-fmriprep")
#'   proj <- bids_project(ds_path, fmriprep=TRUE)
#'   conf_files <- confound_files(proj)
#'   
#'   # Get confound files for specific subjects and tasks
#'   confound_files(proj, subid="sub-01", task="balloonanalogrisktask")
#'   
#'   # Clean up
#'   unlink(ds_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
confound_files <- function (x, ...) {
  UseMethod("confound_files", x)
}

#' Read Event Files from a BIDS Project
#'
#' This generic function reads and nests event files from a BIDS project. Event files
#' contain timing information about task events, conditions, and responses during
#' functional MRI scans. The function can filter events by subject and task, and
#' returns a nested tibble for easy data manipulation.
#'
#' @param x The object to read events from (typically a `bids_project`).
#' @param ... Additional arguments passed to methods.
#'
#' @return A nested tibble with columns:
#'   - `.task`: Task name
#'   - `.run`: Run number
#'   - `.subid`: Subject ID
#'   - `data`: Nested column containing the event data
#'   If no matching data is found, returns an empty tibble with appropriate columns.
#'
#' @examples
#' \donttest{
#' # Create a BIDS project
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path)
#'   
#'   # Read all event files
#'   all_events <- read_events(proj)
#'   
#'   # Read events for specific subjects
#'   sub_events <- read_events(proj, subid="0[123]")
#'   
#'   # Read events for a specific task
#'   task_events <- read_events(proj, task="balloonanalogrisktask")
#'   
#'   # Combine multiple filters
#'   filtered_events <- read_events(proj,
#'                                 subid="01",
#'                                 task="balloonanalogrisktask")
#'   
#'   # Access nested data
#'   if (nrow(filtered_events) > 0) {
#'     first_run <- filtered_events$data[[1]]
#'     print(head(first_run))
#'   }
#'   
#'   # Clean up
#'   unlink(ds001_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
#' @export
read_events <- function(x, ...) {
  UseMethod("read_events")
}


#' Read Confound Files from a BIDS Project
#'
#' This function reads in fMRIPrep confound tables for one or more subjects from a
#' BIDS project. Confound files contain nuisance variables that can be used for
#' denoising fMRI data, such as motion parameters, physiological signals, and other
#' noise components. The function can optionally perform PCA reduction on the confounds
#' and return either nested or flat tibbles.
#'
#' @param x The object to read confounds from (typically a `bids_project`).
#' @param ... Additional arguments passed to methods, including:
#'   - `subid`: Regex to match subject IDs (default: ".*")
#'   - `task`: Regex to match tasks (default: ".*")
#'   - `session`: Regex to match sessions (default: ".*")
#'   - `run`: Regex to match runs (default: ".*")
#'   - `cvars`: Character vector of confound variable names to select
#'   - `npcs`: Integer. Perform PCA reduction and return this many PCs
#'   - `perc_var`: Numeric. Perform PCA reduction to retain this percentage of variance
#'   - `nest`: Logical. If TRUE, nests confound tables by subject/session/run (default: TRUE)
#'
#' @return A tibble containing confound data. If `nest=TRUE` (default), returns a
#'   nested tibble with columns for subject, session, run, and a nested `data` column
#'   containing the confound variables. If `nest=FALSE`, returns a flat tibble with
#'   all confound variables. Returns NULL if no matching files are found.
#'
#' @examples
#' \donttest{
#' # Create a BIDS project with fMRIPrep derivatives
#' tryCatch({
#'   ds_path <- get_example_bids_dataset("ds000001-fmriprep")
#'   proj <- bids_project(ds_path, fmriprep=TRUE)
#'   
#'   # Read all confound files
#'   all_conf <- read_confounds(proj)
#'   
#'   # Read confounds for specific subjects and tasks
#'   sub_conf <- read_confounds(proj,
#'                             subid="01",
#'                             task="balloonanalogrisktask")
#'   
#'   # Select specific confound variables
#'   motion_conf <- read_confounds(proj,
#'                                cvars=c("framewise_displacement",
#'                                       "trans_x", "trans_y", "trans_z",
#'                                       "rot_x", "rot_y", "rot_z"))
#'   
#'   # Perform PCA reduction
#'   pca_conf <- read_confounds(proj, npcs=5)
#'   
#'   # Get confounds as a flat tibble
#'   flat_conf <- read_confounds(proj, nest=FALSE)
#'   
#'   # Combine multiple options
#'   custom_conf <- read_confounds(proj,
#'                                subid="01",
#'                                task="balloonanalogrisktask",
#'                                cvars=c("framewise_displacement",
#'                                       "trans_x", "trans_y", "trans_z"),
#'                                npcs=3,
#'                                nest=FALSE)
#'   
#'   # Clean up
#'   unlink(ds_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
#' @export
read_confounds <- function(x, ...) {
  UseMethod("read_confounds")
}



#' Get functional scans from a BIDS project
#'
#' This function extracts functional scan files from a BIDS project that match specified
#' criteria such as subject ID, task name, run number, and session. It can return either
#' full paths or relative paths to the files.
#'
#' @param x A \code{bids_project} object.
#' @param ... Additional arguments passed to methods, including:
#'   - `subid`: Regex pattern to match subject IDs (default: ".*")
#'   - `task`: Regex pattern to match tasks (default: ".*")
#'   - `run`: Regex pattern to match runs (default: ".*")
#'   - `session`: Regex pattern to match sessions (default: ".*")
#'   - `kind`: Type of functional data (default: "bold")
#'   - `full_path`: Whether to return full file paths (default: TRUE)
#'
#' @return A character vector of file paths to functional scans matching the criteria.
#'   Returns NULL if no matching files are found.
#'
#' @examples
#' \donttest{
#' # Create a BIDS project object
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path)
#'   
#'   # Get all functional scans
#'   all_scans <- func_scans(proj)
#'   
#'   # Get scans for specific subjects
#'   if (length(participants(proj)) > 0) {
#'     sub_scans <- func_scans(proj, subid=participants(proj)[1])
#'   }
#'   
#'   # Get scans for a specific task and run
#'   if (length(tasks(proj)) > 0) {
#'     task_scans <- func_scans(proj, task=tasks(proj)[1], run="01")
#'   }
#'   
#'   # Get scans with relative paths
#'   rel_scans <- func_scans(proj, full_path=FALSE)
#'   
#'   # Also try with a dataset that has sessions
#'   ds007_path <- get_example_bids_dataset("ds007")
#'   ds007_proj <- bids_project(ds007_path)
#'   if (length(sessions(ds007_proj)) > 0) {
#'     session_scans <- func_scans(ds007_proj, session=sessions(ds007_proj)[1])
#'   }
#'   
#'   # Clean up
#'   unlink(ds001_path, recursive=TRUE)
#'   unlink(ds007_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
#' @export
func_scans <- function(x, ...) {
  UseMethod("func_scans", x)
}

#' Get preprocessed functional MRI scans
#' 
#' This function retrieves paths to preprocessed functional MRI scans from a BIDS project.
#' It searches for files in the fMRIPrep derivatives directory that match specified criteria,
#' such as subject ID, task, run, and other BIDS metadata. Preprocessed scans are identified
#' by having either 'desc-preproc' or 'kind-preproc' in their filename.
#' 
#' @param x A \code{bids_project} object
#' @param subid Subject ID regex to match specific subjects (default: ".*" for all subjects)
#' @param task Task regex to match specific tasks (default: ".*" for all tasks)
#' @param run Run regex to match specific runs (default: ".*" for all runs)
#' @param session Session regex to match specific sessions (default: ".*" for all sessions)
#' @param variant Preprocessing variant to match (default: NULL, which matches files without a variant)
#' @param space Space regex to match specific spaces (default: ".*" for all spaces)
#' @param modality Image modality to match (default: "bold" for functional MRI)
#' @param kind Kind regex to match specific kinds (default: ".*" for all kinds)
#' @param full_path If TRUE, return full file paths; if FALSE, return paths relative to the project root (default: FALSE)
#' @param ... Additional arguments passed to internal functions
#' 
#' @return A character vector of file paths to preprocessed functional scans matching the criteria.
#'   If no matching files are found, returns NULL.
#' @export
#' @rdname preproc_scans-method
#' @examples
#' # Get all preprocessed scans from a BIDS project with fMRIPrep derivatives
#' \donttest{
#' # Download and load a BIDS project with fMRIPrep derivatives
#' tryCatch({
#'   ds001_deriv_path <- get_example_bids_dataset("ds000001-fmriprep")
#'   proj <- bids_project(ds001_deriv_path, fmriprep=TRUE)
#'   
#'   # Get all preprocessed scans
#'   scans <- preproc_scans(proj)
#'   
#'   # Get preprocessed scans for a specific subject
#'   if (!is.null(scans) && length(scans) > 0) {
#'     sub01_scans <- preproc_scans(proj, subid="01")
#'   }
#'   
#'   # Clean up
#'   unlink(ds001_deriv_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires derivatives dataset: ", e$message)
#' })
#' }
preproc_scans <- function(x, subid = ".*", task = ".*", run = ".*", session = ".*",
                          variant = NULL, space = ".*", modality = "bold", 
                          kind = ".*", full_path = FALSE, ...) {
  UseMethod("preproc_scans", x)
}

#' Create a preprocessing mask from BIDS data
#'
#' @param x A bids_project object
#' @param subid A regular expression pattern to match subject IDs
#' @param thresh Threshold value for mask creation (default: 0.99)
#' @param ... Additional arguments passed to methods
#' @return A logical mask volume
#' @export
#' @examples
#' \donttest{
#' # Download and load a BIDS project with fMRIPrep derivatives
#' tryCatch({
#'   ds001_deriv_path <- get_example_bids_dataset("ds000001-fmriprep")
#'   proj <- bids_project(ds001_deriv_path, fmriprep=TRUE)
#'   mask <- create_preproc_mask(proj, subid=".*")
#'   
#'   # Create mask for single subject
#'   sub01_mask <- create_preproc_mask(proj, subid="01")
#'   
#'   # Clean up
#'   unlink(ds001_deriv_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires derivatives dataset: ", e$message)
#' })
#' }
create_preproc_mask <- function(x, subid, thresh=0.99, ...) {
  UseMethod("create_preproc_mask", x)
}

#' Retrieve a brain mask for a subject
#'
#' This convenience function wraps [create_preproc_mask()] and
#' returns a brain mask volume for a given subject.
#'
#' @param x A bids_project object
#' @param subid A regular expression pattern to match subject IDs
#' @param ... Additional arguments passed to methods
#' @return A logical mask volume
#' @export
#' @rdname brain_mask
#' @examples
#' \donttest{
#' # Download and load a BIDS project with fMRIPrep derivatives
#' tryCatch({
#'   ds001_deriv_path <- get_example_bids_dataset("ds000001-fmriprep")
#'   proj <- bids_project(ds001_deriv_path, fmriprep=TRUE)
#'   mask <- brain_mask(proj, subid="01")
#'   
#'   # Create mask for multiple subjects
#'   multi_mask <- brain_mask(proj, subid=".*")
#'   
#'   # Clean up
#'   unlink(ds001_deriv_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires derivatives dataset: ", e$message)
#' })
#' }
brain_mask <- function(x, subid, ...) {
  UseMethod("brain_mask", x)
}

#' Search files in BIDS structure
#' 
#' This function searches for files in a BIDS project that match a specified pattern and
#' optional key-value criteria. It can be used to find files in both raw data and preprocessed
#' derivatives based on filename patterns and BIDS metadata.
#' 
#' @param x A \code{bids_project} object created by \code{bids_project()}.
#' @param regex A regular expression to match against filenames. Default is ".*" (all files).
#' @param full_path If TRUE, return full file paths. If FALSE, return paths relative to the project root.
#' @param strict If TRUE, require that all queried keys must exist in matched files.
#'        If FALSE, allow matches for files missing queried keys.
#' @param ... Additional key-value pairs to filter files (e.g., subid = "01", task = "wm").
#'        These are matched against the corresponding metadata in the BIDS files.
#' @return A character vector of file paths matching the criteria, or NULL if no matches found.
#' @export
#' @rdname search_files
#' @examples
#' \donttest{
#' # Search for event files in a BIDS dataset  
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path, fmriprep=FALSE)
#'   event_files <- search_files(proj, regex="events\\.tsv$")
#'   
#'   # Search with additional criteria
#'   sub01_files <- search_files(proj, regex="bold\\.nii\\.gz$", subid="01", 
#'                               task="balloonanalogrisktask")
#'   
#'   # Get full paths
#'   full_paths <- search_files(proj, regex="events\\.tsv$", full_path=TRUE)
#'   
#'   # Search with strict matching
#'   strict_matches <- search_files(proj, regex="\\.tsv$", strict=TRUE, 
#'                                  task="balloonanalogrisktask")
#'   
#'   # Clean up
#'   unlink(ds001_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
search_files <- function(x, ...) {
  UseMethod("search_files", x)
}

#' Load All Event Files
#' 
#' @description Searches for and reads event files (`events.tsv`) from a BIDS 
#' project, combining them into a single (potentially nested) tibble.
#' 
#' @param x A BIDS project object.
#' @param subid Regex to match subject IDs (default: ".*")
#' @param task Regex to match tasks (default: ".*")
#' @param run Regex to match runs (default: ".*")
#' @param session Regex to match sessions (default: ".*")
#' @param full_path If TRUE, return full paths of files (default: TRUE)
#' @param ... Additional arguments passed to methods
#' 
#' @return A tibble containing the combined event data.
#' @export
#' @rdname load_all_events-method
#' @examples
#' \donttest{
#' # Example with a bids_project (assuming events exist)
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path)
#'   all_events <- load_all_events(proj)
#'   print(all_events)
#'   
#'   # Load specific subject/task
#'   if (length(participants(proj)) > 0) {
#'     sub01_events <- load_all_events(proj, subid=participants(proj)[1], task="balloonanalogrisktask")
#'     print(sub01_events)
#'   }
#'   
#'   # Clean up
#'   unlink(ds001_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
load_all_events <- function(x, ...) {
  UseMethod("load_all_events")
}

#' Summarize a BIDS dataset
#'
#' @param x A bids_project object
#' @return A list containing summary statistics about the BIDS dataset
#' @export
#' @examples
#' \donttest{
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path)
#'   summary <- bids_summary(proj)
#'   
#'   # Clean up
#'   unlink(ds001_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
bids_summary <- function(x) {
  UseMethod("bids_summary")
}

#' Basic BIDS Compliance Checks
#'
#' @param x A bids_project object
#' @return A list with compliance check results
#' @export
#' @examples
#' \donttest{
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path)
#'   compliance <- bids_check_compliance(proj)
#'   
#'   # Clean up
#'   unlink(ds001_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
bids_check_compliance <- function(x) {
  UseMethod("bids_check_compliance")
}

#' Access a single subject from a BIDS project
#'
#' `bids_subject` returns a lightweight interface with helper functions
#' for retrieving data associated with one subject.
#'
#' @param x A `bids_project` object.
#' @param subid Subject identifier (with or without the `sub-` prefix).
#' @param ... Additional arguments passed to methods.
#' @return A list of helper functions for the subject.
#' @export
#' @rdname bids_subject
#' @examples
#' \donttest{
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path)
#'   subj <- bids_subject(proj, "01")
#'   subj$events()
#'   subj$scans()
#'   
#'   # Clean up
#'   unlink(ds001_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
bids_subject <- function(x, subid, ...) {
  UseMethod("bids_subject")
}
#' @noRd
anomalies <- function(x, ...) {
  UseMethod("anomalies", x)
}

#' Get data matrix from dataset
#' 
#' Extract data matrix from various dataset types
#' 
#' @param x the dataset object
#' @param ... extra args
#' @noRd
get_data_matrix <- function(x, ...) {
  UseMethod("get_data_matrix")
}
