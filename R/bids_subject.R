#' Access data for a single subject
#'
#' `bids_subject` returns a lightweight facade that exposes convenience
#' functions to work with all data associated with one subject within a
#' BIDS project.
#'
#' @param x A `bids_project` object.
#' @param subid Subject identifier. Can be with or without the `sub-` prefix.
#' @param ... Additional arguments (unused).
#'
#' @return A list containing subject-specific helper functions. Each function
#'   automatically filters results for the specified subject. The returned object
#'   contains the following callable functions:
#' \describe{
#'   \item{\code{events(...)}}{
#'     Returns nested tibble with event data for this subject.
#'     Equivalent to \code{read_events(project, subid = "XX", ...)}.
#'     Additional arguments (task, session, run, nest, etc.) can be passed.
#'   }
#'   \item{\code{event_files(...)}}{
#'     Returns character vector of event file paths for this subject.
#'     Equivalent to \code{event_files(project, subid = "XX", ...)}.
#'     Additional arguments (task, session, run, full_path, etc.) can be passed.
#'   }
#'   \item{\code{scans(...)}}{
#'     Returns character vector of functional scan file paths for this subject.
#'     Equivalent to \code{func_scans(project, subid = "XX", ...)}.
#'     Additional arguments (task, session, run, kind, full_path, etc.) can be passed.
#'   }
#'   \item{\code{confounds(...)}}{
#'     Returns confound data for this subject (requires fMRIPrep derivatives).
#'     Equivalent to \code{read_confounds(project, subid = "XX", ...)}.
#'     Additional arguments (task, session, run, cvars, npcs, etc.) can be passed.
#'   }
#'   \item{\code{preproc_scans(...)}}{
#'     Returns preprocessed scan paths for this subject (requires fMRIPrep derivatives).
#'     Equivalent to \code{preproc_scans(project, subid = "XX", ...)}.
#'     Additional arguments (task, session, run, space, variant, etc.) can be passed.
#'   }
#'   \item{\code{brain_mask(...)}}{
#'     Creates brain mask for this subject (requires fMRIPrep derivatives).
#'     Equivalent to \code{brain_mask(project, subid = "XX", ...)}.
#'     Additional arguments (thresh, etc.) can be passed.
#'   }
#' }
#' @export
#' @rdname bids_subject
#' @examples
#' \donttest{
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path)
#'   
#'   # Create subject interface for subject 01
#'   subj <- bids_subject(proj, "01")
#'   
#'   # Get functional scan paths for this subject
#'   scan_paths <- subj$scans()
#'   print(paste("Subject 01 has", length(scan_paths), "functional scans"))
#'   
#'   # Get event file paths for this subject
#'   event_paths <- subj$event_files()
#'   print(paste("Subject 01 has", length(event_paths), "event files"))
#'   
#'   # Read event data for this subject
#'   event_data <- subj$events()
#'   print("Event data structure:")
#'   print(event_data)
#'   
#'   # You can still pass additional filtering arguments
#'   # For example, get only specific tasks:
#'   task_scans <- subj$scans(task = "balloonanalogrisktask")
#'   
#'   # Note: Don't unlink - cached for performance
#'   # unlink(ds001_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
bids_subject.bids_project <- function(x, subid, ...) {
  if (!inherits(x, "bids_project")) {
    stop("`x` must be a `bids_project` object.")
  }
  
  # Get the plain subject ID (without "sub-") for comparison with participants()
  plain_subid <- stringr::str_remove(as.character(subid), "^sub-")
  
  # Internally, we'll use the full subject ID with "sub-" prefix for node access
  full_sid_for_node_access <- ifelse(startsWith(as.character(subid), "sub-"), 
                                   as.character(subid), 
                                   paste0("sub-", plain_subid))

  # Check if the plain_subid exists in the list of participants (which are also plain)
  if (!(plain_subid %in% participants(x))) {
    stop("Subject not found: ", plain_subid, " (derived from input: ", subid, ")")
  }
  
  # The functions below will use the plain_subid for filtering, 
  # as underlying functions like read_events, func_scans expect plain IDs.
  list(
    events = function(...) read_events(x, subid = plain_subid, ...),
    event_files = function(...) event_files(x, subid = plain_subid, ...),
    scans = function(...) func_scans(x, subid = plain_subid, ...),
    confounds = function(...) read_confounds(x, subid = plain_subid, ...),
    preproc_scans = function(...) preproc_scans(x, subid = plain_subid, ...),
    brain_mask = function(...) brain_mask(x, subid = plain_subid, ...)
  )
}

#' Extract a Single Subject from a BIDS Project
#'
#' This function extracts a single subject's data from a BIDS project, creating
#' a new BIDS project object containing only that subject's files and metadata.
#'
#' @param x A \code{bids_project} object.
#' @param subid Character string. The subject ID to extract (without the "sub-" prefix).
#' @param ... Additional arguments (not currently used).
#'
#' @return A new \code{bids_project} object containing only the specified subject's data.
#'   Returns NULL if the subject is not found in the project.
#'
#' @examples
#' \donttest{
#' # Create a subject interface
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path)
#'   
#'   # Create subject interface for subject 01  
#'   subj <- bids_subject(proj, "01")
#'   
#'   # Use the helper functions
#'   scans <- subj$scans()
#'   events <- subj$event_files()
#'   print(paste("Subject 01:", length(scans), "scans,", length(events), "events"))
#'   
#'   # Note: Don't unlink - cached for performance
#'   # unlink(ds001_path, recursive=TRUE)  
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
#'
#' @export
bids_subject <- function(x, subid, ...) {
  if (!inherits(x, "bids_project")) {
    stop("`x` must be a `bids_project` object.")
  }
  
  # Get the plain subject ID (without "sub-") for comparison with participants()
  plain_subid <- stringr::str_remove(as.character(subid), "^sub-")
  
  # Internally, we'll use the full subject ID with "sub-" prefix for node access
  full_sid_for_node_access <- ifelse(startsWith(as.character(subid), "sub-"), 
                                   as.character(subid), 
                                   paste0("sub-", plain_subid))

  # Check if the plain_subid exists in the list of participants (which are also plain)
  if (!(plain_subid %in% participants(x))) {
    stop("Subject not found: ", plain_subid, " (derived from input: ", subid, ")")
  }
  
  # The functions below will use the plain_subid for filtering, 
  # as underlying functions like read_events, func_scans expect plain IDs.
  list(
    events = function(...) read_events(x, subid = plain_subid, ...),
    event_files = function(...) event_files(x, subid = plain_subid, ...),
    scans = function(...) func_scans(x, subid = plain_subid, ...),
    confounds = function(...) read_confounds(x, subid = plain_subid, ...),
    preproc_scans = function(...) preproc_scans(x, subid = plain_subid, ...),
    brain_mask = function(...) brain_mask(x, subid = plain_subid, ...)
  )
}
