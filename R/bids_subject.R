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
#' @return A list containing helper functions:
#' \itemize{
#'   \item{\code{events}:}{Read event files via [read_events()].}
#'   \item{\code{scans}:}{Retrieve functional scan paths via [func_scans()].}
#'   \item{\code{confounds}:}{Read confound tables with [read_confounds()].}
#'   \item{\code{preproc_scans}:}{Retrieve preprocessed scan paths with [preproc_scans()].}
#'   \item{\code{brain_mask}:}{Create a brain mask via [brain_mask()].}
#' }
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
#' # Extract a single subject from a BIDS project
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path)
#'   
#'   # Extract subject 01
#'   sub01 <- bids_subject(proj, "01")
#'   
#'   # Check subject data
#'   print(participants(sub01))
#'   
#'   # Clean up
#'   unlink(ds001_path, recursive=TRUE)
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
    scans = function(...) func_scans(x, subid = plain_subid, ...),
    confounds = function(...) read_confounds(x, subid = plain_subid, ...),
    preproc_scans = function(...) preproc_scans(x, subid = plain_subid, ...),
    brain_mask = function(...) brain_mask(x, subid = plain_subid, ...)
  )
}
