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
#' }
#' @export
#' @rdname bids_subject
#' @examples
#' proj <- bids_project(system.file("extdata/ds001", package="bidser"))
#' subj <- bids_subject(proj, "01")
#' subj$events()
#' subj$scans()
bids_subject.bids_project <- function(x, subid, ...) {
  if (!inherits(x, "bids_project")) {
    stop("`x` must be a `bids_project` object.")
  }
  sid <- as.character(subid)
  if (!startsWith(sid, "sub-")) {
    sid <- paste0("sub-", sid)
  }
  if (!(sid %in% participants(x))) {
    stop("Subject not found: ", sid)
  }
  list(
    events = function(...) read_events(x, subid = sid, ...),
    scans = function(...) func_scans(x, subid = sid, ...),
    confounds = function(...) read_confounds(x, subid = sid, ...),
    preproc_scans = function(...) preproc_scans(x, subid = sid, ...)
  )
}
