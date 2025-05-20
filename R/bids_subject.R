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
#'   \item{\code{events}:}{Read event files via [read_events()]. When
#'         called with `concatenate = TRUE`, returns a tibble (or a list of
#'         tibbles if multiple tasks are loaded) with `.task` and, optionally,
#'         `.run` columns appended to each event table.}
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
#' # Concatenate events across runs with run indicator
#' subj$events(concatenate = TRUE, add_run = TRUE)
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
    events = function(task = ".*", run = ".*", session = ".*",
                     concatenate = FALSE, add_run = FALSE, ...) {
      evs <- read_events(x, subid = sid, task = task, run = run,
                         session = session, ...)

      if (!concatenate) {
        return(evs)
      }

      if (nrow(evs) == 0) {
        return(tibble::tibble())
      }

      df_list <- lapply(seq_len(nrow(evs)), function(i) {
        d <- evs$data[[i]]
        if (add_run) {
          d$.run <- evs$.run[i]
        }
        d$.task <- evs$.task[i]
        d$.session <- evs$.session[i]
        d$.subid <- evs$.subid[i]
        d
      })

      combined <- dplyr::bind_rows(df_list)

      task_split <- split(combined, combined$.task)

      if (length(task_split) == 1) {
        task_split[[1]]
      } else {
        task_split
      }
    },
    scans = function(...) func_scans(x, subid = sid, ...),
    confounds = function(...) read_confounds(x, subid = sid, ...),
    preproc_scans = function(...) preproc_scans(x, subid = sid, ...)
  )
}
