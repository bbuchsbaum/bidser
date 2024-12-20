#' Retrieve event files from a BIDS project
#' 
#' Finds event files matching the given subject, task, run, and session criteria.
#'
#' @param x A \code{bids_project} object.
#' @param subid Regex to match subject IDs. Default is ".*" (no filtering).
#' @param task Regex to match tasks. Default is ".*".
#' @param run Regex to match runs. Default is ".*".
#' @param session Regex to match sessions. Default is ".*".
#' @param full_path If \code{TRUE}, return full paths of files. Otherwise, return relative paths.
#' @param ... Additional arguments passed on to \code{search_files}.
#' @return A character vector of file paths.
#' @export
event_files.bids_project <- function(x, subid=".*", task=".*", run=".*", session=".*", full_path=TRUE, ...) {
  search_files(
    x,
    kind = "events",
    subid = subid,
    task = task,
    session = session,
    run = run,
    full_path = full_path,
    ...
  )
}


#' Read event files from a BIDS project
#'
#' Reads and nests event files for given subjects and tasks from a \code{bids_project} object.
#' Returns a nested tibble with event data grouped by task, run, and subject.
#'
#' @param x A \code{bids_project} object.
#' @param subid Regex to match subject IDs. Default is ".*" (no filtering).
#' @param task Regex to match tasks. Default is ".*".
#' @return A nested tibble with columns: \code{.task}, \code{.run}, \code{.subid} and a nested column \code{data} containing the event data.
#' @importFrom dplyr mutate group_by bind_rows %>% 
#' @importFrom tidyr nest
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect
#' @examples
#' # x <- bids_project(system.file("inst/extdata/phoneme_stripped", package="bidser"), fmriprep=TRUE)
#' # events <- read_events(x, subid="sub-01", task="task-phoneme")
#' @export
read_events.bids_project <- function(x, subid = ".*", task = ".*") {
  # Check that x is a bids_project
  if (!inherits(x, "bids_project")) {
    stop("`x` must be a `bids_project` object.")
  }
  
  # Get participants
  participants_vec <- participants(x)
  p_idx <- grep(subid, participants_vec)
  
  if (length(p_idx) == 0) {
    stop("No matching participants found for 'subid' regex: ", subid)
  }
  
  sids <- participants_vec[p_idx]
  
  # Get tasks
  task_vec <- tasks(x)
  t_idx <- grep(task, task_vec)
  
  if (length(t_idx) == 0) {
    stop("No matching tasks found for 'task' regex: ", task)
  }
  
  selected_tasks <- task_vec[t_idx]
  
  # Parser for extracting run info
  p <- func_parser()
  
  # For each task and subject, read in event files, parse runs, and return a nested tibble
  results <- lapply(selected_tasks, function(tk) {
    lapply(sids, function(sid) {
      evs <- event_files(x, subid = as.character(sid), task = tk)
      if (length(evs) > 0) {
        # Extract run information from filenames
        runs <- sapply(evs, function(ev) {
          parsed <- parse(p, basename(ev))
          if (!is.null(parsed) && !is.null(parsed$result$run)) {
            parsed$result$run
          } else {
            NA
          }
        })
        
        # Read event files
        event_data <- lapply(seq_along(evs), function(i) {
          df <- tryCatch({
            read.table(evs[i], header=TRUE, stringsAsFactors=FALSE, na.strings=c("n/a", "NA"))
          }, error = function(e) {
            warning("Failed to read event file: ", evs[i], " - ", e$message)
            return(NULL)
          })
          
          if (!is.null(df)) {
            dplyr::mutate(df, .subid = sid, .run = runs[i])
          } else {
            NULL
          }
        })
        
        # Combine all runs for this subject
        combined_data <- dplyr::bind_rows(event_data)
        combined_data
      } else {
        # No event files for this subject & task
        NULL
      }
    }) %>% 
      dplyr::bind_rows() %>% 
      dplyr::mutate(.task = tk) %>%
      dplyr::group_by(.task, .run, .subid) %>%
      tidyr::nest()
  })
  
  # Combine results across all selected tasks
  final_result <- dplyr::bind_rows(results)
  
  # If no data returned at all
  if (nrow(final_result) == 0) {
    message("No event data found for the given selection of subjects and tasks.")
    return(final_result)
  }
  
  final_result
}