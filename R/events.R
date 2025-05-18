#' Retrieve event files from a BIDS project
#' 
#' Finds event files matching the given subject, task, run, and session criteria.
#'
#' @param x A \code{bids_project} object.
#' @param subid Regex pattern to match subject IDs. Default is ".*" (all subjects).
#' @param task Regex pattern to match tasks. Default is ".*" (all tasks).
#' @param run Regex pattern to match runs. Default is ".*" (all runs).
#' @param session Regex pattern to match sessions. Default is ".*" (all sessions).
#' @param full_path If \code{TRUE}, return full paths of files. Otherwise, return relative paths.
#' @param ... Additional arguments passed on to \code{search_files}.
#' @return A character vector of file paths to event files. If no matching files are found, returns an empty character vector.
#' @rdname event_files-method
#' @export
#' @examples
#' \donttest{
#' # Get event files for a specific subject and task
#' x <- bids_project(system.file("extdata/ds001", package="bidser"))
#' files <- event_files(x, subid="01", task="balloonanalogrisktask")
#' }
event_files.bids_project <- function(x, subid=".*", task=".*", run=".*", session=".*", full_path=TRUE, ...) {
  # Validate input
  if (!inherits(x, "bids_project")) {
    stop("`x` must be a `bids_project` object.")
  }
  
  # Use search_files to find event files
  tryCatch({
    search_files(
      x,
      regex = "events\\.tsv$",  # Match files ending with events.tsv
      subid = subid,
      task = task,
      session = session,
      run = run,
      full_path = full_path,
      strict = TRUE,  # Require that all queried keys exist in matched files
      ...
    )
  }, error = function(e) {
    warning("Error searching for event files: ", e$message)
    character(0)  # Return empty character vector on error
  })
}


#' Read event files from a BIDS project
#'
#' Reads and nests event files for given subjects and tasks from a \code{bids_project} object.
#' Returns a nested tibble with event data grouped by task, session, run, and subject. Event files
#' typically contain trial-by-trial information for task-based fMRI data, including onset times,
#' durations, trial types, and other task-specific variables.
#'
#' @param x A \code{bids_project} object.
#' @param subid Regex pattern to match subject IDs. Default is ".*" (all subjects).
#' @param task Regex pattern to match tasks. Default is ".*" (all tasks).
#' @param run Regex pattern to match runs. Default is ".*" (all runs).
#' @param session Regex pattern to match sessions. Default is ".*" (all sessions).
#' @param ... Additional arguments passed to \code{event_files}.
#'
#' @return A nested tibble with columns:
#'   - `.task`: Task name
#'   - `.session`: Session ID (if present)
#'   - `.run`: Run number
#'   - `.subid`: Subject ID
#'   - `data`: A nested tibble containing the event data with columns:
#'     - `onset`: Event onset time in seconds
#'     - `duration`: Event duration in seconds
#'     - Additional task-specific columns (e.g., trial type, response, accuracy)
#'   If no matching data is found, returns an empty tibble with appropriate columns.
#'   Run and session identifiers are parsed from filenames using \code{func_parser()}.
#'
#' @importFrom dplyr mutate group_by bind_rows %>% filter
#' @importFrom tidyr nest
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect
#' @importFrom rlang .data
#' @importFrom readr read_tsv
#' @examples
#' # Create a BIDS project object
#' proj <- bids_project(system.file("extdata/ds001", package="bidser"))
#'
#' # Read all event files
#' all_events <- read_events(proj)
#'
#' # Read events for a specific subject and task
#' sub01_events <- read_events(proj, 
#'                           subid="01", 
#'                           task="balloonanalogrisktask")
#'
#' # Read events for multiple subjects and a specific run
#' multi_sub_events <- read_events(proj, 
#'                               subid="0[1-3]", 
#'                               run="01")
#'
#' # Access nested data for analysis
#' if (nrow(sub01_events) > 0) {
#'   # Get first subject's data
#'   first_sub_data <- sub01_events$data[[1]]
#'   
#'   # Calculate mean trial duration
#'   mean_duration <- mean(first_sub_data$duration)
#' }
#' @export
read_events.bids_project <- function(x, subid=".*", task=".*", run=".*", session=".*", ...) {
  # Validate input
  if (!inherits(x, "bids_project")) {
    stop("`x` must be a `bids_project` object.")
  }
  
  # Create empty result tibble with correct structure
  empty_result <- tibble::tibble(
    .task = character(0),
    .session = character(0),
    .run = character(0),
    .subid = character(0),
    data = list()
  )
  
  # Get matching participants
  participants_vec <- participants(x)
  if (length(participants_vec) == 0) {
    warning("No participants found in the BIDS project.")
    return(empty_result)
  }
  
  p_idx <- grep(subid, participants_vec)
  if (length(p_idx) == 0) {
    warning("No matching participants found for 'subid' pattern: ", subid)
    return(empty_result)
  }
  sids <- participants_vec[p_idx]
  
  # Get matching tasks
  task_vec <- tasks(x)
  if (length(task_vec) == 0) {
    warning("No tasks found in the BIDS project.")
    return(empty_result)
  }
  
  t_idx <- grep(task, task_vec)
  if (length(t_idx) == 0) {
    warning("No matching tasks found for 'task' pattern: ", task)
    return(empty_result)
  }
  selected_tasks <- task_vec[t_idx]
  
  # Parser for extracting run info
  p <- func_parser()
  
  # Process each task and subject combination
  results <- vector("list", length(selected_tasks))
  
  for (i in seq_along(selected_tasks)) {
    tk <- selected_tasks[i]
    task_results <- vector("list", length(sids))
    
    for (j in seq_along(sids)) {
      sid <- sids[j]
      
      # Get event files for this subject and task
      evs <- tryCatch({
        event_files(x, subid = as.character(sid), task = tk, run = run,
                    session = session)
      }, error = function(e) {
        warning("Error retrieving event files for subject ", sid, " and task ", tk, ": ", e$message)
        character(0)
      })
      
      if (length(evs) == 0) {
        # No event files found for this subject and task
        task_results[[j]] <- NULL
        next
      }
      
      # Extract run and session information from filenames
      runs <- character(length(evs))
      sessions <- character(length(evs))
      for (k in seq_along(evs)) {
        parsed <- tryCatch({
          parse(p, basename(evs[k]))
        }, error = function(e) {
          warning("Failed to parse filename: ", basename(evs[k]), " - ", e$message)
          NULL
        })

        runs[k] <- if (!is.null(parsed) && !is.null(parsed$result$run)) {
          parsed$result$run
        } else {
          NA_character_
        }
        sessions[k] <- if (!is.null(parsed) && !is.null(parsed$result$session)) {
          parsed$result$session
        } else {
          NA_character_
        }
      }
      
      # Read event files
      event_data <- vector("list", length(evs))
      for (k in seq_along(evs)) {
        df <- tryCatch({
          readr::read_tsv(evs[k], na = c("n/a", "NA", "N/A", ""))
        }, error = function(e) {
          warning("Failed to read event file: ", evs[k], " - ", e$message)
          NULL
        })
        
        if (!is.null(df)) {
          # Add metadata columns
          event_data[[k]] <- dplyr::mutate(df,
                                          .subid = sid,
                                          .session = sessions[k],
                                          .run = runs[k],
                                          .file = evs[k])
        }
      }
      
      # Combine all runs for this subject
      combined_data <- dplyr::bind_rows(event_data)
      if (nrow(combined_data) > 0) {
        task_results[[j]] <- combined_data
      }
    }
    
    # Combine results for this task
    task_combined <- dplyr::bind_rows(task_results)
    if (nrow(task_combined) > 0) {
      results[[i]] <- task_combined %>% 
        dplyr::mutate(.task = tk) %>%
        dplyr::group_by(.data$.task, .data$.session, .data$.run, .data$.subid) %>%
        tidyr::nest()
    }
  }
  
  # Combine results across all selected tasks
  final_result <- dplyr::bind_rows(results)
  
  # If no data returned at all, return empty tibble with correct structure
  if (nrow(final_result) == 0) {
    message("No event data found for the given selection of subjects and tasks.")
    return(empty_result)
  }
  
  final_result
}