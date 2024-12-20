#' Read sidecar JSON files and return metadata as a tidy tibble
#'
#' This function searches for JSON sidecar files matching the given criteria (subject, task, run, session),
#' reads the JSON content, and converts all top-level fields into columns of a tibble. Each file's metadata
#' becomes one row in the returned tibble.
#'
#' @param x A \code{bids_project} object.
#' @param subid A regex for matching subject IDs. Default is `".*"`.
#' @param task A regex for matching tasks. Default is `".*"`.
#' @param run A regex for matching runs. Default is `".*"`.
#' @param session A regex for matching sessions. Default is `".*"`.
#' @param modality A regex for matching modality (e.g. "bold"). Default is `"bold"`.
#' @param full_path If TRUE, return full file paths in the `file` column. Default is TRUE.
#' @param ... Additional arguments passed to `search_files()`.
#'
#' @return A tibble with one row per JSON file. Columns include `file` (the JSON file path),
#'         `.subid`, `.session`, `.task`, `.run` (extracted from filename), and all top-level keys from the JSON.
#'         If no files are found, returns an empty tibble.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows mutate
#' @importFrom jsonlite read_json
#' @importFrom stringr str_match
#' @export
read_sidecar_jsons <- function(x, subid=".*", task=".*", run=".*", session=".*", modality="bold", full_path=TRUE, ...) {
  # Find all JSON sidecar files (assumed to end with .json)
  # and match given criteria:
  json_files <- search_files(x, regex="\\.json$", full_path=full_path, strict=TRUE,
                             subid=subid, task=task, run=run, session=session, modality=modality, ...)
  
  if (is.null(json_files) || length(json_files) == 0) {
    message("No matching JSON sidecar files found.")
    return(tibble::tibble())
  }
  
  parse_metadata <- function(fn) {
    bname <- basename(fn)
    # Extract metadata from filename
    subid_val <- stringr::str_match(bname, "sub-([A-Za-z0-9]+)")[,2]
    session_val <- stringr::str_match(bname, "ses-([A-Za-z0-9]+)")[,2]
    task_val <- stringr::str_match(bname, "task-([A-Za-z0-9]+)")[,2]
    run_val <- stringr::str_match(bname, "run-([0-9]+)")[,2]
    
    # Read the JSON
    jdata <- tryCatch({
      jsonlite::read_json(fn, simplifyVector = TRUE)
    }, error=function(e) {
      warning("Failed to read JSON: ", fn, " - ", e$message)
      return(NULL)
    })
    if (is.null(jdata)) return(NULL)
    
    # Convert JSON named list into a one-row tibble
    meta_tibble <- as.data.frame(jdata, stringsAsFactors = FALSE)
    if (nrow(meta_tibble) == 0) {
      # If empty, just return a row of NAs
      meta_tibble <- tibble::tibble()
    }
    meta_tibble <- tibble::as_tibble(meta_tibble)
    
    # Add identifying columns
    meta_tibble <- meta_tibble %>%
      dplyr::mutate(.subid = subid_val,
                    .session = session_val,
                    .task = task_val,
                    .run = run_val,
                    file = fn)
    
    meta_tibble
  }
  
  df_list <- lapply(json_files, parse_metadata)
  df_list <- df_list[!sapply(df_list, is.null)]
  
  if (length(df_list) == 0) {
    message("No valid JSON files could be read.")
    return(tibble::tibble())
  }
  
  dplyr::bind_rows(df_list)
}


#' Get Repetition Time (TR) from a sidecar JSON
#'
#' This function attempts to find and return the repetition time (TR) for a given subject, task, and run
#' (and optionally session) by locating the associated BOLD sidecar JSON file and extracting the 
#' 'RepetitionTime' field. If not found, returns NA.
#'
#' @param x A \code{bids_project} object.
#' @param subid Subject ID (exact or regex).
#' @param task Task name (exact or regex).
#' @param run Run number (exact or regex). Default is ".*" to allow flexible matching.
#' @param session Session ID (exact or regex). Default is ".*".
#' @param ... Additional arguments passed to `read_sidecar_jsons()`.
#'
#' @return A numeric value representing the RepetitionTime in seconds, or NA if not found.
#'
#' @export
get_repetition_time <- function(x, subid, task, run=".*", session=".*", ...) {
  # Load sidecar JSONs for matching subid, task, run, session with a 'bold' modality by default
  sidecars <- read_sidecar_jsons(x, subid=subid, task=task, run=run, session=session, modality="bold", ...)
  
  if (nrow(sidecars) == 0) {
    message("No matching sidecar JSON file found for the specified criteria.")
    return(NA_real_)
  }
  
  # If multiple files match, just take the first (or implement more logic if needed)
  # TR is usually consistent per run.
  # `RepetitionTime` is the BIDS key for TR in seconds.
  tr_val <- sidecars$RepetitionTime[1]
  if (is.null(tr_val) || is.na(tr_val)) {
    # Not found
    return(NA_real_)
  } else {
    return(as.numeric(tr_val))
  }
}