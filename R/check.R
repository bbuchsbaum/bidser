#' Check Functional Scans in a BIDS Project
#'
#' This function performs a comprehensive inspection of functional scans within a BIDS project,
#' providing detailed summaries of scan counts and file sizes per subject and task. It helps
#' identify potential issues such as missing scans, inconsistent file sizes, or unexpected
#' variations in the data.
#'
#' @param x A \code{bids_project} object created by \code{bids_project()}.
#'
#' @return A list containing:
#'   - `scans`: A tibble with details of all functional scans, including:
#'     - Subject ID
#'     - Task name
#'     - Run number
#'     - File size
#'     - Full file path
#'   - `tasklist`: A vector of unique tasks found in the project
#'   - `scans_per_subject`: A summary tibble showing the number of scans per subject
#'   
#'   If multiple tasks are present, also includes:
#'   - `scans_per_task`: Summary of scan counts by task
#'   - `scans_per_task_subject`: Summary of scan counts by subject and task
#'   - `size_per_task`: Tibble with file size statistics by task
#'   
#'   If only one task is present:
#'   - `size_per_subject`: Tibble with file size statistics by subject
#'
#' @examples
#' # Create a BIDS project object
#' proj <- bids_project(system.file("extdata/ds001", package="bidser"))
#'
#' # Check functional scans
#' scan_check <- check_func_scans(proj)
#'
#' # View available tasks
#' print(scan_check$tasklist)
#'
#' # Check scan counts per subject
#' print(scan_check$scans_per_subject)
#'
#' # Example with multiple tasks
#' ds007 <- bids_project(system.file("extdata/ds007", package="bidser"))
#' multi_check <- check_func_scans(ds007)
#'
#' # View scan distribution across tasks
#' print(multi_check$scans_per_task)
#'
#' # Check for potential issues
#' if (nrow(multi_check$scans) > 0) {
#'   # Look for subjects with fewer scans than expected
#'   expected_scans <- 4  # Example: expecting 4 scans per subject
#'   missing <- multi_check$scans_per_subject[
#'     multi_check$scans_per_subject$n < expected_scans,
#'   ]
#'   if (nrow(missing) > 0) {
#'     print("Subjects with missing scans:")
#'     print(missing)
#'   }
#' }
#'
#' @importFrom fs file_size
#' @importFrom dplyr group_by summarize mutate select everything
#' @importFrom dplyr bind_rows tibble filter
#' @importFrom tidyr unnest
#' @importFrom stringr str_detect
#' @importFrom magrittr %>%
#' @export
check_func_scans <- function(x) {
  if (!inherits(x, "bids_project")) {
    stop("`x` must be a `bids_project` object.")
  }
  
  # Retrieve functional scans and task list
  fscans <- func_scans(x)
  tasklist <- tasks(x)
  
  if (length(fscans) == 0) {
    warning("No functional scans found in this project.")
    return(list(
      scans = tibble::tibble(),
      tasklist = character(0),
      scans_per_subject = tibble::tibble()
    ))
  }
  
  # For each scan file, encode and gather metadata
  ret <- lapply(fscans, function(fn) {
    enc <- encode(basename(fn))
    sz <- fs::file_size(fn)
    if (is.null(enc)) {
      warning("Could not encode file: ", fn)
      dplyr::tibble(file = fn, size = sz)
    } else {
      dplyr::as_tibble(enc) %>%
        dplyr::mutate(file = basename(fn), size = sz) %>%
        dplyr::select(file, size, dplyr::everything())
    }
  }) %>% dplyr::bind_rows()
  
  # Summaries
  scans_per_subject <- ret %>%
    dplyr::group_by(subid) %>%
    dplyr::summarize(nscans = dplyr::n(), .groups = "drop")
  
  size_per_subject <- ret %>%
    dplyr::group_by(subid) %>%
    dplyr::mutate(size_delta = size - median(size)) %>%
    dplyr::ungroup()
  
  if (length(tasklist) > 1) {
    scans_per_task <- ret %>%
      dplyr::group_by(task) %>%
      dplyr::summarize(nscans = dplyr::n(), .groups = "drop")
    
    scans_per_task_subject <- ret %>%
      dplyr::group_by(subid, task) %>%
      dplyr::summarize(nscans = dplyr::n(), .groups = "drop")
    
    size_per_task <- ret %>%
      dplyr::group_by(task) %>%
      dplyr::mutate(size_delta = size - median(size)) %>%
      dplyr::ungroup()
    
    out <- list(
      scans = ret,
      tasklist = tasklist,
      scans_per_subject = scans_per_subject,
      scans_per_task = scans_per_task,
      scans_per_task_subject = scans_per_task_subject,
      size_per_task = size_per_task
    )
  } else {
    out <- list(
      scans = ret,
      tasklist = tasklist,
      scans_per_subject = scans_per_subject,
      size_per_subject = size_per_subject
    )
  }
  
  class(out) <- c("check", "check_func_scans")
  out
}


#' Find File Pairs in a BIDS Project
#'
#' This function matches pairs of related files (e.g., BOLD and event files) in a BIDS project,
#' returning a tibble with matched filenames. It's useful for verifying that corresponding files
#' exist for each subject and task, such as ensuring every BOLD file has an associated events file.
#'
#' @param x A \code{bids_project} object.
#' @param pair A character string specifying which pair of files to match. Currently supported:
#'   - "bold-events": matches BOLD files with event files
#'   - "preproc-events": matches preprocessed BOLD files with event files
#' @param task A regex pattern to filter tasks. Default is ".*" (no filter).
#' @param matchon A character vector of keys to match on, usually c("run", "task").
#' @param ... Additional arguments passed to internal functions.
#'
#' @return A tibble with columns:
#'   - `subid`: The subject ID
#'   - `task`: The task name
#'   - `[type1]`: The name of the first file type (e.g., "bold" or "preproc")
#'   - `[type2]`: The matched file of the second type (e.g., "events"), or `NA` if no match found
#'   - Additional columns for matched metadata (e.g., run, session)
#'
#' @examples
#' # Create a BIDS project object
#' proj <- bids_project(system.file("extdata/ds001", package="bidser"))
#'
#' # Match BOLD files with their corresponding event files
#' bold_pairs <- file_pairs(proj, pair="bold-events")
#'
#' # Check pairs for a specific task
#' task_pairs <- file_pairs(proj, 
#'                         pair="bold-events",
#'                         task="balloonanalogrisktask")
#'
#' \dontrun{
#' # Create a project with preprocessed data
#' prep_proj <- bids_project(system.file("extdata/phoneme_stripped", package="bidser"),
#'                          fmriprep=TRUE)
#'
#' # Match preprocessed files with event files
#' preproc_pairs <- file_pairs(prep_proj, pair="preproc-events")
#'
#' # Check for missing pairs
#' missing_pairs <- preproc_pairs[is.na(preproc_pairs$events), ]
#' }
#'
#' @importFrom dplyr filter mutate tibble bind_rows group_by summarize
#' @importFrom assertthat assert_that
#' @importFrom stringr str_detect str_match
#' @importFrom stringdist stringdistmatrix
#' @importFrom rlang sym
#' @export
file_pairs <- function(x, pair=c("bold-events", "preproc-events"), task=".*", matchon=c("run", "task"), ...) {
  assertthat::assert_that(inherits(x, "bids_project"))
  
  pair <- match.arg(pair)
  sids <- participants(x)
  
  # Extract types from pair
  parts <- strsplit(pair, "-")[[1]]
  type1 <- parts[1]
  type2 <- parts[2]
  
  # Determine appropriate regex for files based on pair
  if (pair == "bold-events") {
    # Bold is typically associated with .nii or .nii.gz files
    # Events with .tsv
    regex_mod1 <- "(nii|nii\\.gz)$"
    regex_mod2 <- "events\\.tsv$"
  } else if (pair == "preproc-events") {
    regex_mod1 <- "preproc\\.nii(\\.gz)*$"
    regex_mod2 <- "events\\.tsv$"
  } else {
    stop("Unsupported pair: ", pair)
  }
  
  results <- lapply(sids, function(s) {
    # Filter for type1 files
    df1 <- dplyr::filter(x$tbl,
                         subid == s,
                         modality == type1,
                         stringr::str_detect(task, task))
    df1 <- df1[grep(regex_mod1, df1$name), , drop=FALSE]
    
    # Filter for type2 files
    df2 <- dplyr::filter(x$tbl,
                         subid == s,
                         modality == type2,
                         stringr::str_detect(task, task))
    df2 <- df2[grep(regex_mod2, df2$name), , drop=FALSE]
    
    # If no type2 matches
    if (nrow(df1) > 0 && nrow(df2) == 0) {
      # Return df1 with NA for type2
      return(dplyr::tibble(
        subid = s,
        task = df1$task,
        !!rlang::sym(type1) := df1$name,
        !!rlang::sym(type2) := NA_character_
      ))
    }
    
    # If no type1 matches
    if (nrow(df2) > 0 && nrow(df1) == 0) {
      # Return a row with no matches for type1
      return(dplyr::tibble(
        subid = s,
        task = NA_character_,
        !!rlang::sym(type1) := character(0),
        !!rlang::sym(type2) := character(0)
      ))
    }
    
    # If both are present
    if (nrow(df1) == 0 && nrow(df2) == 0) {
      # No files found at all for this subject
      return(dplyr::tibble(
        subid = s,
        task = NA_character_,
        !!rlang::sym(type1) := character(0),
        !!rlang::sym(type2) := character(0)
      ))
    }
    
    # Match rows by run/task strings using stringdist
    mat1 <- df1[, matchon, drop=FALSE]
    mat2 <- df2[, matchon, drop=FALSE]
    
    # Create strings to match on
    str1 <- apply(mat1, 1, paste, collapse="-")
    str2 <- apply(mat2, 1, paste, collapse="-")
    
    sdmat <- stringdist::stringdistmatrix(str1, str2)
    
    # For each row in df1, find the best match in df2 with a distance of 0
    s2match <- apply(sdmat, 1, function(z) {
      zmin <- min(z)
      if (zmin == 0) {
        df2$name[which.min(z)]
      } else {
        NA_character_
      }
    })
    
    dplyr::tibble(
      subid = s,
      task = df1$task,
      !!rlang::sym(type1) := df1$name,
      !!rlang::sym(type2) := s2match
    )
  })
  
  dplyr::bind_rows(results)
}

