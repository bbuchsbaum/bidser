#' Read sidecar JSON files and return metadata as a tidy tibble
#'
#' This function searches for JSON sidecar files matching the given criteria (subject, task, run, session),
#' reads the JSON content, and converts all top-level fields into columns of a tibble. Each file's metadata
#' becomes one row in the returned tibble. This is particularly useful for extracting metadata about BIDS
#' imaging files, such as acquisition parameters, task descriptions, and other relevant information.
#'
#' @param x A \code{bids_project} object.
#' @param subid A regex for matching subject IDs. Default is `".*"`.
#' @param task A regex for matching tasks. Default is `".*"`.
#' @param run A regex for matching runs. Default is `".*"`.
#' @param session A regex for matching sessions. Default is `".*"`.
#' @param modality A regex for matching modality/kind (e.g. "bold"). Default is `"bold"`.
#'   This is matched against the 'kind' field in parsed BIDS filenames.
#' @param full_path If TRUE, return full file paths in the `file` column. Default is TRUE.
#' @param ... Additional arguments passed to `search_files()`.
#'
#' @return A tibble with one row per JSON file. Columns include:
#'   - `file`: the JSON file path
#'   - `.subid`: subject ID extracted from filename
#'   - `.session`: session ID extracted from filename (if present)
#'   - `.task`: task name extracted from filename (if present)
#'   - `.run`: run number extracted from filename (if present)
#'   - Additional columns for each top-level key in the JSON files
#'   If no files are found, returns an empty tibble.
#'
#' @examples
#' \donttest{
#' # Read all BOLD sidecar files from a BIDS dataset
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path)
#'   metadata <- read_sidecar(proj)
#'   
#'   # Read sidecar files for a specific subject and task
#'   sub01_meta <- read_sidecar(proj, 
#'                             subid="01", 
#'                             task="balloonanalogrisktask")
#'   
#'   # Read sidecar files for anatomical data
#'   anat_meta <- read_sidecar(proj, 
#'                            modality="T1w",
#'                            full_path=FALSE)
#'   
#'   # Clean up
#'   unlink(ds001_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
#'
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows mutate
#' @importFrom jsonlite read_json
#' @importFrom stringr str_match
#' @export
read_sidecar <- function(x, subid=".*", task=".*", run=".*", session=".*", modality="bold", full_path=TRUE, ...) {
  # Find all JSON sidecar files (assumed to end with .json)
  # and match given criteria:
  # Note: We use 'kind' instead of 'modality' because the BIDS parser stores

  # the modality component (e.g. "bold") in the 'kind' field for JSON files
  json_files <- search_files(x, regex="\\.json$", full_path=full_path, strict=TRUE,
                             subid=subid, task=task, run=run, session=session, kind=modality, ...)
  
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
    
    # Convert JSON named list into a one-row tibble.
    # Keep only scalar (length-1) fields as columns; store vector-valued
    # fields (e.g. SliceTiming) as list-columns to avoid differing-row errors.
    scalar <- vapply(jdata, function(v) length(v) == 1L && is.atomic(v), logical(1))
    row <- jdata[scalar]
    vec_fields <- jdata[!scalar]
    meta_tibble <- tibble::as_tibble(row)
    for (nm in names(vec_fields)) {
      meta_tibble[[nm]] <- list(vec_fields[[nm]])
    }
    
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
#' @param ... Additional arguments passed to `read_sidecar()`.
#'
#' @return A numeric value representing the RepetitionTime in seconds, or NA if not found.
#'
#' @examples
#' \donttest{
#' # Download and get TR for a specific subject and task
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path)
#'   
#'   if (length(participants(proj)) > 0 && length(tasks(proj)) > 0) {
#'     tr <- get_repetition_time(proj, 
#'                              subid=participants(proj)[1], 
#'                              task=tasks(proj)[1])
#'     cat("TR:", tr, "seconds\n")
#'   }
#'   
#'   # Try with a dataset that has sessions
#'   ds007_path <- get_example_bids_dataset("ds007")
#'   ds007_proj <- bids_project(ds007_path)
#'   if (length(participants(ds007_proj)) > 0 && length(sessions(ds007_proj)) > 0) {
#'     tr_session <- get_repetition_time(ds007_proj,
#'                                      subid=participants(ds007_proj)[1],
#'                                      session=sessions(ds007_proj)[1])
#'     cat("TR with session:", tr_session, "seconds\n")
#'   }
#'   
#'   # Clean up
#'   unlink(ds001_path, recursive=TRUE)
#'   unlink(ds007_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
#'
#' @export
get_repetition_time <- function(x, subid, task, run=".*", session=".*", ...) {
  # Load sidecar JSONs for matching subid, task, run, session with a 'bold' modality by default
  sidecars <- read_sidecar(x, subid=subid, task=task, run=run, session=session, modality="bold", ...)
  
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


#' Infer TR (Repetition Time) from a BOLD file or sidecar
#'
#' Given a path to a BOLD NIfTI file (`*.nii` or `*.nii.gz`) or its JSON
#' sidecar (`*.json`), this function locates the appropriate sidecar JSON and
#' returns the TR (in seconds). It prefers the JSON `RepetitionTime` field
#' (BIDS-compliant). If that is not available, it falls back to computing TR as
#' the median difference of `VolumeTiming` (if present). Optionally, when the
#' sidecar cannot be found or is missing both fields, the function attempts to
#' read TR from the NIfTI header (pixdim\[4\]) if an appropriate reader is
#' installed.
#'
#' For NIfTI inputs, the JSON sidecar is resolved by replacing the
#' `*.nii`/`*.nii.gz` suffix with `.json` in the same directory. If that file is
#' not found, the function searches the directory for a `.json` file with the
#' same stem (filename without the NIfTI extension).
#'
#' @param x A character path to a BOLD `.nii[.gz]` file or its `.json` sidecar,
#'   or a `bids_project` object.
#' @param ... Additional arguments passed to methods.
#' @return Numeric TR in seconds, or `NA_real_` if it cannot be determined. The
#'   return value includes attributes: `source` (e.g., `json:RepetitionTime`,
#'   `json:VolumeTiming`, `nifti:pixdim4`), `path` (the file used), and
#'   optionally `variable = TRUE` if `VolumeTiming` indicates non-constant TR; a
#'   `unit = "ms->s"` attribute is added if units were auto-converted.
#' @examples
#' tmp_json <- tempfile(fileext = ".json")
#' writeLines('{"RepetitionTime": 2}', tmp_json)
#' infer_tr(tmp_json)
#' unlink(tmp_json)
#'
#' tmp_json2 <- tempfile(fileext = ".json")
#' writeLines('{"VolumeTiming": [0, 2, 4, 6]}', tmp_json2)
#' infer_tr(tmp_json2)
#' unlink(tmp_json2)
#' @export
infer_tr <- function(x, ...) {
  UseMethod("infer_tr")
}

#' @rdname infer_tr
#' @param prefer Preferred source of TR: `"json"` (default) or `"nifti"`.
#' @param fallback If TRUE (default), attempt NIfTI header fallback when JSON is
#'   not available or incomplete.
#' @param coerce_units Unit handling for non-compliant values. `"strict"`
#'   (default) assumes seconds as per BIDS and returns values as-is. `"auto"`
#'   will convert clearly millisecond-like values to seconds (divide by 1000)
#'   and annotate the conversion in the return value's attributes.
#' @param verbose If TRUE, print informative messages when falling back or when
#'   encountering special cases (e.g., SBRef files).
#' @export
infer_tr.character <- function(x,
                               prefer = c("json", "nifti"),
                               fallback = TRUE,
                               coerce_units = c("strict", "auto"),
                               verbose = FALSE,
                               ...) {
  prefer <- match.arg(prefer)
  coerce_units <- match.arg(coerce_units)

  # normalize path
  f <- as.character(x)
  if (length(f) != 1L) {
    stop("infer_tr() expects a single file path.")
  }
  if (!file.exists(f)) {
    stop("File not found: ", f)
  }

  is_json <- grepl("\\.json$", f, ignore.case = TRUE)
  is_nifti <- grepl("\\.nii(\\.gz)?$", f, ignore.case = TRUE)

  # Warn and return NA for SBRef files unless user insists
  if (is_nifti && grepl("_sbref", basename(f), ignore.case = TRUE)) {
    if (verbose) message("SBRef file detected; TR is not applicable.")
    return(NA_real_)
  }

  # Helper to read JSON and extract TR
  read_tr_from_json <- function(jf) {
    if (!file.exists(jf)) return(NA_real_)
    j <- tryCatch(jsonlite::read_json(jf, simplifyVector = TRUE),
                  error = function(e) NULL)
    if (is.null(j)) return(NA_real_)
    src_path <- jf
    # Prefer RepetitionTime
    if (!is.null(j$RepetitionTime) && is.numeric(j$RepetitionTime) && length(j$RepetitionTime) >= 1) {
      tr <- as.numeric(j$RepetitionTime[1])
      if (coerce_units == "auto" && is.finite(tr) && tr > 50 && tr < 10000) {
        # likely milliseconds
        tr <- tr/1000
        attr(tr, "unit") <- "ms->s"
      }
      attr(tr, "source") <- "json:RepetitionTime"
      attr(tr, "path") <- src_path
      return(tr)
    }
    # Fallback: VolumeTiming -> median diff
    if (!is.null(j$VolumeTiming) && is.numeric(j$VolumeTiming) && length(j$VolumeTiming) >= 2) {
      vt <- as.numeric(j$VolumeTiming)
      diffs <- diff(vt)
      if (coerce_units == "auto" && median(diffs, na.rm = TRUE) > 50 && median(diffs, na.rm = TRUE) < 10000) {
        diffs <- diffs/1000
        attr_val <- "ms->s"
      } else {
        attr_val <- NULL
      }
      tr <- as.numeric(stats::median(diffs, na.rm = TRUE))
      attr(tr, "source") <- "json:VolumeTiming"
      attr(tr, "path") <- src_path
      if (!is.null(attr_val)) attr(tr, "unit") <- attr_val
      # flag variable TR if diffs are not constant
      if (any(abs(diffs - tr) > 1e-6, na.rm = TRUE)) attr(tr, "variable") <- TRUE
      return(tr)
    }
    NA_real_
  }

  # Helper to read TR from NIfTI header if RNifti is available
  read_tr_from_nifti <- function(nf) {
    if (!file.exists(nf)) return(NA_real_)
    tr <- NA_real_
    if (requireNamespace("RNifti", quietly = TRUE)) {
      hdr <- tryCatch(RNifti::niftiHeader(nf), error = function(e) NULL)
      if (!is.null(hdr) && !is.null(hdr$pixdim) && length(hdr$pixdim) >= 5) {
        tr <- as.numeric(hdr$pixdim[5])
      }
    }
    if (is.finite(tr)) {
      attr(tr, "source") <- "nifti:pixdim4"
      attr(tr, "path") <- nf
    }
    tr
  }

  # Resolve sidecar if needed
  json_path <- NA_character_
  if (is_json) {
    json_path <- f
  } else if (is_nifti) {
    # first attempt: swap extension
    json_guess <- sub("\\.nii(\\.gz)?$", ".json", f, ignore.case = TRUE)
    if (file.exists(json_guess)) {
      json_path <- json_guess
    } else {
      # search directory for matching stem
      dirp <- dirname(f)
      stem <- sub("\\.nii(\\.gz)?$", "", basename(f), ignore.case = TRUE)
      candidates <- list.files(dirp, pattern = "\\.json$", full.names = TRUE)
      cand_base <- sub("\\.json$", "", basename(candidates), ignore.case = TRUE)
      hit <- which(cand_base == stem)
      if (length(hit) >= 1) {
        json_path <- candidates[hit[1]]
      }
    }
  }

  # Prefer JSON
  if (prefer == "json" && !is.na(json_path) && nzchar(json_path)) {
    tr <- read_tr_from_json(json_path)
    if (is.finite(tr)) return(tr)
    if (!fallback) return(NA_real_)
    if (is_nifti) {
      if (verbose) message("Falling back to NIfTI header for TR.")
      return(read_tr_from_nifti(f))
    }
    return(NA_real_)
  }

  # Otherwise prefer NIfTI or JSON not available
  if (is_nifti && prefer == "nifti") {
    tr <- read_tr_from_nifti(f)
    if (is.finite(tr)) return(tr)
    if (!fallback) return(NA_real_)
    if (!is.na(json_path) && nzchar(json_path)) return(read_tr_from_json(json_path))
    return(NA_real_)
  }

  # If input was JSON, use it directly
  if (is_json) {
    return(read_tr_from_json(json_path))
  }

  NA_real_
}

#' @export
infer_tr.bids_project <- function(x, subid, task, run = ".*", session = ".*", ...) {
  get_repetition_time(x, subid = subid, task = task, run = run, session = session, ...)
}
