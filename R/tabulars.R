# R/tabulars.R
# Typed tabular metadata layer for BIDS TSV files
# Part of bidser milestone 0.6

# ---------------------------------------------------------------------------
# Base constructor
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.new_bids_tabular <- function(data, sidecar, file, subclass) {
  # data must already be a tibble
  structure(
    data,
    sidecar = sidecar,
    file    = file,
    class   = c(subclass, "bids_tabular", class(data))
  )
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.read_tabular_sidecar <- function(tsv_path) {
  json_path <- sub("\\.tsv$", ".json", tsv_path)
  if (!file.exists(json_path)) {
    return(list())
  }
  tryCatch(
    jsonlite::read_json(json_path, simplifyVector = TRUE),
    error = function(e) {
      warning("Could not read tabular sidecar: ", json_path, " -- ", conditionMessage(e))
      list()
    }
  )
}

#' @keywords internal
#' @noRd
.read_tabular_tsv <- function(tsv_path, ...) {
  readr::read_tsv(
    tsv_path,
    na            = c("n/a", "NA", "N/A", ""),
    show_col_types = FALSE,
    ...
  )
}

# ---------------------------------------------------------------------------
# Validators (warn-only, internal)
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.validate_participants <- function(tbl, file) {
  if (!"participant_id" %in% names(tbl)) {
    warning("participants.tsv missing 'participant_id' column: ", file)
  } else if (!identical(names(tbl)[[1L]], "participant_id")) {
    warning("'participant_id' is not the first column in: ", file)
  }
  invisible(NULL)
}

#' @keywords internal
#' @noRd
.validate_scans <- function(tbl, file) {
  if (!"filename" %in% names(tbl)) {
    warning("scans.tsv missing 'filename' column: ", file)
  }
  invisible(NULL)
}

#' @keywords internal
#' @noRd
.validate_sessions <- function(tbl, file) {
  if (!"session_id" %in% names(tbl)) {
    warning("sessions.tsv missing 'session_id' column: ", file)
  }
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# read_participants methods (generic in R/all_generic.R)
# ---------------------------------------------------------------------------

#' @export
#' @rdname read_participants
read_participants.bids_project <- function(x, ...) {
  tsv_path <- file.path(x$path, "participants.tsv")
  if (!file.exists(tsv_path)) {
    message("No participants.tsv found")
    return(NULL)
  }
  tbl     <- .read_tabular_tsv(tsv_path, ...)
  sidecar <- .read_tabular_sidecar(tsv_path)
  .validate_participants(tbl, tsv_path)
  .new_bids_tabular(tbl, sidecar, tsv_path, "bids_participants")
}

#' @export
#' @rdname read_participants
read_participants.character <- function(x, ...) {
  path <- x
  if (dir.exists(path)) {
    tsv_path <- file.path(path, "participants.tsv")
  } else {
    tsv_path <- path
  }
  if (!file.exists(tsv_path)) {
    message("No participants.tsv found at: ", path)
    return(NULL)
  }
  tbl     <- .read_tabular_tsv(tsv_path, ...)
  sidecar <- .read_tabular_sidecar(tsv_path)
  .validate_participants(tbl, tsv_path)
  .new_bids_tabular(tbl, sidecar, tsv_path, "bids_participants")
}

# ---------------------------------------------------------------------------
# read_scans_tsv methods (generic in R/all_generic.R)
# ---------------------------------------------------------------------------

#' @export
#' @rdname read_scans_tsv
read_scans_tsv.bids_project <- function(x, subid, session = NULL, ...) {
  if (!is.null(session) && nzchar(session)) {
    rel <- file.path(
      sprintf("sub-%s", subid),
      sprintf("ses-%s", session),
      sprintf("sub-%s_ses-%s_scans.tsv", subid, session)
    )
  } else {
    rel <- file.path(
      sprintf("sub-%s", subid),
      sprintf("sub-%s_scans.tsv", subid)
    )
  }
  tsv_path <- file.path(x$path, rel)
  if (!file.exists(tsv_path)) {
    message("No scans.tsv found for sub-", subid)
    return(NULL)
  }
  tbl     <- .read_tabular_tsv(tsv_path, ...)
  sidecar <- .read_tabular_sidecar(tsv_path)
  .validate_scans(tbl, tsv_path)
  .new_bids_tabular(tbl, sidecar, tsv_path, "bids_scans_tsv")
}

# ---------------------------------------------------------------------------
# read_sessions_tsv methods (generic in R/all_generic.R)
# ---------------------------------------------------------------------------

#' @export
#' @rdname read_sessions_tsv
read_sessions_tsv.bids_project <- function(x, subid, ...) {
  rel      <- file.path(
    sprintf("sub-%s", subid),
    sprintf("sub-%s_sessions.tsv", subid)
  )
  tsv_path <- file.path(x$path, rel)
  if (!file.exists(tsv_path)) {
    message("No sessions.tsv found for sub-", subid)
    return(NULL)
  }
  tbl     <- .read_tabular_tsv(tsv_path, ...)
  sidecar <- .read_tabular_sidecar(tsv_path)
  .validate_sessions(tbl, tsv_path)
  .new_bids_tabular(tbl, sidecar, tsv_path, "bids_sessions_tsv")
}

# ---------------------------------------------------------------------------
# sidecar() accessor (generic in R/all_generic.R)
# ---------------------------------------------------------------------------

#' @export
#' @rdname sidecar
sidecar.bids_tabular <- function(x, ...) {
  attr(x, "sidecar") %||% list()
}

# ---------------------------------------------------------------------------
# S3 methods for bids_tabular
# ---------------------------------------------------------------------------

#' @export
print.bids_tabular <- function(x, n = 10L, ...) {
  file_path <- attr(x, "file") %||% ""
  has_sidecar <- length(attr(x, "sidecar") %||% list()) > 0L
  cls <- class(x)[[1L]]
  cat(
    sprintf("<%s> %s (%d rows x %d cols, sidecar: %s)\n",
            cls, file_path, nrow(x), ncol(x),
            if (has_sidecar) "yes" else "no")
  )
  NextMethod()
  invisible(x)
}

#' @export
summary.bids_tabular <- function(object, ...) {
  file_path   <- attr(object, "file") %||% ""
  sc          <- attr(object, "sidecar") %||% list()
  tsv_cols    <- names(object)
  sc_keys     <- names(sc)
  missing_descs <- setdiff(tsv_cols, sc_keys)
  absent_keys   <- setdiff(sc_keys, tsv_cols)

  cat("File:              ", file_path, "\n")
  cat("Rows:              ", nrow(object), "\n")
  cat("Columns:           ", ncol(object), "\n")
  cat("Sidecar present:   ", if (length(sc) > 0L) "yes" else "no", "\n")
  if (length(missing_descs) > 0L) {
    cat("Cols without sidecar desc:", paste(missing_descs, collapse = ", "), "\n")
  }
  if (length(absent_keys) > 0L) {
    cat("Sidecar keys absent from TSV:", paste(absent_keys, collapse = ", "), "\n")
  }
  invisible(object)
}

#' @export
as_tibble.bids_tabular <- function(x, ...) {
  structure(
    x,
    class   = c("tbl_df", "tbl", "data.frame"),
    sidecar = NULL,
    file    = NULL
  )
}
