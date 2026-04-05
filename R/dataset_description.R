# R/dataset_description.R
# Typed metadata layer for BIDS dataset_description.json
# Part of bidser milestone 0.6

# ---------------------------------------------------------------------------
# Internal constructor
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.new_bids_dataset_description <- function(fields, parent_directory, source_file = NA_character_) {
  structure(
    list(
      fields          = fields,
      parent_directory = normalizePath(parent_directory, mustWork = FALSE),
      source_file     = source_file
    ),
    class = "bids_dataset_description"
  )
}

# ---------------------------------------------------------------------------
# Validator (warn-only, never stops)
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.validate_dataset_description <- function(fields, source_file) {
  if (is.null(fields$Name) || identical(fields$Name, "")) {
    warning("dataset_description.json missing 'Name' field: ", source_file)
  }
  if (is.null(fields$BIDSVersion) || identical(fields$BIDSVersion, "")) {
    warning("dataset_description.json missing 'BIDSVersion' field: ", source_file)
  }
  if (identical(fields$DatasetType, "derivative") && is.null(fields$GeneratedBy)) {
    warning("Derivative dataset missing 'GeneratedBy': ", source_file)
  }
  if (!is.null(fields$DatasetLinks)) {
    for (nm in names(fields$DatasetLinks)) {
      val <- fields$DatasetLinks[[nm]]
      if (!is.character(val) || length(val) != 1L) {
        warning("DatasetLinks contains non-scalar value in: ", source_file)
        break
      }
    }
  }
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Reader: read_dataset_description() methods
# (generic declared in R/all_generic.R)
# ---------------------------------------------------------------------------

#' @export
#' @rdname read_dataset_description
read_dataset_description.character <- function(x, ...) {
  path <- x
  # If path is a directory, look for dataset_description.json inside it
  if (dir.exists(path)) {
    json_path <- file.path(path, "dataset_description.json")
  } else {
    json_path <- path
  }

  if (!file.exists(json_path)) {
    warning("No dataset_description.json found at: ", path)
    return(NULL)
  }

  fields <- jsonlite::read_json(json_path, simplifyVector = TRUE)

  # Coerce fields
  if (is.null(fields$DatasetType)) {
    fields$DatasetType <- "raw"
  }

  # GeneratedBy: normalise to list-of-lists
  if (!is.null(fields$GeneratedBy)) {
    if (is.data.frame(fields$GeneratedBy)) {
      fields$GeneratedBy <- lapply(seq_len(nrow(fields$GeneratedBy)),
                                   function(i) as.list(fields$GeneratedBy[i, , drop = FALSE]))
    } else if (is.list(fields$GeneratedBy) && !is.null(names(fields$GeneratedBy))) {
      # single named list -> wrap in outer list
      fields$GeneratedBy <- list(fields$GeneratedBy)
    }
    # already a list-of-lists: leave as-is
  }

  # HEDVersion: coerce to character vector
  if (!is.null(fields$HEDVersion)) {
    fields$HEDVersion <- as.character(unlist(fields$HEDVersion))
  }

  # DatasetLinks: coerce to named list
  if (!is.null(fields$DatasetLinks) && is.data.frame(fields$DatasetLinks)) {
    fields$DatasetLinks <- as.list(fields$DatasetLinks)
  }

  .validate_dataset_description(fields, json_path)

  .new_bids_dataset_description(fields, dirname(json_path), json_path)
}

#' @export
#' @rdname read_dataset_description
read_dataset_description.bids_project <- function(x, ...) {
  read_dataset_description(x$path, ...)
}

# ---------------------------------------------------------------------------
# Accessors (generics in R/all_generic.R)
# ---------------------------------------------------------------------------

#' @noRd
dataset_name.bids_dataset_description <- function(x, ...) {
  x$fields$Name %||% NA_character_
}

#' @noRd
dataset_type.bids_dataset_description <- function(x, ...) {
  x$fields$DatasetType %||% "raw"
}

#' @noRd
generated_by.bids_dataset_description <- function(x, ...) {
  x$fields$GeneratedBy %||% list()
}

#' @noRd
dataset_links.bids_dataset_description <- function(x, ...) {
  x$fields$DatasetLinks %||% list()
}

#' @noRd
hed_version.bids_dataset_description <- function(x, ...) {
  x$fields$HEDVersion %||% character(0)
}

#' @noRd
license.bids_dataset_description <- function(x, ...) {
  x$fields$License %||% NA_character_
}

#' @export
#' @rdname bids_version
bids_version.bids_dataset_description <- function(x, ...) {
  x$fields$BIDSVersion %||% NA_character_
}

# ---------------------------------------------------------------------------
# dataset_description accessor for bids_project (generic in all_generic.R)
# ---------------------------------------------------------------------------

#' @export
#' @rdname dataset_description
dataset_description.bids_project <- function(x, ...) {
  x$description
}

# ---------------------------------------------------------------------------
# S3 methods for bids_dataset_description
# ---------------------------------------------------------------------------

#' @export
print.bids_dataset_description <- function(x, ...) {
  use_crayon <- requireNamespace("crayon", quietly = TRUE)

  fmt_val <- function(v, default = "(none)") {
    if (is.null(v) || (length(v) == 1L && is.na(v)) || length(v) == 0L) {
      if (use_crayon) crayon::silver(default) else default
    } else {
      val <- paste(as.character(v), collapse = ", ")
      if (use_crayon) crayon::yellow(val) else val
    }
  }

  hdr <- "<bids_dataset_description>"
  if (use_crayon) hdr <- crayon::bold(crayon::cyan(hdr))
  cat(hdr, "\n")

  lbl <- function(s) if (use_crayon) crayon::bold(sprintf("  %-14s", s)) else sprintf("  %-14s", s)

  gen_by <- x$fields$GeneratedBy
  gen_str <- if (is.null(gen_by) || length(gen_by) == 0L) {
    if (use_crayon) crayon::silver("(none)") else "(none)"
  } else {
    nms <- vapply(gen_by, function(g) g$Name %||% "?", character(1L))
    if (use_crayon) crayon::yellow(paste(nms, collapse = ", ")) else paste(nms, collapse = ", ")
  }

  dl <- x$fields$DatasetLinks
  dl_str <- if (is.null(dl) || length(dl) == 0L) {
    if (use_crayon) crayon::silver("(none)") else "(none)"
  } else {
    if (use_crayon) crayon::yellow(paste(names(dl), collapse = ", ")) else paste(names(dl), collapse = ", ")
  }

  cat(lbl("Name:"),        fmt_val(x$fields$Name),         "\n")
  cat(lbl("BIDSVersion:"), fmt_val(x$fields$BIDSVersion),  "\n")
  cat(lbl("DatasetType:"), fmt_val(x$fields$DatasetType),  "\n")
  cat(lbl("License:"),     fmt_val(x$fields$License),      "\n")
  cat(lbl("GeneratedBy:"), gen_str,                         "\n")
  cat(lbl("DatasetLinks:"),dl_str,                          "\n")

  invisible(x)
}

#' @export
format.bids_dataset_description <- function(x, ...) {
  nm  <- x$fields$Name        %||% "?"
  ver <- x$fields$BIDSVersion %||% "?"
  sprintf("[bids_dataset_description: %s (%s)]", nm, ver)
}

#' @export
as.list.bids_dataset_description <- function(x, ...) {
  x$fields
}
