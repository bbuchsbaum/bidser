#' @keywords internal
#' @noRd
.bidser_is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:[/\\\\])", path)
}

#' @keywords internal
#' @noRd
.bidser_to_relative_path <- function(project_path, path) {
  rel <- gsub("\\\\", "/", as.character(path))
  rel <- sub("^\\./", "", rel)

  if (!.bidser_is_absolute_path(rel)) {
    return(rel)
  }

  root <- normalizePath(project_path, winslash = "/", mustWork = FALSE)
  abs_path <- normalizePath(rel, winslash = "/", mustWork = FALSE)
  root_prefix <- paste0(root, "/")

  if (identical(abs_path, root)) {
    return("")
  }
  if (startsWith(abs_path, root_prefix)) {
    return(substr(abs_path, nchar(root_prefix) + 1, nchar(abs_path)))
  }

  abs_path
}

#' @keywords internal
#' @noRd
.bidser_escape_regex_literal <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x, perl = TRUE)
}

#' @keywords internal
#' @noRd
.bidser_derivative_registry <- function(x) {
  if (!is.null(x$derivatives) && nrow(x$derivatives) > 0) {
    return(x$derivatives)
  }

  if (isTRUE(x$has_fmriprep) && nzchar(x$prep_dir)) {
    return(tibble::tibble(
      pipeline = "fmriprep",
      root = x$prep_dir,
      description = list(list()),
      source = "legacy"
    ))
  }

  tibble::tibble(
    pipeline = character(0),
    root = character(0),
    description = list(),
    source = character(0)
  )
}

#' @keywords internal
#' @noRd
.bidser_load_cached_index <- function(x) {
  state <- .bidser_load_cached_index_state(x, refresh = FALSE, persist = FALSE)
  if (is.null(state)) NULL else .bidser_index_state_manifest_tibble(state)
}

#' @keywords internal
#' @noRd
.bidser_dir_mtime <- function(path) {
  info <- file.info(path)
  if (is.na(info$mtime)) return(Sys.time())
  info$mtime
}

#' @keywords internal
#' @noRd
.bidser_derivative_roots <- function(x) {
  reg <- .bidser_derivative_registry(x)
  if (nrow(reg) == 0) {
    return(character(0))
  }
  reg$root
}

#' @keywords internal
#' @noRd
.bidser_path_pipeline <- function(x, rel_path) {
  reg <- .bidser_derivative_registry(x)
  if (nrow(reg) == 0 || !nzchar(rel_path)) {
    return(NA_character_)
  }

  for (i in seq_len(nrow(reg))) {
    root <- reg$root[[i]]
    prefix <- paste0(root, "/")
    if (identical(rel_path, root) || startsWith(rel_path, prefix)) {
      return(reg$pipeline[[i]])
    }
  }

  NA_character_
}

#' @keywords internal
#' @noRd
.bidser_filter_pipeline_paths <- function(x, paths, pipeline) {
  if (is.null(paths) || length(paths) == 0 || is.null(pipeline)) {
    return(paths)
  }

  rel_paths <- vapply(paths, function(p) .bidser_to_relative_path(x$path, p), character(1))
  keep <- vapply(rel_paths, function(p) {
    pipe <- .bidser_path_pipeline(x, p)
    !is.na(pipe) && pipe %in% pipeline
  }, logical(1))

  out <- paths[keep]
  if (length(out) == 0) {
    return(NULL)
  }
  out
}

#' @keywords internal
#' @noRd
.bidser_is_derivative_path <- function(x, rel_path, pipeline = NULL) {
  pipe <- .bidser_path_pipeline(x, rel_path)
  if (is.na(pipe)) {
    return(FALSE)
  }
  if (is.null(pipeline)) {
    return(TRUE)
  }
  pipe %in% pipeline
}

#' @keywords internal
#' @noRd
.bidser_valid_query_entities <- function() {
  sort(unique(c(
    "subid", "session", "task", "run", "kind", "suffix", "type", "modality",
    "acq", "ce", "dir", "rec", "echo", "space", "res", "desc", "label", "variant",
    "from", "to", "target", "class", "mod", "hemi", "mode", "fmriprep",
    "sub", "ses",
    "extension", "datatype"
  )))
}

#' @keywords internal
#' @noRd
.bidser_normalize_filter_names <- function(filters) {
  if (length(filters) == 0) {
    return(filters)
  }

  if (is.null(names(filters)) || any(names(filters) == "")) {
    stop("All query filters in `...` must be named.")
  }

  out <- list()
  for (nm in names(filters)) {
    canonical <- switch(nm,
      sub = "subid",
      ses = "session",
      nm
    )

    val <- filters[[nm]]
    if (!is.null(out[[canonical]]) && !identical(out[[canonical]], val)) {
      stop("Conflicting filter values for entity `", canonical, "`.")
    }
    out[[canonical]] <- val
  }
  out
}

#' @keywords internal
#' @noRd
.bidser_prepare_query_filters <- function(filters, match_mode) {
  filters <- .bidser_normalize_filter_names(filters)

  if (length(filters) == 0) {
    return(filters)
  }

  if (identical(match_mode, "exact")) {
    filters <- lapply(filters, function(v) {
      if (is.null(v)) {
        return(v)
      }
      vals <- as.character(v)
      escaped <- vapply(vals, .bidser_escape_regex_literal, character(1))
      if (length(escaped) == 1L) {
        paste0("^", escaped, "$")
      } else {
        paste0("^(?:", paste(escaped, collapse = "|"), ")$")
      }
    })
  } else if (identical(match_mode, "glob")) {
    filters <- lapply(filters, function(v) {
      if (is.null(v)) {
        return(v)
      }
      vals <- as.character(v)
      # Convert each glob to regex using utils::glob2rx, strip anchors added
      # by glob2rx so they integrate with existing matching logic
      rxs <- vapply(vals, function(g) {
        rx <- utils::glob2rx(g)
        sub("\\$$", "", sub("^\\^", "", rx))
      }, character(1))
      rxs
    })
  } else {
    filters <- lapply(filters, function(v) if (is.null(v)) v else as.character(v))
  }

  filters
}

#' @keywords internal
#' @noRd
.bidser_filter_scope_paths <- function(x, paths, scope) {
  if (is.null(paths) || length(paths) == 0 || identical(scope, "all")) {
    return(paths)
  }

  roots <- .bidser_derivative_roots(x)
  if (length(roots) == 0) {
    if (identical(scope, "derivatives")) {
      return(NULL)
    }
    return(paths)
  }

  is_derivative <- vapply(paths, function(p) {
    rel <- .bidser_to_relative_path(x$path, p)
    .bidser_is_derivative_path(x, rel)
  }, logical(1))

  keep <- if (identical(scope, "derivatives")) is_derivative else !is_derivative
  out <- paths[keep]

  if (length(out) == 0) {
    return(NULL)
  }
  out
}

#' @keywords internal
#' @noRd
.bidser_parse_entities_from_path <- function(path) {
  rel <- gsub("\\\\", "/", as.character(path))
  pieces <- strsplit(rel, "/", fixed = TRUE)[[1]]

  entities <- list()

  if (length(pieces) > 1L) {
    dir_parts <- pieces[-length(pieces)]
    for (part in dir_parts) {
      if (grepl("^sub-[A-Za-z0-9]+$", part)) {
        entities$subid <- sub("^sub-", "", part)
      } else if (grepl("^ses-[A-Za-z0-9]+$", part)) {
        entities$session <- sub("^ses-", "", part)
      }
    }
  }

  fname <- basename(rel)
  stem <- sub("\\.nii\\.gz$", "", fname, ignore.case = TRUE)
  stem <- sub("\\.tsv\\.gz$", "", stem, ignore.case = TRUE)
  stem <- sub("\\.(json|nii|tsv|csv|txt|h5|gii)$", "", stem, ignore.case = TRUE)

  parts <- strsplit(stem, "_", fixed = TRUE)[[1]]
  if (length(parts) == 0L) {
    return(entities)
  }

  entities$kind <- parts[[length(parts)]]

  if (length(parts) == 1L) {
    return(entities)
  }

  kv_parts <- parts[-length(parts)]
  for (token in kv_parts) {
    if (!grepl("-", token, fixed = TRUE)) {
      next
    }

    key <- sub("-.*$", "", token)
    val <- sub("^[^-]+-", "", token)
    if (!nzchar(val)) {
      next
    }

    canonical_key <- switch(key,
      sub = "subid",
      ses = "session",
      key
    )
    entities[[canonical_key]] <- val
  }

  entities
}

#' @keywords internal
#' @noRd
.bidser_extract_extension <- function(path) {
  fname <- basename(path)
  if (grepl("\\.nii\\.gz$", fname, ignore.case = TRUE)) return(".nii.gz")
  if (grepl("\\.tsv\\.gz$", fname, ignore.case = TRUE)) return(".tsv.gz")
  m <- regmatches(fname, regexpr("\\.[^.]+$", fname))
  if (length(m) == 1L) m else ""
}

#' @keywords internal
#' @noRd
.bidser_extract_datatype <- function(path) {
  pieces <- strsplit(gsub("\\\\", "/", as.character(path)), "/", fixed = TRUE)[[1]]
  known <- c("anat", "func", "fmap", "dwi", "perf", "meg", "eeg", "ieeg", "beh", "pet", "micr")
  found <- intersect(pieces, known)
  if (length(found) > 0) found[[length(found)]] else NA_character_
}

#' @keywords internal
#' @noRd
.bidser_filter_extension <- function(paths, ext_pattern) {
  if (is.null(ext_pattern) || is.null(paths) || length(paths) == 0) {
    return(paths)
  }
  keep <- vapply(paths, function(p) {
    ext <- .bidser_extract_extension(p)
    any(stringr::str_detect(ext, ext_pattern))
  }, logical(1))
  out <- paths[keep]
  if (length(out) == 0) NULL else out
}

#' @keywords internal
#' @noRd
.bidser_filter_datatype <- function(paths, dt_pattern) {
  if (is.null(dt_pattern) || is.null(paths) || length(paths) == 0) {
    return(paths)
  }
  keep <- vapply(paths, function(p) {
    dt <- .bidser_extract_datatype(p)
    !is.na(dt) && any(stringr::str_detect(dt, dt_pattern))
  }, logical(1))
  out <- paths[keep]
  if (length(out) == 0) NULL else out
}

#' @keywords internal
#' @noRd
.bidser_sort_query_tibble <- function(tbl) {
  if (is.null(tbl) || nrow(tbl) == 0) {
    return(tbl)
  }
  sort_cols <- intersect(c("subid", "session", "task", "run", "path"), names(tbl))
  if (length(sort_cols) > 0) {
    tbl <- dplyr::arrange(tbl, dplyr::across(dplyr::all_of(sort_cols)))
  }
  tbl
}

#' @keywords internal
#' @noRd
.bidser_require_entity_keys <- function(paths, required_keys, project_path) {
  if (is.null(paths) || length(paths) == 0 || length(required_keys) == 0) {
    return(paths)
  }

  keep <- vapply(paths, function(p) {
    rel <- .bidser_to_relative_path(project_path, p)
    entities <- .bidser_parse_entities_from_path(rel)
    all(vapply(required_keys, function(k) {
      val <- entities[[k]]
      !is.null(val) && !is.na(val) && nzchar(as.character(val))
    }, logical(1)))
  }, logical(1))

  out <- paths[keep]
  if (length(out) == 0) {
    return(NULL)
  }
  out
}

#' @keywords internal
#' @noRd
.bidser_index_row_from_path <- function(x, rel_path) {
  rel_path <- .bidser_to_relative_path(x$path, rel_path)
  file_info <- file.info(file.path(x$path, rel_path))
  encoded <- tryCatch(encode(basename(rel_path)), error = function(e) NULL)
  parsed <- .bidser_parse_entities_from_path(rel_path)

  if (is.null(encoded)) {
    encoded <- list()
  }

  entity_info <- utils::modifyList(parsed, encoded, keep.null = TRUE)
  fields <- c(
    "subid", "session", "task", "run", "kind", "suffix", "type", "modality",
    "acq", "ce", "dir", "rec", "echo", "space", "res", "desc", "label",
    "variant", "from", "to", "target", "class", "mod", "hemi", "mode"
  )

  row <- c(
    list(
      path = rel_path,
      file = basename(rel_path),
      scope = if (.bidser_is_derivative_path(x, rel_path)) "derivatives" else "raw",
      pipeline = .bidser_path_pipeline(x, rel_path),
      extension = .bidser_extract_extension(rel_path),
      datatype = .bidser_extract_datatype(rel_path),
      size = as.numeric(file_info$size),
      file_mtime = as.numeric(file_info$mtime)
    ),
    setNames(lapply(fields, function(k) {
      val <- entity_info[[k]]
      if (is.null(val) || length(val) == 0) NA_character_ else as.character(val[[1]])
    }), fields)
  )

  tibble::as_tibble(row)
}

#' @keywords internal
#' @noRd
.bidser_build_index_df <- function(x) {
  .bidser_index_state_manifest_tibble(.bidser_build_index_state(x))
}

#' @keywords internal
#' @noRd
.bidser_query_rows_from_index <- function(x, rows, regex, filters,
                                          require_entity = FALSE,
                                          scope = "all",
                                          pipeline = NULL,
                                          full_path = FALSE,
                                          match_mode = c("regex", "exact", "glob"),
                                          raw_filters = list()) {
  match_mode <- match.arg(match_mode)
  rows <- .bidser_finalize_manifest_dt(rows)
  if (nrow(rows) == 0) {
    return(rows)
  }

  rows <- rows[stringr::str_detect(rows$file, regex), ]
  if (nrow(rows) == 0) {
    return(rows)
  }

  if (!identical(scope, "all")) {
    rows <- rows[rows$scope == scope, ]
  }
  if (!is.null(pipeline)) {
    pipeline_vals <- as.character(pipeline)
    rows <- rows[!is.na(rows$pipeline) & rows$pipeline %in% pipeline_vals, ]
  }
  if (nrow(rows) == 0) {
    return(rows)
  }

  for (nm in names(filters)) {
    if (!nm %in% names(rows)) {
      rows <- rows[0]
      break
    }

    vals <- rows[[nm]]
    present <- !is.na(vals) & nzchar(as.character(vals))

    matches <- if (identical(match_mode, "exact") && nm %in% names(raw_filters)) {
      present & as.character(vals) %in% as.character(raw_filters[[nm]])
    } else {
      pattern <- filters[[nm]]
      present & stringr::str_detect(as.character(vals), pattern)
    }

    if (isTRUE(require_entity)) {
      keep <- matches
    } else {
      keep <- matches | !present
    }

    rows <- rows[keep, ]
    if (nrow(rows) == 0) {
      break
    }
  }

  if (nrow(rows) == 0) {
    return(rows)
  }

  if (isTRUE(full_path)) {
    rows$path <- file.path(x$path, rows$path)
  }

  rows
}

#' @export
#' @rdname query_files
query_files.bids_project <- function(x, regex = ".*", full_path = FALSE,
                                     match_mode = c("regex", "exact", "glob"),
                                     require_entity = FALSE,
                                     scope = c("all", "raw", "derivatives"),
                                     pipeline = NULL,
                                     return = c("paths", "tibble"),
                                     use_index = c("auto", "never"),
                                     strict = TRUE, ...) {
  match_mode <- match.arg(match_mode)
  scope <- match.arg(scope)
  return <- match.arg(return)
  use_index <- match.arg(use_index)

  # Formulas passed positionally can land in regex, full_path, or strict slots.
  # Rescue them back into dots_all before checking for formula filters.
  dots_all <- list(...)
  if (inherits(strict, "formula")) {
    dots_all <- c(list(strict), dots_all)
    strict <- TRUE
  }
  if (inherits(full_path, "formula")) {
    dots_all <- c(list(full_path), dots_all)
    full_path <- FALSE
  }
  if (inherits(regex, "formula")) {
    dots_all <- c(list(regex), dots_all)
    regex <- ".*"
  }
  split_f <- .bidser_split_filters(dots_all)
  if (length(split_f$formula_filters) > 0L) {
    result <- do.call(search_files, c(
      list(x, regex = regex, full_path = full_path, strict = strict),
      dots_all
    ))
    if (identical(return, "tibble")) {
      if (is.null(result) || length(result) == 0L) {
        return(tibble::tibble(
          path = character(0), file = character(0),
          scope = character(0), pipeline = character(0)
        ))
      }
      rows <- dplyr::bind_rows(lapply(result, function(p) .bidser_index_row_from_path(x, p)))
      return(.bidser_sort_query_tibble(rows))
    }
    return(result)
  }

  filters_raw <- list(...)
  raw_filters <- .bidser_normalize_filter_names(filters_raw)
  filters <- .bidser_prepare_query_filters(filters_raw, match_mode = match_mode)

  valid_entities <- .bidser_valid_query_entities()
  unknown <- setdiff(names(filters), valid_entities)
  if (length(unknown) > 0) {
    stop(
      "Unknown entity filters: ", paste(sort(unknown), collapse = ", "),
      ". Valid entities include: ", paste(valid_entities, collapse = ", "), "."
    )
  }

  # Extract extension/datatype filters -- these are handled post-hoc, not by search_files
  ext_filter <- filters[["extension"]]
  dt_filter <- filters[["datatype"]]
  search_filters <- filters[setdiff(names(filters), c("extension", "datatype"))]

  pipeline <- if (is.null(pipeline)) NULL else as.character(pipeline)

  cached_state <- if (identical(use_index, "auto")) {
    .bidser_load_cached_index_state(x, refresh = TRUE, persist = TRUE)
  } else {
    NULL
  }

  if (!is.null(cached_state)) {
    rows <- .bidser_query_rows_from_index(
      x = x,
      rows = cached_state$manifest,
      regex = regex,
      filters = filters,
      require_entity = require_entity,
      scope = scope,
      pipeline = pipeline,
      full_path = full_path,
      match_mode = match_mode,
      raw_filters = raw_filters
    )
    if (identical(return, "tibble")) {
      return(.bidser_sort_query_tibble(tibble::as_tibble(rows)))
    }
    if (nrow(rows) == 0) {
      return(NULL)
    }
    return(rows$path)
  }

  results <- do.call(search_files, c(
    list(x = x, regex = regex, full_path = full_path, strict = strict),
    search_filters
  ))

  results <- .bidser_filter_scope_paths(x, results, scope = scope)
  results <- .bidser_filter_pipeline_paths(x, results, pipeline = pipeline)
  results <- .bidser_filter_extension(results, ext_filter)
  results <- .bidser_filter_datatype(results, dt_filter)

  if (isTRUE(require_entity) && !is.null(results) && length(results) > 0) {
    results <- .bidser_require_entity_keys(
      paths = results,
      required_keys = names(search_filters),
      project_path = x$path
    )
  }

  if (identical(return, "tibble")) {
    if (is.null(results) || length(results) == 0) {
      return(tibble::tibble(
        path = character(0), file = character(0),
        scope = character(0), pipeline = character(0)
      ))
    }
    rows <- dplyr::bind_rows(lapply(results, function(p) .bidser_index_row_from_path(x, p)))
    return(.bidser_sort_query_tibble(rows))
  }

  results
}

#' @export
#' @rdname query_files
query_files.mock_bids_project <- function(x, regex = ".*", full_path = FALSE,
                                          match_mode = c("regex", "exact", "glob"),
                                          require_entity = FALSE,
                                          scope = c("all", "raw", "derivatives"),
                                          pipeline = NULL,
                                          return = c("paths", "tibble"),
                                          use_index = c("auto", "never"),
                                          strict = TRUE, ...) {
  match_mode <- match.arg(match_mode)
  scope <- match.arg(scope)
  return <- match.arg(return)
  use_index <- match.arg(use_index)

  # Rescue formulas from positional slots (regex, full_path, strict) into dots_all
  dots_all <- list(...)
  if (inherits(strict, "formula")) {
    dots_all <- c(list(strict), dots_all)
    strict <- TRUE
  }
  if (inherits(full_path, "formula")) {
    dots_all <- c(list(full_path), dots_all)
    full_path <- FALSE
  }
  if (inherits(regex, "formula")) {
    dots_all <- c(list(regex), dots_all)
    regex <- ".*"
  }
  split_f <- .bidser_split_filters(dots_all)
  if (length(split_f$formula_filters) > 0L) {
    result <- do.call(search_files, c(
      list(x, regex = regex, full_path = full_path, strict = strict),
      dots_all
    ))
    if (identical(return, "tibble")) {
      if (is.null(result) || length(result) == 0L) {
        return(tibble::tibble(
          path = character(0), file = character(0),
          scope = character(0), pipeline = character(0)
        ))
      }
      rows <- lapply(result, function(p) {
        rel <- .bidser_to_relative_path(x$path, p)
        .bidser_index_row_from_path(x, rel)
      })
      return(dplyr::bind_rows(rows))
    }
    return(result)
  }

  filters_raw <- list(...)
  filters <- .bidser_prepare_query_filters(filters_raw, match_mode = match_mode)

  valid_entities <- .bidser_valid_query_entities()
  unknown <- setdiff(names(filters), valid_entities)
  if (length(unknown) > 0) {
    stop(
      "Unknown entity filters: ", paste(sort(unknown), collapse = ", "),
      ". Valid entities include: ", paste(valid_entities, collapse = ", "), "."
    )
  }

  if (identical(scope, "all")) {
    raw_res <- do.call(search_files, c(
      list(x = x, regex = regex, full_path = full_path, strict = strict, fmriprep = FALSE),
      filters
    ))
    deriv_res <- do.call(search_files, c(
      list(x = x, regex = regex, full_path = full_path, strict = strict, fmriprep = TRUE),
      filters
    ))
    results <- unique(c(raw_res, deriv_res))
    if (length(results) == 0) {
      results <- NULL
    }
  } else {
    want_derivatives <- identical(scope, "derivatives")
    results <- do.call(search_files, c(
      list(x = x, regex = regex, full_path = full_path, strict = strict, fmriprep = want_derivatives),
      filters
    ))
  }

  if (isTRUE(require_entity) && !is.null(results) && length(results) > 0) {
    results <- .bidser_require_entity_keys(
      paths = results,
      required_keys = names(filters),
      project_path = x$path
    )
  }

  if (!is.null(pipeline) && !is.null(results) && length(results) > 0) {
    pipeline <- as.character(pipeline)
    results <- results[vapply(results, function(p) {
      rel <- .bidser_to_relative_path(x$path, p)
      pipe <- if (startsWith(rel, paste0(x$prep_dir, "/")) || identical(rel, x$prep_dir)) basename(x$prep_dir) else NA_character_
      !is.na(pipe) && pipe %in% pipeline
    }, logical(1))]
    if (length(results) == 0) {
      results <- NULL
    }
  }

  if (identical(return, "tibble")) {
    if (is.null(results) || length(results) == 0) {
      return(tibble::tibble(path = character(0), file = character(0), scope = character(0), pipeline = character(0)))
    }
    rows <- lapply(results, function(p) {
      rel <- .bidser_to_relative_path(x$path, p)
      row <- .bidser_index_row_from_path(x, rel)
      if (isTRUE(full_path)) {
        row$path <- p
      }
      row
    })
    return(dplyr::bind_rows(rows))
  }

  results
}

#' @keywords internal
#' @noRd
.bidser_resolve_metadata_scope <- function(x, file_rel, scope) {
  scope <- match.arg(scope, c("auto", "raw", "derivatives", "all"))

  if (!identical(scope, "auto")) {
    return(scope)
  }

  if (.bidser_is_derivative_path(x, file_rel)) {
    return("derivatives")
  }

  "raw"
}

#' @keywords internal
#' @noRd
.bidser_metadata_scope_root <- function(x, resolved_scope) {
  if (identical(resolved_scope, "raw")) {
    return(normalizePath(x$path, winslash = "/", mustWork = TRUE))
  }
  if (identical(resolved_scope, "derivatives")) {
    default_pipeline <- if (!is.null(x$default_pipeline) && !is.na(x$default_pipeline)) {
      x$default_pipeline
    } else {
      .bidser_default_pipeline(.bidser_derivative_registry(x))
    }
    reg <- .bidser_derivative_registry(x)
    if (is.na(default_pipeline) || nrow(reg) == 0) {
      stop("Derivatives scope requested, but this project has no derivatives index.")
    }
    root <- reg$root[match(default_pipeline, reg$pipeline)]
    return(normalizePath(file.path(x$path, root), winslash = "/", mustWork = TRUE))
  }
  normalizePath(x$path, winslash = "/", mustWork = TRUE)
}

#' @keywords internal
#' @noRd
.bidser_ancestor_chain <- function(dir_path, root_path) {
  dir_path <- normalizePath(dir_path, winslash = "/", mustWork = TRUE)
  root_path <- normalizePath(root_path, winslash = "/", mustWork = TRUE)

  if (!identical(dir_path, root_path) &&
      !startsWith(paste0(dir_path, "/"), paste0(root_path, "/"))) {
    return(character(0))
  }

  chain <- character(0)
  cur <- dir_path
  repeat {
    chain <- c(chain, cur)
    if (identical(cur, root_path)) {
      break
    }
    parent <- dirname(cur)
    if (identical(parent, cur)) {
      break
    }
    cur <- parent
  }

  rev(chain)
}

#' @keywords internal
#' @noRd
.bidser_candidate_applies <- function(candidate_rel, target_entities, target_kind) {
  cand_entities <- .bidser_parse_entities_from_path(candidate_rel)
  cand_kind <- cand_entities$kind

  if (is.null(cand_kind) || !identical(cand_kind, target_kind)) {
    return(FALSE)
  }

  cand_keys <- setdiff(names(cand_entities), "kind")
  if (length(cand_keys) == 0) {
    return(TRUE)
  }

  all(vapply(cand_keys, function(k) {
    tval <- target_entities[[k]]
    cval <- cand_entities[[k]]
    !is.null(tval) && !is.null(cval) && identical(as.character(tval), as.character(cval))
  }, logical(1)))
}

#' @export
#' @rdname get_metadata
get_metadata.bids_project <- function(x, file, inherit = TRUE,
                                      scope = c("auto", "raw", "derivatives", "all"), ...) {
  if (missing(file) || !is.character(file) || length(file) != 1L || !nzchar(file)) {
    stop("`file` must be a single non-empty character path.")
  }

  file_abs <- if (.bidser_is_absolute_path(file)) {
    normalizePath(file, winslash = "/", mustWork = TRUE)
  } else {
    normalizePath(file.path(x$path, file), winslash = "/", mustWork = TRUE)
  }

  project_root <- normalizePath(x$path, winslash = "/", mustWork = TRUE)
  project_prefix <- paste0(project_root, "/")
  if (!identical(file_abs, project_root) && !startsWith(file_abs, project_prefix)) {
    stop("`file` must be inside the project root: ", x$path)
  }

  file_rel <- .bidser_to_relative_path(x$path, file_abs)
  target_entities <- .bidser_parse_entities_from_path(file_rel)
  target_kind <- target_entities$kind

  if (is.null(target_kind) || !nzchar(target_kind)) {
    stop("Could not determine BIDS suffix/kind for file: ", file)
  }

  use_cached_index <- !identical(x$index_mode %||% "auto", "none")
  index_state <- if (use_cached_index) {
    .bidser_load_cached_index_state(x, refresh = TRUE, persist = TRUE)
  } else {
    NULL
  }

  if (!isTRUE(inherit)) {
    json_path <- if (grepl("\\.json$", file_abs, ignore.case = TRUE)) {
      file_abs
    } else {
      stem <- sub("\\.nii\\.gz$", "", file_abs, ignore.case = TRUE)
      stem <- sub("\\.tsv\\.gz$", "", stem, ignore.case = TRUE)
      stem <- sub("\\.(nii|tsv|csv|txt|h5|gii)$", "", stem, ignore.case = TRUE)
      paste0(stem, ".json")
    }

    if (!file.exists(json_path)) {
      return(list())
    }

    direct_meta <- NULL
    if (!is.null(index_state)) {
      json_rel <- .bidser_to_relative_path(x$path, json_path)
      hit <- index_state$sidecars[index_state$sidecars$path == json_rel, ]
      if (nrow(hit) > 0) {
        direct_meta <- hit$data[[1]]
      }
    }
    if (is.null(direct_meta)) {
      direct_meta <- tryCatch(
        jsonlite::read_json(json_path, simplifyVector = TRUE),
        error = function(e) {
          warning("Failed to read JSON sidecar: ", json_path, " (", e$message, ")")
          list()
        }
      )
    }

    if (is.null(direct_meta) || !is.list(direct_meta)) {
      return(list())
    }
    return(direct_meta)
  }

  resolved_scope <- .bidser_resolve_metadata_scope(x, file_rel, scope = scope)
  scope_root <- .bidser_metadata_scope_root(x, resolved_scope)
  target_dir <- dirname(file_abs)

  if (!is.null(index_state)) {
    cached <- .bidser_lookup_resolved_meta(index_state, file_rel, resolved_scope)
    if (!is.null(cached)) {
      return(cached)
    }
  }

  ancestry <- .bidser_ancestor_chain(target_dir, scope_root)
  if (length(ancestry) == 0) {
    return(list())
  }

  candidates <- list()
  ancestry_rel <- vapply(ancestry, function(dir_path) .bidser_to_relative_path(x$path, dir_path), character(1))

  if (!is.null(index_state)) {
    sidecars <- index_state$sidecars
    candidate_idx <- which(as.character(sidecars$directory) %in% ancestry_rel)
    if (length(candidate_idx) > 0) {
      keep <- vapply(
        as.character(sidecars$path[candidate_idx]),
        function(rel) .bidser_candidate_applies(rel, target_entities = target_entities, target_kind = target_kind),
        logical(1)
      )
      candidate_idx <- candidate_idx[keep]
    }

    if (length(candidate_idx) > 0) {
      candidate_paths <- as.character(sidecars$path[candidate_idx])
      candidate_depth <- match(as.character(sidecars$directory[candidate_idx]), ancestry_rel)
      candidate_specificity <- as.integer(sidecars$specificity[candidate_idx])
      ord <- order(candidate_depth, candidate_specificity, candidate_paths)
      deps <- character(0)
      merged <- list()
      for (i in ord) {
        idx <- candidate_idx[[i]]
        candidate_meta <- sidecars$data[[idx]]
        if (is.null(candidate_meta) || !is.list(candidate_meta)) {
          next
        }
        merged <- utils::modifyList(merged, candidate_meta, keep.null = TRUE)
        deps <- c(deps, sidecars$path[[idx]])
      }

      index_state <- .bidser_store_resolved_meta(
        index_state,
        path = file_rel,
        scope = resolved_scope,
        data = merged,
        deps = unique(deps)
      )
      .bidser_set_session_index_state(x, index_state)
      return(merged)
    }
  }

  for (depth in seq_along(ancestry)) {
    sidecars <- sort(list.files(ancestry[[depth]], pattern = "\\.json$", full.names = TRUE, recursive = FALSE))
    if (length(sidecars) == 0) {
      next
    }

    for (sidecar in sidecars) {
      rel <- .bidser_to_relative_path(x$path, sidecar)
      if (.bidser_candidate_applies(rel, target_entities = target_entities, target_kind = target_kind)) {
        cand_entities <- .bidser_parse_entities_from_path(rel)
        specificity <- length(setdiff(names(cand_entities), "kind"))
        candidates[[length(candidates) + 1L]] <- list(
          path = sidecar,
          rel_path = rel,
          depth = depth,
          specificity = specificity
        )
      }
    }
  }

  if (length(candidates) == 0) {
    return(list())
  }

  ord <- order(
    vapply(candidates, function(z) z$depth, integer(1)),
    vapply(candidates, function(z) z$specificity, integer(1)),
    vapply(candidates, function(z) z$path, character(1))
  )
  candidates <- candidates[ord]

  merged <- list()
  deps <- character(0)
  for (cand in candidates) {
    candidate_meta <- tryCatch(
      jsonlite::read_json(cand$path, simplifyVector = TRUE),
      error = function(e) {
        warning("Failed to read JSON sidecar: ", cand$path, " (", e$message, ")")
        NULL
      }
    )

    if (is.null(candidate_meta) || !is.list(candidate_meta)) {
      next
    }
    merged <- utils::modifyList(merged, candidate_meta, keep.null = TRUE)
    deps <- c(deps, cand$rel_path)
  }

  if (!is.null(index_state)) {
    index_state <- .bidser_store_resolved_meta(
      index_state,
      path = file_rel,
      scope = resolved_scope,
      data = merged,
      deps = unique(deps)
    )
    .bidser_set_session_index_state(x, index_state)
  }

  merged
}
