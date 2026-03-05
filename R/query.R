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
.bidser_valid_query_entities <- function() {
  sort(unique(c(
    "subid", "session", "task", "run", "kind", "suffix", "type", "modality",
    "acq", "ce", "dir", "rec", "echo", "space", "res", "desc", "label", "variant",
    "from", "to", "target", "class", "mod", "hemi", "mode", "fmriprep",
    "sub", "ses"
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

  if (!x$has_fmriprep || !nzchar(x$prep_dir)) {
    if (identical(scope, "derivatives")) {
      return(NULL)
    }
    return(paths)
  }

  rel_paths <- vapply(paths, function(p) .bidser_to_relative_path(x$path, p), character(1))
  prep_prefix <- paste0(x$prep_dir, "/")
  is_derivative <- rel_paths == x$prep_dir | startsWith(rel_paths, prep_prefix)

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

#' @export
#' @rdname query_files
query_files.bids_project <- function(x, regex = ".*", full_path = FALSE,
                                     match_mode = c("regex", "exact"),
                                     require_entity = FALSE,
                                     scope = c("all", "raw", "derivatives"),
                                     strict = TRUE, ...) {
  match_mode <- match.arg(match_mode)
  scope <- match.arg(scope)

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

  results <- do.call(search_files, c(
    list(x = x, regex = regex, full_path = full_path, strict = strict),
    filters
  ))

  results <- .bidser_filter_scope_paths(x, results, scope = scope)

  if (isTRUE(require_entity) && !is.null(results) && length(results) > 0) {
    results <- .bidser_require_entity_keys(
      paths = results,
      required_keys = names(filters),
      project_path = x$path
    )
  }

  results
}

#' @export
#' @rdname query_files
query_files.mock_bids_project <- function(x, regex = ".*", full_path = FALSE,
                                          match_mode = c("regex", "exact"),
                                          require_entity = FALSE,
                                          scope = c("all", "raw", "derivatives"),
                                          strict = TRUE, ...) {
  match_mode <- match.arg(match_mode)
  scope <- match.arg(scope)

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

  results
}

#' @keywords internal
#' @noRd
.bidser_resolve_metadata_scope <- function(x, file_rel, scope) {
  scope <- match.arg(scope, c("auto", "raw", "derivatives", "all"))

  if (!identical(scope, "auto")) {
    return(scope)
  }

  if (x$has_fmriprep && nzchar(x$prep_dir)) {
    prep_prefix <- paste0(x$prep_dir, "/")
    if (file_rel == x$prep_dir || startsWith(file_rel, prep_prefix)) {
      return("derivatives")
    }
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
    if (!x$has_fmriprep || !nzchar(x$prep_dir)) {
      stop("Derivatives scope requested, but this project has no derivatives index.")
    }
    return(normalizePath(file.path(x$path, x$prep_dir), winslash = "/", mustWork = TRUE))
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

    direct_meta <- tryCatch(
      jsonlite::read_json(json_path, simplifyVector = TRUE),
      error = function(e) {
        warning("Failed to read JSON sidecar: ", json_path, " (", e$message, ")")
        list()
      }
    )

    if (is.null(direct_meta) || !is.list(direct_meta)) {
      return(list())
    }
    return(direct_meta)
  }

  resolved_scope <- .bidser_resolve_metadata_scope(x, file_rel, scope = scope)
  scope_root <- .bidser_metadata_scope_root(x, resolved_scope)
  target_dir <- dirname(file_abs)

  ancestry <- .bidser_ancestor_chain(target_dir, scope_root)
  if (length(ancestry) == 0) {
    return(list())
  }

  candidates <- list()
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
  }

  merged
}
