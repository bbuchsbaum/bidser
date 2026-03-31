#' @keywords internal
#' @noRd
.bidser_index_entity_fields <- function() {
  c(
    "subid", "session", "task", "run", "kind", "suffix", "type", "modality",
    "acq", "ce", "dir", "rec", "echo", "space", "res", "desc", "label",
    "variant", "from", "to", "target", "class", "mod", "hemi", "mode"
  )
}

#' @keywords internal
#' @noRd
.bidser_index_schema_version <- function() {
  2L
}

#' @keywords internal
#' @noRd
.bidser_manifest_colnames <- function() {
  c(
    "path", "file", "scope", "pipeline", "extension", "datatype",
    "size", "file_mtime",
    .bidser_index_entity_fields()
  )
}

#' @keywords internal
#' @noRd
.bidser_sidecar_colnames <- function() {
  c(
    "path", "file", "directory", "scope", "pipeline",
    "size", "file_mtime", "specificity", "data",
    .bidser_index_entity_fields()
  )
}

#' @keywords internal
#' @noRd
.bidser_empty_manifest_dt <- function() {
  .bidser_finalize_manifest_dt(data.table::data.table())
}

#' @keywords internal
#' @noRd
.bidser_empty_sidecar_dt <- function() {
  .bidser_finalize_sidecar_dt(data.table::data.table())
}

#' @keywords internal
#' @noRd
.bidser_empty_resolved_meta_dt <- function() {
  dt <- data.table::data.table(
    path = character(0),
    scope = character(0),
    data = list(),
    deps = list()
  )
  data.table::setkeyv(dt, c("path", "scope"))
  dt
}

#' @keywords internal
#' @noRd
.bidser_finalize_manifest_dt <- function(dt) {
  cols <- .bidser_manifest_colnames()
  dt <- data.table::as.data.table(data.table::copy(dt))
  char_cols <- setdiff(cols, c("size", "file_mtime"))

  for (nm in setdiff(cols, names(dt))) {
    if (nm %in% c("size", "file_mtime")) {
      dt[[nm]] <- numeric(nrow(dt))
    } else {
      dt[[nm]] <- rep(NA_character_, nrow(dt))
    }
  }

  dt <- dt[, cols, with = FALSE]
  for (nm in char_cols) {
    dt[[nm]] <- as.character(dt[[nm]])
  }
  dt$size <- as.numeric(dt$size)
  dt$file_mtime <- as.numeric(dt$file_mtime)

  data.table::setkeyv(dt, "path")
  data.table::setindexv(
    dt,
    c("file", "scope", "pipeline", "subid", "session", "task", "run", "kind", "datatype", "extension")
  )
  dt
}

#' @keywords internal
#' @noRd
.bidser_finalize_sidecar_dt <- function(dt) {
  cols <- .bidser_sidecar_colnames()
  dt <- data.table::as.data.table(data.table::copy(dt))
  char_cols <- setdiff(cols, c("size", "file_mtime", "specificity", "data"))

  for (nm in setdiff(cols, names(dt))) {
    if (nm %in% c("size", "file_mtime")) {
      dt[[nm]] <- numeric(nrow(dt))
    } else if (nm == "specificity") {
      dt[[nm]] <- integer(nrow(dt))
    } else if (nm == "data") {
      dt[[nm]] <- rep(list(list()), nrow(dt))
    } else {
      dt[[nm]] <- rep(NA_character_, nrow(dt))
    }
  }

  dt <- dt[, cols, with = FALSE]
  for (nm in char_cols) {
    dt[[nm]] <- as.character(dt[[nm]])
  }
  dt$size <- as.numeric(dt$size)
  dt$file_mtime <- as.numeric(dt$file_mtime)
  dt$specificity <- as.integer(dt$specificity)

  data.table::setkeyv(dt, "path")
  data.table::setindexv(dt, c("directory", "scope", "pipeline", "kind"))
  dt
}

#' @keywords internal
#' @noRd
.bidser_finalize_resolved_meta_dt <- function(dt) {
  dt <- data.table::as.data.table(data.table::copy(dt))
  if (nrow(dt) == 0 && length(names(dt)) == 0) {
    return(.bidser_empty_resolved_meta_dt())
  }
  for (nm in c("path", "scope")) {
    if (!nm %in% names(dt)) {
      dt[[nm]] <- character(nrow(dt))
    }
    dt[[nm]] <- as.character(dt[[nm]])
  }
  if (!"data" %in% names(dt)) {
    dt$data <- rep(list(list()), nrow(dt))
  }
  if (!"deps" %in% names(dt)) {
    dt$deps <- rep(list(character(0)), nrow(dt))
  }

  dt <- dt[, c("path", "scope", "data", "deps"), with = FALSE]
  data.table::setkeyv(dt, c("path", "scope"))
  dt
}

#' @keywords internal
#' @noRd
.bidser_index_cache_key <- function(x) {
  paste(normalizePath(x$path, winslash = "/", mustWork = FALSE), x$index_path %||% "", sep = "::")
}

#' @keywords internal
#' @noRd
.bidser_get_session_index_state <- function(x) {
  key <- .bidser_index_cache_key(x)
  if (exists(key, envir = bidser_pkg_env, inherits = FALSE)) {
    get(key, envir = bidser_pkg_env, inherits = FALSE)
  } else {
    NULL
  }
}

#' @keywords internal
#' @noRd
.bidser_set_session_index_state <- function(x, state) {
  assign(.bidser_index_cache_key(x), state, envir = bidser_pkg_env)
  invisible(state)
}

#' @keywords internal
#' @noRd
.bidser_index_state_manifest <- function(state) {
  if (is.null(state) || is.null(state$manifest)) {
    return(.bidser_empty_manifest_dt())
  }
  .bidser_finalize_manifest_dt(state$manifest)
}

#' @keywords internal
#' @noRd
.bidser_index_state_manifest_tibble <- function(state) {
  tibble::as_tibble(.bidser_index_state_manifest(state))
}

#' @keywords internal
#' @noRd
.bidser_make_index_state <- function(manifest, sidecars, resolved_meta = NULL) {
  list(
    backend = "data.table",
    version = .bidser_index_schema_version(),
    manifest = .bidser_finalize_manifest_dt(manifest),
    sidecars = .bidser_finalize_sidecar_dt(sidecars),
    resolved_meta = .bidser_finalize_resolved_meta_dt(
      resolved_meta %||% .bidser_empty_resolved_meta_dt()
    )
  )
}

#' @keywords internal
#' @noRd
.bidser_is_index_state <- function(obj) {
  is.list(obj) &&
    identical(obj$backend, "data.table") &&
    identical(as.integer(obj$version %||% -1L), .bidser_index_schema_version())
}

#' @keywords internal
#' @noRd
.bidser_coerce_index_state <- function(obj) {
  if (.bidser_is_index_state(obj)) {
    return(.bidser_make_index_state(
      manifest = obj$manifest,
      sidecars = obj$sidecars,
      resolved_meta = obj$resolved_meta
    ))
  }

  NULL
}

#' @keywords internal
#' @noRd
.bidser_file_signature_dt <- function(x, rel_paths) {
  rel_paths <- unique(as.character(rel_paths %||% character(0)))
  rel_paths <- rel_paths[nzchar(rel_paths)]
  if (length(rel_paths) == 0) {
    dt <- data.table::data.table(path = character(0), size = numeric(0), file_mtime = numeric(0))
    data.table::setkeyv(dt, "path")
    return(dt)
  }

  abs_paths <- file.path(x$path, rel_paths)
  info <- file.info(abs_paths)
  dt <- data.table::data.table(
    path = rel_paths,
    size = as.numeric(info$size),
    file_mtime = as.numeric(info$mtime)
  )
  dt <- dt[!is.na(dt$file_mtime), ]
  data.table::setkeyv(dt, "path")
  dt
}

#' @keywords internal
#' @noRd
.bidser_list_indexed_paths <- function(x) {
  search_files(x, regex = ".*", full_path = FALSE, strict = FALSE) %||% character(0)
}

#' @keywords internal
#' @noRd
.bidser_list_sidecar_paths <- function(x) {
  json_abs <- list.files(
    x$path,
    pattern = "\\.json$",
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = FALSE
  )
  if (length(json_abs) == 0) {
    return(character(0))
  }

  rel <- vapply(json_abs, function(p) .bidser_to_relative_path(x$path, p), character(1))
  unique(rel[nzchar(rel)])
}

#' @keywords internal
#' @noRd
.bidser_sidecar_row_from_path <- function(x, rel_path) {
  rel_path <- .bidser_to_relative_path(x$path, rel_path)
  abs_path <- file.path(x$path, rel_path)
  info <- file.info(abs_path)

  encoded <- tryCatch(encode(basename(rel_path)), error = function(e) NULL)
  parsed <- .bidser_parse_entities_from_path(rel_path)
  if (is.null(encoded)) {
    encoded <- list()
  }
  all_info <- utils::modifyList(parsed, encoded, keep.null = TRUE)

  entity_fields <- .bidser_index_entity_fields()
  specificity <- length(setdiff(names(parsed), "kind"))
  directory <- dirname(rel_path)
  if (identical(directory, ".")) {
    directory <- ""
  }

  data_value <- tryCatch(
    jsonlite::read_json(abs_path, simplifyVector = TRUE),
    error = function(e) list()
  )
  if (is.null(data_value) || !is.list(data_value)) {
    data_value <- list()
  }

  row <- c(
    list(
      path = rel_path,
      file = basename(rel_path),
      directory = directory,
      scope = if (.bidser_is_derivative_path(x, rel_path)) "derivatives" else "raw",
      pipeline = .bidser_path_pipeline(x, rel_path),
      size = as.numeric(info$size),
      file_mtime = as.numeric(info$mtime),
      specificity = as.integer(specificity),
      data = list(data_value)
    ),
    setNames(lapply(entity_fields, function(k) {
      val <- all_info[[k]]
      if (is.null(val) || length(val) == 0) NA_character_ else as.character(val[[1]])
    }), entity_fields)
  )

  .bidser_finalize_sidecar_dt(data.table::as.data.table(row))
}

#' @keywords internal
#' @noRd
.bidser_build_manifest_dt <- function(x) {
  rel_paths <- .bidser_list_indexed_paths(x)
  if (length(rel_paths) == 0) {
    return(.bidser_empty_manifest_dt())
  }

  rows <- lapply(rel_paths, function(p) data.table::as.data.table(.bidser_index_row_from_path(x, p)))
  .bidser_finalize_manifest_dt(data.table::rbindlist(rows, fill = TRUE))
}

#' @keywords internal
#' @noRd
.bidser_build_sidecars_dt <- function(x) {
  rel_paths <- .bidser_list_sidecar_paths(x)
  if (length(rel_paths) == 0) {
    return(.bidser_empty_sidecar_dt())
  }

  rows <- lapply(rel_paths, function(p) .bidser_sidecar_row_from_path(x, p))
  .bidser_finalize_sidecar_dt(data.table::rbindlist(rows, fill = TRUE))
}

#' @keywords internal
#' @noRd
.bidser_refresh_manifest_dt <- function(x, manifest) {
  manifest <- .bidser_finalize_manifest_dt(manifest %||% .bidser_empty_manifest_dt())
  current_paths <- .bidser_list_indexed_paths(x)
  current_sig <- .bidser_file_signature_dt(x, current_paths)
  old_sig <- manifest[, c("path", "size", "file_mtime"), with = FALSE]

  added <- setdiff(current_sig$path, old_sig$path)
  deleted <- setdiff(old_sig$path, current_sig$path)
  common_paths <- intersect(old_sig$path, current_sig$path)
  changed <- if (length(common_paths) == 0) {
    character(0)
  } else {
    old_match <- old_sig[match(common_paths, old_sig$path), ]
    new_match <- current_sig[match(common_paths, current_sig$path), ]
    common_paths[
      is.na(old_match$size) | is.na(new_match$size) |
        old_match$size != new_match$size |
        old_match$file_mtime != new_match$file_mtime
    ]
  }

  rebuild_paths <- unique(c(added, changed))
  keep_paths <- setdiff(manifest$path, unique(c(changed, deleted)))
  keep_dt <- manifest[manifest$path %in% keep_paths, ]

  rebuilt <- if (length(rebuild_paths) > 0) {
    data.table::rbindlist(
      lapply(rebuild_paths, function(p) data.table::as.data.table(.bidser_index_row_from_path(x, p))),
      fill = TRUE
    )
  } else {
    .bidser_empty_manifest_dt()
  }

  out <- if (nrow(keep_dt) == 0 && nrow(rebuilt) == 0) {
    .bidser_empty_manifest_dt()
  } else if (nrow(keep_dt) == 0) {
    rebuilt
  } else if (nrow(rebuilt) == 0) {
    keep_dt
  } else {
    data.table::rbindlist(list(keep_dt, rebuilt), fill = TRUE)
  }

  list(
    manifest = .bidser_finalize_manifest_dt(out),
    changed = rebuild_paths,
    deleted = deleted
  )
}

#' @keywords internal
#' @noRd
.bidser_refresh_sidecars_dt <- function(x, sidecars) {
  sidecars <- .bidser_finalize_sidecar_dt(sidecars %||% .bidser_empty_sidecar_dt())
  current_paths <- .bidser_list_sidecar_paths(x)
  current_sig <- .bidser_file_signature_dt(x, current_paths)
  old_sig <- sidecars[, c("path", "size", "file_mtime"), with = FALSE]

  added <- setdiff(current_sig$path, old_sig$path)
  deleted <- setdiff(old_sig$path, current_sig$path)
  common_paths <- intersect(old_sig$path, current_sig$path)
  changed <- if (length(common_paths) == 0) {
    character(0)
  } else {
    old_match <- old_sig[match(common_paths, old_sig$path), ]
    new_match <- current_sig[match(common_paths, current_sig$path), ]
    common_paths[
      is.na(old_match$size) | is.na(new_match$size) |
        old_match$size != new_match$size |
        old_match$file_mtime != new_match$file_mtime
    ]
  }

  rebuild_paths <- unique(c(added, changed))
  keep_paths <- setdiff(sidecars$path, unique(c(changed, deleted)))
  keep_dt <- sidecars[sidecars$path %in% keep_paths, ]

  rebuilt <- if (length(rebuild_paths) > 0) {
    data.table::rbindlist(
      lapply(rebuild_paths, function(p) .bidser_sidecar_row_from_path(x, p)),
      fill = TRUE
    )
  } else {
    .bidser_empty_sidecar_dt()
  }

  out <- if (nrow(keep_dt) == 0 && nrow(rebuilt) == 0) {
    .bidser_empty_sidecar_dt()
  } else if (nrow(keep_dt) == 0) {
    rebuilt
  } else if (nrow(rebuilt) == 0) {
    keep_dt
  } else {
    data.table::rbindlist(list(keep_dt, rebuilt), fill = TRUE)
  }

  list(
    sidecars = .bidser_finalize_sidecar_dt(out),
    changed = rebuild_paths,
    deleted = deleted
  )
}

#' @keywords internal
#' @noRd
.bidser_refresh_resolved_meta_dt <- function(resolved_meta,
                                             changed_sidecars = character(0),
                                             changed_targets = character(0),
                                             deleted_targets = character(0)) {
  resolved_meta <- .bidser_finalize_resolved_meta_dt(
    resolved_meta %||% .bidser_empty_resolved_meta_dt()
  )

  if (nrow(resolved_meta) == 0) {
    return(resolved_meta)
  }

  keep <- rep(TRUE, nrow(resolved_meta))
  if (length(changed_sidecars) > 0) {
    keep <- keep & !vapply(
      resolved_meta$deps,
      function(dep) any(as.character(dep %||% character(0)) %in% changed_sidecars),
      logical(1)
    )
  }

  invalid_targets <- unique(c(changed_targets, deleted_targets))
  if (length(invalid_targets) > 0) {
    keep <- keep & !(resolved_meta$path %in% invalid_targets)
  }

  kept <- if (any(keep)) {
    resolved_meta[keep, ]
  } else {
    .bidser_empty_resolved_meta_dt()
  }

  .bidser_finalize_resolved_meta_dt(kept)
}

#' @keywords internal
#' @noRd
.bidser_build_index_state <- function(x) {
  .bidser_make_index_state(
    manifest = .bidser_build_manifest_dt(x),
    sidecars = .bidser_build_sidecars_dt(x)
  )
}

#' @keywords internal
#' @noRd
.bidser_refresh_index_state <- function(x, state) {
  if (is.null(state) || !.bidser_is_index_state(state)) {
    return(list(state = .bidser_build_index_state(x), changed = TRUE))
  }

  manifest_refresh <- .bidser_refresh_manifest_dt(x, state$manifest)
  sidecar_refresh <- .bidser_refresh_sidecars_dt(x, state$sidecars)
  changed <- length(manifest_refresh$changed) > 0 ||
    length(manifest_refresh$deleted) > 0 ||
    length(sidecar_refresh$changed) > 0 ||
    length(sidecar_refresh$deleted) > 0

  resolved_meta <- if (changed) {
    .bidser_refresh_resolved_meta_dt(
      state$resolved_meta,
      changed_sidecars = unique(c(sidecar_refresh$changed, sidecar_refresh$deleted)),
      changed_targets = manifest_refresh$changed,
      deleted_targets = manifest_refresh$deleted
    )
  } else {
    state$resolved_meta
  }

  list(
    state = .bidser_make_index_state(
      manifest = manifest_refresh$manifest,
      sidecars = sidecar_refresh$sidecars,
      resolved_meta = resolved_meta
    ),
    changed = changed
  )
}

#' @keywords internal
#' @noRd
.bidser_persist_index_state <- function(x, state) {
  if (!is.null(x$index_path) && nzchar(x$index_path)) {
    saveRDS(state, x$index_path)
  }
  .bidser_set_session_index_state(x, state)
  invisible(state)
}

#' @keywords internal
#' @noRd
.bidser_load_cached_index_state <- function(x, refresh = FALSE, persist = FALSE) {
  state <- .bidser_get_session_index_state(x)

  if (is.null(state) && !is.null(x$index_path) && nzchar(x$index_path) && file.exists(x$index_path)) {
    state <- .bidser_coerce_index_state(
      tryCatch(readRDS(x$index_path), error = function(e) NULL)
    )
  }

  if (is.null(state)) {
    return(NULL)
  }

  if (isTRUE(refresh)) {
    refreshed <- .bidser_refresh_index_state(x, state)
    state <- refreshed$state
    if (isTRUE(persist) && isTRUE(refreshed$changed)) {
      .bidser_persist_index_state(x, state)
    } else {
      .bidser_set_session_index_state(x, state)
    }
  } else {
    .bidser_set_session_index_state(x, state)
  }

  state
}

#' @keywords internal
#' @noRd
.bidser_get_or_build_index_state <- function(x, persist = FALSE) {
  state <- .bidser_load_cached_index_state(x, refresh = TRUE, persist = persist)
  if (!is.null(state)) {
    return(state)
  }

  state <- .bidser_build_index_state(x)
  if (isTRUE(persist)) {
    .bidser_persist_index_state(x, state)
  } else {
    .bidser_set_session_index_state(x, state)
  }

  state
}

#' @keywords internal
#' @noRd
.bidser_lookup_resolved_meta <- function(state, path, scope) {
  resolved <- .bidser_finalize_resolved_meta_dt(state$resolved_meta)
  hit <- resolved[resolved$path == path & resolved$scope == scope, ]
  if (nrow(hit) == 0) {
    return(NULL)
  }
  hit$data[[1]]
}

#' @keywords internal
#' @noRd
.bidser_store_resolved_meta <- function(state, path, scope, data, deps = character(0)) {
  resolved <- .bidser_finalize_resolved_meta_dt(state$resolved_meta)
  path_val <- path
  scope_val <- scope
  resolved <- resolved[!(resolved$path == path_val & resolved$scope == scope_val), ]
  resolved <- data.table::rbindlist(
    list(
      resolved,
      data.table::data.table(
        path = path,
        scope = scope,
        data = list(data %||% list()),
        deps = list(as.character(deps %||% character(0)))
      )
    ),
    fill = TRUE
  )
  state$resolved_meta <- .bidser_finalize_resolved_meta_dt(resolved)
  state
}
