#' Load and cache the BIDS schema
#'
#' Loads the vendored BIDS JSON schema and caches the result in the package
#' environment so repeated calls within a session are free.
#'
#' @param version Character. BIDS schema version to load (default \code{"1.10.1"}).
#' @return A named list representing the compiled schema with top-level keys
#'   \code{objects}, \code{rules}, \code{meta}, etc.
#' @examples
#' \donttest{
#' s <- bids_schema()
#' names(s)
#' }
#' @seealso \code{\link{bids_schema_versions}}
#' @importFrom jsonlite read_json
#' @export
bids_schema <- function(version = "1.10.1") {
  cache_key <- paste0("schema_", version)
  cached <- bidser_pkg_env[[cache_key]]
  if (!is.null(cached)) return(cached)

  schema_file <- system.file(
    "bids-schema", sprintf("schema-%s.json", version),
    package = "bidser"
  )
  if (!nzchar(schema_file)) {
    stop(
      "BIDS schema version '", version, "' not found in bidser installation. ",
      "Available: ", paste(bids_schema_versions(), collapse = ", "),
      call. = FALSE
    )
  }

  schema <- jsonlite::read_json(schema_file, simplifyVector = FALSE)
  bidser_pkg_env[[cache_key]] <- schema
  schema
}


#' List available BIDS schema versions
#'
#' Returns the version strings for all BIDS schema JSON files shipped with
#' this installation of bidser.
#'
#' @return Character vector of available version strings (e.g. \code{"1.10.1"}).
#' @examples
#' \donttest{
#' bids_schema_versions()
#' }
#' @export
bids_schema_versions <- function() {
  schema_dir <- system.file("bids-schema", package = "bidser")
  if (!nzchar(schema_dir)) return(character(0))
  files <- list.files(schema_dir, pattern = "^schema-.*\\.json$")
  sub("^schema-(.+)\\.json$", "\\1", files)
}


#' Extract canonical BIDS entity keys from schema
#'
#' The schema stores entities as a named list (keyed by long name) where each
#' entry contains a \code{name} field with the short key used in filenames
#' (e.g. \code{"sub"}, \code{"ses"}, \code{"task"}, \code{"run"}).
#'
#' @param schema Result of \code{bids_schema()}.
#' @return Character vector of entity keys.
#' @keywords internal
.bids_schema_entities <- function(schema) {
  ents <- schema$objects$entities
  if (is.null(ents)) return(character(0))
  vapply(ents, function(e) if (!is.null(e$name)) e$name else "", character(1))
}


#' Extract valid suffixes from schema
#'
#' The schema stores suffixes as a named list (keyed by long name) where each
#' entry contains a \code{value} field with the actual suffix string used in
#' filenames (e.g. \code{"bold"}, \code{"T1w"}, \code{"events"}).
#'
#' @param schema Result of \code{bids_schema()}.
#' @param datatype Not used currently; reserved for future datatype filtering.
#' @return Character vector of valid suffix strings.
#' @keywords internal
.bids_schema_suffixes <- function(schema, datatype = NULL) {
  suffs <- schema$objects$suffixes
  if (is.null(suffs)) return(character(0))
  vapply(suffs, function(s) if (!is.null(s$value)) s$value else "", character(1))
}


#' Validate a BIDS filename against the schema
#'
#' Checks that the entities and suffix in a BIDS filename are all recognised by
#' the schema.  Unknown entities and suffixes produce warnings; truly
#' unparseable filenames produce issues (hard failures).
#'
#' @param filename Character. Basename of a BIDS file (not the full path).
#' @param schema Result of \code{bids_schema()}.
#' @return A list with fields:
#'   \describe{
#'     \item{valid}{logical — \code{TRUE} when there are no hard issues.}
#'     \item{issues}{character vector of hard issues.}
#'     \item{warnings}{character vector of soft warnings.}
#'   }
#' @keywords internal
.bids_schema_validate_filename <- function(filename, schema) {
  issues   <- character(0)
  warnings <- character(0)

  # Strip known compound and simple extensions in priority order.
  bare <- filename
  for (ext in c(".nii.gz", ".nii", ".tsv", ".json", ".bvec", ".bval",
                ".gz", ".txt", ".csv", ".png", ".svg", ".html")) {
    if (endsWith(bare, ext)) {
      bare <- substr(bare, 1L, nchar(bare) - nchar(ext))
      break
    }
  }

  parts <- strsplit(bare, "_", fixed = TRUE)[[1]]
  if (length(parts) == 0L) {
    return(list(
      valid    = FALSE,
      issues   = paste("Cannot parse filename:", filename),
      warnings = character(0)
    ))
  }

  # Last part is the suffix (no "key-value" structure).
  suffix_part  <- parts[length(parts)]
  entity_parts <- parts[-length(parts)]

  # Validate suffix.
  valid_suffixes <- .bids_schema_suffixes(schema)
  if (length(valid_suffixes) > 0L && !suffix_part %in% valid_suffixes) {
    warnings <- c(warnings,
                  paste0("Unknown suffix '", suffix_part, "' in: ", filename))
  }

  # Validate entity key-value pairs.
  valid_entity_keys <- .bids_schema_entities(schema)
  for (part in entity_parts) {
    kv <- strsplit(part, "-", fixed = TRUE)[[1]]
    if (length(kv) < 2L) {
      warnings <- c(warnings,
                    paste0("Malformed entity '", part, "' in: ", filename))
      next
    }
    key <- kv[1]
    if (length(valid_entity_keys) > 0L && !key %in% valid_entity_keys) {
      warnings <- c(warnings,
                    paste0("Unknown entity key '", key, "' in: ", filename))
    }
  }

  list(
    valid    = length(issues) == 0L,
    issues   = issues,
    warnings = warnings
  )
}


#' Run schema validation across all files in a bids_project tree
#'
#' Walks all leaf nodes of the project's \code{bids_tree}, validates each
#' BIDS-like filename against the schema, and returns any warnings or issues
#' found.  Files under \code{derivatives/} sub-trees are skipped because the
#' BIDS schema's raw-file rules do not apply to preprocessed outputs.
#'
#' @param x A \code{bids_project} object.
#' @param schema Result of \code{bids_schema()}.
#' @return Character vector of warning/issue strings (empty if all files pass).
#' @keywords internal
.bidser_schema_check_tree <- function(x, schema) {
  if (!requireNamespace("data.tree", quietly = TRUE)) return(character(0))

  tree <- x$bids_tree
  if (is.null(tree)) return(character(0))

  all_warnings <- character(0)

  # Use data.tree's Get() with isLeaf filter — same pattern used elsewhere in
  # bids.R.  We collect the node names of all leaves first.
  leaf_names <- tree$Get(
    "name",
    filterFun = function(node) {
      if (!node$isLeaf) return(FALSE)
      # Skip derivative sub-trees: check if any ancestor is "derivatives"
      anc <- node$parent
      while (!is.null(anc)) {
        if (!is.null(anc$name) && anc$name == "derivatives") return(FALSE)
        anc <- anc$parent
      }
      TRUE
    }
  )

  if (is.null(leaf_names) || length(leaf_names) == 0L) return(character(0))

  for (fname in leaf_names) {
    # Only validate files with BIDS-like names (contain "_" and ".")
    if (!grepl("_", fname, fixed = TRUE)) next
    if (!grepl(".", fname, fixed = TRUE)) next
    if (startsWith(fname, ".")) next

    result <- .bids_schema_validate_filename(fname, schema)
    all_warnings <- c(all_warnings, result$warnings, result$issues)
  }

  unique(all_warnings)
}
