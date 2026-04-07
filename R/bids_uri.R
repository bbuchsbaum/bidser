# R/bids_uri.R
# BIDS URI parsing, construction, and resolution
# Part of bidser milestone 0.6

# ---------------------------------------------------------------------------
# Internal parser
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.parse_bids_uri <- function(uri_string) {
  if (!grepl("^bids:", uri_string)) {
    stop("BIDS URI must start with 'bids:': ", uri_string)
  }
  # Pattern: bids:<dataset_name>:<relative_path>
  # dataset_name may be empty (bids::sub-01/...)
  m <- regmatches(uri_string, regexec("^bids:([^:]*):(.+)$", uri_string))[[1L]]
  if (length(m) != 3L) {
    stop("Invalid BIDS URI -- must contain at least two colons: ", uri_string)
  }
  dataset_name  <- m[[2L]]
  relative_path <- m[[3L]]

  if (grepl("^[/\\\\]", relative_path)) {
    stop(
      "BIDS URI relative_path must not start with '/' or '\\': ",
      uri_string
    )
  }

  list(dataset_name = dataset_name, relative_path = relative_path)
}

# ---------------------------------------------------------------------------
# Constructor
# ---------------------------------------------------------------------------

#' Construct a BIDS URI object
#'
#' Parses a BIDS URI string of the form `bids:<dataset_name>:<relative_path>`
#' and returns an S3 object of class `bids_uri`.
#'
#' @param uri A character scalar BIDS URI, e.g.
#'   `"bids::sub-01/fmap/sub-01_epi.nii.gz"` or
#'   `"bids:deriv1:sub-01/anat/T1w.nii.gz"`.
#'
#' @return An object of class `bids_uri` with fields `dataset_name`,
#'   `relative_path`, and `uri`.
#' @export
#' @examples
#' u <- bids_uri("bids::sub-01/func/sub-01_task-rest_bold.nii.gz")
#' u$dataset_name   # ""
#' u$relative_path  # "sub-01/func/sub-01_task-rest_bold.nii.gz"
#'
#' # URI with a named dataset link
#' u2 <- bids_uri("bids:deriv1:sub-01/anat/sub-01_T1w.nii.gz")
#' u2$dataset_name  # "deriv1"
bids_uri <- function(uri) {
  if (!is.character(uri) || length(uri) != 1L) {
    stop("'uri' must be a character scalar")
  }
  parsed <- .parse_bids_uri(uri)
  structure(
    list(
      dataset_name  = parsed$dataset_name,
      relative_path = parsed$relative_path,
      uri           = uri
    ),
    class = "bids_uri"
  )
}

# ---------------------------------------------------------------------------
# Predicates and coercions
# ---------------------------------------------------------------------------

#' Test whether an object is a BIDS URI
#'
#' @param x Any object.
#' @return `TRUE` if `x` inherits from `"bids_uri"`, `FALSE` otherwise.
#' @export
#' @examples
#' u <- bids_uri("bids::sub-01/anat/sub-01_T1w.nii.gz")
#' is_bids_uri(u)      # TRUE
#' is_bids_uri("bids::sub-01/anat/sub-01_T1w.nii.gz")  # FALSE
is_bids_uri <- function(x) inherits(x, "bids_uri")

#' @export
#' @rdname as_bids_uri
as_bids_uri.character <- function(x, ...) {
  bids_uri(x)
}

#' @export
#' @rdname as_bids_uri
as_bids_uri.bids_uri <- function(x, ...) {
  x
}

# ---------------------------------------------------------------------------
# Resolver methods (generic in R/all_generic.R)
# ---------------------------------------------------------------------------

#' @export
#' @rdname resolve_bids_uri
resolve_bids_uri.bids_uri <- function(uri, description, ..., must_exist = FALSE) {
  # Accept bids_project in place of bids_dataset_description
  if (inherits(description, "bids_project")) {
    description <- description$description
    if (is.null(description)) {
      stop(
        "bids_project has no 'description' field; ",
        "ensure the project was built with a valid dataset_description.json"
      )
    }
  }

  if (!inherits(description, "bids_dataset_description")) {
    stop("'description' must be a 'bids_dataset_description' or 'bids_project' object")
  }

  if (nchar(uri$dataset_name) == 0L) {
    # Empty dataset name -> current dataset root
    base_path <- description$parent_directory
    resolved  <- file.path(base_path, uri$relative_path)
    raw_res   <- resolved
    resolved  <- normalizePath(resolved, mustWork = must_exist)
  } else {
    links <- description$fields$DatasetLinks
    if (is.null(links) || !uri$dataset_name %in% names(links)) {
      available <- if (length(names(links)) > 0L) paste(names(links), collapse = ", ") else "(none)"
      stop(
        "DatasetLinks does not contain '", uri$dataset_name,
        "'. Available keys: ", available
      )
    }
    link_val <- links[[uri$dataset_name]]

    # Remote schemes: return as-is (joined with relative path)
    if (grepl("^(https?://|s3://)", link_val)) {
      result <- paste0(sub("/$", "", link_val), "/", uri$relative_path)
      attr(result, "raw_resolution") <- result
      return(result)
    }

    # Strip file:// scheme
    if (grepl("^file://", link_val)) {
      link_val <- sub("^file://", "", link_val)
    }

    # Normalise link_val first (resolves symlinks when dir exists)
    link_val <- normalizePath(link_val, mustWork = FALSE)
    resolved <- file.path(link_val, uri$relative_path)
    raw_res  <- resolved
    resolved <- normalizePath(resolved, mustWork = must_exist)
  }

  attr(resolved, "raw_resolution") <- raw_res
  resolved
}

#' @export
#' @rdname resolve_bids_uri
resolve_bids_uri.character <- function(uri, description, ..., must_exist = FALSE) {
  resolve_bids_uri(bids_uri(uri), description, ..., must_exist = must_exist)
}

# ---------------------------------------------------------------------------
# S3 methods for bids_uri
# ---------------------------------------------------------------------------

#' @export
print.bids_uri <- function(x, ...) {
  cat("<bids_uri>", format(x), "\n")
  invisible(x)
}

#' @export
format.bids_uri <- function(x, ...) {
  sprintf("bids:%s:%s", x$dataset_name, x$relative_path)
}

#' @export
as.character.bids_uri <- function(x, ...) {
  x$uri
}
