#' Datatype registry for bidser
#'
#' The registry stores named entries for each BIDS datatype known to the
#' package (built-ins) plus any user-registered custom datatypes.  It is
#' initialised during `.onLoad` via `.register_builtin_datatypes()`.
#'
#' The registry lives on the package-level environment `bidser_pkg_env` which
#' is defined in `R/bidser_env.R`.

# ---------------------------------------------------------------------------
# Internal registry accessors
# ---------------------------------------------------------------------------

.bidser_get_registry <- function() {
  if (!exists(".bidser_registry", envir = bidser_pkg_env, inherits = FALSE)) {
    reg <- new.env(parent = emptyenv())
    reg$datatypes <- list()
    reg$builtin_names <- character(0)
    assign(".bidser_registry", reg, envir = bidser_pkg_env)
  }
  get(".bidser_registry", envir = bidser_pkg_env, inherits = FALSE)
}

# ---------------------------------------------------------------------------
# Internal: register the 5 built-in datatypes (called from .onLoad)
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.register_builtin_datatypes <- function() {
  reg <- .bidser_get_registry()

  # Reset to ensure idempotency during testing / re-loads
  reg$datatypes <- list()
  reg$builtin_names <- character(0)

  builtins <- list(
    list(
      name      = "func",
      spec      = func_spec(),
      parser_fn = func_parser(),
      folder    = "func",
      scope     = "raw"
    ),
    list(
      name      = "anat",
      spec      = anat_spec(),
      parser_fn = anat_parser(),
      folder    = "anat",
      scope     = "raw"
    ),
    list(
      name      = "fmap",
      spec      = fmapspec(),
      parser_fn = fmap_parser(),
      folder    = "fmap",
      scope     = "raw"
    ),
    list(
      name      = "funcprep",
      spec      = funcprepspec(),
      parser_fn = fmriprep_func_parser(),
      folder    = "func",
      scope     = "derivative"
    ),
    list(
      name      = "anatprep",
      spec      = anatprepspec(),
      parser_fn = fmriprep_anat_parser(),
      folder    = "anat",
      scope     = "derivative"
    )
  )

  for (entry in builtins) {
    entry$builtin <- TRUE
    reg$datatypes[[entry$name]] <- entry
    reg$builtin_names <- c(reg$builtin_names, entry$name)
  }

  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Register a custom BIDS datatype
#'
#' Adds a new datatype to the bidser runtime registry so that
#' \code{\link{bids_project}} will scan and parse its folder.  The five
#' built-in datatypes (\code{"func"}, \code{"anat"}, \code{"fmap"},
#' \code{"funcprep"}, \code{"anatprep"}) can only be overwritten by passing
#' \code{overwrite = TRUE}.
#'
#' @param name A non-empty character string naming the datatype.
#' @param spec A spec list as returned by \code{func_spec()} etc.
#' @param parser_fn A parser object as returned by \code{func_parser()} etc.
#' @param folder The folder name to scan inside each subject/session directory.
#'   Defaults to \code{name}.
#' @param scope One of \code{"raw"}, \code{"derivative"}, or \code{"both"}.
#' @param overwrite Logical.  If \code{FALSE} (default) an error is thrown when
#'   attempting to overwrite an existing registration.
#'
#' @return The \code{name} string, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' my_spec   <- func_spec()   # placeholder
#' my_parser <- func_parser() # placeholder
#' register_datatype("dwi", spec = my_spec, parser_fn = my_parser,
#'                   folder = "dwi", scope = "raw")
#' list_datatypes()
#' }
register_datatype <- function(name,
                               spec,
                               parser_fn,
                               folder    = name,
                               scope     = c("raw", "derivative", "both"),
                               overwrite = FALSE) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("`name` must be a non-empty character string.")
  }

  scope <- match.arg(scope)
  reg   <- .bidser_get_registry()

  if (name %in% reg$builtin_names && !isTRUE(overwrite)) {
    stop(
      "Cannot overwrite built-in datatype '", name,
      "'. Use overwrite = TRUE to force."
    )
  }

  if (!is.null(reg$datatypes[[name]]) && !isTRUE(overwrite)) {
    stop("Datatype '", name, "' already registered. Use overwrite = TRUE.")
  }

  reg$datatypes[[name]] <- list(
    name      = name,
    spec      = spec,
    parser_fn = parser_fn,
    folder    = folder,
    scope     = scope,
    builtin   = FALSE
  )

  invisible(name)
}


#' List registered BIDS datatypes
#'
#' Returns the names of all datatypes currently registered in the bidser
#' runtime registry.
#'
#' @param scope One of \code{"all"} (default), \code{"raw"},
#'   \code{"derivative"}, or \code{"both"}.  Filters by scope.
#'
#' @return A character vector of datatype names.
#' @export
#'
#' @examples
#' list_datatypes()
#' list_datatypes(scope = "raw")
list_datatypes <- function(scope = c("all", "raw", "derivative", "both")) {
  scope <- match.arg(scope)
  reg   <- .bidser_get_registry()

  if (identical(scope, "all")) {
    return(names(reg$datatypes))
  }

  nms <- vapply(reg$datatypes, function(e) {
    isTRUE(e$scope == scope)
  }, logical(1))
  names(reg$datatypes)[nms]
}


#' @keywords internal
#' @noRd
get_datatype_spec <- function(name) {
  reg <- .bidser_get_registry()
  entry <- reg$datatypes[[name]]
  if (is.null(entry)) {
    stop("No datatype registered as '", name, "'.")
  }
  entry
}


#' Build a custom BIDS datatype spec
#'
#' Constructs a \code{parser_spec} list suitable for passing to
#' \code{\link{register_datatype}}.  Standard BIDS entity keys are recognised
#' automatically and filled with canonical regex patterns and ordering; unknown
#' keys fall back to \code{[A-Za-z0-9]+} and are appended after standard ones.
#'
#' @param type Non-empty character string naming the datatype (e.g.
#'   \code{"dwi"}).
#' @param entities Character vector of BIDS entity keys to include (e.g.
#'   \code{c("sub", "ses", "acq", "run")}).  \code{"sub"} is always present.
#' @param required Character vector of entity keys that are required
#'   (non-optional).  Defaults to \code{"sub"}.
#' @param suffixes Named list mapping kind strings to one or more file
#'   extensions.  Each value may be a single string or a character vector
#'   (e.g. \code{list(dwi = c(".nii.gz", ".nii", ".bvec", ".bval", ".json"))}).
#'
#' @return A \code{parser_spec} list with elements \code{keystruc},
#'   \code{kinds}, and \code{type}, ready for \code{\link{gen_parser}} and
#'   \code{\link{register_datatype}}.
#' @export
#'
#' @examples
#' spec <- bids_datatype_spec(
#'   type     = "dwi",
#'   entities = c("sub", "ses", "acq", "run"),
#'   suffixes = list(
#'     dwi   = c(".nii.gz", ".nii", ".bvec", ".bval", ".json"),
#'     sbref = c(".nii.gz", ".nii")
#'   )
#' )
#' \dontrun{
#' register_datatype("dwi", spec = spec, parser_fn = gen_parser(spec),
#'                   folder = "dwi", scope = "raw")
#' }
bids_datatype_spec <- function(type,
                                entities = c("sub", "ses"),
                                required = "sub",
                                suffixes = list()) {
  if (!is.character(type) || length(type) != 1L || !nzchar(type)) {
    stop("`type` must be a non-empty character string.")
  }
  if (!is.character(entities) || length(entities) == 0L) {
    stop("`entities` must be a non-empty character vector of BIDS entity keys.")
  }
  if (!is.character(required)) {
    stop("`required` must be a character vector of entity keys.")
  }
  if (!is.list(suffixes) || length(suffixes) == 0L || is.null(names(suffixes))) {
    stop("`suffixes` must be a non-empty named list.")
  }

  # Ensure "sub" is always present
  if (!"sub" %in% entities) entities <- c("sub", entities)

  # Canonical BIDS entity table
  .known <- list(
    sub   = list(name = "subid",         pattern = "[A-Za-z0-9]+", order = 1L),
    ses   = list(name = "session",        pattern = "[A-Za-z0-9]+", order = 2L),
    task  = list(name = "task",           pattern = "[A-Za-z0-9]+", order = 3L),
    run   = list(name = "run",            pattern = "[0-9]+",        order = 4L),
    acq   = list(name = "acquisition",    pattern = "[A-Za-z0-9]+", order = 5L),
    ce    = list(name = "contrast",       pattern = "[A-Za-z0-9]+", order = 6L),
    rec   = list(name = "reconstruction", pattern = "[A-Za-z0-9]+", order = 7L),
    echo  = list(name = "echo",           pattern = "[0-9]+",        order = 8L),
    dir   = list(name = "dir",            pattern = "[A-Za-z0-9]+", order = 9L),
    space = list(name = "space",          pattern = "[A-Za-z0-9]+", order = 10L),
    res   = list(name = "res",            pattern = "[A-Za-z0-9]+", order = 11L),
    desc  = list(name = "desc",           pattern = "[A-Za-z0-9]+", order = 12L),
    label = list(name = "label",          pattern = "[A-Za-z0-9]+", order = 13L),
    hemi  = list(name = "hemi",           pattern = "[LR]",          order = 14L),
    from  = list(name = "from",           pattern = "[A-Za-z0-9]+", order = 15L),
    to    = list(name = "to",             pattern = "[A-Za-z0-9]+", order = 16L),
    mod   = list(name = "mod",            pattern = "[A-Za-z0-9]+", order = 17L)
  )

  rows <- lapply(seq_along(entities), function(i) {
    key  <- entities[[i]]
    info <- .known[[key]]
    if (is.null(info)) {
      list(name = key, key = key, optional = !key %in% required,
           pattern = "[A-Za-z0-9]+", order = 100L + i)
    } else {
      list(name = info$name, key = key, optional = !key %in% required,
           pattern = info$pattern, order = info$order)
    }
  })

  keystruc <- tibble::tibble(
    name     = vapply(rows, `[[`, "", "name"),
    key      = vapply(rows, `[[`, "", "key"),
    optional = vapply(rows, `[[`, FALSE, "optional"),
    pattern  = vapply(rows, `[[`, "", "pattern"),
    order    = vapply(rows, `[[`, 0L, "order")
  )

  # Build kinds tibble: single-extension values stay scalar, multi become list
  kind_suffs <- lapply(suffixes, function(s) {
    if (length(s) == 1L) s else as.list(s)
  })
  kinds <- tibble::tibble(kind = names(suffixes), suffix = kind_suffs)

  spec <- list(keystruc = keystruc, kinds = kinds, type = type)
  class(spec) <- c(paste0(type, "_spec"), "parser_spec")
  spec
}


#' Unregister a custom BIDS datatype
#'
#' Removes a previously registered custom datatype from the registry.
#' Built-in datatypes cannot be removed.
#'
#' @param name Character string naming the datatype to remove.
#' @return The \code{name} string, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' register_datatype("dwi", spec = func_spec(), parser_fn = func_parser())
#' unregister_datatype("dwi")
#' }
unregister_datatype <- function(name) {
  reg <- .bidser_get_registry()

  if (name %in% reg$builtin_names) {
    stop("Cannot unregister built-in datatype '", name, "'.")
  }

  if (is.null(reg$datatypes[[name]])) {
    stop("No datatype registered as '", name, "'.")
  }

  reg$datatypes[[name]] <- NULL
  invisible(name)
}
