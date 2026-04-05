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
