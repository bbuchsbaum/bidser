#' List derivative pipelines in a BIDS project
#'
#' Returns a tibble describing the derivative pipelines currently attached to a
#' `bids_project`. This is the preferred inspection entry point for new code;
#' legacy `fmriprep` / `prep_dir` fields remain available for compatibility.
#'
#' @param x A `bids_project` object.
#' @return A tibble with one row per derivative pipeline and columns `pipeline`,
#'   `root`, `description`, and `source`.
#' @export
#' @examples
#' \donttest{
#' tryCatch({
#'   ds_path <- get_example_bids_dataset("ds000001-fmriprep")
#'   proj <- bids_project(ds_path, fmriprep = TRUE)
#'   derivative_pipelines(proj)
#'   unlink(ds_path, recursive = TRUE)
#' }, error = function(e) message("Example requires internet: ", e$message))
#' }
derivative_pipelines <- function(x) {
  if (!inherits(x, "bids_project")) {
    stop("`x` must be a `bids_project` object.")
  }

  .bidser_derivative_registry(x)
}

#' Query derivative files from a BIDS project
#'
#' Convenience wrapper around [query_files()] that restricts results to
#' derivative pipelines.
#'
#' @param x A `bids_project` object.
#' @param pipeline Optional character vector of pipeline names to include.
#'   When `NULL` (default), files from all discovered pipelines are returned.
#' @param ... Additional arguments passed to [query_files()].
#' @return Character vector of file paths (or tibble when `return = "tibble"`).
#' @export
#' @examples
#' \donttest{
#' tryCatch({
#'   ds_path <- get_example_bids_dataset("ds000001-fmriprep")
#'   proj <- bids_project(ds_path, fmriprep = TRUE)
#'   df <- derivative_files(proj)
#'   unlink(ds_path, recursive = TRUE)
#' }, error = function(e) message("Example requires internet: ", e$message))
#' }
derivative_files <- function(x, pipeline = NULL, ...) {
  if (!inherits(x, "bids_project")) {
    stop("`x` must be a `bids_project` object.")
  }

  query_files(x, scope = "derivatives", pipeline = pipeline, ...)
}

#' Build or retrieve a persistent file index for a BIDS project
#'
#' Creates a lightweight on-disk index of files and parsed BIDS entities. The
#' index is stored as an RDS file and does not change user-facing query
#' semantics; it simply provides a cached tabular representation of the project.
#'
#' @param x A `bids_project` object.
#' @param rebuild If `TRUE`, rebuild the index even if one is already available.
#' @param persist If `TRUE`, save the index to `x$index_path`.
#' @return A tibble describing indexed files.
#' @export
#' @examples
#' \donttest{
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path)
#'   idx <- bids_index(proj, persist = FALSE)
#'   print(idx)
#'   unlink(ds001_path, recursive = TRUE)
#' }, error = function(e) message("Example requires internet: ", e$message))
#' }
bids_index <- function(x, rebuild = FALSE, persist = TRUE) {
  if (!inherits(x, "bids_project")) {
    stop("`x` must be a `bids_project` object.")
  }

  if (!isTRUE(rebuild)) {
    state <- .bidser_load_cached_index_state(x, refresh = TRUE, persist = persist)
    if (!is.null(state)) {
      return(.bidser_index_state_manifest_tibble(state))
    }
  }

  state <- .bidser_build_index_state(x)
  if (isTRUE(persist)) {
    .bidser_persist_index_state(x, state)
  } else {
    .bidser_set_session_index_state(x, state)
  }

  .bidser_index_state_manifest_tibble(state)
}

#' @keywords internal
#' @noRd
.bidser_empty_run_keys <- function() {
  tibble::tibble(
    .subid = character(0),
    .session = character(0),
    .task = character(0),
    .run = character(0)
  )
}

#' @keywords internal
#' @noRd
.bidser_normalize_key_col <- function(x, n = length(x)) {
  if (is.null(x)) {
    return(rep("", n))
  }

  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

#' @keywords internal
#' @noRd
.bidser_run_keys_from_df <- function(df, has_sessions = TRUE) {
  if (is.null(df) || nrow(df) == 0) {
    return(.bidser_empty_run_keys())
  }

  n <- nrow(df)

  subid <- if (".subid" %in% names(df)) {
    df$.subid
  } else if ("participant_id" %in% names(df)) {
    stringr::str_remove(df$participant_id, "^sub-")
  } else if ("subid" %in% names(df)) {
    df$subid
  } else {
    rep("", n)
  }

  session <- if (".session" %in% names(df)) {
    df$.session
  } else if ("session" %in% names(df)) {
    df$session
  } else {
    rep("", n)
  }

  task <- if (".task" %in% names(df)) {
    df$.task
  } else if ("task" %in% names(df)) {
    df$task
  } else {
    rep("", n)
  }

  run <- if (".run" %in% names(df)) {
    df$.run
  } else if ("run" %in% names(df)) {
    df$run
  } else {
    rep("", n)
  }

  tibble::tibble(
    .subid = .bidser_normalize_key_col(subid, n),
    .session = .bidser_normalize_key_col(session, n),
    .task = .bidser_normalize_key_col(task, n),
    .run = .bidser_normalize_key_col(run, n)
  ) |>
    dplyr::mutate(
      .session = if (isTRUE(has_sessions)) .session else dplyr::if_else(.session == "1", "", .session)
    )
}

#' @keywords internal
#' @noRd
.bidser_nest_by_run <- function(df, data_col, count_col) {
  key_cols <- c(".subid", ".session", ".task", ".run")
  if (is.null(df) || nrow(df) == 0) {
    return(.bidser_empty_run_keys())
  }

  groups <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) |>
    dplyr::group_split(.keep = TRUE)

  dplyr::bind_rows(lapply(groups, function(g) {
    key <- tibble::as_tibble(g[1, key_cols, drop = FALSE])
    payload <- tibble::as_tibble(g[, setdiff(names(g), key_cols), drop = FALSE])
    key[[data_col]] <- list(payload)
    key[[count_col]] <- nrow(payload)
    key
  }))
}

#' @keywords internal
#' @noRd
.bidser_events_flat <- function(x, subid = ".*", task = ".*", run = ".*", session = ".*", ...) {
  out <- tryCatch(
    load_all_events(x, subid = subid, task = task, run = run, session = session, ...),
    error = function(e) tibble::tibble()
  )

  if (nrow(out) == 0) {
    return(.bidser_empty_run_keys())
  }

  out
}

#' @keywords internal
#' @noRd
.bidser_confounds_flat <- function(x, subid = ".*", task = ".*", run = ".*", session = ".*", ...) {
  out <- tryCatch(
    read_confounds(x, subid = subid, task = task, run = run, session = session, nest = FALSE, ...),
    error = function(e) NULL
  )

  if (is.null(out) || nrow(out) == 0) {
    return(.bidser_empty_run_keys())
  }

  tibble::as_tibble(out)
}

#' Create a tidy run-level variables table
#'
#' Aggregates event tables, confound tables, and scan inventory into a single
#' run-level tibble with normalized identifiers and nested list-columns. This is
#' a lightweight variables layer for downstream R workflows rather than a full
#' model-spec engine.
#'
#' @param x A `bids_project` object.
#' @param subid Regex pattern to match subject IDs.
#' @param task Regex pattern to match tasks.
#' @param run Regex pattern to match runs.
#' @param session Regex pattern to match sessions.
#' @param include Which variable sources to include. Supported values are
#'   `"events"` and `"confounds"`.
#' @param scope Query scope used for scan inventory. One of `"all"`, `"raw"`, or
#'   `"derivatives"`.
#' @param pipeline Optional derivative pipeline name to restrict derivative scan
#'   inventory.
#' @return A tibble with one row per run and list-columns `scans`, `events`, and
#'   `confounds` when available.
#' @export
#' @examples
#' \donttest{
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path)
#'   vt <- variables_table(proj)
#'   print(vt)
#'   unlink(ds001_path, recursive = TRUE)
#' }, error = function(e) message("Example requires internet: ", e$message))
#' }
variables_table <- function(x, subid = ".*", task = ".*", run = ".*", session = ".*",
                            include = c("events", "confounds"),
                            scope = c("all", "raw", "derivatives"),
                            pipeline = NULL) {
  if (!inherits(x, "bids_project")) {
    stop("`x` must be a `bids_project` object.")
  }

  scope <- match.arg(scope)
  include <- unique(intersect(include, c("events", "confounds")))

  scans <- query_files(
    x,
    regex = "bold\\.nii(\\.gz)?$",
    subid = subid,
    task = task,
    run = run,
    session = session,
    scope = scope,
    pipeline = pipeline,
    return = "tibble",
    use_index = "auto"
  )

  scan_runs <- if (nrow(scans) > 0) {
    dplyr::bind_cols(
      .bidser_run_keys_from_df(scans, has_sessions = x$has_sessions),
      scans |>
        dplyr::transmute(
          scan_path = path,
          scan_scope = scope,
          scan_pipeline = dplyr::coalesce(pipeline, "")
        )
    )
  } else {
    dplyr::bind_cols(
      .bidser_empty_run_keys(),
      tibble::tibble(
        scan_path = character(0),
        scan_scope = character(0),
        scan_pipeline = character(0)
      )
    )
  }

  events <- if ("events" %in% include) {
    .bidser_events_flat(x, subid = subid, task = task, run = run, session = session)
  } else {
    .bidser_empty_run_keys()
  }

  confounds <- if ("confounds" %in% include) {
    .bidser_confounds_flat(x, subid = subid, task = task, run = run, session = session)
  } else {
    .bidser_empty_run_keys()
  }

  run_keys <- dplyr::bind_rows(
    .bidser_run_keys_from_df(scan_runs, has_sessions = x$has_sessions),
    .bidser_run_keys_from_df(events, has_sessions = x$has_sessions),
    .bidser_run_keys_from_df(confounds, has_sessions = x$has_sessions)
  ) |>
    dplyr::distinct() |>
    dplyr::arrange(.subid, .session, .task, .run)

  if (nrow(run_keys) == 0) {
    return(run_keys)
  }

  out <- run_keys

  if (nrow(scan_runs) > 0) {
    scan_nested <- .bidser_nest_by_run(scan_runs, data_col = "scans", count_col = "n_scans")
    out <- dplyr::left_join(out, scan_nested, by = c(".subid", ".session", ".task", ".run"))
  }

  if ("events" %in% include && nrow(events) > 0) {
    event_rows <- dplyr::bind_cols(
      .bidser_run_keys_from_df(events, has_sessions = x$has_sessions),
      events[, setdiff(names(events), c(".subid", ".session", ".task", ".run")), drop = FALSE]
    )
    event_nested <- .bidser_nest_by_run(event_rows, data_col = "events", count_col = "n_events")
    out <- dplyr::left_join(out, event_nested, by = c(".subid", ".session", ".task", ".run"))
  }

  if ("confounds" %in% include && nrow(confounds) > 0) {
    confound_rows <- dplyr::bind_cols(
      .bidser_run_keys_from_df(confounds, has_sessions = x$has_sessions),
      confounds[, setdiff(names(confounds), c(".subid", ".session", ".task", ".run")), drop = FALSE]
    )
    confound_nested <- .bidser_nest_by_run(confound_rows, data_col = "confounds", count_col = "n_confound_rows")
    out <- dplyr::left_join(out, confound_nested, by = c(".subid", ".session", ".task", ".run"))
  }

  out
}

#' Assemble lightweight report data for a BIDS project
#'
#' Produces a structured report payload composed from dataset summary,
#' compliance checks, derivative pipeline discovery, and run-level variables
#' coverage.
#'
#' @param x A `bids_project` object.
#' @param ... Additional arguments passed to [variables_table()].
#' @return A list with report-ready summary tables and metadata.
#' @export
#' @examples
#' \donttest{
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path)
#'   rd <- bids_report_data(proj)
#'   unlink(ds001_path, recursive = TRUE)
#' }, error = function(e) message("Example requires internet: ", e$message))
#' }
bids_report_data <- function(x, ...) {
  if (!inherits(x, "bids_project")) {
    stop("`x` must be a `bids_project` object.")
  }

  vars <- variables_table(x, ...)
  list(
    project = tibble::tibble(
      name = x$name,
      participants_source = x$participants_source,
      has_index = isTRUE(x$has_index) || (!is.null(x$index_path) && file.exists(x$index_path)),
      default_pipeline = x$default_pipeline
    ),
    summary = bids_summary(x),
    compliance = bids_check_compliance(x),
    pipelines = derivative_pipelines(x),
    variables = vars,
    run_coverage = if (nrow(vars) > 0) {
      cov <- vars |>
        dplyr::transmute(
          .subid,
          .session,
          .task,
          .run,
          n_scans = dplyr::coalesce(n_scans, 0L)
        )
      cov$n_events <- if ("n_events" %in% names(vars)) {
        dplyr::coalesce(vars$n_events, 0L)
      } else {
        0L
      }
      cov$n_confound_rows <- if ("n_confound_rows" %in% names(vars)) {
        dplyr::coalesce(vars$n_confound_rows, 0L)
      } else {
        0L
      }
      cov
    } else {
      tibble::tibble(
        .subid = character(0),
        .session = character(0),
        .task = character(0),
        .run = character(0),
        n_scans = integer(0),
        n_events = integer(0),
        n_confound_rows = integer(0)
      )
    }
  )
}

#' Create a lightweight text report for a BIDS project
#'
#' @param x A `bids_project` object.
#' @param ... Additional arguments passed to [bids_report_data()].
#' @return An object of class `bids_report`.
#' @export
#' @examples
#' \donttest{
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path)
#'   rpt <- bids_report(proj)
#'   print(rpt)
#'   unlink(ds001_path, recursive = TRUE)
#' }, error = function(e) message("Example requires internet: ", e$message))
#' }
bids_report <- function(x, ...) {
  structure(
    bids_report_data(x, ...),
    class = "bids_report"
  )
}

#' @export
print.bids_report <- function(x, ...) {
  task_names <- if (nrow(x$summary$tasks) > 0) {
    paste(x$summary$tasks$task, collapse = ", ")
  } else {
    "(none)"
  }

  cat("BIDS Report\n")
  cat("Project:", x$project$name, "\n")
  cat("Participants source:", x$project$participants_source, "\n")
  cat("Subjects:", x$summary$n_subjects, "\n")
  cat("Sessions:", if (is.null(x$summary$n_sessions)) 0 else x$summary$n_sessions, "\n")
  cat("Tasks:", task_names, "\n")
  cat("Total runs:", x$summary$total_runs, "\n")
  cat("Compliance:", if (isTRUE(x$compliance$passed)) "passed" else "issues found", "\n")
  cat("Index:", if (isTRUE(x$project$has_index)) "available" else "not available", "\n")

  if (nrow(x$pipelines) > 0) {
    cat("Pipelines:", paste(x$pipelines$pipeline, collapse = ", "), "\n")
  }
  if (nrow(x$run_coverage) > 0) {
    cat("Indexed runs:", nrow(x$run_coverage), "\n")
  }

  invisible(x)
}
