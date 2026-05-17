#' Convenience confound sets for fMRIPrep
#'
#' Provides predefined, version-robust groups of confound variable names as
#' described in the fMRIPrep documentation. These sets abstract over naming
#' changes between fMRIPrep releases via the internal alias resolver used by
#' `read_confounds()`.
#'
#' In addition to exact names, wildcard patterns are supported by the confound
#' resolver:
#' - `prefix*` selects all columns that start with `prefix` (e.g., `cosine_*`,
#'   `motion_outlier_*`, `a_comp_cor_*`).
#' - `prefix*[N]` selects the first `N` matches (e.g., `a_comp_cor_*[6]`).
#' - Suffix combinations such as `_derivative1`, `_power2`, and
#'   `_derivative1_power2` are resolved across both old and new base names
#'   (e.g., `trans_x_derivative1` also finds `X_derivative1` if present).
#'
#' Available sets (case-insensitive):
#' - `"motion6"`: 6 rigid-body motion parameters: `trans_x`, `trans_y`,
#'   `trans_z`, `rot_x`, `rot_y`, `rot_z`.
#' - `"motion12"`: `motion6` + first temporal derivatives (adds
#'   `*_derivative1`).
#' - `"motion24"`: `motion12` + quadratic terms of base and derivatives (adds
#'   `*_power2` and `*_derivative1_power2`).
#' - `"global3"`: global signals: `csf`, `white_matter`, `global_signal`.
#' - `"9p"`: `motion6` + `global3` (9 parameters total).
#' - `"36p"`: `motion24` + `global3` plus their derivatives and quadratics
#'   (i.e., the canonical 36-parameter set).
#' - `"acompcor"`: anatomical CompCor components (`a_comp_cor_*`). Use `n` to
#'   cap the number of components retained, e.g., `n = 6` -> `a_comp_cor_*[6]`.
#' - `"tcompcor"`: temporal CompCor components (`t_comp_cor_*`). Supports `n` as
#'   above.
#' - `"compcor"`: both anatomical and temporal CompCor (applies `n` to each
#'   family if provided).
#' - `"cosine"`: discrete cosine-basis regressors (matches both `cosine_*` and `cosine*`).
#' - `"outliers"`: outlier/censoring covariates including
#'   `framewise_displacement`, `rmsd` (if present), `motion_outlier_*`, and
#'   `non_steady_state_outlier*`.
#' - `"dvars"`: standardized DVARS only (`std_dvars`). This avoids pairing raw
#'   and standardized DVARS by default, which can create collinear nuisance
#'   regressors in downstream models.
#' - `"dvars_family"`: full DVARS family: `dvars`, `std_dvars`, `non_std_dvars`,
#'   `vx_wisestd_dvars` (resolved to whichever names exist in your dataset).
#' - `"raw_dvars"`: raw DVARS only (`dvars`).
#' - `"fd"`: framewise displacement only (`framewise_displacement`).
#'
#' @param name Character. The name of the convenience set (see list above).
#' @param n Optional integer used by CompCor sets to limit the number of
#'   components (e.g., first 5 or 6). Ignored for other sets.
#' @return A character vector of confound variable names and/or wildcard tokens
#'   that can be passed to `read_confounds(..., cvars = confound_set(...))`.
#' @export
#' @examples
#' # Common usage: 24-parameter motion set
#' confound_set("motion24")
#'
#' # 36-parameter model (Satterthwaite/Friston-style)
#' confound_set("36p")
#'
#' # First 6 anatomical CompCor components
#' confound_set("acompcor", n = 6)
#'
#' # All cosine regressors and outlier indicators
#' confound_set("cosine")
#' confound_set("outliers")
confound_set <- function(name, n = NULL) {
  nm <- tolower(as.character(name %||% ""))

  base_motion <- c("trans_x", "trans_y", "trans_z", "rot_x", "rot_y", "rot_z")
  deriv <- paste0(base_motion, "_derivative1")
  quad_base <- paste0(base_motion, "_power2")
  quad_deriv <- paste0(base_motion, "_derivative1_power2")

  global3 <- c("csf", "white_matter", "global_signal")
  global_deriv <- paste0(global3, "_derivative1")
  global_quad_base <- paste0(global3, "_power2")
  global_quad_deriv <- paste0(global3, "_derivative1_power2")

  # CompCor helpers
  acc_all <- if (is.null(n) || is.na(n)) "a_comp_cor_*" else paste0("a_comp_cor_*[", n, "]")
  tcc_all <- if (is.null(n) || is.na(n)) "t_comp_cor_*" else paste0("t_comp_cor_*[", n, "]")

  outliers <- c(
    "framewise_displacement",
    "rmsd",
    "motion_outlier_*",
    "non_steady_state_outlier"
  )

  dvars_family <- c("dvars", "std_dvars", "non_std_dvars", "vx_wisestd_dvars")

  sets <- list(
    motion6  = base_motion,
    motion12 = c(base_motion, deriv),
    motion24 = c(base_motion, deriv, quad_base, quad_deriv),
    global3  = global3,
    `9p`      = c(base_motion, global3),
    `36p`     = c(base_motion, deriv, quad_base, quad_deriv,
                  global3, global_deriv, global_quad_base, global_quad_deriv),
    acomppcor = acc_all,   # accept typo alias just in case
    acompcor  = acc_all,
    tcompcor  = tcc_all,
    compcor   = c(acc_all, tcc_all),
    # include underscore and no-underscore variants (cosine_00 vs cosine00)
    cosine    = c("cosine_*", "cosine*"),
    outliers  = c("framewise_displacement", "rmsd", "motion_outlier_*", "non_steady_state_outlier*"),
    dvars     = "std_dvars",
    dvars_family = dvars_family,
    raw_dvars = "dvars",
    fd        = "framewise_displacement"
  )

  if (!nm %in% names(sets)) {
    stop("Unknown confound set: ", name,
         ". Use list_confound_sets() to see available options.")
  }

  # return as character vector
  unname(unlist(sets[[nm]], use.names = FALSE))
}


#' List available confound sets
#'
#' Returns the names and short descriptions of the predefined confound sets
#' usable with `confound_set()`.
#'
#' @return A data.frame with columns `set` and `description`.
#' @export
#' @examples
#' list_confound_sets()
list_confound_sets <- function() {
  data.frame(
    set = c(
      "motion6", "motion12", "motion24",
      "global3", "9p", "36p", "acompcor", "tcompcor", "compcor",
      "cosine", "outliers", "dvars", "dvars_family", "raw_dvars", "fd"
    ),
    description = c(
      "Rigid-body motion (6 params)",
      "Motion + first derivatives (12)",
      "Friston 24-parameter motion model (24)",
      "CSF, WM, and global signals (3)",
      "9-parameter model: motion6 + CSF + WM + GlobalSignal (9)",
      "36-parameter model: motion24 + globals with derivs/quadratics (36)",
      "Anatomical CompCor components (use n to limit)",
      "Temporal CompCor components (use n to limit)",
      "Both anatomical and temporal CompCor (use n to limit)",
      "Discrete cosine basis regressors",
      "FD/RMSD, motion spike regressors, and nonsteady-state outliers",
      "Standardized DVARS only",
      "Full DVARS family (dvars, std_dvars, non_std_dvars, vx_wisestd_dvars)",
      "Raw DVARS only",
      "Framewise displacement only"
    ),
    stringsAsFactors = FALSE
  )
}


#' Check and clean confound tables
#'
#' `check_confounds()` reports nuisance columns that are unsuitable for model
#' matrices, such as zero-variance columns within a run. `clean_confounds()`
#' drops the flagged columns and stores the diagnostics on the returned object.
#'
#' These helpers understand both nested `bids_confounds` objects returned by
#' `read_confounds(..., nest = TRUE)` and flat confound tables. For flat tables,
#' checks are run within the identifier columns present in the data
#' (`participant_id`, `task`, `session`, and `run` by default).
#'
#' @param x A confound table, typically a `bids_confounds` object.
#' @param checks Character vector of checks to run. Supported values are
#'   `"zero_variance"` and `"rank"`.
#' @param clean Character vector of cleaning operations to apply. Supported
#'   values are `"none"`, `"zero_variance"`, and `"rank"`.
#' @param group_vars Optional character vector of columns defining run-level
#'   groups for flat tables.
#' @param inform Logical. If `TRUE`, report dropped columns with run labels.
#'
#' @return `check_confounds()` returns a tibble of diagnostics. `clean_confounds()`
#'   returns `x` with flagged columns removed and a `confound_diagnostics`
#'   attribute containing the same diagnostic rows.
#' @export
#' @examples
#' df <- tibble::tibble(
#'   participant_id = "01",
#'   task = "rest",
#'   run = "01",
#'   cosine00 = c(1, 0, -1),
#'   cosine01 = c(0, 0, 0)
#' )
#' check_confounds(df)
#' clean_confounds(df)
check_confounds <- function(x, checks = c("zero_variance", "rank"), group_vars = NULL) {
  checks <- .normalize_confound_clean(checks, allow_none = FALSE)
  .confound_apply(x, checks, group_vars = group_vars, drop = FALSE, inform = FALSE)$diagnostics
}


#' @rdname check_confounds
#' @export
clean_confounds <- function(x, clean = c("zero_variance", "rank"),
                            group_vars = NULL, inform = TRUE) {
  clean <- .normalize_confound_clean(clean)
  res <- .confound_apply(x, clean, group_vars = group_vars, drop = TRUE, inform = inform)
  res$data
}


#' Confound denoising strategies
#'
#' Creates a structured confound strategy object that specifies which variables
#' to reduce via PCA and which to keep as-is. Pass the result directly to
#' \code{read_confounds(..., cvars = confound_strategy(...))}.
#'
#' When a strategy is passed to \code{read_confounds}, the function:
#' \enumerate{
#'   \item Selects the \code{pca_vars} columns and reduces them via PCA
#'         (retaining \code{perc_var}\% of variance or \code{npcs} components).
#'   \item Selects the \code{raw_vars} columns and keeps them unchanged.
#'   \item Column-binds the PCA scores with the raw columns.
#' }
#'
#' Available named strategies:
#' \describe{
#'   \item{\code{"pcabasic80"}}{PCA over motion24 + aCompCor + tCompCor + CSF +
#'     white matter, retaining 80\% variance.
#'     Discrete cosine regressors are appended un-reduced.}
#' }
#'
#' @param name Character. Name of a predefined strategy (see above), or
#'   \code{NULL} for a custom strategy.
#' @param pca_vars Character vector of confound names/wildcards to include in
#'   PCA reduction. Ignored when \code{name} is specified.
#' @param raw_vars Character vector of confound names/wildcards to keep without
#'   reduction. Ignored when \code{name} is specified.
#' @param perc_var Numeric. Percentage of variance to retain from PCA (default
#'   -1, meaning use \code{npcs} instead).
#' @param npcs Integer. Number of PCs to retain (default -1, meaning use
#'   \code{perc_var} instead).
#' @return A \code{confound_strategy} object (S3 class) that can be passed as
#'   the \code{cvars} argument to \code{read_confounds()}.
#' @export
#' @examples
#' # Named strategy
#' confound_strategy("pcabasic80")
#'
#' # Custom strategy: PCA motion + compcor to 5 PCs, keep cosine regressors
#' confound_strategy(
#'   pca_vars = c(confound_set("motion24"), confound_set("compcor")),
#'   raw_vars = confound_set("cosine"),
#'   npcs = 5
#' )
confound_strategy <- function(name = NULL, pca_vars = NULL, raw_vars = NULL,
                              perc_var = -1, npcs = -1) {
  if (!is.null(name)) {
    nm <- tolower(as.character(name))
    strategies <- list(
      pcabasic80 = list(
        pca_vars = c(confound_set("motion24"), "csf", "white_matter",
                     confound_set("acompcor"), confound_set("tcompcor")),
        raw_vars = confound_set("cosine"),
        perc_var = 80,
        npcs = -1
      )
    )
    if (!nm %in% names(strategies)) {
      stop("Unknown confound strategy: ", name,
           ". Use list_confound_strategies() to see available options.")
    }
    spec <- strategies[[nm]]
    pca_vars <- spec$pca_vars
    raw_vars <- spec$raw_vars
    perc_var <- spec$perc_var
    npcs <- spec$npcs
  }

  if (is.null(pca_vars) || length(pca_vars) == 0) {
    stop("pca_vars must be specified (either via `name` or directly).")
  }

  structure(list(
    name = name %||% "custom",
    pca_vars = pca_vars,
    raw_vars = raw_vars %||% character(0),
    perc_var = perc_var,
    npcs = npcs
  ), class = "confound_strategy")
}


#' List available confound strategies
#'
#' Returns the names and short descriptions of the predefined confound
#' strategies usable with \code{confound_strategy()}.
#'
#' @return A data.frame with columns \code{strategy} and \code{description}.
#' @export
#' @examples
#' list_confound_strategies()
list_confound_strategies <- function() {
  data.frame(
    strategy = c("pcabasic80"),
    description = c(
      "PCA(motion24 + aCompCor + tCompCor + CSF + WM, 80% var) + cosine"
    ),
    stringsAsFactors = FALSE
  )
}


.normalize_confound_clean <- function(clean, allow_none = TRUE) {
  choices <- if (allow_none) c("none", "zero_variance", "rank") else c("zero_variance", "rank")
  if (is.null(clean) || length(clean) == 0) {
    clean <- if (allow_none) "none" else choices
  }
  clean <- match.arg(clean, choices, several.ok = TRUE)
  clean <- unique(clean)
  if (allow_none && "none" %in% clean && length(clean) > 1) {
    stop('`clean = "none"` cannot be combined with other cleaning operations.', call. = FALSE)
  }
  clean
}


.empty_confound_diagnostics <- function() {
  tibble::tibble(
    participant_id = character(),
    task = character(),
    session = character(),
    run = character(),
    source = character(),
    role = character(),
    column = character(),
    reason = character(),
    action = character(),
    sd = numeric(),
    rank = integer()
  )
}


.confound_numeric_columns <- function(dfx) {
  names(dfx)[vapply(dfx, function(x) {
    is.numeric(x) || is.integer(x) || is.logical(x)
  }, logical(1))]
}


.confound_is_zero_variance <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  length(unique(x)) <= 1
}


.confound_sd <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) <= 1) {
    return(0)
  }
  stats::sd(x)
}


.confound_matrix <- function(dfx, cols) {
  m <- as.matrix(data.frame(lapply(dfx[cols], as.numeric), check.names = FALSE))
  for (j in seq_len(ncol(m))) {
    v <- m[, j]
    finite <- is.finite(v)
    fill <- if (any(finite)) stats::median(v[finite]) else 0
    v[!finite] <- fill
    m[, j] <- v
  }
  m
}


.confound_rank_drops <- function(dfx) {
  cols <- .confound_numeric_columns(dfx)
  if (length(cols) <= 1) {
    return(list(columns = character(), rank = length(cols)))
  }

  m <- .confound_matrix(dfx, cols)
  qr_m <- qr(m)
  if (qr_m$rank >= ncol(m)) {
    return(list(columns = character(), rank = qr_m$rank))
  }

  keep <- if (qr_m$rank > 0) cols[sort(qr_m$pivot[seq_len(qr_m$rank)])] else character()
  list(columns = setdiff(cols, keep), rank = qr_m$rank)
}


.diagnostic_context <- function(id, n) {
  defaults <- list(
    participant_id = NA_character_,
    task = NA_character_,
    session = NA_character_,
    run = NA_character_,
    source = NA_character_
  )
  id <- utils::modifyList(defaults, id %||% list())
  tibble::tibble(
    participant_id = rep(as.character(id$participant_id), n),
    task = rep(as.character(id$task), n),
    session = rep(as.character(id$session), n),
    run = rep(as.character(id$run), n),
    source = rep(as.character(id$source), n)
  )
}


.clean_confound_frame <- function(dfx, clean, id = list(), role = NA_character_,
                                  action = "drop", drop = TRUE) {
  clean <- .normalize_confound_clean(clean)
  if (identical(clean, "none")) {
    return(list(data = dfx, diagnostics = .empty_confound_diagnostics()))
  }

  working <- tibble::as_tibble(dfx)
  diagnostics <- list()

  if ("zero_variance" %in% clean) {
    num_cols <- .confound_numeric_columns(working)
    zero_cols <- num_cols[vapply(working[num_cols], .confound_is_zero_variance, logical(1))]
    if (length(zero_cols) > 0) {
      diagnostics[[length(diagnostics) + 1]] <- dplyr::bind_cols(
        .diagnostic_context(id, length(zero_cols)),
        tibble::tibble(
          role = rep(as.character(role), length(zero_cols)),
          column = zero_cols,
          reason = "zero_variance",
          action = action,
          sd = vapply(working[zero_cols], .confound_sd, numeric(1)),
          rank = NA_integer_
        )
      )
      working <- working[setdiff(names(working), zero_cols)]
    }
  }

  if ("rank" %in% clean) {
    rank_drop <- .confound_rank_drops(working)
    rank_cols <- rank_drop$columns
    if (length(rank_cols) > 0) {
      diagnostics[[length(diagnostics) + 1]] <- dplyr::bind_cols(
        .diagnostic_context(id, length(rank_cols)),
        tibble::tibble(
          role = rep(as.character(role), length(rank_cols)),
          column = rank_cols,
          reason = "rank_deficient",
          action = action,
          sd = vapply(working[rank_cols], .confound_sd, numeric(1)),
          rank = as.integer(rank_drop$rank)
        )
      )
      working <- working[setdiff(names(working), rank_cols)]
    }
  }

  diag <- if (length(diagnostics) > 0) dplyr::bind_rows(diagnostics) else .empty_confound_diagnostics()
  if (!drop) {
    working <- tibble::as_tibble(dfx)
  }
  list(data = working, diagnostics = diag)
}


.confound_group_vars <- function(x, group_vars = NULL) {
  if (!is.null(group_vars)) {
    return(intersect(group_vars, names(x)))
  }
  intersect(c("participant_id", "task", "session", "run", ".subid", ".task", ".session", ".run", ".desc"),
            names(x))
}


.confound_id_from_row <- function(row) {
  as_list <- as.list(row)
  list(
    participant_id = as_list$participant_id %||% as_list$.subid %||% NA_character_,
    task = as_list$task %||% as_list$.task %||% NA_character_,
    session = as_list$session %||% as_list$.session %||% NA_character_,
    run = as_list$run %||% as_list$.run %||% NA_character_,
    source = as_list$source %||% NA_character_
  )
}


.confound_apply <- function(x, clean, group_vars = NULL, drop = TRUE, inform = TRUE) {
  old_class <- class(x)
  old_pca <- attr(x, "pca", exact = TRUE)
  action <- if (drop) "drop" else "flag"

  if ("data" %in% names(x) && is.list(x$data)) {
    out <- x
    diagnostics <- vector("list", nrow(x))
    for (i in seq_len(nrow(x))) {
      row <- x[i, setdiff(names(x), "data"), drop = FALSE]
      id <- .confound_id_from_row(row)
      res <- .clean_confound_frame(x$data[[i]], clean, id = id, role = "confound",
                                   action = action, drop = drop)
      out$data[[i]] <- res$data
      diagnostics[[i]] <- res$diagnostics
    }
    diag <- if (length(diagnostics) > 0) dplyr::bind_rows(diagnostics) else .empty_confound_diagnostics()
    class(out) <- old_class
    attr(out, "pca") <- old_pca
    attr(out, "confound_diagnostics") <- diag
    if (drop && inform) .inform_confound_diagnostics(diag)
    return(list(data = out, diagnostics = diag))
  }

  group_vars <- .confound_group_vars(x, group_vars)
  if (length(group_vars) == 0) {
    res <- .clean_confound_frame(x, clean, id = list(), role = "confound",
                                 action = action, drop = drop)
    out <- res$data
    class(out) <- old_class
    attr(out, "pca") <- old_pca
    attr(out, "confound_diagnostics") <- res$diagnostics
    if (drop && inform) .inform_confound_diagnostics(res$diagnostics)
    return(list(data = out, diagnostics = res$diagnostics))
  }

  split_key <- interaction(x[group_vars], drop = TRUE, lex.order = TRUE)
  row_groups <- split(seq_len(nrow(x)), split_key)
  data_out <- vector("list", length(row_groups))
  diagnostics <- vector("list", length(row_groups))
  confound_cols <- setdiff(names(x), group_vars)

  for (i in seq_along(row_groups)) {
    idx <- row_groups[[i]]
    id <- .confound_id_from_row(x[idx[1], group_vars, drop = FALSE])
    res <- .clean_confound_frame(x[idx, confound_cols, drop = FALSE], clean,
                                 id = id, role = "confound",
                                 action = action, drop = drop)
    data_out[[i]] <- dplyr::bind_cols(x[idx, group_vars, drop = FALSE], res$data)
    diagnostics[[i]] <- res$diagnostics
  }

  out <- dplyr::bind_rows(data_out)
  diag <- dplyr::bind_rows(diagnostics)
  class(out) <- old_class
  attr(out, "pca") <- old_pca
  attr(out, "confound_diagnostics") <- diag
  if (drop && inform) .inform_confound_diagnostics(diag)
  list(data = out, diagnostics = diag)
}


.diagnostic_run_label <- function(dfx) {
  label <- paste0("sub-", dfx$participant_id[1])
  if (!is.na(dfx$session[1])) label <- paste0(label, " ses-", dfx$session[1])
  if (!is.na(dfx$task[1])) label <- paste0(label, " task-", dfx$task[1])
  if (!is.na(dfx$run[1])) label <- paste0(label, " run-", dfx$run[1])
  label
}


.inform_confound_diagnostics <- function(diagnostics) {
  if (is.null(diagnostics) || nrow(diagnostics) == 0) {
    return(invisible(NULL))
  }
  dropped <- diagnostics[diagnostics$action == "drop", , drop = FALSE]
  if (nrow(dropped) == 0) {
    return(invisible(NULL))
  }

  by_reason <- split(dropped, dropped$reason)
  messages <- character()
  for (reason in names(by_reason)) {
    title <- switch(reason,
      zero_variance = "Dropped zero-variance confounds:",
      rank_deficient = "Dropped rank-deficient confounds:",
      paste0("Dropped confounds flagged as ", reason, ":")
    )
    reason_df <- by_reason[[reason]]
    run_key <- interaction(reason_df[c("participant_id", "session", "task", "run")],
                           drop = TRUE, lex.order = TRUE)
    by_run <- split(reason_df, run_key)
    lines <- vapply(by_run, function(run_df) {
      paste0(.diagnostic_run_label(run_df), ": ", paste(unique(run_df$column), collapse = ", "))
    }, character(1))
    messages <- c(messages, title, unname(lines))
  }

  rlang::inform(messages)
  invisible(NULL)
}
