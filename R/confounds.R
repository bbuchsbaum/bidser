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
#' - `"dvars"`: DVARS family: `dvars`, `std_dvars`, `non_std_dvars`,
#'   `vx_wisestd_dvars` (resolved to whichever names exist in your dataset).
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

  dvars <- c("dvars", "std_dvars", "non_std_dvars", "vx_wisestd_dvars")

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
    dvars     = dvars,
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
      "cosine", "outliers", "dvars", "fd"
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
      "DVARS family (dvars, std_dvars, non_std_dvars, vx_wisestd_dvars)",
      "Framewise displacement only"
    ),
    stringsAsFactors = FALSE
  )
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
