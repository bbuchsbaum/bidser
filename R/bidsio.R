#' Read in a set of four-dimensional functional scans
#'
#' @param x A \code{bids_project} object
#' @param mask A brain mask of type \code{LogicalNeuroVol}
#' @param mode The file mode: 'normal' for in-memory files or 'bigvec' for on-disk files
#' @param subid One or more subject IDs (regex)
#' @param task An optional task regex
#' @param run An optional run regex
#' @param modality The image modality (usually "bold")
#' @param ... Extra arguments passed to \code{neuroim2::read_vec}
#' @return An instance of type \code{NeuroVec}
#' @rdname read_func_scans
#' @examples
#' \donttest{
#' tryCatch({
#'   ds_path <- get_example_bids_dataset("ds000001-fmriprep")
#'   proj <- bids_project(ds_path, fmriprep=TRUE)
#'   mask <- brain_mask(proj, subid="01")
#'   vec <- read_func_scans.bids_project(proj, mask,
#'                                      subid="01",
#'                                      task="balloonanalogrisktask",
#'                                      run="01")
#'   unlink(ds_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires derivatives dataset: ", e$message)
#' })
#' }
#' @export
read_func_scans.bids_project <- function(x, mask, mode = c("normal", "bigvec"),
                                         subid="^sub-.*", task=".*", run = ".*", modality="bold", ...) {
  mode <- match.arg(mode)
  
  # Check required arguments
  if (!inherits(x, "bids_project")) {
    stop("`x` must be a `bids_project` object.")
  }
  if (is.null(mask)) {
    stop("`mask` cannot be NULL. Please provide a LogicalNeuroVol mask.")
  }
  
  fnames <- func_scans(x, subid=subid, task=task, run=run, modality=modality)
  if (length(fnames) == 0 || all(is.na(fnames))) {
    stop("No matching scans found for the given subject/task/run/modality criteria.")
  }
  
  if (!requireNamespace("neuroim2", quietly=TRUE)) {
    stop("Package `neuroim2` is required for `read_func_scans`.")
  }
  
  neuroim2::read_vec(fnames, mask=mask, mode=mode, ...)
}


#' Read preprocessed functional MRI scans from a BIDS project
#'
#' This function reads preprocessed functional MRI scans from a BIDS project's fMRIPrep
#' derivatives directory. It uses the \code{preproc_scans} function to locate the files
#' and then reads them into a \code{NeuroVec} object using the neuroim2 package. If a
#' mask is not provided, one will be automatically created from available brainmask files.
#'
#' @param x A \code{bids_project} object with fMRIPrep derivatives
#' @param mask A brain mask of type \code{LogicalNeuroVol}, or NULL (if NULL, a mask will be created automatically)
#' @param mode The file mode: 'normal' for in-memory files or 'bigvec' for on-disk files
#' @param subid Regular expression to match subject IDs (default: "^sub-.*" to match all subjects)
#' @param task Regular expression to match tasks (default: ".*" to match all tasks)
#' @param run Regular expression to match runs (default: ".*" to match all runs)
#' @param modality Image modality to match (default: "bold" for functional MRI)
#' @param ... Extra arguments passed to \code{neuroim2::read_vec}
#' 
#' @return An instance of type \code{NeuroVec} containing the preprocessed functional data.
#' 
#' @details
#' This function requires the \code{neuroim2} package to be installed. It will throw an
#' error if the package is not available or if fMRIPrep derivatives are not found in the
#' BIDS project. If no mask is provided, it will create one using the \code{create_preproc_mask}
#' function.
#'
#' @examples
#' \donttest{
#' # Load a BIDS project with fMRIPrep derivatives
#' tryCatch({
#'   ds_path <- get_example_bids_dataset("ds000001-fmriprep")
#'   proj <- bids_project(ds_path, fmriprep=TRUE)
#'   
#'   # Read preprocessed scans for all subjects
#'   # (mask will be created automatically)
#'   all_scans <- read_preproc_scans(proj)
#'   
#'   # Read preprocessed scans for a specific subject
#'   sub01_scans <- read_preproc_scans(proj, subid="01")
#'   
#'   # Read preprocessed scans for a specific task and run
#'   task_scans <- read_preproc_scans(proj, 
#'                                   task="balloonanalogrisktask",
#'                                   run="01")
#'   
#'   # Specify mode for large datasets
#'   bigvec_scans <- read_preproc_scans(proj, mode="bigvec")
#'   
#'   # Provide a custom mask
#'   mask <- create_preproc_mask(proj, thresh=0.95)
#'   masked_scans <- read_preproc_scans(proj, mask=mask)
#'   
#'   # Clean up
#'   unlink(ds_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires derivatives dataset: ", e$message)
#' })
#' }
#'
#' @export
read_preproc_scans.bids_project <- function(x, mask=NULL, mode = c("normal", "bigvec"),
                                            subid="^sub-.*", task=".*", run = ".*", modality="bold", ...) {
  mode <- match.arg(mode)
  
  if (!inherits(x, "bids_project")) {
    stop("`x` must be a `bids_project` object.")
  }
  
  if (!x$has_fmriprep) {
    stop("Fmriprep derivatives not found. Please run `bids_project()` with `fmriprep=TRUE` or ensure fmriprep data is present.")
  }
  
  fnames <- preproc_scans(x, subid=subid, task=task, run=run, modality=modality, full_path=TRUE)
  if (length(fnames) == 0 || all(is.na(fnames))) {
    stop("No matching preprocessed scans found for the given subject/task/run/modality criteria.")
  }
  
  # Create mask if not provided
  if (is.null(mask)) {
    mask <- create_preproc_mask(x, subid)
    if (is.null(mask) || all(mask == 0)) {
      stop("Could not create a valid mask from preprocessed scans.")
    }
  }
  
  if (!requireNamespace("neuroim2", quietly=TRUE)) {
    stop("Package `neuroim2` is required for `read_preproc_scans`.")
  }
  
  neuroim2::read_vec(fnames, mask=mask, mode=mode, ...)
}


#' Create a binary brain mask from preprocessed scans
#'
#' This function creates a binary brain mask from preprocessed functional scans
#' in a BIDS project. It searches for BOLD brain mask files in the fMRIPrep
#' derivatives directory (i.e., files in the \code{func/} folder matching the
#' pattern \code{*_desc-brain_mask.nii.gz} or the older \code{*_brainmask.nii.gz}),
#' reads them with neuroim2, averages them, and thresholds the result to produce
#' a consensus binary mask.
#'
#' @param x A \code{bids_project} object with fMRIPrep derivatives.
#' @param subid Regular expression to match subject IDs (e.g., \code{"01"} for
#'   subject 01, \code{".*"} for all subjects).
#' @param thresh Threshold value between 0 and 1 (default 0.99). Voxels below
#'   this value in the averaged mask are excluded. Higher values produce more
#'   conservative masks.
#' @param task Regular expression for task filtering. Defaults to \code{".*"}
#'   (any task). Because functional masks always carry a \code{task} entity,
#'   this also implicitly excludes anatomical masks which lack it.
#' @param space Regular expression for output-space filtering (e.g.,
#'   \code{"MNI152NLin2009cAsym"}). Defaults to \code{".*"} (all spaces).
#'   When masks from multiple spaces are found the function stops with an error
#'   because their dimensions are incompatible.
#' @param mask_kinds Character vector of BIDS suffixes to search. Defaults to
#'   both \code{"brainmask"} (older fMRIPrep) and \code{"mask"} with
#'   \code{desc="brain"} (fMRIPrep >= 21).
#' @param ... Additional arguments passed to \code{search_files} for finding
#'   mask files (e.g., \code{session}, \code{run}).
#'
#' @return A logical mask volume (\code{LogicalNeuroVol}) suitable for use with
#'   preprocessed functional data.
#'
#' @details
#' The search is restricted to **functional** brain masks by requiring the
#' \code{task} BIDS entity (anatomical masks do not carry \code{task}).
#' When masks from multiple output spaces are discovered the function raises an
#' error; pass a specific \code{space} value to disambiguate.
#'
#' @examples
#' \donttest{
#' tryCatch({
#'   ds_path <- get_example_bids_dataset("ds000001-fmriprep")
#'   proj <- bids_project(ds_path, fmriprep=TRUE)
#'
#'   # Mask for one subject in a specific space
#'   mask <- create_preproc_mask(proj, subid="01",
#'                               space="MNI152NLin2009cAsym")
#'
#'   # Consensus mask across all subjects / runs
#'   all_mask <- create_preproc_mask(proj, subid=".*",
#'                                   space="MNI152NLin2009cAsym")
#'
#'   # Restrict to a single task
#'   task_mask <- create_preproc_mask(proj, subid=".*",
#'                                   task="balloonanalogrisktask",
#'                                   space="MNI152NLin2009cAsym")
#'
#'   unlink(ds_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires derivatives dataset: ", e$message)
#' })
#' }
#'
#' @export
create_preproc_mask.bids_project <- function(x, subid, thresh=.99,
                                             task = ".*", space = ".*",
                                             mask_kinds = c("brainmask", "mask"), ...) {
  if (!inherits(x, "bids_project")) {
    stop("`x` must be a `bids_project` object.")
  }

  if (!is.numeric(thresh) || length(thresh) != 1 || thresh < 0 || thresh > 1) {
    stop("`thresh` must be between 0 and 1.")
  }

  if (!x$has_fmriprep) {
    stop("No fmriprep data available. Cannot create preproc mask.")
  }

  maskfiles <- c()
  if ("brainmask" %in% mask_kinds) {
    maskfiles <- c(maskfiles,
                   search_files(x, subid = subid, kind = "brainmask",
                                task = task, space = space,
                                full_path = TRUE, ...))
  }
  if ("mask" %in% mask_kinds) {
    maskfiles <- c(maskfiles,
                   search_files(x, subid = subid, kind = "mask",
                                desc = "brain", task = task, space = space,
                                full_path = TRUE, ...))
  }
  maskfiles <- unique(maskfiles)

  # Restrict to functional (BOLD) masks: require `_task-` in the filename.
  # Anatomical brain masks (anat/) lack a task entity, so this reliably

  # separates func masks from anat masks even though search_files treats
  # ".*" as "don't filter" rather than "require key to exist".
  func_pattern <- "_task-[A-Za-z0-9]+"
  maskfiles <- maskfiles[grepl(func_pattern, basename(maskfiles))]

  if (length(maskfiles) == 0) {
    stop("No BOLD brain mask files found matching the specified criteria.")
  }

  # Error when masks span multiple output spaces (incompatible dimensions)
  spaces_found <- unique(na.omit(stringr::str_match(basename(maskfiles),
                                                     "_space-([A-Za-z0-9]+)")[, 2]))
  if (length(spaces_found) > 1) {
    stop("Mask files span multiple output spaces (",
         paste(spaces_found, collapse = ", "),
         "). Pass an explicit `space` to select one.")
  }

  if (!requireNamespace("neuroim2", quietly = TRUE)) {
    stop("Package `neuroim2` is required for `create_preproc_mask`.")
  }

  vols <- lapply(maskfiles, neuroim2::read_vol)
  if (length(vols) == 0) {
    stop("Could not read any mask volumes.")
  }

  avg <- Reduce("+", vols) / length(vols)
  avg[avg < thresh] <- 0
  as.logical(avg)
}

#' @rdname brain_mask
#' @export
brain_mask.bids_project <- function(x, subid, ...) {
  create_preproc_mask(x, subid = subid, ...)
}


DEFAULT_CVARS <- c("CSF", "WhiteMatter", "GlobalSignal", "stdDVARS", "non.stdDVARS",
                   "vx.wisestdDVARS", "FramewiseDisplacement", "tCompCor00", "tCompCor01", "tCompCor02",
                   "tCompCor03", "tCompCor04", "tCompCor05", "aCompCor00", "aCompCor01",
                   "aCompCor02", "aCompCor03", "aCompCor04", "aCompCor05", "X", "Y", "Z",
                   "RotX", "RotY", "RotZ")

# canonical confound variables and their possible aliases across fmriprep versions
CVARS_ALIASES <- list(
  csf = c("CSF", "csf"),
  white_matter = c("WhiteMatter", "white_matter"),
  global_signal = c("GlobalSignal", "global_signal"),
  std_dvars = c("stdDVARS", "std_dvars"),
  # fMRIPrep newer versions use `dvars`; older used non-stdDVARS variants
  dvars = c("dvars", "non_std_dvars", "non.stdDVARS"),
  non_std_dvars = c("non.stdDVARS", "non_std_dvars", "dvars"),
  vx_wisestd_dvars = c("vx.wisestdDVARS", "vx_wisestd_dvars"),
  framewise_displacement = c("FramewiseDisplacement", "framewise_displacement"),
  t_comp_cor_00 = c("tCompCor00", "t_comp_cor_00"),
  t_comp_cor_01 = c("tCompCor01", "t_comp_cor_01"),
  t_comp_cor_02 = c("tCompCor02", "t_comp_cor_02"),
  t_comp_cor_03 = c("tCompCor03", "t_comp_cor_03"),
  t_comp_cor_04 = c("tCompCor04", "t_comp_cor_04"),
  t_comp_cor_05 = c("tCompCor05", "t_comp_cor_05"),
  a_comp_cor_00 = c("aCompCor00", "a_comp_cor_00"),
  a_comp_cor_01 = c("aCompCor01", "a_comp_cor_01"),
  a_comp_cor_02 = c("aCompCor02", "a_comp_cor_02"),
  a_comp_cor_03 = c("aCompCor03", "a_comp_cor_03"),
  a_comp_cor_04 = c("aCompCor04", "a_comp_cor_04"),
  a_comp_cor_05 = c("aCompCor05", "a_comp_cor_05"),
  trans_x = c("X", "trans_x"),
  trans_y = c("Y", "trans_y"),
  trans_z = c("Z", "trans_z"),
  rot_x = c("RotX", "rot_x"),
  rot_y = c("RotY", "rot_y"),
  rot_z = c("RotZ", "rot_z")
)

# DEPRECATED: use `CVARS_ALIASES` instead
DEFAULT_CVARS2 <- names(CVARS_ALIASES)


#' Resolve canonical confound variable names
#'
#' Given a set of desired confound variables, returns the matching column names
#' present in a dataset, taking into account aliases across fmriprep versions.
#'
#' @param cvars Character vector of canonical or alias confound names.
#' @param col_names Character vector of available column names.
#' @param rename If TRUE, a named vector is returned where names are canonical
#'   variables and values are the matching column names. When FALSE the result is
#'   an unnamed vector of column names to select.
#' @return Character vector of resolved column names.
#' @importFrom utils head
#' @keywords internal
resolve_cvars <- function(cvars, col_names, rename = FALSE) {
  res <- character()
  # helper: select first n from a vector (n <= 0 => all)
  take_n <- function(x, n) {
    if (is.null(n) || is.na(n) || n <= 0) return(x)
    head(x, n)
  }

  for (cv in cvars) {
    # 1) Wildcard support (with optional [N] limiter): prefix* or prefix*[N]
    #    Examples: 'cosine_*', 'motion_outlier_*', 'a_comp_cor_*[6]'
    if (grepl("\\*", cv)) {
      # extract N in [N] if present
      n_lim <- NA_integer_
      if (grepl("\\[[0-9]+\\]$", cv)) {
        n_lim <- as.integer(sub("^.*\\[([0-9]+)\\]$", "\\1", cv))
      }
      # remove optional [N]
      cv_no_lim <- sub("\\[[0-9]+\\]$", "", cv)
      # keep everything before the first '*'
      prefix <- sub("\\*.*$", "", cv_no_lim)
      matched <- col_names[startsWith(col_names, prefix)]
      matched <- sort(unique(matched))
      matched <- take_n(matched, n_lim)
      res <- c(res, matched)
      next
    }

    # 2) Handle suffix variants for derivative/square combos:
    #    base, base_derivative1, base_power2, base_derivative1_power2
    if (grepl("(_derivative1|_power2)$|_derivative1_power2$", cv)) {
      # separate base and suffix part
      base <- sub("(_derivative1)?(_power2)?$", "", cv)
      suffix <- sub(paste0("^", base), "", cv)

      # map base via aliases if available, else use as-is
      canon <- names(CVARS_ALIASES)[sapply(CVARS_ALIASES, function(a) base %in% a)]
      if (length(canon) == 0) {
        alias_bases <- base
      } else {
        alias_bases <- CVARS_ALIASES[[canon[1]]]
      }

      # Build candidates for each alias form with the same suffix (e.g., X_derivative1)
      candidates <- paste0(alias_bases, suffix)
      found <- intersect(candidates, col_names)
      if (length(found) > 0) {
        # pick the first present (or all?) — choose first for consistency with prior behavior
        res <- c(res, found[1])
        next
      }
      # fall through to alias resolution on the whole token as a last resort
    }

    # 3) Standard alias resolution (single column variables)
    canon <- names(CVARS_ALIASES)[sapply(CVARS_ALIASES, function(a) cv %in% a)]
    if (length(canon) == 0) {
      # not in alias map — try exact match to columns
      if (cv %in% col_names) {
        res <- c(res, cv)
      }
    } else {
      canon <- canon[1]
      aliases <- CVARS_ALIASES[[canon]]
      found <- intersect(aliases, col_names)
      if (length(found) > 0) {
        if (rename) {
          res <- c(res, setNames(found[1], canon))
        } else {
          res <- c(res, found[1])
        }
      }
    }
  }
  unique(res)
}


#' Locate confound files
#'
#' @param x A \code{bids_project} object
#' @param subid Subject ID regex
#' @param task Task regex
#' @param session Session regex
#' @param ... Additional arguments (not currently used)
#' @return A character vector of file paths
#' @rdname confound_files-method
#' @export
confound_files.bids_project <- function(x, subid=".*", task=".*", session=".*", ...) {
  if (!inherits(x, "bids_project")) {
    stop("`x` must be a `bids_project` object.")
  }
  
  # Check if project has fmriprep derivatives
  if (!x$has_fmriprep) {
    rlang::inform("Project does not have fmriprep derivatives enabled. Cannot search for confound files.")
    return(NULL)
  }
  
  # Get the prep directory name from the project
  pdir <- x$prep_dir
  if (is.null(pdir) || !nzchar(pdir)) {
    warning("No prep_dir specified in BIDS project")
    return(NULL)
  }
  
  # Search within the fmriprep derivatives tree only
  if (!(pdir %in% names(x$bids_tree$children))) {
    warning("fMRIPrep derivatives directory '", pdir, "' not found in BIDS tree")
    return(NULL)
  }
  
  # Extract relative path helper specific to derivatives
  extract_relative_path_deriv <- function(node) {
    # For derivatives, the path structure is: project/prep_dir/sub-XX/...
    pdir_parts <- strsplit(pdir, "/")[[1]]
    if (length(node$path) > (1 + length(pdir_parts))) {
      paste0(node$path[2:length(node$path)], collapse="/")
    } else {
      node$name
    }
  }
  
  # Create filter function for confounds
  filter_fun <- function(z) {
    if (!z$isLeaf) return(FALSE)
    
    # Check filename matches confounds pattern
    node_name <- z$name
    if (is.null(node_name) || is.na(node_name)) return(FALSE)
    
    is_confounds_file <- stringr::str_detect(node_name, "_confounds\\.tsv$") ||
                        str_detect_null(z$desc, "confounds") ||
                        str_detect_null(z$kind, "confounds")
    
    if (!is_confounds_file) return(FALSE)
    
    # Check BIDS entity matches
    subid_match <- str_detect_null(z$subid, subid, default = TRUE)
    task_match <- str_detect_null(z$task, task, default = TRUE)
    session_match <- str_detect_null(z$session, session, default = TRUE)
    
    return(subid_match && task_match && session_match)
  }
  
  # Search only within the fmriprep derivatives tree
  found_files <- x$bids_tree$children[[pdir]]$Get(extract_relative_path_deriv, 
                                                   filterFun = filter_fun, 
                                                   simplify = FALSE)
  
  if (length(found_files) == 0) {
    return(NULL)
  }
  
  # Convert to full paths
  found_files <- unique(unname(unlist(found_files)))
  full_paths <- file.path(x$path, found_files)
  
  # Filter to only files that actually exist
  existing_files <- full_paths[file.exists(full_paths)]
  
  if (length(existing_files) == 0) {
    return(NULL)
  }
  
  return(existing_files)
}


#' Read confound files
#'
#' Reads in fmriprep confound tables for one or more subjects.
#'
#' @param x A \code{bids_project} object
#' @param subid Subject ID regex
#' @param task Task regex
#' @param session Session regex
#' @param run Run regex. If the run identifier cannot be extracted from
#'   the filename, the run value defaults to "1".
#' @param cvars The names of the confound variables to select. Defaults to \code{DEFAULT_CVARS}.
#'   Canonical names such as \code{"csf"} are automatically mapped to any
#'   matching column names found in the dataset using \code{CVARS_ALIASES}.
#'   You can also pass convenience sets from \code{confound_set()}, e.g.,
#'   \code{confound_set("motion24")}, or wildcard patterns like
#'   \code{"cosine_*"}, \code{"motion_outlier_*"}, or \code{"a_comp_cor_*[6]"}.
#' @param npcs Perform PCA reduction on confounds and return \code{npcs} PCs.
#' @param perc_var Perform PCA reduction to retain \code{perc_var}% variance.
#' @param nest If TRUE, nests confound tables by subject/task/session/run.
#' @param ... Additional arguments (not currently used)
#' @import dplyr
#' @importFrom readr read_tsv
#' @importFrom tidyr nest
#' @importFrom tidyselect any_of
#' @return A `bids_confounds` tibble (nested if nest=TRUE) with identifier columns
#'   for participant_id, task, session, and run. When PCA is requested, the
#'   object includes a `pca` attribute with per-run loadings and variance used
#'   by `plot()`.
#' @examples
#' \donttest{
#' # Try to load a BIDS project with fMRIPrep derivatives
#' tryCatch({
#'   ds_path <- get_example_bids_dataset("ds000001-fmriprep")
#'   proj <- bids_project(ds_path, fmriprep=TRUE)
#'   
#'   # Read confounds with canonical names (automatically resolve to actual columns)
#'   conf <- read_confounds(proj, cvars = c("csf", "framewise_displacement"))
#'
#'   # Use convenience sets
#'   conf_36p <- read_confounds(proj, cvars = confound_set("36p"))
#'   conf_compcor6 <- read_confounds(proj, cvars = confound_set("acompcor", n = 6))
#'   
#'   # Read confounds for specific subjects and tasks
#'   conf_sub <- read_confounds(proj, subid="01", task="balloonanalogrisktask")
#'   
#'   # Get confounds as flat tibble
#'   conf_flat <- read_confounds(proj, nest=FALSE)
#'   
#'   # Clean up
#'   unlink(ds_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires derivatives dataset with confounds: ", e$message)
#' })
#' }
#' @export
read_confounds.bids_project <- function(x, subid=".*", task=".*", session=".*", run=".*",
                                        cvars=DEFAULT_CVARS, npcs=-1, perc_var=-1, nest=TRUE, ...) {
  if (!inherits(x, "bids_project")) {
    stop("`x` must be a `bids_project` object.")
  }

  # Detect confound_strategy objects
  use_strategy <- inherits(cvars, "confound_strategy")

  # Check participants
  sids <- participants(x)
  if (length(sids) == 0) {
    warning("No participants found in the BIDS project.")
    return(NULL)
  }
  gidx <- grep(subid, sids)
  if (length(gidx) == 0) {
    warning("No matching participants found for regex: ", subid)
    return(NULL)
  }
  sids <- sids[gidx]

  ret <- lapply(sids, function(s) {
    # Use confound_files to get all possible confound files
    fnames <- confound_files(x, subid=paste0("^", as.character(s), "$"), task=task, session=session)

    # Fallback task filter based on filename in case tree metadata is missing
    if (!is.null(fnames) && length(fnames) > 0 && task != ".*") {
      task_vals <- stringr::str_match(basename(fnames), "_task-([^_]+)")[, 2]
      keep <- !is.na(task_vals) & stringr::str_detect(task_vals, task)
      fnames <- fnames[keep]
    }

    # Filter by run if specified
    if (run != ".*") {
      fnames <- fnames[grepl(paste0("_run-", run), fnames)]
    }

    if (length(fnames) == 0) {
      # No confound files for this participant; return empty frame
      return(list(data = data.frame(), pca = NULL))
    }

    # Process each confound file
    dflist <- lapply(fnames, function(fn) {
      # Extract run and session from filename
      fname <- basename(fn)
      task_val <- stringr::str_match(fname, "_task-([^_]+)")[1, 2]
      run_val <- stringr::str_match(fname, "_run-([^_]+)")[1, 2]
      sess_val <- stringr::str_match(fname, "_ses-([^_]+)")[1, 2]

      if (is.na(run_val)) {
        run_val <- "1"
      }

      if (is.na(sess_val)) {
        sess_val <- "1"
      }

      # Read table
      dfx <- tryCatch({
        readr::read_tsv(fn, na=c("n/a", "NA"))
      }, error=function(e) {
        warning("Unable to read file: ", fn, " Error: ", e$message)
        return(NULL)
      })

      if (is.null(dfx)) return(NULL)

      pca_row <- NULL

      if (use_strategy) {
        # Strategy mode: PCA a subset, keep the rest raw
        strat <- cvars
        pca_cols <- resolve_cvars(strat$pca_vars, colnames(dfx))
        raw_cols <- resolve_cvars(strat$raw_vars, colnames(dfx))
        # Remove any overlap (pca_vars take precedence)
        raw_cols <- setdiff(raw_cols, pca_cols)

        if (length(pca_cols) == 0) {
          warning("No PCA confounds were found for file: ", fn)
          return(NULL)
        }

        dfx_pca <- dfx %>% dplyr::select(any_of(pca_cols))
        s_npcs <- strat$npcs
        s_pv   <- strat$perc_var
        if ((s_npcs > 0 || s_pv > 0) && ncol(dfx_pca) > 1) {
          proc <- process_confounds(dfx_pca, npcs = s_npcs, perc_var = s_pv, return_pca = TRUE)
          dfx_pca <- proc$scores
          if (!is.null(proc$pca)) {
            pca_row <- tibble::tibble(
              participant_id = s,
              task = task_val,
              run = run_val,
              session = sess_val,
              pca = list(proc$pca)
            )
          }
        }

        if (length(raw_cols) > 0) {
          dfx_raw <- dfx %>% dplyr::select(any_of(raw_cols))
          dfx <- dplyr::bind_cols(tibble::as_tibble(dfx_pca), dfx_raw)
        } else {
          dfx <- tibble::as_tibble(dfx_pca)
        }
      } else {
        # Standard mode: resolve and select
        sel_cvars <- resolve_cvars(cvars, colnames(dfx))

        if (length(sel_cvars) == 0) {
          warning("No requested confounds were found for file: ", fn)
          return(NULL)
        }

        dfx <- dfx %>% dplyr::select(any_of(sel_cvars))

        # Process confounds if PCA requested
        if ((npcs > 0 || perc_var > 0) && ncol(dfx) > 1) {
          proc <- process_confounds(dfx, npcs=npcs, perc_var=perc_var, return_pca=TRUE)
          dfx <- proc$scores
          if (!is.null(proc$pca)) {
            pca_row <- tibble::tibble(
              participant_id = s,
              task = task_val,
              run = run_val,
              session = sess_val,
              pca = list(proc$pca)
            )
          }
        }
      }

      # Add identifying columns
      list(
        data = dfx %>%
          mutate(participant_id=s, task=task_val, run=run_val, session=sess_val),
        pca = pca_row
      )
    })

    # Filter out any NULL returns
    dflist <- dflist[!sapply(dflist, is.null)]
    if (length(dflist) == 0) return(list(data = data.frame(), pca = NULL))

    data_list <- lapply(dflist, `[[`, "data")
    pca_list <- lapply(dflist, `[[`, "pca")
    data_list <- data_list[!sapply(data_list, is.null)]
    pca_list <- pca_list[!sapply(pca_list, is.null)]

    data_out <- dplyr::bind_rows(data_list)
    pca_out <- if (length(pca_list) > 0) dplyr::bind_rows(pca_list) else NULL

    list(data = data_out, pca = pca_out)
  })

  ret <- ret[!sapply(ret, function(z) nrow(z$data)==0)]
  if (length(ret) == 0) {
    message("No confound data found for the given selection.")
    return(NULL)
  }

  ret_data <- dplyr::bind_rows(lapply(ret, `[[`, "data"))
  pca_list <- lapply(ret, `[[`, "pca")
  pca_list <- pca_list[!sapply(pca_list, is.null)]
  pca_meta <- if (length(pca_list) > 0) dplyr::bind_rows(pca_list) else NULL

  if (nest) {
    ret_data <- ret_data %>% dplyr::group_by(participant_id, task, run, session) %>% tidyr::nest()
  }

  class(ret_data) <- c("bids_confounds", class(ret_data))
  attr(ret_data, "pca") <- pca_meta

  ret_data
}


#' @keywords internal
process_confounds <- function(dfx, center=TRUE, scale=TRUE, npcs=-1, perc_var=-1, return_pca=FALSE) {
  m <- as.matrix(dfx)
  # Impute NAs
  if (anyNA(m)) {
    m <- apply(m, 2, function(v) {
      mu <- median(v, na.rm=TRUE)
      v[is.na(v)] <- mu
      v
    })
  }
  
  sm <- scale(m, center=center, scale=scale)
  pca <- NULL
  # If PCA requested
  if ((npcs > 0 || perc_var > 0) && ncol(sm) > 1) {
    pres <- prcomp(sm, scale.=FALSE)
    var_pct <- pres$sdev^2 / sum(pres$sdev^2) * 100
    varexp <- cumsum(var_pct)
    
    # Determine how many PCs to keep
    if (npcs > 0 && perc_var <= 0) {
      # Use npcs directly
      keep_npcs <- min(npcs, ncol(pres$x))
    } else if (npcs <= 0 && perc_var > 0) {
      # Keep PCs until we exceed perc_var
      keep_npcs <- which((varexp - perc_var) >= 0)[1]
      if (is.na(keep_npcs)) keep_npcs <- ncol(pres$x) # fallback
    } else {
      # Both npcs and perc_var specified
      keep_npcs_var <- which((varexp - perc_var) >= 0)[1]
      if (is.na(keep_npcs_var)) keep_npcs_var <- ncol(pres$x)
      keep_npcs <- max(c(keep_npcs_var, npcs))
      keep_npcs <- max(1, keep_npcs) # at least 1 PC
    }
    
    pc_names <- paste0("PC", seq_len(keep_npcs))
    sm <- pres$x[, 1:keep_npcs, drop=FALSE]
    colnames(sm) <- pc_names
    rotation <- pres$rotation[, 1:keep_npcs, drop=FALSE]
    colnames(rotation) <- pc_names
    variance <- var_pct[1:keep_npcs]
    names(variance) <- pc_names

    if (return_pca) {
      pca <- list(
        rotation = rotation,
        variance = variance,
        variance_cum = cumsum(variance)
      )
    }
  }
  
  sm <- as.data.frame(sm)
  if (return_pca) {
    return(list(scores = sm, pca = pca))
  }
  sm
}
