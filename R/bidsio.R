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
#' proj <- bids_project("/path/to/bids/dataset", fmriprep=TRUE)
#'
#' # Read preprocessed scans for all subjects
#' # (mask will be created automatically)
#' all_scans <- read_preproc_scans(proj)
#'
#' # Read preprocessed scans for a specific subject
#' sub01_scans <- read_preproc_scans(proj, subid="01")
#'
#' # Read preprocessed scans for a specific task and run
#' task_scans <- read_preproc_scans(proj, 
#'                                 task="rest",
#'                                 run="01")
#'
#' # Specify mode for large datasets
#' bigvec_scans <- read_preproc_scans(proj, mode="bigvec")
#'
#' # Provide a custom mask
#' mask <- create_preproc_mask(proj, thresh=0.95)
#' masked_scans <- read_preproc_scans(proj, mask=mask)
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
#' This function creates a binary brain mask from preprocessed functional scans in a BIDS project.
#' It searches for brainmask files in the fMRIPrep derivatives directory, reads them using the 
#' neuroim2 package, and averages them to create a single mask. The resulting mask can be used
#' for subsequent analyses with preprocessed functional data.
#'
#' @param x A \code{bids_project} object with fMRIPrep derivatives
#' @param subid Regular expression to match subject IDs (e.g., "01" for subject 01, ".*" for all subjects)
#' @param thresh Threshold value between 0 and 1 (default 0.99). Values outside
#'   this range will trigger an error. Voxels with values below the threshold are
#'   excluded from the mask.
#' @param ... Additional arguments passed to \code{search_files} for finding mask files
#'
#' @return A logical mask volume (\code{LogicalNeuroVol}) that can be used for subsequent analyses with preprocessed functional data.
#'
#' @details
#' The function works by finding all brainmask files that match the subject ID pattern,
#' reading them into memory, averaging them, and then thresholding the result to create
#' a binary mask. This is useful when you want to analyze multiple runs or subjects together
#' and need a common mask that covers the brain areas present in all scans.
#' 
#' The threshold parameter controls how conservative the mask is. Higher values (closer to 1)
#' result in a more conservative mask that includes only voxels that are consistently marked
#' as brain across all subjects/runs. Lower values create a more inclusive mask.
#'
#' @examples
#' \donttest{
#' # Load a BIDS project with fMRIPrep derivatives
#' proj <- bids_project("/path/to/bids/dataset", fmriprep=TRUE)
#'
#' # Create a mask for all subjects (conservative threshold)
#' all_subj_mask <- create_preproc_mask(proj, subid=".*")
#'
#' # Create a mask for a specific subject
#' sub01_mask <- create_preproc_mask(proj, subid="01")
#'
#' # Create a more inclusive mask with a lower threshold
#' inclusive_mask <- create_preproc_mask(proj, subid=".*", thresh=0.8)
#'
#' # Use additional search criteria
#' task_mask <- create_preproc_mask(proj, subid=".*", task="rest")
#' }
#'
#' @export
create_preproc_mask.bids_project <- function(x, subid, thresh=.99, ...) {
  if (!inherits(x, "bids_project")) {
    stop("`x` must be a `bids_project` object.")
  }

  if (!is.numeric(thresh) || length(thresh) != 1 || thresh < 0 || thresh > 1) {
    stop("`thresh` must be between 0 and 1.")
  }
  
  if (!x$has_fmriprep) {
    stop("No fmriprep data available. Cannot create preproc mask.")
  }
  
  maskfiles <- search_files(x, subid=subid, deriv="brainmask", full_path=TRUE, ...)
  if (length(maskfiles) == 0) {
    stop("No brainmask files found matching the specified subject.")
  }
  
  if (!requireNamespace("neuroim2", quietly=TRUE)) {
    stop("Package `neuroim2` is required for `create_preproc_mask`.")
  }
  
  vols <- lapply(maskfiles, neuroim2::read_vol)
  if (length(vols) == 0) {
    stop("Could not read any mask volumes.")
  }
  
  avg <- Reduce("+", vols)/length(vols)
  avg[avg < thresh] <- 0
  as.logical(avg)
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
  non_std_dvars = c("non.stdDVARS", "non_std_dvars"),
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
#' @keywords internal
resolve_cvars <- function(cvars, col_names, rename = FALSE) {
  res <- character()
  for (cv in cvars) {
    # find canonical entry containing this name
    canon <- names(CVARS_ALIASES)[sapply(CVARS_ALIASES, function(a) cv %in% c(cv, a))]
    if (length(canon) == 0) {
      canon <- cv
      aliases <- cv
    } else {
      canon <- canon[1]
      aliases <- CVARS_ALIASES[[canon]]
    }
    found <- intersect(aliases, col_names)
    if (length(found) > 0) {
      if (rename) {
        res <- c(res, setNames(found[1], canon))
      } else {
        res <- c(res, found[1])
      }
    }
  }
  res
}


#' Locate confound files
#'
#' @param x A \code{bids_project} object
#' @param subid Subject ID regex
#' @param task Task regex
#' @param session Session regex
#' @return A character vector of file paths
#' @export
confound_files.bids_project <- function(x, subid=".*", task=".*", session=".*") {
  if (!inherits(x, "bids_project")) {
    stop("`x` must be a `bids_project` object.")
  }
  
  # Check if project has fmriprep derivatives
  if (!x$has_fmriprep) {
    rlang::inform("Project does not have fmriprep derivatives enabled. Cannot search for confound files.")
    return(NULL)
  }
  
  # Search for confound files with different possible formats *within derivatives*
  fnames1 <- search_files(x, subid=subid, task=task, session=session, deriv="confounds", full_path=TRUE)
  fnames2 <- search_files(x, subid=subid, task=task, session=session, desc="confounds", full_path=TRUE)
  fnames3 <- search_files(x, subid=subid, task=task, session=session, kind="confounds", full_path=TRUE)
  
  # Also search for files with _confounds.tsv suffix pattern *within derivatives*
  fnames4 <- search_files(x, regex="_confounds\\.tsv$", subid=subid, task=task, session=session, full_path=TRUE)
  
  # Combine all results and remove duplicates
  found_files <- unique(c(fnames1, fnames2, fnames3, fnames4))
  
  if (length(found_files) == 0) {
    return(NULL)
  }
  
  return(found_files)
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
#' @param npcs Perform PCA reduction on confounds and return \code{npcs} PCs.
#' @param perc_var Perform PCA reduction to retain \code{perc_var}% variance.
#' @param nest If TRUE, nests confound tables by subject/session/run.
#' @import dplyr
#' @importFrom tidyr nest
#' @importFrom tidyselect any_of
#' @return A nested tibble (if nest=TRUE) or a flat tibble (if nest=FALSE) of confounds.
#' @examples
#' \donttest{
#' proj <- bids_project("/path/to/bids", fmriprep = TRUE)
#' # canonical names automatically resolve to actual columns
#' conf <- read_confounds(proj, cvars = c("csf", "framewise_displacement"))
#' }
#' @export
read_confounds.bids_project <- function(x, subid=".*", task=".*", session=".*", run=".*",
                                        cvars=DEFAULT_CVARS, npcs=-1, perc_var=-1, nest=TRUE) {
  if (!inherits(x, "bids_project")) {
    stop("`x` must be a `bids_project` object.")
  }
  
  # Check participants
  sids <- participants(x)
  gidx <- grep(subid, sids)
  if (length(gidx) == 0) {
    stop("No matching participants found for regex: ", subid)
  }
  sids <- sids[gidx]
  
  ret <- lapply(sids, function(s) {
    # Use confound_files to get all possible confound files
    fnames <- confound_files(x, subid=paste0("^", as.character(s), "$"), task=task, session=session)
    
    # Filter by run if specified
    if (run != ".*") {
      fnames <- fnames[grepl(paste0("_run-", run), fnames)]
    }
    
    if (length(fnames) == 0) {
      # No confound files for this participant; return empty frame
      return(data.frame())
    }
    
    # Process each confound file
    dflist <- lapply(fnames, function(fn) {
      # Extract run and session from filename
      run_val <- stringr::str_match(fn, "_run-([0-9]+)")[1,2]
      sess_val <- stringr::str_match(fn, "_ses-([A-Za-z0-9]+)")[1,2]

      if (is.na(run_val)) {
        run_val <- "1"
      }

      if (is.na(sess_val)) {
        sess_val <- "1"
      }
      
      # Read table
      dfx <- tryCatch({
        read.table(fn, header=TRUE, na.strings=c("NA", "n/a"), stringsAsFactors=FALSE)
      }, error=function(e) {
        warning("Unable to read file: ", fn, " Error: ", e$message)
        return(NULL)
      })
      
      if (is.null(dfx)) return(NULL)

      # Resolve canonical confound names to available columns
      sel_cvars <- resolve_cvars(cvars, colnames(dfx))

      # Select requested confound columns
      dfx <- dfx %>% dplyr::select(any_of(sel_cvars))
      
      # Process confounds if PCA requested
      if ((npcs > 0 || perc_var > 0) && ncol(dfx) > 1) {
        dfx <- process_confounds(dfx, npcs=npcs, perc_var=perc_var)
      }
      
      # Add identifying columns
      dfx %>%
        mutate(participant_id=s, run=run_val, session=sess_val)
    })
    
    # Filter out any NULL returns
    dflist <- dflist[!sapply(dflist, is.null)]
    if (length(dflist) == 0) return(data.frame())
    
    dplyr::bind_rows(dflist)
  })
  
  ret <- ret[!sapply(ret, function(z) nrow(z)==0)]
  if (length(ret) == 0) {
    message("No confound data found for the given selection.")
    return(NULL)
  }
  
  ret <- dplyr::bind_rows(ret)
  
  if (nest) {
    ret %>% dplyr::group_by(participant_id, run, session) %>% tidyr::nest()
  } else {
    ret
  }
}


#' @keywords internal
process_confounds <- function(dfx, center=TRUE, scale=TRUE, npcs=-1, perc_var=-1) {
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
  # If PCA requested
  if ((npcs > 0 || perc_var > 0) && ncol(sm) > 1) {
    pres <- prcomp(sm, scale.=FALSE)
    varexp <- cumsum(pres$sdev^2)/sum(pres$sdev^2) * 100
    
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
    
    sm <- pres$x[, 1:keep_npcs, drop=FALSE]
    colnames(sm) <- paste0("PC", seq_len(ncol(sm)))
  }
  
  as.data.frame(sm)
}

