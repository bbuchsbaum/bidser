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
#' @param thresh Threshold value between 0 and 1 (default 0.99) - voxels with values below this threshold are excluded from the mask
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

DEFAULT_CVARS2 <- c("csf", "white_matter", "global_signal", "std_dvars",
                    "framewise_displacement", "t_comp_cor_00", "t_comp_cor_01", "t_comp_cor_02",
                    "t_comp_cor_03", "t_comp_cor_04", "t_comp_cor_00", "a_comp_cor_00", "a_comp_cor_01",
                    "a_comp_cor_02" , "a_comp_cor_03", "a_comp_cor_04", "a_comp_cor_03", "trans_x", "trans_y", "trans_z",
                    "rot_x", "rot_y", "rot_z")


#' Locate confound files
#'
#' @param x A \code{bids_project} object
#' @param subid Subject ID regex
#' @param task Task regex
#' @param session Session regex
#' @param nest If TRUE, results are nested
#' @return A character vector of file paths
#' @export
confound_files.bids_project <- function(x, subid=".*", task=".*", session=".*", nest=TRUE) {
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
#' @param run Run regex
#' @param cvars The names of the confound variables to select. Defaults to \code{DEFAULT_CVARS}.
#' @param npcs Perform PCA reduction on confounds and return \code{npcs} PCs.
#' @param perc_var Perform PCA reduction to retain \code{perc_var}% variance.
#' @param nest If TRUE, nests confound tables by subject/session/run.
#' @import dplyr
#' @importFrom tidyr nest
#' @importFrom tidyselect any_of
#' @return A nested tibble (if nest=TRUE) or a flat tibble (if nest=FALSE) of confounds.
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
    fnames <- confound_files(x, subid=paste0("^", as.character(s), "$"), task=task, session=session, nest=FALSE)
    
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
      
      # Select requested confound columns
      dfx <- dfx %>% dplyr::select(any_of(cvars))
      
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

