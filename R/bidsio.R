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


#' Read in a set of preprocessed functional scans
#'
#' @inheritParams read_func_scans
#' @param x A \code{bids_project} object
#' @param mask A brain mask of type \code{LogicalNeuroVol}, or NULL (if NULL, a mask will be created)
#' @param mode The file mode: 'normal' or 'bigvec'
#' @param subid Subject IDs (regex)
#' @param task Task regex
#' @param run Run regex
#' @param modality Image modality (usually "bold")
#' @param ... Extra arguments passed to \code{neuroim2::read_vec}
#' @return An instance of type \code{NeuroVec}
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


#' Create a preprocessing mask
#'
#' @param x A \code{bids_project} object
#' @param subid Subject ID regex
#' @param thresh Threshold value (default 0.99)
#' @param ... Additional arguments passed to `search_files`
#' @return A logical mask volume
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
  
  fnames1 <- search_files(x, subid=subid, task=task, session=session, deriv="confounds", full_path=TRUE)
  fnames2 <- search_files(x, subid=subid, task=task, session=session, desc="confounds", full_path=TRUE)
  c(fnames1, fnames2)
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
    fnames <- search_files(x, subid=paste0("^", as.character(s), "$"), task=task,
                           run=run, session=session, kind="(confounds|regressors|timeseries)", suffix="tsv",
                           strict=TRUE, full_path=TRUE)
    
    if (length(fnames) == 0) {
      # No confound files for this participant; return empty frame
      return(data.frame())
    }
    
    # Process each confound file
    dflist <- lapply(fnames, function(fn) {
      run_val <- stringr::str_match(fn, "_run-(\\d+)")[1,2]
      sess_val <- stringr::str_match(fn, "_ses-(\\d+)")[1,2]
      
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

