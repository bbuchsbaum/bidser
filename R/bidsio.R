
#' read in a set of four-dimensioanl functional scans
#' 
#' @param x the \code{bids_project} object
#' @param mask a brain mask of type \code{LogicalNeuroVol}
#' @param mode the file mode: 'normal' for in-memory files or 'bigvec' for on-disk files
#' @param subid one or more subject ids
#' @param task an optional task regex
#' @param run an optional run regex
#' @param modality the image modality (usually "bold")
#' @param ... extra args send to \code{neuroim2::read_vec}
#' @return a instance of type \code{NeuroVec} 
#' @export
read_func_scans.bids_project <- function(x, mask, mode = c("normal", "bigvec"), subid="^sub-.*", task=".*", run = ".*", modality="bold", ...) {
  ## TODO mmap and filebacked need to be fixed to first ungz files.
  fnames <- func_scans(x,subid=subid,task=task,run=run, modality=modality)
  mode <- match.arg(mode)
  neuroim2::read_vec(fnames, mask=mask, mode=mode, ...)
}



#' @inheritParams read_func_scans
#' @example 
#' 
#' proj <- bids_project(system.file("inst/extdata/megalocalizer", package="bidser"), fmriprep=TRUE)
#' sc <- read_preproc_scans(proj)
read_preproc_scans.bids_project <- function(x, mask=NULL, mode = c("normal", "bigvec"), subid="^sub-.*", task=".*", 
                                            run = ".*", modality="bold", ...) {
  
  fnames <- preproc_scans(x,subid=subid,task=task,run=run, modality=modality, full_path=TRUE)
  

  mode <- match.arg(mode)
  if (is.null(mask)) {
    mask <- create_preproc_mask(x, subid)
  }
  neuroim2::read_vec(fnames, mask=mask, mode=mode, ...)
}

#' @export
create_preproc_mask.bids_project <- function(x, subid, thresh=.99, ...) {
  maskfiles <- search_files(x, subid=subid, deriv="brainmask", full_path=TRUE, ...)
  vols <- lapply(maskfiles, neuroim2::read_vol)
  avg <- Reduce("+", vols)/length(vols)
  avg[avg < thresh] <- 0
  as.logical(avg)
}

DEFAULT_CVARS <- c("CSF", "WhiteMatter", "GlobalSignal", "stdDVARS", "non.stdDVARS",
                   "vx.wisestdDVARS", "FramewiseDisplacement", "tCompCor00", "tCompCor01", "tCompCor02",           
                   "tCompCor03", "tCompCor04", "tCompCor05", "aCompCor00", "aCompCor01",           
                    "aCompCor02" , "aCompCor03", "aCompCor04", "aCompCor05", "X", "Y", "Z",
                    "RotX", "RotY", "RotZ")

DEFAULT_CVARS2 <- c("csf", "white_matter", "global_signal", "std_dvars", 
                    "framewise_displacement", "t_comp_cor_00", "t_comp_cor_01", "t_comp_cor_02",           
                   "t_comp_cor_03", "t_comp_cor_04", "t_comp_cor_00", "a_comp_cor_00", "a_comp_cor_01",           
                   "a_comp_cor_02" , "a_comp_cor_03", "a_comp_cor_04", "a_comp_cor_03", "trans_x", "trans_y", "trans_z",
                   "rot_x", "rot_y", "rot_z")


#' @export
confound_files.bids_project <- function(x, subid=".*", task=".*", session=".*", nest=TRUE) {
  sids <- participants(x)
  gidx <- grep(subid, sids)
  if (length(gidx) == 0) {
    stop(paste("no matching participants found for regex: ", subid))
  }
  sids <- sids[gidx]
  ret <- lapply(sids, function(s) {
    ## new fmriprep (e.g. (> 1.5)), where confounds are denotedd by 'deriv-confounds``
    fnames1 <- search_files(x, subid=as.character(s), task=task, session=session, deriv="confounds", full_path=TRUE)
    ## old fmriprep (e.g. 1.1.8)
    fnames2 <- search_files(x, subid=as.character(s), task=task, session=session, desc="confounds", full_path=TRUE)
    c(fnames1, fnames2)
  })
  
  ret
}
  

#' read confound files
#' 
#' read in fmriprep confound tables for one or more subjects
#' 
#' @param subid (optional) subid regex selector
#' @param task (optional) task regex selector
#' @param cvars the names of the confound variables to select. If missing, defaults to a set defined by constant \code{DEFAULT_CVARS}.
#' @param npcs perform pca reduction on confound matrix and reduce to \code{npcs} dimensions
#' @param perc_var perform pca reduction on confound matrix and retain \code{perc_var} percent of total (selected) confound variance
#' @param nest nest confound tables by subject/sesssion/run
#' @import dplyr
#' @importFrom tidyr nest
#' @importFrom tidyselect all_of any_of
#' @export
read_confounds.bids_project <- function(x, subid=".*", task=".*", session=".*", cvars=DEFAULT_CVARS, 
                                        npcs=-1, perc_var=-1, nest=TRUE) {
  sids <- participants(x)
  gidx <- grep(subid, sids)
  if (length(gidx) == 0) {
    stop(paste("no matching participants found for regex: ", subid))
  }
  sids <- sids[gidx]
  
  ret <- lapply(sids, function(s) {
    fnames <- search_files(x, subid=as.character(s), task=task, 
                           session=session, deriv="(confounds|regressors)", strict=TRUE, full_path=TRUE)
    ret <- lapply(fnames, function(fn) {
      ## temporary hack
      run <- stringr::str_match(fn, "_run-(\\d+)")[1,2]
      session <- stringr::str_match(fn, "_ses-(\\d+)")[1,2]
      
      if (is.na(session)) {
        session <- "1"
      }
      ## 
      
      dfx <- read.table(fn, header=TRUE, na.strings=c("NA", "n/a")) %>% select(any_of(cvars)) 
      
      if (npcs >= 0 || perc_var > 0) {
        dfx <- process_confounds(dfx, npcs=npcs, perc_var=perc_var)
      }
      
      dfx %>% mutate(participant_id=s, run=run, session=session) 
    }) %>% bind_rows()
  }) %>% bind_rows()
  
  if (nest) {
    ret %>% group_by(participant_id,run,session) %>% tidyr::nest()
  } else {
    ret
  }
  
}


#' @keywords internal
process_confounds <- function(dfx, center=TRUE, scale=TRUE, npcs=-1, perc_var=-1) {
  m <- as.matrix(dfx)
  if (any(is.na(m))) {
    m <- apply(m, 2, function(v) {
      mu <- median(v, na.rm=TRUE)
      v[is.na(v)] <- mu
      v
    })
  }
  
  sm <- scale(m, center=center,scale=scale)
  
  if (npcs > 0 || perc_var > 0 && (ncol(sm) > 1)) {
    pres <- prcomp(sm, scale=TRUE)
    npcs <- min(npcs, ncol(pres$x))
    varexp <- cumsum(pres$sdev^2)/sum(pres$sdev^2) * 100
    if (npcs > 0 && perc_var <= 0) {
      
      sm <- pres$x[,1:npcs,drop=FALSE]
    } else if (npcs <= 0 && perc_var > 0) {
      keep <- which( (varexp - perc_var) > 0)[1]
      sm <- pres$x[, 1:keep,drop=FALSE]
    } else {
      keep <- which( (varexp - perc_var) > 0)[1]
      npcs <- max(c(keep, npcs))
      if (npcs < 1) {
        npcs <- 1
      }
      
      sm <- pres$x[, 1:npcs, drop=FALSE]
    }
    
    colnames(sm) <- paste0("PC", 1:ncol(sm))
  }
  
  as.data.frame(sm)
}




