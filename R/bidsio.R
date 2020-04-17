
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


#' @export
confound_files.bids_project <- function(x, subid=".*", task=".*", nest=TRUE) {
  sids <- participants(x)
  gidx <- grep(subid, sids)
  if (length(gidx) == 0) {
    stop(paste("no matching participants found for regex: ", subid))
  }
  sids <- sids[gidx]
  ret <- lapply(sids, function(s) {
    fnames <- search_files(x, subid=as.character(s), task=task, deriv="confounds", full_path=TRUE)
  })
  
  ret
}
  


#' read in fmriprep confound tables for a set of subjects
#' 
#' @param subid (optional) subid regex selector
#' @param task (optional) task regex selector
#' @param cvars a vector of confound variables to select. If missing, defaults to a set defined by constant \code{DEFAULT_CVARS}.
#' @param npcs perform pca reduction on confound matrix and reduce to \code{npcs} dimensions
#' @param perc_var perform pca reduction on confound matrix and retain \code{perc_var} percent of variance
#' @import dplyr
#' @importFrom tidyr nest
#' @importFrom tidyselect all_of
#' @export
read_confounds.bids_project <- function(x, subid=".*", task=".*", cvars=DEFAULT_CVARS, 
                                        npcs=-1, perc_var=-1, nest=TRUE) {
  sids <- participants(x)
  gidx <- grep(subid, sids)
  if (length(gidx) == 0) {
    stop(paste("no matching participants found for regex: ", subid))
  }
  sids <- sids[gidx]
  
  ret <- lapply(sids, function(s) {
    fnames <- search_files(x, subid=as.character(s), deriv="confounds", full_path=TRUE)
    ret <- lapply(fnames, function(fn) {
      dfx <- read.table(fn, header=TRUE, na.strings=c("NA", "n/a")) %>% select(all_of(cvars)) 
      if (npcs >= 0 || perc_var > 0) {
        dfx <- process_confounds(dfx, npcs=npcs, perc_var=perc_var)
      }
      
      dfx %>% mutate(participant_id=s) 
    }) %>% bind_rows()
  }) %>% bind_rows()
  
  if (nest) {
    ret %>% group_by(participant_id) %>% tidyr::nest()
  } else {
    ret
  }
  
}

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


read_events.bids_project <- function(x, subid = ".*", task = ".*") {
  pt <- participants(x)
  idx <- grep(subid, pt)
  
  if (length(x) == 0) {
    stop(paste("no matching participants for 'subid' regex: ", subid))
  }
  
  sids <- pt[idx]
  
  taskset <- tasks(x)
  task.idx <- grep(task, taskset)
  
  if (length(task.idx) == 0) {
    stop(paste("no matching tasks for 'task' regex: ", task))
  }
  
  tasks <- taskset[task.idx]
  p=func_parser()
  
  lapply(tasks, function(t) {
    lapply(sids, function(sid) {
      evs <- event_files(x,subid=as.character(sid),task=t)
      runs <- sapply(evs, function(ev) parse(p, basename(ev))$result$run)
      res <- lapply(evs, read.table, header=TRUE,stringsAsFactors=FALSE, na.strings=c("n/a", "NA"))
      lapply(1:length(res), function(i) dplyr::mutate(res[[i]], .subid=sid, .run=runs[i])) %>% bind_rows()       
    }) %>% bind_rows() %>% mutate(.task=t) %>% group_by(.task, .run, .subid) %>% nest()
  }) %>% bind_rows() 
  

}



