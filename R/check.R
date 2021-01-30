#check_tasks <- function(x) {
#  fnames <- search_files(x, type="func", regex="task-.*-")  
#}


#' x <- bids_project(system.file("inst/extdata/phoneme_stripped", package="bidser"), fmriprep=TRUE)
#' @import fs
check_func_scans <- function(x) {
  fscans <- func_scans(x)
  tasklist <- tasks(x)
  
  ret <- lapply(fscans, function(fn) {
    enc <- encode(basename(fn))
    sz <- file_size(fn)
    if (is.null(enc)) {
      warning("could not encode file: ", fn)
      tibble(file=fn)
    } else {
      as_tibble(enc) %>% mutate(file=basename(fn), size=sz) %>% select(file, size, everything())
    }
  }) %>% bind_rows()
  
  scans_per_subject <- ret %>% group_by(subid) %>% summarize(nscans=n())
  size_per_subject <- ret %>% group_by(subid) %>% mutate(size_delta=size - median(size))
  
  if (length(tasklist) > 1) {
    scans_per_task <- ret %>% group_by(task) %>% summarize(nscans=n())
    scans_per_task_subject <- ret %>% group_by(subid, task) %>% summarize(nscans=n())
    size_per_task <- ret %>% group_by(task) %>% mutate(size_delta=size - median(size))
  
    out <- list(scans=ret,
                tasklist=tasklist,
                scans_per_subject=scans_per_subject,
                scans_per_task=scans_per_task,
                scans_per_task_subject=scans_per_task_subject,
                size_per_task=size_per_task)
  } else {
    out <- list(scans=ret,
                tasklist=tasklist,
                scans_per_subject=scans_per_subject,
                size_per_subject=size_per_subject)
  }
  
  class(out) <- c("check", "check_func_scans")     
  out     
}

#' @importFrom stringdist stringdistmatrix
#' @export
file_pairs <- function(x, pair=c("bold-events", "preproc-events"), task=".*", matchon=c("run", "task"), ...) {
  assertthat::assert_that(inherits(x, "bids_project"))
  
  pair <- match.arg(pair)
  sids <- participants(x)
  
  type1 <- strsplit(pair, "-")[[1]][1]
  type2 <- strsplit(pair, "-")[[1]][2]
  
  if (pair == "bold-event") {
    regex1 <- paste0("bold.nii(.gz)*", "$")
    regex2 <- paste0("events.tsv", "$")
  } else if (pair == "preproc-event") {
    regex1 <- paste0("preproc.nii(.gz)*", "$")
    regex2 <- paste0("events.tsv", "$")
  }
  
  task_regex <- task
  
  lapply(sids, function(s) {
    
      s1 <- filter(x$tbl, subid==s & modality == type1 & (suffix == "nii" | suffix == "nii.gz") & 
                     stringr::str_detect(task, task_regex))
      #s1 <- search_files(x, subid=s, regex=regex1, task=task)
      #s2 <- search_files(x, subid=s, regex=regex2, task=task)
      
      s2 <- filter(x$tbl, subid==s & modality == type2 & stringr::str_detect(task, task_regex))
      
      if (nrow(s2) == 0) {
        return(tibble(subid=s, task=s1$task, !!rlang::sym(type1):=s1$name, !!rlang::sym(type2):=NA))
      }
      if (nrow(s1) == 0) {
        return(tibble(subid=s, task=NA, !!rlang::sym(type1):=NULL, !!rlang::sym(type2):=NULL))
      }
      
      #enc1 <- lapply(s1, function(fn) encode(basename(fn)))
      #enc2 <- lapply(s2, function(fn) encode(basename(fn)))
      
      enc1 <- lapply(s1$name, function(fn) encode(basename(fn)))
      enc2 <- lapply(s2$name, function(fn) encode(basename(fn)))
      
      mat1 <- cbind(s1[, matchon])
      mat2 <- cbind(s2[, matchon])
      #mat1 <- do.call(cbind, lapply(matchon, function(key) {
      #  out <- sapply(enc1, "[[", key)
      #  out[is.null(out)] <- "NULL"
      #  out
      #}))
      
      #mat2 <- do.call(cbind, lapply(matchon, function(key) {
      #  out <- sapply(enc2, "[[", key)
      #  out[is.null(out)] <- "NULL"
      #  out
      #}))
      
      sdmat <- stringdistmatrix(apply(mat1,1,paste,collapse="-"),apply(mat2,1,paste,collapse="-"))
      
      s2match <- apply(sdmat,1, function(z) {
        zmin <- min(z)
        if (zmin == 0) {
          s2$name[which.min(z)]
        } else {
          NA
        }
      })
      
      tibble(subid=s, task=s1$task, !!rlang::sym(type1):=s1$name, !!rlang::sym(type2):=s2match)
    }) %>% bind_rows()
    
}
# 
# check_counfounds <- function(x) {
#   
# }


#anomalies.check_func_scans <- function(x) {
#  
#}
