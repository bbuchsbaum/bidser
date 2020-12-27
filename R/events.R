

#' @export
event_files.bids_project <- function (x, subid=".*", task=".*", run=".*", session=".*", full_path=TRUE, ...) {
  search_files(x, modality = "events", subid=subid, task=task, session=session, run=run, full_path=full_path, ...)
}

#' @export
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
      if (length(evs) > 0) {
        runs <- sapply(evs, function(ev) parse(p, basename(ev))$result$run)
        res <- lapply(evs, read.table, header=TRUE,stringsAsFactors=FALSE, na.strings=c("n/a", "NA"))
        lapply(1:length(res), function(i) dplyr::mutate(res[[i]], .subid=sid, .run=runs[i])) %>% bind_rows()    
      } else {
        NULL
      }
    }) %>% bind_rows() %>% mutate(.task=t) %>% group_by(.task, .run, .subid) %>% nest()
  }) %>% bind_rows() 
  
}

