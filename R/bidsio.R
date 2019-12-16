read_func_data <- function(x, mask, mode = c("bigvec", 
                                                  "mmap", 
                                                  "filebacked",
                                                  "normal"), subid="^sub-.*", task=".*", run = ".*", modality="bold", ...) {
  fnames <- func_scans(x,subid=subid,task=task,run=run, modality=modality)
  mode <- match.arg(mode)
  neuroim2::read_vec(fnames, mask=mask, mode=mode, ...)
}