
#' @export
parse <- function (x, fname,...) {
  UseMethod("parse", x)
}

#' @export
encode <- function(x,...) {
  UseMethod("encode")
}


#' Get sessions
#' 
#' get a vector of sessions in the project
#' 
#' @param x the `bids_project` object
#' @param ... extra args
#' @export
sessions <- function (x, ...) {
  UseMethod("sessions", x)
}

#' @export
tasks <- function (x, ...) {
  UseMethod("tasks", x)
}


#' @export
flat_list <- function(x, ...) {
  UseMethod("flat_list", x)
}


#' @export
participants <- function (x, ...) {
  UseMethod("participants", x)
}


#' @export
event_files <- function (x, ...) {
  UseMethod("event_files", x)
}

#' Get list of confound files
#' 
#' get a vector of confound files in a bids project.
#' 
#' @export
confound_files <- function (x, ...) {
  UseMethod("confound_files", x)
}

#' read in event files
#' 
#' read tab-separated event files as a `data.frame`
#' 
#' @param x the `bids_project` object
#' @param ... extra args
#' 
#' @export
read_events <- function (x, ...) {
  UseMethod("read_events", x)
}

# @export
#match_attr <- function (x, ...) {
#  UseMethod("match_attr", x)
#}


#' read in confound files
#' 
#' 
#' @export
#' @param x the `bids_project` object
#' @param ... extra args
read_confounds <- function (x, ...) {
  UseMethod("read_confounds", x)
}

#' Get functional scans
#' 
#' extract functional scans from bids project
#' 
#' @param x the `bids_project` object
#' @param ... extra args
#' @export
func_scans <- function(x, ...) {
  UseMethod("func_scans", x)
}

#' Get pre-processed fmri scans 
#' 
#' extract fmriprep-created `preproc` scans from bids project
#' 
#' @param x the `bids_project` object
#' @param ... extra args
#' @export
preproc_scans <- function(x, ...) {
  UseMethod("preproc_scans", x)
}

#' create pre-processing mask
#' 
#' create a binary mask from pre-processed functional scans
#' 
#' @param x the `bids_project` object
#' @param ... extra args
#' @export
create_preproc_mask <- function(x, ...) {
  UseMethod("create_preproc_mask", x)
}

#' Search files in BIDS structure
#' 
#' find files in BIDS tree by matching file names
#' 
#' @param x the `bids_project` object
#' @param ... extra args
#' @export
search_files <- function(x, ...) {
  UseMethod("search_files", x)
}

#' @export
anomalies <- function(x, ...) {
  UseMethod("anomalies", x)
}
