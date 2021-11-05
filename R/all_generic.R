
#' Parse a file-name
#'
#' @param x the string to parse
#' @param ... extra args
#' @export
parse <- function (x, fname,...) {
  UseMethod("parse", x)
}

#' Encode a string into a BIDS key-value list
#' 
#' @param x the string to encode
#' @param ... extra args
#' @export
encode <- function(x,...) {
  UseMethod("encode")
}


#' Decode a key-value list into a string
#' 
#' @param x the list to decode
#' @param ... extra args
#' @export
decode <- function(x,...) {
  UseMethod("decode")
}


#' Get sessions
#' 
#' get a vector of sessions in the project
#' 
#' @param x the `bids_project` object
#' @param ... extra args
#' @return a vector of session ids
#' @export
#' @examples 
#' p <- system.file("inst/extdata/ds001", package="bidser")
#' sessions(bids_project(p))
sessions <- function (x, ...) {
  UseMethod("sessions", x)
}


#' Get Tasks
#' 
#' get the tasks in the project
#' 
#' @param x the `bids_project` object
#' @param ... extra args
#' @return a vector of task names
#' 
#' @export
#' @examples 
#' p <- system.file("inst/extdata/ds001", package="bidser")
#' tasks(bids_project(p))
tasks <- function (x, ...) {
  UseMethod("tasks", x)
}

#' Get "flat" representation of BIDS Project
#' 
#' get a flattened (non-hiearchical) representation of project formatted as a `data.frame`
#' 
#' @param x the `bids_project` object
#' @param ... extra args
#' 
#' @export
#' @examples 
#' p <- system.file("inst/extdata/ds001", package="bidser")
#' flat_list(bids_project(p))
flat_list <- function(x, ...) {
  UseMethod("flat_list", x)
}

#' Get participants
#' 
#' get vector of participant ids
#' 
#' @param x the `bids_project` object
#' @param ... extra args
#' @export
#' @examples 
#' p <- system.file("inst/extdata/ds001", package="bidser")
#' participants(bids_project(p))
participants <- function (x, ...) {
  UseMethod("participants", x)
}

#' Get event files
#' 
#' get vector of event files in project
#' 
#' @param x the `bids_project` object
#' @param ... extra args
#' @export
#' 
#' @examples 
#' p <- system.file("inst/extdata/ds001", package="bidser")
#' event_files(bids_project(p))
event_files <- function (x, ...) {
  UseMethod("event_files", x)
}

#' Get list of confound files
#' 
#' get a vector of confound files in a bids project.
#' 
#' @param x the `bids_project` object
#' @param ... extra args
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
