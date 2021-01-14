
#' @export
parse <- function (x, fname,...) {
  UseMethod("parse", x)
}

#' @export
encode <- function(x,...) {
  UseMethod("encode")
}

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

#' @export
confound_files <- function (x, ...) {
  UseMethod("confound_files", x)
}

#' @export
read_events <- function (x, ...) {
  UseMethod("read_events", x)
}

# @export
#match_attr <- function (x, ...) {
#  UseMethod("match_attr", x)
#}


#' @export
read_confounds <- function (x, ...) {
  UseMethod("read_confounds", x)
}

#' extract functional scans from bids project
#' 
#' @export
func_scans <- function(x, ...) {
  UseMethod("func_scans", x)
}

#' get pre-processed fmri scans
#' 
#' extract fmriprep-created `preproc` scans from bids project
#' 
#' @export
preproc_scans <- function(x, ...) {
  UseMethod("preproc_scans", x)
}

#' create a binary mask from pre-processed functional scans
#' 
#' @export
create_preproc_mask <- function(x, ...) {
  UseMethod("create_preproc_mask", x)
}

#' search files in BIDS structure
#' 
#' find files in BIDS tree by matching file names
#' 
#' @export
search_files <- function(x, ...) {
  UseMethod("search_files", x)
}

#' @export
anomalies <- function(x, ...) {
  UseMethod("anomalies", x)
}
