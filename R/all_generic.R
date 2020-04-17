
#' @export
parse <- function (x, fname,...) {
  UseMethod("parse", x)
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

#' extract fmriprep "preproc" scans from bids project
#' @export
preproc_scans <- function(x, ...) {
  UseMethod("preproc_scans", x)
}

#' @export
create_preproc_mask <- function(x, ...) {
  UseMethod("create_preproc_mask", x)
}


#' @export
search_files <- function(x, ...) {
  UseMethod("search_files", x)
}
