
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
func_scans <- function(x, ...) {
  UseMethod("func_scans", x)
}

#' @export
scans <- function(x, ...) {
  UseMethod("scans", x)
}
