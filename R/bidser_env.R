#' Internal environment for package-wide state
#'
#' This environment is used to store session level objects such as the
#' example dataset cache without assigning them to the global environment.
#' @return An environment used for internal package state.
#' @keywords internal
bidser_pkg_env <- new.env(parent = emptyenv())
