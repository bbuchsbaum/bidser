#' Plot a BIDS project as a dendrogram
#'
#' This method visualises the hierarchical file structure of a BIDS
#' project. The tree is converted to a dendrogram and drawn using base
#' graphics. Large projects can be trimmed by setting a maximum depth.
#'
#' @param x A \code{bids_project} object.
#' @param max_depth Maximum depth of the tree to display. Defaults to
#'   \code{Inf} so the full hierarchy is shown.
#' @param ... Additional arguments passed to \code{graphics::plot}.
#'
#' @return The input object \code{x} is returned invisibly.
#' @export
#' @examples
#' \donttest{
#' proj <- bids_project(system.file("extdata/ds001", package = "bidser"))
#' plot(proj)
#' }
plot.bids_project <- function(x, max_depth = Inf, ...) {
  if (!inherits(x, "bids_project")) {
    stop("x must be a 'bids_project' object")
  }
  if (!requireNamespace("data.tree", quietly = TRUE)) {
    stop("Package 'data.tree' is required for plotting")
  }

  tree <- data.tree::Clone(x$bids_tree)
  if (is.finite(max_depth)) {
    data.tree::Prune(tree, pruneFun = function(node) node$level <= max_depth)
  }
  dend <- data.tree::as.dendrogram(tree)

  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  graphics::plot(dend, horiz = TRUE, main = x$name, ...)

  invisible(x)
}
