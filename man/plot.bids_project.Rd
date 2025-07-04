% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_bids_project.R
\name{plot.bids_project}
\alias{plot.bids_project}
\alias{plot.mock_bids_project}
\title{Plot a BIDS project as a dendrogram}
\usage{
\method{plot}{bids_project}(x, max_depth = Inf, ...)

\method{plot}{mock_bids_project}(x, max_depth = Inf, ...)
}
\arguments{
\item{x}{A \code{bids_project} object.}

\item{max_depth}{Maximum depth of the tree to display. Defaults to
\code{Inf} so the full hierarchy is shown.}

\item{...}{Additional arguments passed to \code{graphics::plot}.}
}
\value{
The input object \code{x} is returned invisibly.
}
\description{
This method visualises the hierarchical file structure of a BIDS
project. The tree is converted to a dendrogram and drawn using base
graphics. Large projects can be trimmed by setting a maximum depth.
}
\examples{
\donttest{
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  plot(proj)
  
  # Clean up
  unlink(ds001_path, recursive=TRUE)
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})
}
}
