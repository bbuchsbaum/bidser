#' @importFrom Combin8R pSeq pLiteral pRegex pAlt pMany
NULL


#' Create a parser for a generic BIDS file
#'
#' This parser tries to match against various known parsers (anat, func, fmriprep anat/func).
#' @keywords internal
bids_parser <- function() {
  parser <- pAlt(
    "bids_generic",
    anat_parser()$parser,
    func_parser()$parser,
    fmriprep_anat_parser()$parser,
    fmriprep_func_parser()$parser
  )
  
  ret <- list(parser = parser)
  class(ret) <- c("bids_parser", "parser")
  ret
}

#' @export
parse.parser <- function(x, fname) {
  if (!is.list(x) || !"parser" %in% class(x)) {
    stop("`x` must be a valid parser object.")
  }
  if (!is.character(fname) || length(fname) != 1) {
    stop("`fname` must be a single character string (a filename).")
  }
  x$parser(fname)
}

#' Functional parser constructor
#'
#' @keywords internal
func_parser <- function() {
  spec <- func_spec()
  parser <- gen_parser(spec)
  ret <- list(parser = parser)
  class(ret) <- c("func_parser", "parser")
  ret
}

#' Anatomical parser constructor
#'
#' @keywords internal
anat_parser <- function() {
  spec <- anat_spec()
  parser <- gen_parser(spec)
  ret <- list(parser = parser)
  class(ret) <- c("anat_parser", "parser")
  ret
}

#' fMRIPrep anatomical parser constructor
#'
#' @keywords internal
fmriprep_anat_parser <- function() {
  spec <- anatprepspec()
  parser <- gen_parser(spec)
  ret <- list(parser = parser)
  class(ret) <- c("anatprep_parser", "parser")
  ret
}

#' fMRIPrep functional parser constructor
#'
#' @keywords internal
fmriprep_func_parser <- function() {
  spec <- funcprepspec()
  parser <- gen_parser(spec)
  ret <- list(parser = parser)
  class(ret) <- c("funcprep_parser", "parser")
  ret
}

#' Fieldmap parser constructor
#'
#' @keywords internal
fmap_parser <- function() {
  spec <- fmap_spec()
  parser <- gen_parser(spec)
  ret <- list(parser = parser)
  class(ret) <- c("fmap_parser", "parser")
  ret
}