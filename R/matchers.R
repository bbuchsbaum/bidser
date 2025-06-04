#' BIDS filename parsers using regex
#' 
#' These functions create parsers for different types of BIDS files using
#' regex-based pattern matching instead of parser combinators.

#' Create a parser for a generic BIDS file
#'
#' This parser tries to match against various known parsers (anat, func, fmriprep anat/func).
#' @return A BIDS parser object that can parse various types of BIDS files
#' @examples
#' # Create a generic BIDS parser
#' parser <- bids_parser()
#' 
#' # Parse different types of files
#' anat_result <- parse(parser, "sub-01_T1w.nii.gz")
#' func_result <- parse(parser, "sub-01_task-rest_bold.nii.gz")
#' prep_result <- parse(parser, "sub-01_task-rest_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz")
#' @export
bids_parser <- function() {
  # Create individual parsers
  anat_p <- anat_parser()
  func_p <- func_parser()
  fmriprep_anat_p <- fmriprep_anat_parser()
  fmriprep_func_p <- fmriprep_func_parser()
  
  # Create a combined parser that tries each in sequence
  # Try fMRIPrep parsers first since they handle more entities
  parser <- function(filename) {
    # Try fMRIPrep parsers first (they handle more entities like desc, space)
    result <- parse(fmriprep_func_p, filename)
    if (!is.null(result)) return(result)
    
    result <- parse(fmriprep_anat_p, filename)
    if (!is.null(result)) return(result)
    
    # Then try basic parsers
    result <- parse(func_p, filename)
    if (!is.null(result)) return(result)
    
    result <- parse(anat_p, filename)
    if (!is.null(result)) return(result)
    
    # No match found
    return(NULL)
  }
  
  ret <- list(parser = parser)
  class(ret) <- c("bids_parser", "parser")
  ret
}

#' @export
parse.parser <- function(x, fname, ...) {
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
#' @return A functional BIDS parser object for parsing functional MRI files
#' @examples
#' # Create a functional parser
#' parser <- func_parser()
#' 
#' # Parse a functional file
#' result <- parse(parser, "sub-01_task-rest_run-01_bold.nii.gz")
#' @export
func_parser <- function() {
  spec <- func_spec()
  parser <- gen_parser(spec)
  ret <- list(parser = parser)
  class(ret) <- c("func_parser", "parser")
  ret
}

#' Anatomical parser constructor
#'
#' @return An anatomical BIDS parser object for parsing anatomical files
#' @examples
#' # Create an anatomical parser
#' parser <- anat_parser()
#' 
#' # Parse an anatomical file
#' result <- parse(parser, "sub-01_T1w.nii.gz")
#' @export
anat_parser <- function() {
  spec <- anat_spec()
  parser <- gen_parser(spec)
  ret <- list(parser = parser)
  class(ret) <- c("anat_parser", "parser")
  ret
}

#' fMRIPrep anatomical parser constructor
#'
#' @return An fMRIPrep anatomical parser object for parsing preprocessed anatomical files
#' @examples
#' # Create an fMRIPrep anatomical parser
#' parser <- fmriprep_anat_parser()
#' 
#' # Parse a preprocessed anatomical file
#' result <- parse(parser, "sub-01_space-MNI152NLin2009cAsym_desc-preproc_T1w.nii.gz")
#' @export
fmriprep_anat_parser <- function() {
  spec <- anatprepspec()
  parser <- gen_parser(spec)
  ret <- list(parser = parser)
  class(ret) <- c("anatprep_parser", "parser")
  ret
}

#' fMRIPrep functional parser constructor
#'
#' @return An fMRIPrep functional parser object for parsing preprocessed functional files
#' @examples
#' # Create an fMRIPrep functional parser
#' parser <- fmriprep_func_parser()
#' 
#' # Parse a preprocessed functional file
#' result <- parse(parser, "sub-01_task-rest_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz")
#' @export
fmriprep_func_parser <- function() {
  spec <- funcprepspec()
  parser <- gen_parser(spec)
  ret <- list(parser = parser)
  class(ret) <- c("funcprep_parser", "parser")
  ret
}

#' Fieldmap parser constructor
#'
#' @return A fieldmap BIDS parser object for parsing fieldmap files
#' @examples
#' # Create a fieldmap parser
#' parser <- fmap_parser()
#' 
#' # Parse a fieldmap file
#' result <- parse(parser, "sub-01_magnitude1.nii.gz")
#' @export
fmap_parser <- function() {
  spec <- fmapspec()
  parser <- gen_parser(spec)
  ret <- list(parser = parser)
  class(ret) <- c("fmap_parser", "parser")
  ret
}