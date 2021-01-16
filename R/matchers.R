#' @importFrom Combin8R pSeq pLiteral pRegex pAlt pMany
NULL


#' parse any BIDS file types
#' @keywords internal
bids_parser <- function() {
  parser <- pAlt("bids_generic",
                 anat_parser()$parser,
                 func_parser()$parser,
                 fmriprep_anat_parser()$parser,
                 fmriprep_func_parser()$parser)
  
  ret <- list(parser=parser)
  class(ret) <- c("bids_parser", "parser")
  ret
}


#' @export
parse.parser <- function(x, fname) {
  x$parser(fname)
}



func_parser <- function() {
  spec <- func_spec()
  parser <- gen_parser(spec)
  ret <- list(parser=parser)
  class(ret) <- c("func_parser", "parser")
  ret
}
                                                                               
anat_parser <- function() {
  spec <- anat_spec()
  parser <- gen_parser(spec)
  ret <- list(parser=parser)
  class(ret) <- c("anat_parser", "parser")
  ret
}

fmriprep_anat_parser <- function() {
  spec <- anatprepspec()
  parser <- gen_parser(spec)
  ret <- list(parser=parser)
  class(ret) <- c("anatprep_parser", "parser")
  ret
}

fmriprep_func_parser <- function() {
  spec <- funcprepspec()
  parser <- gen_parser(spec)
  ret <- list(parser=parser)
  class(ret) <- c("funcprep_parser", "parser")
  ret
}

fmap_parser <- function() {
  spec <- fmap_spec()
  parser <- gen_parser(spec)
  ret <- list(parser=parser)
  class(ret) <- c("fmap_parser", "parser")
  ret
}