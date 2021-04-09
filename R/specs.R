

#' given a match specification specification, generate a parser
#' 
#' @keywords internal
#' @param spec the specification as a table with `keystruc` and `modality` tables
#' @param typename the name given to the final type element
gen_parser <- function(spec, typename="modality") {
  keystruc <- spec$keystruc
  
  keymatchers <- lapply(1:nrow(keystruc), function(i) {
    if (i == 1) {
      start_key(keystruc$key[[i]], keystruc$pattern[[i]])
    } else {
      if (is.null(keystruc$pattern[[i]])) {
        if (is.list(keystruc$key[[i]])) {
          zero_or_one_of(unlist(keystruc$key[[i]]), keystruc$name[[i]])
        } else {
          optional_literal(keystruc$key[[i]],keystruc$name[[i]])
        }
      } else if (keystruc$optional[[i]]) {
        optional_key(keystruc$key[[i]], keystruc$pattern[[i]])
      } else {
        mandatory_key(keystruc$key[[i]], keystruc$pattern[[i]])
      }
    }
  })
  
 
  typematchers <- lapply(1:nrow(spec$modalities), function(i) {
    pat <- spec$modalities$suffix[i]
    if (is.list(pat)) {
      fun <- purrr::partial(pAlt, tag=function(x) {  x })
      lits <- lapply(unlist(pat), pLiteral)
      pSeq(alt_extractor, pLiteral(spec$modalities$modality[i]), do.call(fun, lits))
    } else {
      pSeq(extractor, pLiteral(spec$modalities$modality[i]), pLiteral(pat))
    }
  })
  
  p_alt <- purrr::partial(pAlt, tag=function(x) x)
  
  typematchers <- do.call(p_alt,typematchers)
  typematchers <- pSeq(function(x) {  x[[2]] }, pLiteral("_"), typematchers)
  
  filematcher <- c(keymatchers, typematchers)
  
  builder <- function(x) {
   
    out <- list()
    for (i in 1:nrow(spec$keystruc)) {
      if (i == 1) {
        out[[spec$keystruc$name[i]]] <- x[[i]]
      } else if (spec$keystruc$optional[i]) {
        out[[spec$keystruc$name[i]]] <- unlist(x[[i]]$value)
      } else {
        out[[spec$keystruc$name[i]]] <- x[[i]]
      }
    }
    

    index <- nrow(spec$keystruc)+1
    out[[typename]] <- x[[index]]$type
    out$suffix <- substr(x[[index]]$suffix, 2, nchar(x[[index]]$suffix))
    out$type <- spec$type
    out
  }
  
  pseq <- purrr::partial(pSeq, tag=builder)
  pout <- do.call(pseq, filematcher)

}

## "preproc" is modality instead of "bold"
## "sub-1001_task-phoneme_run-01_bold_space-MNI152NLin2009cAsym_preproc.nii.gz"

#' func spec
#' 
#' create a spec table for "func" files
#' 
#' @details 
#' 
#' "spec" objects describe the pattern rules for matching bids files for a given domain, e.g. ("func", "anat", etc.)
#' A specification consists of two tables, a `keystruc` table and a `modalities` table. The `keystruc` table describes
#' the allowable keys (e.g. `sub`, `ses`, etc.), their order, whether they are "optional" or not, and a regular expression
#' indicating constraints on the pattern of the value associated with the key. The `name` field indicates how each key 
#' can be referred to in the resultant parsed list structure, e.g. (`sub` will be named `subid`).
#' 
#' The `modalities` table describes the types of files and their suffixes that are allowed. the `modality` variable
#' indicates the name of the file category (e.g. "bold") and suffix is used match different file formats. 
#' 
#' @keywords internal
func_spec <- function() {
  keystruc <- tribble(
    ~name, ~key, ~optional, ~pattern,
    "subid", "sub", FALSE, "[A-Za-z0-9]+",
    "session", "ses", TRUE, "[A-Za-z0-9]+",
    "task", "task", FALSE, "[A-Za-z0-9]+",
    "acquisition", "acq", TRUE, "[A-Za-z0-9]+",
    "contrast", "ce", TRUE, "[A-Za-z0-9]+",
    "reconstruction", "rec", TRUE, "[A-Za-z0-9]+",
    "run", "run", TRUE, "[0-9]+",
    "echo", "echo", TRUE, "[0-9]+"
  )
  
  modalities <- tribble(
    ~modality, ~suffix,
    "bold", list(".nii.gz",".nii", ".json"),
    "events", ".tsv",
    "sbref", list(".nii.gz",".nii", ".json"),
    "physio", ".tsv"
  )
  
  ret <- list(keystruc=keystruc, modalities=modalities, type="func")
  class(ret) <- "parser_spec"
  ret
}



#' create a spec table for "anat" files
#' @keywords internal
anat_spec <- function() {
  keystruc <- tribble(
    ~name, ~key, ~optional, ~pattern,
    "subid", "sub", FALSE, "[A-Za-z0-9]+",
    "session", "ses", TRUE, "[A-Za-z0-9]+",
    "acquisition", "acq", TRUE, "[A-Za-z0-9]+",
    "contrast", "ce", TRUE, "[A-Za-z0-9]+",
    "dir", "dir", TRUE, "[A-Za-z0-9]+",
    "reconstruction", "rec", TRUE, "[A-Za-z0-9]+",
    "run", "run", TRUE, "[0-9]+"
  )
  
  modalities <- tribble(
    ~modality, ~ suffix,
    "defacemask", list(".nii.gz", ".nii", ".json"),
    "T1w", list(".nii.gz", ".nii", ".json"),
    "T2w", list(".nii.gz", ".nii", ".json"),
    "T1map", list(".nii.gz", ".nii", ".json"),
    "T2map", list(".nii.gz", ".nii", ".json"),
    "T2star", list(".nii.gz", ".nii", ".json"),
    "FLAIR", list(".nii.gz", ".nii", ".json"),
    "FLASH", list(".nii.gz", ".nii", ".json"),
    "PDmap", list(".nii.gz", ".nii", ".json"),
    "PDT2", list(".nii.gz", ".nii", ".json"),
    "inplaneT1", list(".nii.gz", ".nii", ".json"),
    "inplaneT2", list(".nii.gz", ".nii", ".json"),
    "angio", list(".nii.gz", ".nii", ".json"),
    
  )
  
  ret <- list(keystruc=keystruc, modalities=modalities, type="func")
  class(ret) <- "parser_spec"
  ret
}

#' create a spec table for fmriprep "func" files
#' @keywords internal
funcprepspec <- function() {
  keystruc <- tribble(
    ~name, ~key, ~optional, ~pattern,
    "subid", "sub", FALSE, "[A-Za-z0-9]+",
    "session", "ses", TRUE, "[A-Za-z0-9]+",
    "task", "task", FALSE, "[A-Za-z0-9]+",
    "acquisition", "acq", TRUE, "[A-Za-z0-9]+",
    "contrast", "ce", TRUE, "[A-Za-z0-9]+",
    "reconstruction", "rec", TRUE, "[A-Za-z0-9]+",
    "run", "run", TRUE, "[0-9]+",
    "modality", "bold", TRUE, NULL,
    "space", "space", TRUE,"[A-Za-z0-9]+",
    "res", "res", TRUE,"[A-Za-z0-9]+",
    "desc", "desc", TRUE,"[A-Za-z0-9]+",
    "label", "label", TRUE,"[A-Za-z0-9]+",
    "variant", "variant", TRUE,"[A-Za-z0-9]+")

  modalities <- tribble(
    ~modality, ~ suffix,
    "roi", list(".nii.gz", ".nii", ".json"),
    "regressors", ".tsv",
    "preproc", list(".nii.gz", ".nii", ".json"),
    "bold", list(".nii.gz", ".nii", ".json"),
    "brainmask", list(".nii.gz", ".nii", ".json"),
    "confounds", ".tsv",
    "MELODICmix", ".tsv",
    "AROMAnoiseICs", ".tsv"
  )
  
  ret <- list(keystruc=keystruc, modalities=modalities, type="funcprep")
  class(ret) <- "parser_spec"
  ret
}

#' create a spec table for fmriprep "anat" files
#' @keywords internal
anatprepspec <- function() {
  
  anat_types <- c("defacemask","T1w", "T2w","T1map", "T2map", "T2star","FLAIR", "FLASH", "PDmap","PD","PDT2",
                  "inplaneT1", "inplaneT2", "angio")
  
  keystruc <- tribble(
    ~name, ~key, ~optional, ~pattern,
    "subid", "sub", FALSE, "[A-Za-z0-9]+",
    "session", "ses", TRUE,"[A-Za-z0-9]+",
    "acquisition", "acq",TRUE,"[A-Za-z0-9]+",
    "contrast", "ce", TRUE,"[A-Za-z0-9]+",
    "dir", "dir", TRUE,"[A-Za-z0-9]+",
    "reconstruction", "rec",TRUE,"[A-Za-z0-9]+",
    "run", "run",TRUE,"[0-9]+",
    "modality", list(anat_types),TRUE, NULL,
    "label", "label",TRUE,"[A-Za-z0-9]+",
    "space", "space",TRUE,"[A-Za-z0-9]+",
    "desc", "desc",TRUE,"[A-Za-z0-9]+",
    "target", "target",TRUE,"[A-Za-z0-9]+",
    "class", "class",TRUE,"[A-Za-z0-9]+",
    "mod", "mod",TRUE,"[A-Za-z0-9]+"
    )
  
  modalities <- tribble(
    ~modality, ~ suffix,
    "preproc",  list(".nii.gz", ".nii", ".json"),
    "brainmask", list(".nii.gz", ".nii", ".json"),
    "probtissue",list(".nii.gz", ".nii", ".json"),
    "mask", list(".nii.gz", ".nii", ".json"),
    "T1w", list(".nii.gz", ".nii", ".json"),
    "probseg", list(".nii.gz", ".nii", ".json"),
    "dtissue", list(".nii.gz", ".nii", ".json"),
    "warp", ".h5",
    "inflated.L.surf", ".gii",
    "inflated.R.surf", ".gii",
    "midthickness.L.surf", ".gii",
    "midthickness.R.surf", ".gii",
    "pial.L.surf", ".gii",
    "pial.R.surf", ".gii",
    "smoothwm.L.surf", ".gii",
    "smoothwm.R.surf", ".gii",
    "roi", list(".nii.gz", ".nii", ".json"),
    "affine", ".txt"
  )
  
  ret <- list(keystruc=keystruc, modalities=modalities, type="anatprep")
  class(ret) <- "parser_spec"
  ret
  
}


#' @keywords internal
fmapspec <- function() {
  keystruc <- tribble(
    ~name, ~key, ~optional, ~pattern,
    "subid", "sub", FALSE, "[A-Za-z0-9]+",
    "session", "ses", TRUE, "[A-Za-z0-9]+",
    "acquisition", "acq", TRUE, "[A-Za-z0-9]+",
    "contrast", "ce", TRUE, "[A-Za-z0-9]+",
    "dir", "dir", TRUE, "[A-Za-z0-9]+",
    "reconstruction", "rec", TRUE, "[A-Za-z0-9]+",
    "run", "run", TRUE, "[0-9]+",
    "mod", "mod", TRUE, "[A-Za-z0-9]+"
  )
  
  modalities <- tribble(
    ~modality, ~ suffix,
    "magnitude", list(".nii.gz", ".nii", ".json"),
    "magnitude1", list(".nii.gz", ".nii", ".json"),
    "magnitude2", list(".nii.gz", ".nii", ".json"),
    "phase", list(".nii.gz", ".nii", ".json"),
    "phase1", list(".nii.gz", ".nii", ".json"),
    "phase2", list(".nii.gz", ".nii", ".json"),
    "phasediff", list(".nii.gz", ".nii", ".json")
  )
  
  ret <- list(keystruc=keystruc, modalities=modalities, type="fmap")
  class(ret) <- "parser_spec"
  ret
  
  
}


