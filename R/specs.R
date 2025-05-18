#' Given a match specification, generate a parser
#'
#' @param spec A specification object containing `keystruc` and `kinds` tables.
#'   `keystruc` defines the keys and their regex patterns, and `kinds` defines 
#'   the possible file types and suffixes.
#' @param typename The name given to the final type element. Default is "kind".
#' @return A parser function generated from the specification.
#' @examples
#' spec <- func_spec()
#' parser <- gen_parser(spec)
#' @keywords internal
gen_parser <- function(spec, typename = "kind") {
  # Check input
  if (!is.list(spec) || !all(c("keystruc", "kinds", "type") %in% names(spec))) {
    stop("`spec` must be a list containing 'keystruc', 'kinds', and 'type'.")
  }
  if (!is.data.frame(spec$keystruc) || !is.data.frame(spec$kinds)) {
    stop("`spec$keystruc` and `spec$kinds` must be data frames.")
  }
  if (!is.character(typename) || length(typename) != 1) {
    stop("`typename` must be a single character string.")
  }
  
  keystruc <- spec$keystruc
  
  # Construct key matchers from the specification
  keymatchers <- lapply(seq_len(nrow(keystruc)), function(i) {
    key <- keystruc$key[[i]]
    pat <- keystruc$pattern[[i]]
    opt <- keystruc$optional[[i]]
    nm  <- keystruc$name[[i]]
    
    if (i == 1) {
      # The first key is mandatory and at the start, so we use start_key
      start_key(key, pat)
    } else {
      # Subsequent keys can be optional or mandatory, or can be a zero_or_one_of variant
      if (is.null(pat)) {
        # This likely means a literal or zero_or_one_of scenario
        if (is.list(key)) {
          zero_or_one_of(unlist(key), nm)
        } else {
          optional_literal(key, nm)
        }
      } else {
        # Key has a pattern
        if (opt) {
          optional_key(key, pat)
        } else {
          mandatory_key(key, pat)
        }
      }
    }
  })
  
  # Construct type matchers for the kinds of files this spec supports
  # Each row in `kinds` defines a `kind` and its associated suffixes
  typematchers <- lapply(seq_len(nrow(spec$kinds)), function(i) {
    knd <- spec$kinds$kind[i]
    suf <- spec$kinds$suffix[i]
    
    if (is.list(suf)) {
      # Multiple possible suffixes
      lits <- lapply(unlist(suf), pLiteral)
      # Combine suffix literals into an alternative parser
      fun <- purrr::partial(pAlt, tag = function(x) x)
      pSeq(alt_extractor, pLiteral(knd), do.call(fun, lits))
    } else {
      # Single suffix
      pSeq(extractor, pLiteral(knd), pLiteral(suf))
    }
  })
  
  # Create a combined type matcher that starts with "_"
  p_alt <- purrr::partial(pAlt, tag = function(x) x)
  typematchers <- do.call(p_alt, typematchers)
  typematchers <- pSeq(function(x) x[[2]], pLiteral("_"), typematchers)
  
  # Combine key matchers and type matchers into one sequence
  filematcher <- c(keymatchers, list(typematchers))
  
  # A builder function to construct the output list from parsed values
  builder <- function(x) {
    out <- list()
    for (i in seq_len(nrow(spec$keystruc))) {
      nm  <- spec$keystruc$name[i]
      opt <- spec$keystruc$optional[i]
      
      if (i == 1) {
        # The first key is mandatory
        out[[nm]] <- x[[i]]
      } else if (opt) {
        out[[nm]] <- unlist(x[[i]]$value)
      } else {
        out[[nm]] <- x[[i]]
      }
    }
    
    # The type and suffix are after all keys
    index <- nrow(spec$keystruc) + 1
    out[[typename]] <- x[[index]]$type
    # Remove the leading "." from suffix if present
    out$suffix <- substring(x[[index]]$suffix, 2)
    out$type <- spec$type
    
    out
  }
  
  # Create the final parser
  pseq <- purrr::partial(pSeq, tag = builder)
  pout <- do.call(pseq, filematcher)
  pout
}


#' Create a spec table for "func" files
#'
#' The `spec` object describes pattern rules for matching BIDS filenames.
#' It consists of a `keystruc` table and a `kinds` table. The `keystruc` table 
#' defines keys, their regex patterns, their optionality, etc. The `kinds` table 
#' defines allowed file kinds and their suffixes.
#'
#' @return A list containing `keystruc`, `kinds`, and `type` describing
#'   functional files.
#' @examples
#' func_spec()
#' @keywords internal
func_spec <- function() {
  keystruc <- tibble::tribble(
    ~name,          ~key, ~optional, ~pattern,         ~order,
    "subid",        "sub", FALSE,    "[A-Za-z0-9]+",   1,
    "session",      "ses", TRUE,     "[A-Za-z0-9]+",   2,
    "task",         "task",FALSE,    "[A-Za-z0-9]+",   3,
    "acquisition",  "acq", TRUE,     "[A-Za-z0-9]+",   5,
    "contrast",     "ce",  TRUE,     "[A-Za-z0-9]+",   6,
    "reconstruction","rec",TRUE,     "[A-Za-z0-9]+",   7,
    "run",          "run", TRUE,     "[0-9]+",         4,
    "echo",         "echo",TRUE,     "[0-9]+",         8
  )
  
  kinds <- tibble::tribble(
    ~kind,    ~suffix,
    "bold",   list(".nii.gz",".nii", ".json"),
    "events", ".tsv",
    "sbref",  list(".nii.gz",".nii", ".json"),
    "physio", ".tsv"
  )
  
  ret <- list(keystruc = keystruc, kinds = kinds, type = "func")
  class(ret) <- c("func_spec", "parser_spec")
  ret
}


#' Create a spec table for "anat" files
#'
#' @return A list containing `keystruc`, `kinds`, and `type` describing
#'   anatomical files.
#' @examples
#' anat_spec()
#' @keywords internal
anat_spec <- function() {
  keystruc <- tibble::tribble(
    ~name,            ~key, ~optional, ~pattern,         ~order,
    "subid",          "sub",FALSE,    "[A-Za-z0-9]+",    1,
    "session",        "ses",TRUE,     "[A-Za-z0-9]+",    2,
    "acquisition",    "acq",TRUE,     "[A-Za-z0-9]+",    4,
    "contrast",       "ce", TRUE,     "[A-Za-z0-9]+",    5,
    "dir",            "dir",TRUE,     "[A-Za-z0-9]+",    6,
    "reconstruction", "rec",TRUE,     "[A-Za-z0-9]+",    7,
    "run",            "run",TRUE,     "[0-9]+",          3
  )
  
  kinds <- tibble::tribble(
    ~kind,       ~suffix,
    "defacemask", list(".nii.gz", ".nii", ".json"),
    "T1w",        list(".nii.gz", ".nii", ".json"),
    "T2w",        list(".nii.gz", ".nii", ".json"),
    "T1map",      list(".nii.gz", ".nii", ".json"),
    "T2map",      list(".nii.gz", ".nii", ".json"),
    "T2star",     list(".nii.gz", ".nii", ".json"),
    "FLAIR",      list(".nii.gz", ".nii", ".json"),
    "FLASH",      list(".nii.gz", ".nii", ".json"),
    "PDmap",      list(".nii.gz", ".nii", ".json"),
    "PDT2",       list(".nii.gz", ".nii", ".json"),
    "inplaneT1",  list(".nii.gz", ".nii", ".json"),
    "inplaneT2",  list(".nii.gz", ".nii", ".json"),
    "angio",       list(".nii.gz", ".nii", ".json")
  )
  
  ret <- list(keystruc = keystruc, kinds = kinds, type = "anat") 
 
  
  class(ret) <- c("anat_spec", "parser_spec")
  ret
}


#' Create a spec table for fMRIPrep "func" files
#'
#' @return A list containing `keystruc`, `kinds`, and `type` describing
#'   fMRIPrep functional files.
#' @examples
#' funcprepspec()
#' @keywords internal
funcprepspec <- function() {
  keystruc <- tibble::tribble(
    ~name,          ~key,     ~optional, ~pattern,       ~order,
    "subid",        "sub",    FALSE,    "[A-Za-z0-9]+",  1,
    "session",      "ses",    TRUE,     "[A-Za-z0-9]+",  2,
    "task",         "task",   FALSE,    "[A-Za-z0-9]+",  3,
    "acquisition",  "acq",    TRUE,     "[A-Za-z0-9]+",  5,
    "contrast",     "ce",     TRUE,     "[A-Za-z0-9]+",  6,
    "reconstruction","rec",   TRUE,     "[A-Za-z0-9]+",  6,
    "run",          "run",    TRUE,     "[a-z0-9]+",     4,
    "echo",         "echo",   TRUE,     "[0-9]+",        6,
    "modality",     "bold",   TRUE,     NULL,            8,
    "space",        "space",  TRUE,     "[A-Za-z0-9]+",  9,
    "res",          "res",    TRUE,     "[A-Za-z0-9]+", 10,
    "desc",         "desc",   TRUE,     "[A-Za-z0-9]+", 11,
    "label",        "label",  TRUE,     "[A-Za-z0-9]+", 12,
    "variant",      "variant",TRUE,     "[A-Za-z0-9]+", 14
  )
  
  kinds <- tibble::tribble(
    ~kind,         ~suffix,
    "roi",         list(".nii.gz", ".nii", ".json"),
    "regressors",  ".tsv",
    "latent",      ".lv.h5",
    "preproc",     list(".nii.gz", ".nii", ".json"),
    "bold",        list(".nii.gz", ".nii", ".json", ".lv.h5"),
    "brainmask",   list(".nii.gz", ".nii", ".json"),
    "mask",        list(".nii.gz", ".nii", ".json"),
    "confounds",   ".tsv",
    "timeseries",  ".tsv",
    "MELODICmix",  ".tsv",
    "mixing",      ".tsv",
    "AROMAnoiseICs",".tsv"
  )
  
  ret <- list(keystruc = keystruc, kinds = kinds, type = "funcprep")
  class(ret) <- c("funcprep_spec", "parser_spec")
  ret
}


#' Create a spec table for fMRIPrep "anat" files
#'
#' @return A list containing `keystruc`, `kinds`, and `type` describing
#'   fMRIPrep anatomical files.
#' @examples
#' anatprepspec()
#' @keywords internal
anatprepspec <- function() {
  anat_types <- c("defacemask","T1w", "T2w","T1map", "T2map", "T2star","FLAIR", "FLASH", "PDmap","PD","PDT2",
                  "inplaneT1", "inplaneT2", "angio")
  
  keystruc <- tibble::tribble(
    ~name,           ~key,      ~optional, ~pattern,        ~order,
    "subid",         "sub",     FALSE,    "[A-Za-z0-9]+",   1,
    "session",       "ses",     TRUE,     "[A-Za-z0-9]+",   2,
    "acquisition",   "acq",     TRUE,     "[A-Za-z0-9]+",   4,
    "from",          "from",    TRUE,     "[A-Za-z0-9]+",   4,
    "to",            "to",      TRUE,     "[A-Za-z0-9]+",   5,
    "contrast",      "ce",      TRUE,     "[A-Za-z0-9]+",   5,
    "dir",           "dir",     TRUE,     "[A-Za-z0-9]+",   6,
    "reconstruction","rec",     TRUE,     "[A-Za-z0-9]+",   7,
    "run",           "run",     TRUE,     "[0-9]+",         3,
    "modality",      list(anat_types),TRUE, NULL,            8,
    "space",         "space",   TRUE,     "[A-Za-z0-9]+",   9,
    "label",         "label",   TRUE,     "[A-Za-z0-9]+",  10,
    "desc",          "desc",    TRUE,     "[A-Za-z0-9]+",  11,
    "mode",          "mode",    TRUE,     "[A-Za-z0-9]+",  11,
    "target",        "target",  TRUE,     "[A-Za-z0-9]+",  12,
    "class",         "class",   TRUE,     "[A-Za-z0-9]+",  13,
    "mod",           "mod",     TRUE,     "[A-Za-z0-9]+",  14
  )
  
  kinds <- tibble::tribble(
    ~kind,         ~suffix,
    "preproc",     list(".nii.gz", ".nii", ".json"),
    "brainmask",   list(".nii.gz", ".nii", ".json"),
    "probtissue",  list(".nii.gz", ".nii", ".json"),
    "mask",        list(".nii.gz", ".nii", ".json"),
    "T1w",         list(".nii.gz", ".nii", ".json"),
    "probseg",     list(".nii.gz", ".nii", ".json"),
    "dtissue",     list(".nii.gz", ".nii", ".json"),
    "dseg",        list(".nii.gz", ".nii", ".json"),
    "warp",        ".h5",
    "xfm",         c(".txt", ".h5"),
    "inflated.L.surf", ".gii",
    "inflated.R.surf", ".gii",
    "midthickness.L.surf", ".gii",
    "midthickness.R.surf", ".gii",
    "pial.L.surf", ".gii",
    "pial.R.surf", ".gii",
    "smoothwm.L.surf", ".gii",
    "smoothwm.R.surf", ".gii",
    "roi",         list(".nii.gz", ".nii", ".json"),
    "affine",      ".txt"
  )
  
  ret <- list(keystruc = keystruc, kinds = kinds, type = "anatprep")
  class(ret) <- c("anatprep_spec", "parser_spec")
  ret
}


#' Create a spec table for fieldmap files
#'
#' @return A list containing `keystruc`, `kinds`, and `type` describing
#'   fieldmap files.
#' @examples
#' fmapspec()
#' @keywords internal
fmapspec <- function() {
  keystruc <- tibble::tribble(
    ~name,            ~key, ~optional, ~pattern,         ~order,
    "subid",          "sub",FALSE,    "[A-Za-z0-9]+",    1,
    "session",        "ses",TRUE,     "[A-Za-z0-9]+",    2,
    "acquisition",    "acq",TRUE,     "[A-Za-z0-9]+",    4,
    "contrast",       "ce", TRUE,     "[A-Za-z0-9]+",    5,
    "dir",            "dir",TRUE,     "[A-Za-z0-9]+",    6,
    "reconstruction", "rec",TRUE,     "[A-Za-z0-9]+",    7,
    "run",            "run",TRUE,     "[0-9]+",          8,
    "mod",            "mod",TRUE,     "[A-Za-z0-9]+",    9
  )
  
  kinds <- tibble::tribble(
    ~kind,       ~suffix,
    "magnitude",  list(".nii.gz", ".nii", ".json"),
    "magnitude1", list(".nii.gz", ".nii", ".json"),
    "magnitude2", list(".nii.gz", ".nii", ".json"),
    "phase",      list(".nii.gz", ".nii", ".json"),
    "phase1",     list(".nii.gz", ".nii", ".json"),
    "phase2",     list(".nii.gz", ".nii", ".json"),
    "phasediff",  list(".nii.gz", ".nii", ".json")
  )
  
  ret <- list(keystruc = keystruc, kinds = kinds, type = "fmap")
  class(ret) <- c("fmap_spec", "parser_spec")
  ret
}

