#' @importFrom crayon green cyan magenta yellow bold
#' @importFrom purrr partial
#' @importFrom stats median na.omit prcomp reorder runif setNames
#' @importFrom utils read.table
#' @importFrom httr GET stop_for_status content
#' @importFrom rio import
NULL

# Global variables used in dplyr/ggplot2 operations to avoid R CMD check warnings
utils::globalVariables(c(
  # Variables used in dplyr operations
  "subid", "session", "task", "run", "type", "kind", "modality", "suffix",
  "file_size", "file_count", "total_size", "derivative", "proportion", "total",
  "participant_id", "name", "path", "pathString", "size", "task_run", "runs",
  "total_files", "label", "missing"
))

#' @noRd
set_key <- function(fname, key, value) {
  p <- encode(fname)
  p[[key]] <- value
  p
}



#' @export
#' @rdname encode
#' @param x The filename string to encode
encode.character <- function(x, ...) {
  p <- bids_parser()
  ret <- bidser::parse(p, x)  # Use explicit namespace to avoid masking issues
  if (!is.null(ret)) {
    # The regex-based parser returns result directly at ret$result
    v <- ret$result
    v[!sapply(v, is.null)]
  } else {
    NULL
  }
}


#' @keywords internal
#' @noRd
list_files_github <- function(user, repo, subdir="") {
  gurl <- paste0("https://api.github.com/repos/", user, "/", repo, "/git/trees/master?recursive=1")
  req <- httr::GET(gurl)
  httr::stop_for_status(req)
  filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
  if (subdir != "") {
    grep(paste0(subdir, "/"), filelist, value = TRUE, fixed = TRUE)
  } else {
    filelist
  }
}

#' @keywords internal
read_example <- function(project) {
  projurl <- paste0("https://raw.githubusercontent.com/bids-standard/bids-examples/master/", project)
  part_df <- rio::import(paste0(projurl, "/participants.tsv"))
}


#' @keywords internal
#' @noRd
get_sessions <- function(path, sid) {
  dnames <- basename(fs::dir_ls(paste0(path, "/", sid)))
  ret <- str_detect(dnames, "ses-.*")
  if (any(ret)) {
    dnames[ret]
  } else {
    list()
  }
}


#' @keywords internal
#' @noRd
descend <- function(node, path, ftype, parser) {
  # List all files in the directory
  dnames <- basename(fs::dir_ls(paste0(path)))
  ret <- str_detect(dnames, ftype)
  
  # Add the folder node (e.g., 'anat', 'func')
  node <- add_node(node, ftype, folder=ftype)
  
  if (any(ret)) {
    # Get all files in the folder
    fnames <- basename(fs::dir_ls(paste0(path, "/", ftype)))
    
    # Debug info to see which files we're attempting to parse
    # message("Processing ", length(fnames), " files in ", ftype, " folder at ", path)
    
    for (fname in fnames) {
      # Try to parse the filename using the provided parser
      mat <- parse(parser, fname)
      
      if (!is.null(mat)) {
        # The parser matched the file - extract the results
        keep <- sapply(mat$result, function(x) !is.null(x) && length(x) > 0)
        res <- mat$result[keep]
        
        # Ensure 'kind' attribute is always set when dealing with func files
        if (ftype == "func" && !is.null(res$suffix)) {
          # For func files with .nii or .nii.gz extension but no explicit kind
          if (grepl("nii(\\.gz)?$", res$suffix) && 
              (is.null(res$kind) || is.na(res$kind) || res$kind == "")) {
            # Explicitly set kind to "bold" for functional MRI files
            res$kind <- "bold"
          }
        }
        
        # Create a new node for this file using parsed attributes
        args <- c(list(fname), res)
        n <- do.call(Node$new, args)
        
        # Add file path for reference
        n$relative_path <- file.path(ftype, fname)
        
        # Add the file node to the parent folder node
        node$AddChildNode(n)
      } else {
        # Parser didn't match - could add warning or debug info here
        # message("Could not parse file: ", fname, " in ", ftype, " folder")
      }
    }
  }
  
  return(node)
}


#' @keywords internal
#' @noRd
add_node <- function(bids, name, ...) {
  bids$AddChild(name, ...)
}

#' @keywords internal
#' @noRd
add_file <- function(bids, name,...) {
  bids$AddChild(name, ...)
}



#' Create a BIDS Project Object
#'
#' This function creates a BIDS project object from a directory containing BIDS-formatted
#' neuroimaging data. It can optionally load preprocessed derivatives from fMRIPrep.
#' The function validates the basic BIDS structure and provides methods for accessing
#' raw and preprocessed data, querying subjects, sessions, and tasks, reading event
#' files, and checking BIDS compliance.
#'
#' @param path Character string. The file path to the root of the BIDS project.
#'   Defaults to the current directory (".").
#' @param fmriprep Logical. Whether to load the fMRIPrep derivatives folder hierarchy.
#'   Defaults to FALSE.
#' @param prep_dir Character string. The location of the fMRIPrep subfolder relative
#'   to the derivatives directory. Defaults to "derivatives/fmriprep".
#'
#' @return A `bids_project` object representing the BIDS project structure. The object
#'   provides methods for:
#'   - Accessing raw and preprocessed data files
#'   - Querying subjects, sessions, and tasks
#'   - Reading event files and confound regressors
#'   - Checking BIDS compliance
#'   - Extracting metadata from file names
#'   Returns NULL if the directory does not contain a valid BIDS dataset.
#'
#' @examples
#' \donttest{
#' # Create a BIDS project
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path)
#'   
#'   # Get all functional scans
#'   all_scans <- func_scans(proj)
#'   
#'   # Get scans for specific subjects
#'   sub_scans <- func_scans(proj, subid="0[123]")
#'   
#'   # Get scans for a specific task
#'   task_scans <- func_scans(proj, task="rest")
#'   
#'   # Get scans from specific runs
#'   run_scans <- func_scans(proj, run="0[123]")
#'   
#'   # Combine multiple filters
#'   filtered_scans <- func_scans(proj,
#'                               subid="01",
#'                               task="rest",
#'                               run="01")
#'   
#'   # Get relative paths instead of full paths
#'   rel_scans <- func_scans(proj, full_path=FALSE)
#'   
#'   # Clean up
#'   unlink(ds001_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
#'
#' @export
bids_project <- function(path=".", fmriprep=FALSE, prep_dir="derivatives/fmriprep") {
  aparser <- anat_parser()
  fparser <- func_parser()
  
  path <- normalizePath(path)

  if (!file.exists(paste0(path, "/participants.tsv"))) {
    stop("participants.tsv is missing")
  }

  if (!file.exists(paste0(path, "/dataset_description.json"))) {
    warning("dataset_description.json is missing")
    desc <- list()
  } else {
    desc <- jsonlite::read_json(paste0(path, "/dataset_description.json"))
  }

  part_df <- read.table(paste0(path, "/participants.tsv"), header=TRUE, stringsAsFactors=FALSE, 
                        colClasses=c(participant_id="character"))
  project_name <- basename(path)

  bids <- Node$new(project_name)
  bids_raw <- add_node(bids, "raw")
  
  if (fmriprep) {
    #bids_prep <- bids$AddChild("derivatives/fmriprep")
    bids_prep <- add_node(bids, prep_dir)
    prep_func_parser <- fmriprep_func_parser()
    prep_anat_parser <- fmriprep_anat_parser() 
  } 
    
  sdirs <- as.character(part_df$participant_id)
  
  if (!all(stringr::str_detect(sdirs, "^sub"))) {
    ind <- which(!str_detect(sdirs, "^sub"))
    sdirs[ind] <- paste0("sub-", sdirs[ind])
  }
  
  has_sessions <- FALSE

  # pb <- progress::progress_bar$new(total = length(sdirs))

  for (sdir in sdirs) {
    # Check if subject exists in raw data or derivatives
    has_raw_data <- file.exists(paste0(path, "/", sdir))
    has_derivatives_data <- fmriprep && file.exists(paste0(path, "/", prep_dir, "/", sdir))
    
    # Skip if subject doesn't exist in either location
    if (!has_raw_data && !has_derivatives_data) {
      # pb$tick()
      next
    }
    
    # Create nodes only if the data exists
    node <- NULL
    prepnode <- NULL
    
    if (has_raw_data) {
      node <- add_node(bids_raw, sdir)
    }
    
    if (has_derivatives_data) {
      prepnode <- add_node(bids_prep, sdir)
    }

    # Get sessions from raw data if it exists, otherwise from derivatives
    sessions_path <- if (has_raw_data) path else paste0(path, "/", prep_dir)
    sessions <- get_sessions(sessions_path, sdir)

    if (length(sessions) > 0) {
      has_sessions <- TRUE
      for (sess in sessions) {
        # Process raw data sessions if they exist
        if (has_raw_data) {
          snode <- add_node(node, sess, session=gsub("ses-", "", sess))
          descend(snode, paste0(path, "/", sdir, "/", sess), "anat", aparser)
          descend(snode, paste0(path, "/", sdir, "/", sess), "func", fparser)
        }
        
        # Process derivatives sessions if they exist
        if (has_derivatives_data) {
          snode_prepped <- add_node(prepnode, sess, session=gsub("ses-", "", sess))
          descend(snode_prepped, paste0(path, "/", prep_dir, "/", sdir, "/", sess), "anat", prep_anat_parser)
          descend(snode_prepped, paste0(path, "/", prep_dir, "/", sdir, "/", sess), "func", prep_func_parser)
        }
      }
    } else {
      # No sessions - process directly
      if (has_raw_data) {
        descend(node, paste0(path, "/", sdir), "anat", aparser)
        descend(node, paste0(path, "/", sdir), "func", fparser)
      }
      
      if (has_derivatives_data) {
        descend(prepnode, paste0(path, "/", prep_dir, "/", sdir), "anat", prep_anat_parser)
        descend(prepnode, paste0(path, "/", prep_dir, "/", sdir), "func", prep_func_parser)
      }
    }
    
    # pb$tick()
  }
  
  tbl <- tibble::as_tibble(data.tree::ToDataFrameTypeCol(bids, 'name', 'type', 'subid', 'session', 'task', 'run', 'modality', 'suffix', 'desc', 'space'))
  tbl <- tbl %>% select(-starts_with("level_"))
  
  ret <- list(name=project_name, 
              part_df=part_df,
              bids_tree = bids,
              tbl = tbl,
              path=path,
              has_fmriprep=fmriprep,
              prep_dir=if (fmriprep) prep_dir else "",
              has_sessions=has_sessions)

  class(ret) <- "bids_project"
  ret
}

#' @export
#' @rdname flat_list-method
#' @method flat_list bids_project
flat_list.bids_project <- function(x, full_path=TRUE, ...) {
  if (full_path) {
    data.tree::ToDataFrameTable(x$bids_tree, "pathString", "name") %>% filter(stringr::str_detect(name, "^sub-")) %>%
    rename(path=pathString) %>% select(path)
  } else {
    data.tree::ToDataFrameTable(x$bids_tree, "pathString", "name") %>% filter(stringr::str_detect(name, "^sub-")) %>%
      rename(path=pathString) %>% select(name)
  }
}



#' @export
print.bids_project <- function(x, ...) {
  # Ensure crayon is available
  if (!requireNamespace("crayon", quietly = TRUE)) {
    warning("`crayon` is not installed. Please install `crayon` for colored output.")
    # fallback to original print if crayon not available
    cat("project: ", x$name, "\n")
    cat("participants (n):", nrow(x$part_df), "\n")
    cat("tasks: ", tasks(x), "\n")
    if (x$has_sessions) {
      cat("sessions: ", sessions(x), "\n")
    }
    if (x$has_fmriprep) {
      cat("fmriprep: ", x$prep_dir, "\n")
    }
    cat("image types: ", unique(x$tbl$type[!is.na(x$tbl$type)]), "\n")
    cat("modalities: ", paste(unique(x$tbl$modality[!is.na(x$tbl$modality)]), collapse=", "), "\n")
    cat("keys: ", paste(unique(x$bids_tree$attributesAll), collapse=", "), "\n")
    return(invisible(x))
  }
  
  # Using crayon for colored output
  project_col <- crayon::bold(crayon::cyan(x$name))
  participant_count_col <- crayon::bold(crayon::green(nrow(x$part_df)))
  task_list <- tasks(x)
  task_list_col <- if (length(task_list) > 0) {
    crayon::yellow(paste(task_list, collapse=", "))
  } else {
    crayon::yellow("(none)")
  }
  
  cat(crayon::bold("BIDS Project Summary"), "\n")
  cat(crayon::bold("Project Name: "), project_col, "\n")
  cat(crayon::bold("Participants (n): "), participant_count_col, "\n")
  cat(crayon::bold("Tasks: "), task_list_col, "\n")
  
  if (x$has_sessions) {
    sess <- sessions(x)
    sess_col <- if (length(sess) > 0) crayon::yellow(paste(sess, collapse=", ")) else crayon::yellow("(none)")
    cat(crayon::bold("Sessions: "), sess_col, "\n")
  }
  
  if (x$has_fmriprep) {
    cat(crayon::bold("fMRIPrep Derivatives: "), crayon::magenta(x$prep_dir), "\n")
  }
  
  # Image types
  img_types <- unique(x$tbl$type[!is.na(x$tbl$type)])
  img_types_col <- if (length(img_types) > 0) crayon::green(paste(img_types, collapse=", ")) else crayon::green("(none)")
  cat(crayon::bold("Image Types: "), img_types_col, "\n")
  
  # Modalities
  mods <- unique(x$tbl$modality[!is.na(x$tbl$modality)])
  mods_col <- if (length(mods) > 0) crayon::green(paste(mods, collapse=", ")) else crayon::green("(none)")
  cat(crayon::bold("Modalities: "), mods_col, "\n")
  
  # Keys
  keys <- unique(x$bids_tree$attributesAll)
  keys_col <- if (length(keys) > 0) crayon::yellow(paste(keys, collapse=", ")) else crayon::yellow("(none)")
  cat(crayon::bold("Keys: "), keys_col, "\n")
  
  invisible(x)
}


#' @export
#' @rdname sessions-method
#' @method sessions bids_project
sessions.bids_project <- function(x, ...) {
  if (x$has_sessions) {
    sort(unique(unlist(x$bids_tree$Get(
      "session",
      filterFun = function(node) !is.null(node$session)
    ))))
  } else {
    NULL
  }
}

#' @export
#' @rdname tasks-method
#' @method tasks bids_project
tasks.bids_project <- function(x, ...) {
  sort(unique(x$bids_tree$Get("task", filterFun = function(x) {!is.na(x$task) && !is.null(x$task) } )))
  ##unique(x$bids_tree$Get("task", filterFun = function(x) !is.null(x$task) & !is.na(x$task)))
}


#' @importFrom stringr str_remove
#' @export
#' @rdname participants-method
participants.bids_project <- function(x, ...) {
  collected_ids <- character(0)

  # Get IDs from participants.tsv (part_df)
  # These might or might not have "sub-" prefix.
  if (!is.null(x$part_df) && "participant_id" %in% names(x$part_df)) {
    # Ensure participant_id is character and not NA
    valid_part_ids <- x$part_df$participant_id[!is.na(x$part_df$participant_id)]
    if (length(valid_part_ids) > 0) {
      collected_ids <- c(collected_ids, as.character(valid_part_ids))
    }
  }

  # Get IDs from parsed file structure (tbl)
  # These are usually the numeric/alphanumeric part, e.g., "01", 
  # and typically do not have the "sub-" prefix if parsed from "sub-01".
  if (!is.null(x$tbl) && "subid" %in% names(x$tbl)) {
    valid_subids_from_tbl <- x$tbl$subid[!is.na(x$tbl$subid)]
    if (length(valid_subids_from_tbl) > 0) {
      collected_ids <- c(collected_ids, as.character(valid_subids_from_tbl))
    }
  }

  if (length(collected_ids) == 0) {
    return(character(0))
  }

  # Make unique first
  unique_ids_before_stripping <- unique(collected_ids)
  
  # Remove "sub-" prefix if present from all collected IDs
  ids_stripped <- stringr::str_remove(unique_ids_before_stripping, "^sub-")
  
  # Make unique again after stripping prefix to handle cases like ("sub-01", "01") -> ("01", "01") -> "01"
  # Also remove any empty strings that might result from IDs like "sub-"
  final_unique_ids <- unique(ids_stripped[nchar(ids_stripped) > 0])

  if (length(final_unique_ids) == 0) {
    return(character(0))
  }
  
  sort(final_unique_ids)
}



#' Get Functional Scans from a BIDS Project
#'
#' This method extracts functional scan files from a BIDS project based on specified
#' criteria such as subject ID, task name, run number, and session. It can return
#' either full or relative file paths to the functional scans.
#'
#' @param x A `bids_project` object.
#' @param subid Regular expression for matching subject IDs. Default is ".*".
#' @param task Regular expression for matching task names. Default is ".*".
#' @param run Regular expression for matching run numbers. Default is ".*".
#' @param session Regular expression for matching session IDs. Default is ".*".
#' @param kind Regular expression for matching scan type. Default is "bold".
#' @param full_path Logical. If TRUE, return full file paths. If FALSE, return
#'   relative paths. Default is TRUE.
#' @param ... Additional arguments (not currently used).
#'
#' @return A character vector of file paths to functional scans matching the criteria.
#'   Returns NULL if:
#'   - No matching files are found
#'   - The project doesn't contain functional data
#'   - The specified criteria don't match any files
#'
#' @examples
#' \donttest{
#' # Create a BIDS project
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path)
#'   
#'   # Get all functional scans
#'   all_scans <- func_scans(proj)
#'   
#'   # Get scans for specific subjects
#'   sub_scans <- func_scans(proj, subid="0[123]")
#'   
#'   # Get scans for a specific task
#'   task_scans <- func_scans(proj, task="rest")
#'   
#'   # Get scans from specific runs
#'   run_scans <- func_scans(proj, run="0[123]")
#'   
#'   # Combine multiple filters
#'   filtered_scans <- func_scans(proj,
#'                               subid="01",
#'                               task="rest",
#'                               run="01")
#'   
#'   # Get relative paths instead of full paths
#'   rel_scans <- func_scans(proj, full_path=FALSE)
#'   
#'   # Clean up
#'   unlink(ds001_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
#'
#' @export
func_scans.bids_project <- function (x, subid=".*", task=".*", run = ".*", session=".*", 
                                     kind="bold", full_path=TRUE, ...) {
  
  f <- function(node) {
    paste0(node$path[3:length(node$path)], collapse="/")
  }
  
  ret <- x$bids_tree$children$raw$Get(f, filterFun = function(z) {
    if (z$isLeaf && !is.null(z$task) &&  !is.null(z$type) && str_detect_null(z$kind,kind)
        && str_detect_null(z$subid, subid)  && str_detect_null(z$task, task)
        && str_detect_null(z$session, session, default=TRUE)
        && str_detect_null(z$run, run, default=TRUE) && str_detect_null(z$suffix, "nii(.gz)?$")) {
      TRUE
    } else {
      FALSE
    }
  })
  
  if (length(ret) == 0) {
    return(NULL)
  }

  ret <- unique(unname(unlist(ret)))

  if (full_path) {
    ret <- file.path(x$path, ret)
  }

  as.vector(ret)
}


#' @keywords internal
#' @noRd
str_detect_null <- function(x, pat, default=FALSE) {
  if (is.null(x) || is.na(x)) {
    return(default)
  }
  stringr::str_detect(as.character(x), pat)
}

#' Get preprocessed scans from a BIDS project
#'
#' This function retrieves paths to preprocessed functional MRI scans from a BIDS project's
#' fMRIPrep derivatives. It allows filtering by various BIDS entities such as subject,
#' task, run, session, and space. The function is particularly useful for accessing
#' preprocessed data for analysis pipelines.
#'
#' @param x A \code{bids_project} object.
#' @param subid A regex pattern for matching subjects. Default is ".*".
#' @param task A regex pattern for matching tasks. Default is ".*".
#' @param run A regex pattern for matching runs. Default is ".*".
#' @param variant A regex pattern for matching preprocessing variants. Default is NULL
#'   (no variant filtering).
#' @param space A regex pattern for matching spaces (e.g., "MNI152NLin2009cAsym").
#'   Default is ".*".
#' @param session A regex pattern for matching sessions. Default is ".*".
#' @param modality A regex pattern for matching modality. Default is "bold".
#'   Set this to something else if you need a different modality.
#' @param kind The kind of preprocessed data to return. Default is ".*" to match any kind.
#' @param full_path If TRUE, return full file paths. Otherwise return relative paths.
#'   Default is FALSE.
#' @param ... Additional arguments passed to internal functions.
#'
#' @return A character vector of file paths to preprocessed scans matching the criteria.
#'   Returns NULL if:
#'   - No matching files are found
#'   - The project doesn't have fMRIPrep derivatives
#'   - The specified criteria don't match any files
#'
#' @examples
#' \donttest{
#' # Create a BIDS project with fMRIPrep derivatives
#' tryCatch({
#'   ds_path <- get_example_bids_dataset("phoneme_stripped")
#'   proj <- bids_project(ds_path, fmriprep=TRUE)
#'   
#'   # Get all preprocessed BOLD scans
#'   all_scans <- preproc_scans(proj)
#'   
#'   # Get preprocessed scans for specific subjects
#'   sub_scans <- preproc_scans(proj, subid="0[12]")
#'   
#'   # Get scans in MNI space
#'   mni_scans <- preproc_scans(proj, space="MNI152NLin2009cAsym")
#'   
#'   # Get scans for a specific task with full paths
#'   task_scans <- preproc_scans(proj,
#'                              task="phoneme",
#'                              full_path=TRUE)
#'   
#'   # Get scans from a specific session
#'   session_scans <- preproc_scans(proj, session="test")
#'   
#'   # Combine multiple filters
#'   filtered_scans <- preproc_scans(proj,
#'                                  subid="01",
#'                                  task="phoneme",
#'                                  run="01",
#'                                  space="MNI152NLin2009cAsym")
#'   
#'   # Clean up
#'   unlink(ds_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
#'
#' @export
preproc_scans.bids_project <- function(x, subid=".*", task=".*", run=".*", session=".*",
                                      variant=NULL, space=".*", modality="bold", kind=".*",
                                      full_path=FALSE, ...) {
  # Function to extract path from node
  f <- function(node) paste0(node$path[2:length(node$path)], collapse="/")
  
  pdir <- x$prep_dir

  # check that fMRIPrep derivatives are available
  if (!x$has_fmriprep || !(pdir %in% names(x$bids_tree$children))) {
    message("fMRIPrep derivatives not found; returning NULL.")
    return(NULL)
  }
  
  # If variant is NULL, treat it as ".*"
  var_pattern <- if (is.null(variant)) ".*" else variant
  
  # Create a list of criteria to check
  criteria <- list(
    # Basic file criteria
    is_leaf = function(z) z$isLeaf,
    is_nifti = function(z) str_detect_null(z$suffix, "nii(.gz)?$"),
    
    # Metadata criteria - for fMRIPrep files, 'kind' often contains what would be 'modality' in raw files
    matches_modality = function(z) {
      # For fMRIPrep files, check both 'modality' and 'kind' fields
      modality_match <- str_detect_null(z$modality, modality, default=FALSE)
      kind_match <- str_detect_null(z$kind, modality, default=FALSE) 
      return(modality_match || kind_match)
    },
    matches_kind = function(z) str_detect_null(z$kind, kind, default=TRUE),
    matches_subid = function(z) str_detect_null(z$subid, subid),
    matches_task = function(z) str_detect_null(z$task, task, default=TRUE),
    matches_run = function(z) str_detect_null(z$run, run, default=TRUE),
    matches_session = function(z) str_detect_null(z$session, session, default=TRUE),
    matches_space = function(z) str_detect_null(z$space, space, default=TRUE),
    
    # Special handling for variant
    matches_variant = function(z) {
      # If variant not specified but z$variant is not null, skip this node
      if (is.null(variant) && !is.null(z$variant)) {
        return(FALSE)
      }
      return(str_detect_null(z$variant, var_pattern, default=TRUE))
    },
    
    # Preprocessed file criteria - must have either desc=preproc OR kind=preproc
    is_preprocessed = function(z) {
      return(str_detect_null(z$desc, "preproc", default=FALSE) || 
             str_detect_null(z$kind, "preproc", default=FALSE))
    }
  )
  
  # Get files matching all criteria
  ret <- x$bids_tree$children[[pdir]]$Get(f, filterFun = function(z) {
    # Apply all criteria and return TRUE only if all are met
    all(sapply(criteria, function(criterion) criterion(z)))
  })
  
  # Add full path if requested
  if (!is.null(ret) && full_path) {
    ret <- file.path(x$path, ret)
  }
  
  ret
}

#' @keywords internal
#' @noRd
key_match <- function(default=FALSE, ...) {
  keyvals <- list(...)
  
  # If no key-value pairs provided, always return TRUE
  if (length(keyvals) == 0) {
    return(function(x) TRUE)
  }
  
  keys <- names(keyvals)
  
  # Return a function that checks if an object matches all patterns
  function(x) {
    all(vapply(keys, function(k) {
      # Case 1: Key value is NULL in pattern but exists in object - no match
      if (is.null(keyvals[[k]]) && !is.null(x[[k]])) {
        return(FALSE)
      }
      
      # Case 2: Key value is NULL in pattern and NULL in object - match
      if (is.null(keyvals[[k]]) && is.null(x[[k]])) {
        return(TRUE)
      }
      
      # Case 3: Wildcard pattern ".*" - always match
      if (identical(keyvals[[k]], ".*")) {
        return(TRUE)
      }
      
      # Convert value to character when not NULL/NA before pattern matching
      node_val <- x[[k]]
      if (!is.null(node_val) && !is.na(node_val)) {
        node_val <- as.character(node_val)
      }

      # Case 4: Use str_detect_null to match pattern
      return(str_detect_null(node_val, keyvals[[k]], default))
    }, logical(1)))
  }
}


#' Search for files in a BIDS project
#' 
#' This function searches for files in a BIDS project that match a specified pattern
#' and optional key-value criteria. It can search in both raw data and preprocessed 
#' derivatives (if available).
#'
#' @param x A \code{bids_project} object.
#' @param regex A regular expression to match against filenames. Default is ".*" (all files).
#' @param full_path If TRUE, return full file paths. If FALSE, return paths relative to the project root.
#' @param strict If TRUE, require that all queried keys must exist in matched files.
#'        If FALSE, allow matches for files missing queried keys.
#' @param ... Additional key-value pairs to filter files (e.g., subid = "01", task = "wm").
#'        These are matched against the corresponding metadata in the BIDS files.
#' @return A character vector of file paths matching the criteria, or NULL if no matches found.
#' @export
#' @rdname search_files 
#' @importFrom stringr str_detect
#' @examples
#' \donttest{
#' # Search for event files in a BIDS dataset
#' tryCatch({
#'   ds001_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds001_path, fmriprep=FALSE)
#'   event_files <- search_files(proj, regex="events\\.tsv$")
#'   
#'   # Search with additional criteria (note: ds001 only has one subject '01')
#'   sub01_files <- search_files(proj, regex="bold\\.nii\\.gz$", subid="01", 
#'                               task="balloonanalogrisktask")
#'   
#'   # Get full paths
#'   full_paths <- search_files(proj, regex="events\\.tsv$", full_path=TRUE)
#'   
#'   # Clean up
#'   unlink(ds001_path, recursive=TRUE)
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
search_files.bids_project <- function(x, regex=".*", full_path=FALSE, strict=TRUE, ...) {
  # Helper function to extract the relative path from a node
  extract_relative_path <- function(node) {
    pdir_parts <- character(0) # Initialize to empty
    if (x$has_fmriprep && nzchar(x$prep_dir)) { # Check if prep_dir is non-empty
        pdir_parts <- strsplit(x$prep_dir, "/")[[1]]
    }

    is_prep_data <- x$has_fmriprep && 
                   length(pdir_parts) > 0 && # Ensure pdir_parts is not empty
                   length(node$path) > (1 + length(pdir_parts)) && 
                   all(node$path[2:(1+length(pdir_parts))] == pdir_parts)
    
    if (is_prep_data) {
      paste0(node$path[2:length(node$path)], collapse="/")
    } else {
      if (length(node$path) > 2 && node$path[2] == "raw") {
         paste0(node$path[3:length(node$path)], collapse="/")
      } else if (length(node$path) == 2 && node$path[[1]] == x$name) { # Files directly under project root node, like participants.tsv
         node$name
      } else if (node$path[[1]] == x$name && length(node$path) > 1 && node$path[[2]] != "raw" && !is_prep_data) {
        # This case handles files that might be at the root of the project directory (e.g. dataset_description.json)
        # or other top-level files/dirs not under 'raw' or 'derivatives'
        paste0(node$path[2:length(node$path)], collapse="/")
      } else {
         # Fallback for other unexpected paths or if node$path is shorter than expected
         # This could be a root file if node$path is just the project name and the filename
         if (length(node$path) > 1) paste0(node$path[2:length(node$path)], collapse="/") else node$name
      }
    }
  }
  
  search_params <- list(...)
  has_kind_param <- "kind" %in% names(search_params)
  
  base_params <- search_params
  if (has_kind_param && search_params$kind == "bold") {
    base_params$kind <- NULL
  }
  base_matcher <- do.call(key_match, c(list(default = !strict), base_params))
  
  filter_fun <- function(z) {
    # Ensure z$name is a character string before using in str_detect
    node_name <- z$name
    if (is.null(node_name) || is.na(node_name)) {
      return(FALSE) # Cannot match if name is missing
    }
    if (!is.character(node_name)) {
      node_name <- as.character(node_name)
    }

    if (!(z$isLeaf && stringr::str_detect(node_name, regex))) {
      return(FALSE)
    }
    if (!base_matcher(z)) {
      return(FALSE)
    }
    if (has_kind_param) {
      if (search_params$kind == "bold") {
        is_explicitly_bold <- str_detect_null(z$kind, "^bold$", default = FALSE)
        is_implicitly_bold <- FALSE
        if (!is_explicitly_bold) {
           is_func_folder <- any(z$path == "func") 
           is_bold_filename <- str_detect(z$name, "_bold\\\\.nii(\\\\.gz)?$")
           is_implicitly_bold <- is_func_folder && is_bold_filename
        }
        if (!(is_explicitly_bold || is_implicitly_bold)) {
           return(FALSE)
        }
      } else {
        # For other 'kind' values, use standard matching (delegated to base_matcher if kind wasn't removed)
        # This part is tricky: if kind was specified and NOT bold, it's already in base_matcher.
        # If kind was specified AND bold, it was removed from base_params. So this 'else' branch
        # might not be strictly needed if base_matcher handles all non-bold kind cases.
        # However, to be explicit for non-bold kind filtering:
        if (!is.null(search_params$kind)) { # ensure kind was actually passed
            kind_matcher_specific <- do.call(key_match, list(default = !strict, kind = search_params$kind))
            if (!kind_matcher_specific(z)) {
                return(FALSE)
            }
        }
      }
    }
    return(TRUE)
  }
  
  ret <- x$bids_tree$Get(extract_relative_path, filterFun = filter_fun, simplify = FALSE)
  
  if (length(ret) == 0) {
    return(NULL)
  }
  
  # Ensure ret is a character vector of unique paths
  # unlist can produce names, so unname it.
  ret <- unique(unname(unlist(ret)))

  if (full_path && !is.null(ret)) {
    # file.path(x$path, ret) might be problematic if x$path is NULL (for virtual projects)
    # or if ret contains paths that are already absolute (though extract_relative_path should prevent this)
    if (!is.null(x$path)) {
      ret <- file.path(x$path, ret)
    } # If x$path is NULL, we assume `ret` contains the desired (likely relative) paths.
  }
  
  as.vector(ret)
}


#' @keywords internal
match_attribute <- function(x, ...) {
  ll <- list(...)
  
  all(sapply(names(ll), function(key) {
    stringr::str_detect(attr(x, key), as.character(ll[[key]]))
  }))
}

#' Load all event files into a combined tibble
#'
#' This function searches for all `events.tsv` files that match the provided
#' filters (subid, task, run, session) and loads them into a single tibble.
#' If `full_path=TRUE`, full file paths are returned; otherwise relative paths.
#'
#' @param x A \code{bids_project} object.
#' @param subid A regex for matching participant IDs. Default is `".*"`.
#' @param task A regex for matching tasks. Default is `".*"`.
#' @param run A regex for matching runs. Default is `".*"`.
#' @param session A regex for matching sessions. Default is `".*"`.
#' @param full_path If TRUE, return full file paths before reading. Default is TRUE.
#' @param ... Additional arguments passed on to \code{search_files}.
#'
#' @return A tibble combining all matched event files, with columns `.subid`, `.task`, `.run`, `.session`
#' and all event columns. If no events are found, returns an empty tibble.
#' @rdname load_all_events-method
#' @importFrom dplyr bind_rows mutate
#' @importFrom stringr str_match
#' @importFrom readr read_tsv
#' @importFrom tibble tibble
#' @export
load_all_events.bids_project <- function(x, subid=".*", task=".*", run=".*", session=".*", full_path=TRUE, ...) {
  # Find all events files matching criteria
  event_files <- search_files(x, regex="events\\.tsv$", full_path=full_path, strict=TRUE,
                              subid=subid, task=task, run=run, session=session, ...)
  
  if (is.null(event_files) || length(event_files) == 0) {
    message("No matching event files found.")
    return(tibble::tibble())
  }
  
  # A helper to parse metadata from file name using keys in x$tbl
  parse_metadata <- function(fn) {
    # We will rely on encode() if available, or parse keys using regex
    # If `encode` is not stable enough, we can directly extract from file name:
    # sub-XXX[_ses-XXX][_task-XXX][_run-XXX]_events.tsv
    bname <- basename(fn)
    
    # Extract sub, ses, task, run from filename:
    subid_val <- stringr::str_match(bname, "sub-([A-Za-z0-9]+)")[,2]
    session_val <- stringr::str_match(bname, "ses-([A-Za-z0-9]+)")[,2]
    task_val <- stringr::str_match(bname, "task-([A-Za-z0-9]+)")[,2]
    run_val <- stringr::str_match(bname, "run-([0-9]+)")[,2]
    
    tibble::tibble(
      .subid = subid_val,
      .session = session_val,
      .task = task_val,
      .run = run_val,
      file = fn
    )
  }
  
  # Read and combine
  df_list <- lapply(event_files, function(fn) {
    meta <- parse_metadata(fn)
    dfx <- tryCatch({
      readr::read_tsv(fn, na = c("n/a", "NA"))
    }, error = function(e) {
      warning("Failed to read file: ", fn, " - ", e$message)
      return(NULL)
    })
    if (is.null(dfx)) return(NULL)
    dfx <- dfx %>% dplyr::mutate(.file = fn)
    dplyr::bind_cols(meta, dfx)
  })
  
  # Filter out any NULLs
  df_list <- df_list[!sapply(df_list, is.null)]
  
  if (length(df_list) == 0) {
    message("No valid event files could be read.")
    return(tibble::tibble())
  }
  
  dplyr::bind_rows(df_list)
}


#' Summarize a BIDS dataset
#'
#' Provides a quick summary of dataset statistics, including:
#' - Number of subjects
#' - Number of sessions (if applicable)
#' - Available tasks and the number of runs per task
#' - Total number of runs
#'
#' @param x A \code{bids_project} object.
#' @return A list with summary information:
#'   - `n_subjects`: number of participants
#'   - `n_sessions`: number of sessions (if any), otherwise NULL
#'   - `tasks`: a data frame with `task` and `n_runs` columns
#'   - `total_runs`: total number of runs across the dataset
#'
#' @importFrom dplyr group_by summarize n distinct
#' @importFrom tibble as_tibble
#' @export
bids_summary <- function(x) {
  # Participants
  subs <- participants(x)
  n_subs <- length(subs)
  
  # Sessions (if any)
  sess <- NULL
  if (x$has_sessions) {
    sess <- sessions(x)
  }
  
  # Tasks and runs
  # Assuming x$tbl has columns: subid, session, task, run
  # Count runs per task
  # Some datasets may have no runs or tasks columns populated, so guard checks:
  tbl <- x$tbl
  if (!"task" %in% names(tbl)) {
    # If no tasks at all, empty summary
    tasks_df <- tibble::tibble(task = character(0), n_runs = integer(0))
    total_runs <- 0
  } else {
    # Count how many runs per task:
    # If run is missing or always NA, treat each file as a run
    # If run is present, count unique run values:
    if ("run" %in% names(tbl)) {
      tasks_df <- tbl %>%
        dplyr::filter(!is.na(task)) %>%
        dplyr::group_by(task) %>%
        dplyr::summarize(n_runs = dplyr::n_distinct(run, na.rm=TRUE), .groups = "drop")
    } else {
      # If no run column, count occurrences of task as runs:
      tasks_df <- tbl %>%
        dplyr::filter(!is.na(task)) %>%
        dplyr::group_by(task) %>%
        dplyr::summarize(n_runs = n(), .groups = "drop")
    }
    
    total_runs <- sum(tasks_df$n_runs)
  }
  
  # Return a list summary
  list(
    n_subjects = n_subs,
    n_sessions = if (!is.null(sess)) length(sess) else NULL,
    tasks = tasks_df,
    total_runs = total_runs
  )
}

#' Basic BIDS Compliance Checks
#'
#' This function performs a simple, lightweight check of common BIDS requirements:
#' - Checks that `participants.tsv` and `dataset_description.json` exist at the root.
#' - Ensures all subject directories begin with `sub-`.
#' - If sessions are present, ensures that session directories begin with `ses-`.
#'
#' Note: This is not a full BIDS validator. For complete validation, use the 
#' official BIDS validator.
#'
#' @param x A \code{bids_project} object.
#'
#' @return A list with:
#'   - `passed` (logical): TRUE if all checks passed, FALSE otherwise.
#'   - `issues` (character vector): Descriptions of any issues found.
#'
#' @export
bids_check_compliance <- function(x) {
  issues <- character(0)
  
  # Check for participants.tsv
  if (!file.exists(file.path(x$path, "participants.tsv"))) {
    issues <- c(issues, "Missing participants.tsv at the root level.")
  }
  
  # Check for dataset_description.json
  if (!file.exists(file.path(x$path, "dataset_description.json"))) {
    issues <- c(issues, "Missing dataset_description.json at the root level.")
  }
  
  # Check subject directories
  # We assume subjects are identified by directories starting with "sub-"
  # Retrieve participant directories from the project object or files
  sub_dirs <- list.dirs(x$path, recursive = FALSE, full.names = FALSE)
  # Filter only directories that might be subjects (i.e. start with 'sub-')
  # We know from the project object, or we can guess by presence in participants
  # For a lightweight check, let's just ensure that all subjects in participants are present and start with "sub-"
  expected_subs <- participants(x)
  # participants(x) should return strings like "sub-01", "sub-02", etc.
  
  for (sid in expected_subs) {
    # participants() returns IDs without "sub-" prefix, so add it for directory checking
    sub_dir <- if (!grepl("^sub-", sid)) paste0("sub-", sid) else sid
    if (!dir.exists(file.path(x$path, sub_dir))) {
      issues <- c(issues, paste("Subject directory not found for:", sub_dir))
    }
  }
  
  # Check session directories if sessions are present
  if (x$has_sessions) {
    # We assume sessions are directories inside subject directories
    # and should start with "ses-"
    for (sid in expected_subs) {
      # participants() returns IDs without "sub-" prefix, so add it for directory checking
      sub_dir <- if (!grepl("^sub-", sid)) paste0("sub-", sid) else sid
      s_path <- file.path(x$path, sub_dir)
      if (dir.exists(s_path)) {
        # list sessions
        sess_dirs <- list.dirs(s_path, recursive = FALSE, full.names = FALSE)
        # Filter out known raw or derivative directories
        sess_dirs <- sess_dirs[grepl("^ses-", sess_dirs)]
        
        # sessions(x) returns all sessions; we can cross-check
        proj_sess <- sessions(x)
        # If proj_sess is NULL or empty, no sessions to check
        if (!is.null(proj_sess) && length(proj_sess) > 0) {
          # All sessions in proj_sess should appear as ses-xxx directories
          # also ensure they start with 'ses-'
          for (ss in proj_sess) {
            sdir <- paste0("ses-", ss)
            if (!dir.exists(file.path(s_path, sdir))) {
              issues <- c(issues, paste("Session directory not found for:", sdir, "in", sub_dir))
            }
            if (!grepl("^ses-", sdir)) {
              issues <- c(issues, paste("Session ID does not start with 'ses-':", sdir))
            }
          }
        }
      }
    }
  }
  
  # Determine pass/fail
  passed <- length(issues) == 0
  
  list(passed = passed, issues = issues)
}

#' @keywords internal
#' @noRd
get_example_bids_dataset <- function(dataset_name = "ds001") {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required for downloading example data")
  }
  
  # Check if we have internet connectivity
  has_internet <- function() {
    tryCatch({
      httr::HEAD("https://github.com", httr::timeout(5))
      TRUE
    }, error = function(e) FALSE)
  }
  
  if (!has_internet()) {
    stop("Internet connection required to download example BIDS data")
  }
  
  # Create a temporary directory for the dataset
  temp_dir <- file.path(tempdir(), paste0("bids_example_", dataset_name))
  
  if (dir.exists(temp_dir)) {
    return(temp_dir)
  }
  
  # Download the dataset from BIDS examples
  tryCatch({
    # Download the ZIP file
    zip_url <- paste0("https://github.com/bids-standard/bids-examples/archive/refs/heads/master.zip")
    zip_file <- file.path(tempdir(), "bids-examples.zip")
    
    if (!file.exists(zip_file)) {
      utils::download.file(zip_url, zip_file, mode = "wb", quiet = TRUE)
    }
    
    # Extract only the specific dataset
    utils::unzip(zip_file, 
                files = paste0("bids-examples-master/", dataset_name, "/"),
                exdir = tempdir(),
                junkpaths = FALSE)
    
    # Move to the expected location
    source_dir <- file.path(tempdir(), "bids-examples-master", dataset_name)
    if (dir.exists(source_dir)) {
      file.rename(source_dir, temp_dir)
      return(temp_dir)
    } else {
      stop("Dataset '", dataset_name, "' not found in BIDS examples")
    }
  }, error = function(e) {
    stop("Failed to download BIDS example data: ", e$message)
  })
}

