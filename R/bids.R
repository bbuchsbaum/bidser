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

# Package environment for caching
bidser_pkg_env <- new.env(parent = emptyenv())

#' @noRd
set_key <- function(fname, key, value) {
  p <- encode(fname)
  p[[key]] <- value
  p
}

#' Apply a transformation to BIDS files
#'
#' This function orchestrates the process of selecting files from a BIDS project,
#' applying a transformation to each file, and saving the output in a new BIDS
#' derivative directory. It leverages the existing bidser parsing and search
#' infrastructure.
#'
#' @param x A `bids_project` object.
#' @param transformer A function that performs the transformation. It must take 
#'   the input file path and return the output file path. The transformer is 
#'   responsible for creating the output file.
#' @param pipeline_name The name for the new derivative pipeline.
#' @param ... Additional arguments passed to \code{\link{search_files}} to select
#'   files (e.g., \code{subid = "01"}, \code{task = "rest"}).
#'
#' @return A character vector of paths to the newly created files.
#'
#' @examples
#' \donttest{
#' tryCatch({
#'   ds_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds_path)
#'
#'   # Create a simple transformer that adds a description
#'   add_desc_transformer <- function(infile) {
#'     entities <- encode(basename(infile))
#'     entities$desc <- if (is.null(entities$desc)) "smooth6mm" else 
#'                      paste(entities$desc, "smooth6mm", sep="")
#'     
#'     # Generate new filename
#'     new_name <- decode_bids_entities(entities)
#'     outfile <- file.path(dirname(infile), new_name)
#'     
#'     # For demo, just copy the file (real transformer would process it)
#'     file.copy(infile, outfile)
#'     return(outfile)
#'   }
#'
#'   # Apply transformation to functional files for subject 01
#'   new_files <- bids_transform(proj, add_desc_transformer, "smoothed",
#'                               subid = "01", suffix = "bold.nii.gz")
#'   print(length(new_files))
#'
#' }, error = function(e) {
#'   message("Example failed: ", e$message)
#' })
#' }
#' @export
bids_transform <- function(x, transformer, pipeline_name, ...) {
  if (!inherits(x, "bids_project")) {
    stop("`x` must be a `bids_project` object.")
  }
  
  # Use existing search_files with additional arguments
  files_to_transform <- search_files(x, full_path = TRUE, ...)
  
  if (is.null(files_to_transform) || length(files_to_transform) == 0) {
    message("No files found matching the selection criteria.")
    return(character(0))
  }

  # Create output directory
  deriv_root <- file.path(x$path, "derivatives", pipeline_name)
  if (!dir.exists(deriv_root)) {
    dir.create(deriv_root, recursive = TRUE)
  }
  
  new_files <- character(0)
  
  for (infile in files_to_transform) {
    # Preserve directory structure relative to project root
    rel_path <- gsub(paste0("^", normalizePath(x$path), .Platform$file.sep), "", 
                     normalizePath(infile), fixed = TRUE)
    
    # Find subject directory part for derivatives structure
    path_parts <- strsplit(rel_path, .Platform$file.sep)[[1]]
    sub_idx <- which(startsWith(path_parts, "sub-"))
    
    if (length(sub_idx) > 0) {
      subdir_part <- paste(path_parts[sub_idx[1]:(length(path_parts)-1)], 
                          collapse = .Platform$file.sep)
      output_dir <- file.path(deriv_root, subdir_part)
    } else {
      output_dir <- deriv_root
    }
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Apply transformer
    tryCatch({
      new_file <- transformer(infile, output_dir)
      if (!is.null(new_file) && file.exists(new_file)) {
        new_files <- c(new_files, new_file)
      }
    }, error = function(e) {
      warning("Transformation failed for '", basename(infile), "': ", e$message)
    })
  }
  
  new_files
}

#' Decode BIDS entities back into a filename
#'
#' This function reconstructs a BIDS filename from parsed entities, using the
#' standard BIDS entity ordering.
#'
#' @param entities A named list of BIDS entities (from \code{\link{encode}}).
#' @return A character string representing the BIDS filename.
#' @export
#' @examples
#' # Parse a filename and reconstruct it
#' entities <- encode("sub-01_task-rest_run-01_bold.nii.gz")
#' filename <- decode_bids_entities(entities)
#' print(filename)
#' 
#' # Modify entities and create new filename
#' entities$desc <- "smooth6mm"
#' new_filename <- decode_bids_entities(entities)
#' print(new_filename)
decode_bids_entities <- function(entities) {
  if (is.null(entities) || !is.list(entities)) {
    stop("entities must be a named list from encode()")
  }
  
  # Standard BIDS entity order
  ordered_keys <- c("sub", "ses", "task", "acq", "ce", "dir", "rec", "run", "echo")
  
  # Build filename parts
  parts <- character(0)
  entities_copy <- entities
  
  # Add ordered entities first
  for (key in ordered_keys) {
    name_key <- switch(key,
                      "sub" = "subid",
                      "ses" = "session", 
                      key)
    
    if (name_key %in% names(entities_copy) && !is.null(entities_copy[[name_key]])) {
      parts <- c(parts, paste0(key, "-", entities_copy[[name_key]]))
      entities_copy[[name_key]] <- NULL
    }
  }
  
  # Add remaining entities (excluding suffix, kind, type)
  remaining_keys <- setdiff(names(entities_copy), c("suffix", "kind", "type"))
  remaining_keys <- sort(remaining_keys) # For consistency
  
  for (key in remaining_keys) {
    if (!is.null(entities_copy[[key]])) {
      parts <- c(parts, paste0(key, "-", entities_copy[[key]]))
    }
  }
  
  # Add kind and suffix
  kind <- entities_copy$kind %||% "unknown"
  suffix <- entities_copy$suffix %||% "nii.gz"
  
  base_name <- paste(parts, collapse = "_")
  paste(base_name, kind, suffix, sep = c("_", "."))
}

#' Create a simple smoothing transformer
#'
#' This creates a transformer function that adds a smoothing description to
#' BIDS filenames. This is a lightweight example - real implementations would
#' perform actual image processing.
#'
#' @param fwhm The smoothing FWHM to add to the description.
#' @param suffix_pattern Optional regex pattern to match specific file types.
#' @return A transformer function for use with \code{\link{bids_transform}}.
#' @export
#' @examples
#' \donttest{
#' # Create a smoothing transformer
#' smooth_6mm <- create_smooth_transformer(6)
#' 
#' # Use with bids_transform (example)
#' # ds_path <- get_example_bids_dataset("ds001")
#' # proj <- bids_project(ds_path)
#' # new_files <- bids_transform(proj, smooth_6mm, "smoothed", 
#' #                             subid = "01", suffix = "bold.nii.gz")
#' }
create_smooth_transformer <- function(fwhm, suffix_pattern = "bold\\.nii") {
  function(infile, outdir) {
    # Skip files that don't match suffix pattern
    if (!is.null(suffix_pattern) && !grepl(suffix_pattern, basename(infile))) {
      return(NULL)
    }
    
    # Parse filename using existing encode function
    entities <- encode(basename(infile))
    if (is.null(entities)) {
      warning("Could not parse BIDS filename: ", basename(infile))
      return(NULL)
    }
    
    # Add smoothing description
    smooth_desc <- paste0("smooth", fwhm, "mm")
    entities$desc <- if (is.null(entities$desc)) smooth_desc else 
                     paste(entities$desc, smooth_desc, sep="")
    
    # Generate output filename using existing decode function
    new_filename <- decode_bids_entities(entities)
    outfile <- file.path(outdir, new_filename)
    
    # For demonstration, just copy the file
    # Real implementation would perform smoothing here:
    # if (requireNamespace("RNifti", quietly = TRUE)) {
    #   img <- RNifti::readNifti(infile)
    #   # Apply smoothing...
    #   RNifti::writeNifti(smoothed_img, outfile)
    # }
    
    message("Processing: ", basename(infile), " -> ", basename(outfile))
    file.copy(infile, outfile, overwrite = TRUE)
    
    return(outfile)
  }
}

# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

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
#' @param ... Additional key-value filters for BIDS entities. These are matched
#'   against parsed file entities in the derivatives tree. Common examples:
#'   `space = "MNI152NLin2009cAsym"`, `res = "2"`, `acq = "ap"`, `echo = "1"`.
#'   Values are treated as regex. Keys already covered by explicit arguments
#'   (`subid`, `task`, `run`, `session`, `space`, `variant`, `modality`, `kind`)
#'   are ignored in `...`.
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
#'   # Filter by resolution (BIDS entity 'res')
#'   res2_scans <- preproc_scans(proj, res = "2")
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
  
  # Pull extra filters from ... (e.g., res = "2", acq = "ap") and drop ones we already handle
  extra_filters <- list(...)
  if (length(extra_filters) > 0) {
    drop_keys <- c("subid","task","run","session","space","variant","modality","kind","desc")
    extra_filters <- extra_filters[setdiff(names(extra_filters), drop_keys)]
  }
  extra_matcher <- if (length(extra_filters) > 0) do.call(key_match, c(list(default = FALSE), extra_filters)) else function(z) TRUE

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
    },
    # Additional entity filters passed via ... (strict: missing key fails)
    matches_extra = function(z) extra_matcher(z)
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
      
      # Case 3: Wildcard pattern ".*" - always match (even missing keys).
      # NOTE: this means ".*" behaves as "don't filter on this key" rather
      # than "require key to exist, match any value".  Callers that need to
      # require a key's existence should post-filter by filename pattern
      # (e.g. grepl("_task-", ...)).
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
#' @importFrom readr read_delim
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
      readr::read_delim(fn, delim = " ", na = c("n/a", "NA"))
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

#' Download Example BIDS Dataset
#'
#' Downloads and extracts an example BIDS dataset for testing and demonstration purposes.
#' The datasets are sourced from the official BIDS examples repository on GitHub.
#'
#' @param dataset_name Character string specifying which dataset to download. 
#'   Common options include "ds001", "ds002", "ds007", "phoneme_stripped", etc.
#' @return Character string containing the path to the downloaded dataset directory.
#' @details This function requires an internet connection to download data from GitHub.
#'   The datasets are cached in the temporary directory AND in memory for the session, 
#'   so repeated calls with the same dataset_name will reuse the already downloaded data.
#'   Note: Don't call \code{unlink()} on the returned path in examples, as this defeats
#'   the caching mechanism and forces re-downloads.
#' @examples
#' \donttest{
#' tryCatch({
#'   ds_path <- get_example_bids_dataset("ds001")
#'   proj <- bids_project(ds_path)
#'   print(participants(proj))
#'   
#'   # Note: Don't unlink the path - it's cached for performance
#'   # unlink(ds_path, recursive=TRUE)  # DON'T DO THIS
#' }, error = function(e) {
#'   message("Example requires internet connection: ", e$message)
#' })
#' }
#' @export
get_example_bids_dataset <- function(dataset_name = "ds001") {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required for downloading example data")
  }
  
  # Session-level cache for better performance (stored in package environment)
  if (!exists(".bidser_examples_cache", envir = bidser_pkg_env)) {
    bidser_pkg_env$.bidser_examples_cache <- new.env()
  }
  cache_env <- bidser_pkg_env$.bidser_examples_cache
  
  # Check session cache first
  if (exists(dataset_name, envir = cache_env)) {
    cached_path <- get(dataset_name, envir = cache_env)
    if (dir.exists(cached_path)) {
      return(cached_path)
    }
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
    # Cache and return
    assign(dataset_name, temp_dir, envir = cache_env)
    return(temp_dir)
  }
  
  # Download the dataset from BIDS examples
  tryCatch({
    # Download the ZIP file ("master" branch is the historical default)
    zip_url <- paste0("https://github.com/bids-standard/bids-examples/archive/refs/heads/master.zip")
    zip_file <- file.path(tempdir(), "bids-examples.zip")

    if (!file.exists(zip_file)) {
      utils::download.file(zip_url, zip_file, mode = "wb", quiet = TRUE)
    }

    # Determine the root folder name (e.g. bids-examples-master or bids-examples-main)
    zip_contents <- utils::unzip(zip_file, list = TRUE)
    root_folder <- sub("/.*", "", zip_contents$Name[1])

    # Filter files belonging to the requested dataset
    dataset_pattern <- paste0(root_folder, "/", dataset_name, "/")
    dataset_files <- zip_contents$Name[grepl(dataset_pattern, zip_contents$Name, fixed = TRUE)]
    
    if (length(dataset_files) == 0) {
      stop("Dataset '", dataset_name, "' not found in BIDS examples")
    }
    
    # Extract the specific dataset files
    utils::unzip(zip_file, 
                files = dataset_files,
                exdir = tempdir(),
                junkpaths = FALSE)
    
    # Move to the expected location
    source_dir <- file.path(tempdir(), root_folder, dataset_name)
    if (dir.exists(source_dir)) {
      file.rename(source_dir, temp_dir)
      # Cache the result
      assign(dataset_name, temp_dir, envir = cache_env)
      return(temp_dir)
    } else {
      stop("Dataset '", dataset_name, "' not found in BIDS examples")
    }
  }, error = function(e) {
    stop("Failed to download BIDS example data: ", e$message)
  })
}

#' Clear Example BIDS Dataset Cache
#'
#' Clears the session-level cache of downloaded example BIDS datasets.
#' This can be useful to free up memory or force re-download of datasets.
#'
#' @return Invisible NULL
#' @examples
#' # Clear the cache
#' clear_example_bids_cache()
#' @export
clear_example_bids_cache <- function() {
  if (exists(".bidser_examples_cache", envir = bidser_pkg_env)) {
    cache_env <- bidser_pkg_env$.bidser_examples_cache
    rm(list = ls(envir = cache_env), envir = cache_env)
    message("Example BIDS dataset cache cleared")
  }
  invisible(NULL)
}


#' @export
#' @rdname transform_files
transform_files.bids_project <- function(x, subid = ".*", session = ".*",
                                         from = ".*", to = ".*", mode = ".*",
                                         kind = ".*", full_path = TRUE, ...) {
  # Match transform file extensions

  transform_regex <- "\\.(h5|txt)$"

  # Build the search parameters
  search_params <- list(
    x = x,
    regex = transform_regex,
    subid = subid,
    session = session,
    full_path = full_path,
    strict = FALSE
  )


  # Add optional entity filters if they are not wildcards
  if (from != ".*") search_params$from <- from
  if (to != ".*") search_params$to <- to
  if (mode != ".*") search_params$mode <- mode

  # Execute search
  results <- do.call(search_files, c(search_params, list(...)))

  # Post-filter by kind if specified (xfm, warp, affine)
  if (!is.null(results) && kind != ".*") {
    kind_pattern <- paste0("_", kind, "\\.(h5|txt)$")
    results <- results[stringr::str_detect(basename(results), kind_pattern)]
  }

  if (length(results) == 0) NULL else results
}


#' @export
#' @rdname surface_files
surface_files.bids_project <- function(x, subid = ".*", session = ".*",
                                       hemi = ".*", surf_type = ".*",
                                       space = ".*", full_path = TRUE, ...) {
  # Match GIFTI surface files
  surface_regex <- "\\.surf\\.gii$"

  # Build the search parameters
  search_params <- list(
    x = x,
    regex = surface_regex,
    subid = subid,
    session = session,
    full_path = full_path,
    strict = FALSE
  )

  # Add space filter if not wildcard
  if (space != ".*") search_params$space <- space

  # Add hemi entity filter if not wildcard (for files with hemi-L/R entity)
  if (hemi != ".*") search_params$hemi <- toupper(hemi)

  # Execute search
  results <- do.call(search_files, c(search_params, list(...)))

  # Post-filter by surf_type and hemi in filename (for legacy files without hemi entity)
  if (!is.null(results) && (surf_type != ".*" || hemi != ".*")) {
    hemi_pattern <- if (hemi == ".*") "[LR]" else toupper(hemi)
    surf_pattern <- if (surf_type == ".*") "[a-zA-Z]+" else surf_type

    # Pattern matches _surftype.H.surf.gii format
    pattern <- paste0("_", surf_pattern, "\\.", hemi_pattern, "\\.surf\\.gii$")
    results <- results[stringr::str_detect(basename(results), pattern)]
  }

  if (length(results) == 0) NULL else results
}


#' @export
#' @rdname mask_files
mask_files.bids_project <- function(x, subid = ".*", session = ".*",
                                    space = ".*", full_path = TRUE, ...) {
  # Match NIfTI mask files
  mask_regex <- "\\.nii(\\.gz)?$"

  # Build the search parameters
  search_params <- list(
    x = x,
    regex = mask_regex,
    subid = subid,
    session = session,
    full_path = full_path,
    strict = FALSE
  )

  # Add space filter if not wildcard
  if (space != ".*") search_params$space <- space

  # Execute search with kind filter for masks
  results <- do.call(search_files, c(search_params, list(...)))

  # Post-filter for mask/brainmask kinds
  if (!is.null(results)) {
    mask_pattern <- "_(mask|brainmask)\\."
    results <- results[stringr::str_detect(basename(results), mask_pattern)]
  }

  if (length(results) == 0) NULL else results
}


#' @export
#' @rdname build_subject_graph
build_subject_graph.bids_project <- function(x, subid, session = ".*",
                                             flatten = FALSE, ...) {
  # Normalize subject ID (remove sub- prefix if present)
  plain_subid <- stringr::str_remove(as.character(subid), "^sub-")

  # Validate subject exists
  if (!(plain_subid %in% participants(x))) {
    stop("Subject not found: ", plain_subid)
  }

  # Get sessions for this subject
  subj_sessions <- character()
  if (x$has_sessions) {
    all_files <- search_files(x, subid = plain_subid, full_path = FALSE)
    if (!is.null(all_files)) {
      ses_matches <- stringr::str_extract(all_files, "ses-[A-Za-z0-9]+")
      subj_sessions <- unique(stats::na.omit(stringr::str_remove(ses_matches, "^ses-")))
    }
  }

  # Get EPI files organized by task.run
  epi_files <- preproc_scans(x, subid = plain_subid, session = session,
                             full_path = TRUE, ...)
  epi_list <- list()
  if (!is.null(epi_files) && length(epi_files) > 0) {
    epi_df <- tibble::tibble(path = epi_files)
    epi_df$task <- stringr::str_extract(basename(epi_files), "(?<=task-)[A-Za-z0-9]+")
    epi_df$run <- stringr::str_extract(basename(epi_files), "(?<=run-)[0-9]+")
    epi_df$run[is.na(epi_df$run)] <- "01"
    epi_df$key <- paste(epi_df$task, epi_df$run, sep = ".")
    epi_list <- split(epi_df$path, epi_df$key)
    epi_list <- lapply(epi_list, unlist, use.names = FALSE)
  }

  # Get T1w files
  t1w_files <- search_files(x, subid = plain_subid, kind = "T1w",
                            regex = "\\.nii(\\.gz)?$", full_path = TRUE, ...)
  if (is.null(t1w_files)) t1w_files <- character()

  # Get mask files
  mask_list <- mask_files(x, subid = plain_subid, session = session,
                          full_path = TRUE, ...)
  if (is.null(mask_list)) mask_list <- character()

  # Get transform files organized by from_to key
  xfm_files <- transform_files(x, subid = plain_subid, session = session,
                               full_path = TRUE, ...)
  transforms_list <- list()
  if (!is.null(xfm_files) && length(xfm_files) > 0) {
    xfm_df <- tibble::tibble(path = xfm_files)
    xfm_df$from <- stringr::str_extract(basename(xfm_files), "(?<=from-)[A-Za-z0-9]+")
    xfm_df$to <- stringr::str_extract(basename(xfm_files), "(?<=to-)[A-Za-z0-9]+")
    xfm_df$key <- paste(xfm_df$from, "to", xfm_df$to, sep = "_")
    xfm_df$key[is.na(xfm_df$from) | is.na(xfm_df$to)] <- "unknown"
    transforms_list <- split(xfm_df$path, xfm_df$key)
    transforms_list <- lapply(transforms_list, unlist, use.names = FALSE)
  }

  # Get surface files organized by space then hemisphere
  surf_files <- surface_files(x, subid = plain_subid, session = session,
                              full_path = TRUE, ...)
  surfaces_list <- list()
  if (!is.null(surf_files) && length(surf_files) > 0) {
    surf_df <- tibble::tibble(path = surf_files)
    surf_df$space <- stringr::str_extract(basename(surf_files), "(?<=space-)[A-Za-z0-9]+")
    surf_df$space[is.na(surf_df$space)] <- "unknown"
    # Extract hemisphere from filename (e.g., pial.L.surf.gii -> L)
    surf_df$hemi <- stringr::str_extract(basename(surf_files), "\\.[LR]\\.surf\\.gii$")
    surf_df$hemi <- stringr::str_extract(surf_df$hemi, "[LR]")
    surf_df$hemi[is.na(surf_df$hemi)] <- "unknown"

    by_space <- split(surf_df, surf_df$space)
    surfaces_list <- lapply(by_space, function(sp_df) {
      by_hemi <- split(sp_df$path, sp_df$hemi)
      lapply(by_hemi, unlist, use.names = FALSE)
    })
  }

  # Get confound files
  conf_files <- confound_files(x, subid = plain_subid, session = session,
                               full_path = TRUE, ...)
  if (is.null(conf_files)) conf_files <- character()

  # Build the nested structure
  graph <- structure(
    list(
      subid = plain_subid,
      sessions = subj_sessions,
      epi = epi_list,
      anat = list(t1w = t1w_files, masks = mask_list),
      transforms = transforms_list,
      surfaces = surfaces_list,
      confounds = conf_files
    ),
    class = c("bids_subject_graph", "list")
  )

  # Return flat tibble if requested
  if (flatten) {
    return(.flatten_subject_graph(graph))
  }

  graph
}


#' Flatten a bids_subject_graph to a tibble
#' @param graph A bids_subject_graph object
#' @return A tibble with file_type, path, and metadata columns
#' @keywords internal
#' @noRd
.flatten_subject_graph <- function(graph) {
  rows <- list()

  # Add EPI files
  if (length(graph$epi) > 0) {
    for (key in names(graph$epi)) {
      parts <- strsplit(key, "\\.")[[1]]
      task <- parts[1]
      run <- if (length(parts) > 1) parts[2] else NA_character_
      for (path in graph$epi[[key]]) {
        rows <- c(rows, list(tibble::tibble(
          file_type = "epi",
          path = path,
          subid = graph$subid,
          session = NA_character_,
          task = task,
          run = run,
          space = stringr::str_extract(basename(path), "(?<=space-)[A-Za-z0-9]+"),
          hemi = NA_character_,
          from = NA_character_,
          to = NA_character_
        )))
      }
    }
  }

  # Add T1w files
  for (path in graph$anat$t1w) {
    rows <- c(rows, list(tibble::tibble(
      file_type = "anat",
      path = path,
      subid = graph$subid,
      session = NA_character_,
      task = NA_character_,
      run = NA_character_,
      space = stringr::str_extract(basename(path), "(?<=space-)[A-Za-z0-9]+"),
      hemi = NA_character_,
      from = NA_character_,
      to = NA_character_
    )))
  }

  # Add mask files
  for (path in graph$anat$masks) {
    rows <- c(rows, list(tibble::tibble(
      file_type = "mask",
      path = path,
      subid = graph$subid,
      session = NA_character_,
      task = NA_character_,
      run = NA_character_,
      space = stringr::str_extract(basename(path), "(?<=space-)[A-Za-z0-9]+"),
      hemi = NA_character_,
      from = NA_character_,
      to = NA_character_
    )))
  }

  # Add transform files
  if (length(graph$transforms) > 0) {
    for (key in names(graph$transforms)) {
      parts <- strsplit(key, "_to_")[[1]]
      from_space <- parts[1]
      to_space <- if (length(parts) > 1) parts[2] else NA_character_
      for (path in graph$transforms[[key]]) {
        rows <- c(rows, list(tibble::tibble(
          file_type = "transform",
          path = path,
          subid = graph$subid,
          session = NA_character_,
          task = NA_character_,
          run = NA_character_,
          space = NA_character_,
          hemi = NA_character_,
          from = from_space,
          to = to_space
        )))
      }
    }
  }

  # Add surface files
  if (length(graph$surfaces) > 0) {
    for (space in names(graph$surfaces)) {
      for (hemi in names(graph$surfaces[[space]])) {
        for (path in graph$surfaces[[space]][[hemi]]) {
          rows <- c(rows, list(tibble::tibble(
            file_type = "surface",
            path = path,
            subid = graph$subid,
            session = NA_character_,
            task = NA_character_,
            run = NA_character_,
            space = space,
            hemi = hemi,
            from = NA_character_,
            to = NA_character_
          )))
        }
      }
    }
  }

  # Add confound files
  for (path in graph$confounds) {
    rows <- c(rows, list(tibble::tibble(
      file_type = "confound",
      path = path,
      subid = graph$subid,
      session = NA_character_,
      task = stringr::str_extract(basename(path), "(?<=task-)[A-Za-z0-9]+"),
      run = stringr::str_extract(basename(path), "(?<=run-)[0-9]+"),
      space = NA_character_,
      hemi = NA_character_,
      from = NA_character_,
      to = NA_character_
    )))
  }

  if (length(rows) == 0) {
    return(tibble::tibble(
      file_type = character(),
      path = character(),
      subid = character(),
      session = character(),
      task = character(),
      run = character(),
      space = character(),
      hemi = character(),
      from = character(),
      to = character()
    ))
  }

  dplyr::bind_rows(rows)
}
