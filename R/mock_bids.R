#' @importFrom data.tree Node ToDataFrameTypeCol Traverse Aggregate Get Clone
#' @importFrom dplyr mutate filter group_by summarize ungroup select everything bind_rows tibble tribble n_distinct n any_of distinct lead lag case_when if_else arrange rename slice pull row_number
#' @importFrom tidyr nest unnest pivot_longer pivot_wider replace_na
#' @importFrom stringr str_detect str_match str_split str_remove str_replace str_c str_glue
#' @importFrom jsonlite read_json toJSON write_json
#' @importFrom readr read_tsv write_tsv read_delim write_delim cols col_character col_double col_integer col_logical col_factor spec locale default_locale read_lines write_lines read_file write_file guess_encoding format_tsv write_csv read_csv problems type_convert output_column
#' @importFrom fs file_size dir_create file_create path_dir file_exists dir_exists
#' @importFrom tibble as_tibble is_tibble tibble add_row deframe enframe has_name lst is_tibble
#' @importFrom rlang sym := abort warn inform is_scalar_character is_scalar_logical list2 exec enquo quo_name is_installed is_interactive check_installed check_dots_empty check_dots_used check_dots_unnamed caller_env current_env global_env interrupt %||% maybe_missing missing_arg seq2 set_names try_fetch with_options zap format_error_bullets is_string as_name inject parse_expr eval_tidy expr exprs new_environment env env_bind env_get env_has env_parent env_parents env_poke expr_deparse f_lhs f_rhs is_call is_call_simple is_formula is_integerish is_list is_named is_null is_primitive is_true is_false is_symbol local_options parse_expr caller_env empty_env global_env is_environment new_formula new_quosure quo quo_get_env quo_get_expr quo_is_call quo_is_missing quo_is_null quo_is_symbol quos rep_along splice with_handlers
#' @importFrom crayon has_color bold cyan green magenta yellow
NULL

# ---------------------------------------------------------------------------
# Helper Functions (Internal)
# ---------------------------------------------------------------------------

#' Generate BIDS Filename String
#'
#' Constructs a BIDS filename from provided entities based on common order.
#' Assumes entities are single strings or NULL/NA.
#'
#' @param subid Subject ID (required).
#' @param session Session ID (optional).
#' @param task Task label (optional).
#' @param acq Acquisition label (optional).
#' @param ce Contrast enhancement label (optional).
#' @param rec Reconstruction label (optional).
#' @param dir Direction label (optional).
#' @param run Run index (optional).
#' @param mod Modality label (optional, e.g., for fieldmaps).
#' @param echo Echo index (optional).
#' @param space Space label (optional, derivatives).
#' @param res Resolution label (optional, derivatives).
#' @param desc Description label (optional, derivatives).
#' @param label Label (optional, derivatives).
#' @param variant Variant label (optional, derivatives).
#' @param suffix The final suffix including extension (e.g., "bold.nii.gz") (required).
#' @param other_entities A named list of less common key-value pairs (optional).
#' @return A string representing the BIDS filename.
#' @keywords internal
#' @noRd
generate_bids_filename <- function(subid, session = NULL, task = NULL, acq = NULL, ce = NULL,
                                   rec = NULL, dir = NULL, run = NULL, mod = NULL, echo = NULL,
                                   space = NULL, res = NULL, desc = NULL, label = NULL,
                                   variant = NULL, suffix, other_entities = list()) {

  if (is.null(subid) || subid == "" || is.na(subid)) stop("'subid' is required.")
  if (is.null(suffix) || suffix == "" || is.na(suffix)) stop("'suffix' is required.")

  # Ensure prefix for subject ID
  if (!startsWith(subid, "sub-")) subid <- paste0("sub-", subid)

  # Common order of entities
  entities <- list(
    sub = subid,
    ses = session,
    task = task,
    acq = acq,
    ce = ce,
    dir = dir,
    rec = rec,
    run = run,
    mod = mod,
    echo = echo,
    space = space,
    res = res,
    label = label, # Often before desc
    desc = desc,
    variant = variant
  )

  # Append other entities
  entities <- c(entities, other_entities)

  # Filter out NULL, NA, or empty string values
  entities <- entities[!sapply(entities, function(x) is.null(x) || is.na(x) || x == "")]

  # Construct the filename parts
  parts <- lapply(names(entities), function(key) {
    val <- entities[[key]]
    # Check if value already has the key prefix (e.g., subid already "sub-01")
    if (startsWith(as.character(val), paste0(key, "-"))) {
      as.character(val)
    } else {
      paste0(key, "-", val)
    }
  })

  # Ensure suffix does not start with '_' (handled by joining)
  if (startsWith(suffix, "_")) suffix <- sub("^_", "", suffix)

  # Join parts with underscores and append the final suffix
  filename_base <- paste(unlist(parts), collapse = "_")
  filename <- paste0(filename_base, "_", suffix)

  # Replace potential double underscores (if an entity was empty but kept)
  filename <- gsub("__", "_", filename, fixed = TRUE)

  return(filename)
}


#' Generate Relative BIDS Path
#'
#' Constructs the relative directory path for a file within a BIDS structure.
#'
#' @param subid Subject ID.
#' @param session Session ID (optional).
#' @param datatype Datatype folder ('func', 'anat', 'fmap', 'dwi', etc.).
#' @param fmriprep Logical, is this a derivative file?
#' @param prep_dir The relative path to the derivatives directory.
#' @return A string representing the relative path (without the filename).
#' @keywords internal
#' @noRd
generate_bids_path <- function(subid, session = NULL, datatype, fmriprep = FALSE, prep_dir = "derivatives/fmriprep") {

  if (is.null(subid) || subid == "" || is.na(subid)) stop("'subid' is required.")
  if (is.null(datatype) || datatype == "" || is.na(datatype)) stop("'datatype' is required.")

  # Ensure prefix for subject ID
  if (!startsWith(subid, "sub-")) subid <- paste0("sub-", subid)

  path_parts <- character()

  if (fmriprep) {
    path_parts <- c(path_parts, prep_dir)
  } else {
    # Assumes raw data implies no top-level 'raw' folder in path string itself
    # path_parts <- c(path_parts, "raw") # Usually not part of the path string
  }

  path_parts <- c(path_parts, subid)

  if (!is.null(session) && !is.na(session) && session != "") {
    if (!startsWith(session, "ses-")) session <- paste0("ses-", session)
    path_parts <- c(path_parts, session)
  }

  path_parts <- c(path_parts, datatype)

  return(paste(path_parts, collapse = "/"))
}

#' @keywords internal
mock_key_match <- function(node_attrs, filters, default = FALSE) {
  # No longer uses ..., takes filters list directly
  if (length(filters) == 0) {
    return(TRUE)
  }

  keys <- names(filters)

  # Directly return the result of the checks for the given node_attrs
  result <- all(vapply(keys, function(k) {
    query_val <- filters[[k]]

    # Check if the key 'k' exists in the node attributes
    if (!k %in% names(node_attrs)) {
      # Key is missing in node. Match only if query allows ".*" or default=TRUE (strict=FALSE)
      match_result <- (is.character(query_val) && query_val == ".*") || default
      return(match_result)
    }

    # Key exists, get the node value
    node_val <- node_attrs[[k]]

    # If query is NULL, node must be NULL for a match
    if (is.null(query_val)) {
      match_result <- is.null(node_val)
      return(match_result)
    }

    # If query is ".*", it matches anything (if key exists or default allows missing)
    if (identical(query_val, ".*")) {
      return(TRUE)
    }

    # If node value is missing/NULL (despite key existing), match depends on default
    if (is.null(node_val) || is.na(node_val)) {
      match_result <- default
      return(match_result)
    }

    # Perform comparison/regex match
    if (is.character(node_val)) {
      # Use anchored regex match (^...$) for exact entity value comparison
      # Ensure query_val is treated as a pattern
      match_result <- any(stringr::str_detect(node_val, paste0("^(?:", query_val, ")$")))
      return(match_result)
    } else if (is.numeric(node_val) || is.integer(node_val)) {
      # Allow direct comparison or regex if query looks like one
      query_char <- as.character(query_val)
      # Check if query_char contains any common regex metacharacters
      if (grepl("[.^$*+?()\\[\\]\\{\\|\\\\]", query_char)) { 
        match_result <- stringr::str_detect(as.character(node_val), paste0("^(?:", query_char, ")$"))
        return(match_result)
      } else {
        match_result <- as.character(node_val) == query_char # Direct match
        return(match_result)
      }
    } else if (is.logical(node_val) && is.logical(query_val)) {
        match_result <- node_val == query_val # Allow logical matching
        return(match_result)
    }
    else {
      # Default to FALSE for other types or mismatches
      return(FALSE)
    }
  }, logical(1)))
  
  return(result)
}


#' Reconstruct Relative Path from Node
#'
#' Given a data.tree node from the mock BIDS structure, reconstructs its
#' relative path string. Handles raw vs derivative paths.
#'
#' @param node The data.tree node.
#' @param prep_dir The derivatives directory name used in the tree.
#' @return Relative path string.
#' @keywords internal
#' @noRd
reconstruct_node_path <- function(node, prep_dir = "derivatives/fmriprep") {
    pdir_parts <- strsplit(prep_dir, "/")[[1]]
    node_path_parts <- node$path

    # Check if the node is within the derivatives path structure
    # The root is level 1, 'raw' or 'prep_dir' is level 2
    is_prep <- length(node_path_parts) > (1 + length(pdir_parts)) &&
               all(node_path_parts[2:(1 + length(pdir_parts))] == pdir_parts)

    if (is_prep) {
        # Path starts from prep_dir downwards
        # e.g. node$path = [proj, deriv, fmriprep, sub-01, func, file.nii.gz]
        # result should be "derivatives/fmriprep/sub-01/func/file.nii.gz"
        paste(node_path_parts[-1], collapse = "/")
    } else if (length(node_path_parts) > 2 && node_path_parts[2] == "raw") {
        # Path starts from raw downwards (skip root and 'raw')
        # e.g. node$path = [proj, raw, sub-01, func, file.nii.gz]
        # result should be "sub-01/func/file.nii.gz"
        paste(node_path_parts[-(1:2)], collapse = "/")
    } else if (length(node_path_parts) == 2 && node$name %in% c("participants.tsv", "dataset_description.json")) {
         # Handle root files like participants.tsv directly
         node$name
    }
    else {
        # Fallback or unexpected structure, return full path minus root?
        # Or perhaps return NA/error. Let's try full path minus root for now.
        if (length(node_path_parts) > 1) {
            paste(node_path_parts[-1], collapse = "/")
        } else {
            node$name # Should only be the root node name
        }
    }
}



# ---------------------------------------------------------------------------
# Constructor Function: create_mock_bids
# ---------------------------------------------------------------------------

#' Create a Mock BIDS Project Object
#'
#' Generates an in-memory representation of a BIDS project, suitable for
#' testing and demonstration without requiring actual data files. Can optionally
#' create a "stub" directory structure on disk.
#'
#' @param project_name A character string for the project name.
#' @param participants Either a `data.frame` mirroring `participants.tsv` content
#'   (must include 'participant_id') or a character vector of participant IDs
#'   (e.g., `c("01", "02")`). If IDs are given, a minimal `part_df` is created.
#' @param dataset_description A list representing the `dataset_description.json`
#'   content. Defaults to a minimal valid description.
#' @param file_structure A `data.frame` or `tibble` defining the files in the
#'   mock structure. Each row represents a file. Required columns:
#'   `subid`, `datatype`, `suffix`. Optional BIDS entity columns: `session`,
#'   `task`, `run`, `acq`, `rec`, `dir`, `space`, `desc`, etc. Must also include
#'   a logical column `fmriprep` indicating if the file belongs in the derivatives
#'   directory specified by `prep_dir`.
#' @param event_data A named list where names are the *relative paths* of
#'   `events.tsv` files (e.g., "sub-01/func/sub-01_task-A_run-1_events.tsv")
#'   and values are the corresponding `tibble` or `data.frame` content for
#'   those files. These paths must correspond to files defined in `file_structure`
#'   with a `suffix` like "events.tsv".
#' @param confound_data A named list where names are relative paths of
#'   confound TSV files within the derivatives directory and values are
#'   their `tibble` or `data.frame` content. Paths must match files defined
#'   in `file_structure`.
#' @param create_stub Logical (default `FALSE`). If `TRUE`, write a stub BIDS
#'   directory structure to disk at `stub_path`. Zero-byte files are created
#'   except for `participants.tsv`, `dataset_description.json`, and `events.tsv`
#'   files specified in `event_data`.
#' @param stub_path Character string, the path where the stub directory will be
#'   created. Required if `create_stub = TRUE`.
#' @param prep_dir Character string, the path relative to the root for derivatives
#'   (default "derivatives/fmriprep"). This path structure will be used both in
#'   the internal `data.tree` and for stub creation.
#'
#' @return An object of class `mock_bids_project`.
#' @export
#' @examples
#' \donttest{
#' # --- Example Setup ---
#' participants_df <- tibble::tibble(participant_id = c("01", "02"), age = c(25, 30))
#'
#' file_structure_df <- tibble::tribble(
#'   ~subid, ~session, ~datatype, ~task,   ~run, ~suffix,                  ~fmriprep, ~desc,
#'   "01",   NA,       "anat",    NA,      NA,   "T1w.nii.gz",             FALSE,     NA,
#'   "01",   NA,       "func",    "taskA", "01", "bold.nii.gz",            FALSE,     NA,
#'   "01",   NA,       "func",    "taskA", "01", "events.tsv",             FALSE,     NA,
#'   "02",   "test",   "anat",    NA,      NA,   "T1w.nii.gz",             FALSE,     NA,
#'   "02",   "test",   "func",    "taskA", "01", "bold.nii.gz",            FALSE,     NA,
#'   "02",   "test",   "func",    "taskA", "01", "events.tsv",             FALSE,     NA,
#'   # Example derivative
#'   "01",   NA,       "func",    "taskA", "01", "preproc_bold.nii.gz",    TRUE,      "preproc"
#' )
#'
#' # Define event data (paths must match generated structure)
#' event_data_list <- list()
#' event_data_list[["sub-01/func/sub-01_task-taskA_run-01_events.tsv"]] <- tibble::tibble(
#'   onset = c(1.0, 5.0), duration = c(0.5, 0.5), trial_type = c("condA", "condB")
#' )
#' event_data_list[["sub-02/ses-test/func/sub-02_ses-test_task-taskA_run-01_events.tsv"]] <-
#'   tibble::tibble(
#'     onset = c(1.5, 5.5), duration = c(0.5, 0.5), trial_type = c("condC", "condD")
#' )
#'
#' # Create the mock project (in memory only)
#' mock_proj <- create_mock_bids(
#'   project_name = "MockTaskA",
#'   participants = participants_df,
#'   file_structure = file_structure_df,
#'   event_data = event_data_list
#' )
#'
#' # Create the mock project and write stubs
#' mock_proj_stub <- create_mock_bids(
#'   project_name = "MockTaskA_stub",
#'   participants = c("01", "02"), # Example using just IDs
#'   file_structure = file_structure_df,
#'   event_data = event_data_list,
#'   create_stub = TRUE,
#'   stub_path = tempdir() # Use a temporary directory for example
#' )
#'
#' # --- Using the Mock Project ---
#' print(mock_proj)
#' print(participants(mock_proj))
#' print(tasks(mock_proj))
#' print(sessions(mock_proj)) # Should return "test"
#'
#' print(func_scans(mock_proj, subid = "01"))
#' print(event_files(mock_proj, subid = "02", session = "test"))
#'
#' # Read the injected event data
#' events_sub1 <- read_events(mock_proj, subid = "01")
#' print(events_sub1)
#' if (nrow(events_sub1) > 0) print(tidyr::unnest(events_sub1, cols = data))
#'
#' # Search for derivatives
#' print(search_files(mock_proj, suffix = "preproc_bold.nii.gz"))
#'
#' # Check stub directory (if created)
#' # list.files(mock_proj_stub$path, recursive = TRUE)
#' # if (file.exists(file.path(mock_proj_stub$path, names(event_data_list)[1]))) {
#' #   print(readLines(file.path(mock_proj_stub$path, names(event_data_list)[1])))
#' # }
#'
#' # Clean up stub directory if created in temp
#' # unlink(mock_proj_stub$path, recursive = TRUE)
#' }
create_mock_bids <- function(project_name,
                             participants,
                             file_structure,
                             dataset_description = NULL,
                             event_data = list(),
                             confound_data = list(),
                             create_stub = FALSE,
                             stub_path = NULL,
                             prep_dir = "derivatives/fmriprep") {

  # --- Input Validation ---
  if (!rlang::is_scalar_character(project_name)) abort("'project_name' must be a single character string.")
  if (missing(file_structure) || !is.data.frame(file_structure)) abort("'file_structure' must be a data.frame or tibble.")
  req_cols <- c("subid", "datatype", "suffix", "fmriprep")
  if (!all(req_cols %in% names(file_structure))) {
    abort(paste("'file_structure' must contain columns:", paste(req_cols, collapse=", ")))
  }
  if (!is.logical(file_structure$fmriprep)) abort("'file_structure$fmriprep' must be logical.")

  if (create_stub && (!rlang::is_scalar_character(stub_path) || stub_path == "")) {
    abort("'stub_path' must be provided as a character string when 'create_stub' is TRUE.")
  }
  if (!rlang::is_list(event_data)) abort("'event_data' must be a list.")
  if (length(event_data) > 0 && !rlang::is_named(event_data)) abort("'event_data' list must be named with relative file paths.")
  if (!rlang::is_list(confound_data)) abort("'confound_data' must be a list.")
  if (length(confound_data) > 0 && !rlang::is_named(confound_data)) abort("'confound_data' list must be named with relative file paths.")

  # --- Process Participants ---
  if (is.character(participants)) {
    part_df <- tibble::tibble(participant_id = participants)
  } else if (is.data.frame(participants)) {
    if (!"participant_id" %in% names(participants)) {
      abort("'participants' data.frame must contain a 'participant_id' column.")
    }
    part_df <- tibble::as_tibble(participants)
  } else {
    abort("'participants' must be a character vector of IDs or a data.frame.")
  }
  # Ensure participant_id is character
  part_df$participant_id <- as.character(part_df$participant_id)
   # Ensure participant IDs in file_structure are valid
   # Remove "sub-" prefix from participant IDs for comparison with file_structure subids
  valid_subs <- stringr::str_remove(part_df$participant_id, "^sub-")
  invalid_file_subs <- unique(file_structure$subid[!file_structure$subid %in% valid_subs])
   if (length(invalid_file_subs) > 0) {
       abort(paste("The following subids in 'file_structure' are not present in 'participants':", paste(invalid_file_subs, collapse=", ")))
   }


  # --- Process Dataset Description ---
  if (is.null(dataset_description)) {
    desc <- list(
      Name = project_name,
      BIDSVersion = "1.7.0", # Example version
      DatasetType = "raw", # Or specify if derivatives are primary
      GeneratedBy = list(list(Name = "bidser::create_mock_bids"))
    )
  } else if (rlang::is_list(dataset_description)) {
    desc <- dataset_description
  } else {
    abort("'dataset_description' must be a list or NULL.")
  }

  # --- Determine Metadata ---
  # Need to check the file_structure for sessions or fmriprep=TRUE
  has_sessions <- "session" %in% names(file_structure) && any(!is.na(file_structure$session) & file_structure$session != "")
  has_fmriprep <- any(file_structure$fmriprep)

  # --- Initialize Data Tree ---
  bids_tree <- data.tree::Node$new(project_name)
  bids_tree$AddChild("raw")
  if (has_fmriprep) {
    # Add prep_dir structure, potentially nested (e.g., derivatives/fmriprep)
    prep_parts <- strsplit(prep_dir, "/")[[1]]
    current_node <- bids_tree
    for (part in prep_parts) {
      if (is.null(current_node[[part]])) {
         current_node$AddChild(part)
      }
       current_node <- current_node[[part]]
    }
     # prep_root_node <- bids_tree[[prep_dir]] # simple case
     prep_root_node <- current_node # Use the deepest node created
  } else {
    prep_root_node <- NULL
  }
  raw_root_node <- bids_tree$raw

  # --- Populate Data Tree ---
  generated_event_paths <- character() # Keep track of event files defined
  generated_confound_paths <- character() # Keep track of confound files defined

  for (i in 1:nrow(file_structure)) {
    row <- file_structure[i, ]

    # Extract all potential BIDS entities from the row
    # Handle cases where optional columns might not exist
    get_entity <- function(colname) {
      if (colname %in% names(row)) row[[colname]] else NULL
    }
    entities_in <- list(
        subid = get_entity("subid"),
        session = get_entity("session"),
        task = get_entity("task"),
        acq = get_entity("acq"),
        ce = get_entity("ce"),
        rec = get_entity("rec"),
        dir = get_entity("dir"),
        run = get_entity("run"),
        mod = get_entity("mod"),
        echo = get_entity("echo"),
        space = get_entity("space"),
        res = get_entity("res"),
        desc = get_entity("desc"),
        label = get_entity("label"),
        variant = get_entity("variant"),
        suffix = get_entity("suffix") # suffix is mandatory
    )
    # Clean NULLs for filename generation if function expects no NULLs
    entities_clean <- entities_in[!sapply(entities_in, is.null)]
    # Ensure mandatory fields are present if needed by helper
    entities_clean$subid <- row$subid # Ensure subid is always passed
    entities_clean$suffix <- row$suffix # Ensure suffix is always passed

    # If suffix already includes the desc prefix, avoid doubling it
    if ("desc" %in% names(row) && !is.null(row$desc) && !is.na(row$desc)) {
      prefix_check <- paste0("desc-", row$desc, "_")
      if (startsWith(row$suffix, prefix_check)) {
        entities_clean$desc <- NULL
      }
    }

    # Generate filename and path
    filename <- tryCatch({
        rlang::exec(generate_bids_filename, !!!entities_clean)
    }, error = function(e) {
        abort(paste("Error generating filename for row", i, ":", e$message))
    })

    relative_dir <- generate_bids_path(
      subid = row$subid,
      session = get_entity("session"),
      datatype = row$datatype,
      fmriprep = row$fmriprep,
      prep_dir = prep_dir
    )
    relative_path <- file.path(relative_dir, filename)

    # Use bidser::encode to get canonical entities (important!)
    # Call directly and handle any errors
    encoded_entities <- tryCatch({
        bidser::encode(filename)
    }, error = function(e) {
        warn(paste("Could not encode generated filename:", filename, " - May impact querying. Error:", e$message))
        # Fallback: use entities from file_structure row directly with standardized names
        fallback_entities <- list(
            name = filename,
            relative_path = relative_path,
            sub = row$subid,  # Use standard BIDS key
            ses = row$session, # Use standard BIDS key
            task = row$task,
            run = row$run,
            desc = row$desc,
            space = row$space
        )
        
        # Determine 'kind' from suffix or explicit kind if provided
        if (!is.null(row$kind) && !is.na(row$kind)) {
            fallback_entities$kind <- row$kind
        } else {
            # Try to guess kind from suffix
            if (grepl("bold", row$suffix, ignore.case = TRUE)) {
                fallback_entities$kind <- "bold"
                fallback_entities$suffix <- "bold" # Extract BIDS suffix part
            } 
            else if (grepl("T1w", row$suffix, ignore.case = TRUE)) {
                fallback_entities$kind <- "T1w"
                fallback_entities$suffix <- "T1w"
            } 
            else if (grepl("events.tsv", row$suffix, fixed=TRUE)) {
                fallback_entities$kind <- "events"
                fallback_entities$suffix <- "events"
            }
            else {
                # Default: try to extract suffix without extension
                suffix_part <- sub("\\.[^.]*$", "", row$suffix)
                fallback_entities$suffix <- suffix_part
                fallback_entities$kind <- suffix_part
            }
        }
        
        # Remove NULL or NA values
        fallback_entities <- fallback_entities[!sapply(fallback_entities, is.null)]
        fallback_entities <- fallback_entities[!sapply(fallback_entities, is.na)]
                
        return(fallback_entities)
    })

    if (is.null(encoded_entities)) {
       warn(paste("Encoding failed for:", filename, "- skipping this file in mock tree."))
       next # Skip file if encoding failed entirely
    }

    # Add filename itself to entities list for the node
    encoded_entities$name <- filename
    encoded_entities$relative_path <- relative_path # Store for convenience

    # Determine parent node in the tree
    parent_node <- if (row$fmriprep) prep_root_node else raw_root_node

    # Create directory structure within the data.tree
    path_parts <- strsplit(relative_dir, "/")[[1]]
    # Remove prep_dir parts if fmriprep=TRUE, as prep_root_node is already there
    if (row$fmriprep) {
        prep_dir_parts <- strsplit(prep_dir,"/")[[1]]
        # Check if path_parts start with prep_dir_parts and remove them
        if (length(path_parts) >= length(prep_dir_parts) &&
            all(path_parts[1:length(prep_dir_parts)] == prep_dir_parts)) {
            path_parts <- path_parts[-(1:length(prep_dir_parts))]
        }
    }

    current_node <- parent_node
    # Create intermediate nodes (subject, session, datatype)
    for (part in path_parts) {
       if (is.null(current_node[[part]])) {
            current_node$AddChild(part)
            # Add session ID attribute to session node when created
            if (grepl("^ses-", part) && !is.null(get_entity("session"))) {
                current_node[[part]]$session <- get_entity("session")
            }
             # Add subid attribute to subject node when created
             if (grepl("^sub-", part) && !is.null(get_entity("subid"))) {
                current_node[[part]]$subid <- get_entity("subid")
            }
        }
        current_node <- current_node[[part]]
    }

    # Add the leaf node (file) with its BIDS entities
    leaf_node <- current_node$AddChild(filename)
        
    # Assign all encoded entities to the node
    for (entity_name in names(encoded_entities)) {
      leaf_node[[entity_name]] <- encoded_entities[[entity_name]]
    }
    
    # Add datatype from row for compatibility with plot functions
    leaf_node$datatype <- row$datatype
    
    # Ensure both 'sub' and 'subid' are present for consistent searching
    if ("sub" %in% names(encoded_entities) && !("subid" %in% names(encoded_entities))) {
      leaf_node$subid <- encoded_entities$sub
    } else if ("subid" %in% names(encoded_entities) && !("sub" %in% names(encoded_entities))) {
      leaf_node$sub <- encoded_entities$subid
    }

    # Ensure both 'session' and 'ses' are present
    if ("session" %in% names(encoded_entities) && !("ses" %in% names(encoded_entities))) {
      leaf_node$ses <- encoded_entities$session
    } else if ("ses" %in% names(encoded_entities) && !("session" %in% names(encoded_entities))) {
      leaf_node$session <- encoded_entities$ses
    }

    # Track generated event and confound file paths
    if (isTRUE(endsWith(row$suffix, "events.tsv"))) {
        generated_event_paths <- c(generated_event_paths, relative_path)
    }
    if (grepl("(confounds|regressors|timeseries)", row$suffix) && endsWith(row$suffix, ".tsv")) {
        generated_confound_paths <- c(generated_confound_paths, relative_path)
    }

  } # End loop through file_structure

  # --- Validate Event Data ---
  event_data_names <- names(event_data)
  mismatched_event_paths <- event_data_names[!event_data_names %in% generated_event_paths]
  if (length(mismatched_event_paths) > 0) {
    warn(paste("The following names in 'event_data' do not correspond to any 'events.tsv' files generated from 'file_structure':",
               paste(mismatched_event_paths, collapse=", ")))
    # Filter event_data to keep only valid ones
    event_data <- event_data[event_data_names %in% generated_event_paths]
  }
  # Ensure event data is tibble
  event_data_store <- lapply(event_data, tibble::as_tibble)

  # --- Validate Confound Data ---
  confound_data_names <- names(confound_data)
  mismatched_confound_paths <- confound_data_names[!confound_data_names %in% generated_confound_paths]
  if (length(mismatched_confound_paths) > 0) {
    warn(paste("The following names in 'confound_data' do not correspond to any confound files generated from 'file_structure':",
               paste(mismatched_confound_paths, collapse=", ")))
    confound_data <- confound_data[confound_data_names %in% generated_confound_paths]
  }
  confound_data_store <- lapply(confound_data, tibble::as_tibble)


  # --- Create Stub Directory (Optional) ---
  actual_stub_path <- NULL
  if (create_stub) {
    actual_stub_path <- normalizePath(stub_path, mustWork = FALSE)
    fs::dir_create(actual_stub_path)

    # Write participants.tsv
    part_tsv_path <- file.path(actual_stub_path, "participants.tsv")
    tryCatch({
        readr::write_tsv(part_df, part_tsv_path)
    }, error = function(e) {
        warn(paste("Failed to write participants.tsv stub:", e$message))
    })

    # Write dataset_description.json
    desc_json_path <- file.path(actual_stub_path, "dataset_description.json")
    tryCatch({
        jsonlite::write_json(desc, desc_json_path, auto_unbox = TRUE, pretty = TRUE)
    }, error = function(e) {
        warn(paste("Failed to write dataset_description.json stub:", e$message))
    })

    # Get leaf nodes first
    leaf_nodes <- data.tree::Traverse(bids_tree, 
                        filterFun = function(node) node$isLeaf, 
                        traversal = "pre-order")

    # Now iterate over the collected leaf nodes
    for (node in leaf_nodes) { 
        # Skip root node files if handled above
        if (node$level <= 2 && node$name %in% c("participants.tsv", "dataset_description.json")) next

        # Reconstruct the intended relative path for this leaf node
        # This needs care to match the event_data keys exactly
        rel_path_for_file <- node$relative_path # Use stored path

        if (is.null(rel_path_for_file)) {
            warn(paste("Node", node$name, "missing relative_path attribute, cannot create stub file."))
            next # Skip this node
        }

        full_disk_path <- file.path(actual_stub_path, rel_path_for_file)

        # Ensure directory exists
        fs::dir_create(fs::path_dir(full_disk_path))

        # Check if it's an event or confound file with data
        if (endsWith(node$name, "events.tsv") && rel_path_for_file %in% names(event_data_store)) {
            tryCatch({
                readr::write_tsv(event_data_store[[rel_path_for_file]], full_disk_path, na = "n/a")
            }, error = function(e) {
                warn(paste("Failed to write event file stub:", full_disk_path, "-", e$message))
                fs::file_create(full_disk_path) # Create empty if write fails
            })
        } else if (grepl("(confounds|regressors|timeseries)", node$name) && endsWith(node$name, ".tsv") &&
                   rel_path_for_file %in% names(confound_data_store)) {
            tryCatch({
                 readr::write_tsv(confound_data_store[[rel_path_for_file]], full_disk_path, na = "n/a")
            }, error = function(e) {
                 warn(paste("Failed to write confound file stub:", full_disk_path, "-", e$message))
                 fs::file_create(full_disk_path)
            })
        } else {
            # Create zero-byte file
            if (!fs::file_exists(full_disk_path)) { # Avoid overwriting if somehow exists
                 fs::file_create(full_disk_path)
            }
        }
    }
  }

  # --- Construct Mock Project Object ---
  
  # Create raw data table from the bids_tree
  raw_data_rows <- list()
  leaf_nodes <- data.tree::Traverse(bids_tree, filterFun = function(n) n$isLeaf)
  
  for (node in leaf_nodes) {
    if (!is.null(node$relative_path)) {
      row_data <- list(
        path = node$relative_path,
        name = node$name,
        subid = node$subid %||% node$sub,
        session = node$session %||% node$ses,
        task = node$task,
        run = node$run,
        type = if (!is.null(node$datatype)) {
          node$datatype
        } else if (!is.null(node$kind)) {
          # Map kind to type for compatibility
          if (node$kind %in% c("T1w", "T2w", "FLAIR")) "anat"
          else if (node$kind == "bold") "func"
          else if (node$kind == "events") "func"
          else node$kind
        } else {
          # Guess from path
          if (grepl("/anat/", node$relative_path)) "anat"
          else if (grepl("/func/", node$relative_path)) "func"
          else if (grepl("/dwi/", node$relative_path)) "dwi"
          else if (grepl("/fmap/", node$relative_path)) "fmap"
          else "other"
        },
        kind = node$kind,
        suffix = node$suffix,
        desc = node$desc,  # Add desc attribute
        space = node$space,  # Add space attribute
        extension = if (!is.null(node$extension)) node$extension else {
          # Extract extension from filename
          ext_match <- regmatches(node$name, regexpr("\\.[^.]+$", node$name))
          if (length(ext_match) > 0) ext_match else NA
        },
        file_size = runif(1, 1e6, 50e6) # Random file size for mock
      )
      
      # Add any other attributes from the node, but skip complex types
      node_attrs <- node$attributes
      for (attr_name in names(node_attrs)) {
        if (!attr_name %in% c("name", "relative_path", "children", "parent", "path", "isLeaf", "level") &&
            !attr_name %in% names(row_data)) {
          attr_val <- node_attrs[[attr_name]]
          # Skip complex types that can't be in a data frame
          if (!is.environment(attr_val) && !is.function(attr_val) && !inherits(attr_val, "Node")) {
            row_data[[attr_name]] <- attr_val
          }
        }
      }
      
      raw_data_rows[[length(raw_data_rows) + 1]] <- row_data
    }
  }
  
  # Convert to data frame
  if (length(raw_data_rows) > 0) {
    raw_data_df <- do.call(rbind, lapply(raw_data_rows, function(x) {
      # Convert list to data frame row, handling NULLs
      x[sapply(x, is.null)] <- NA
      as.data.frame(x, stringsAsFactors = FALSE)
    }))
  } else {
    # Empty data frame with expected columns
    raw_data_df <- data.frame(
      path = character(),
      name = character(),
      subid = character(),
      session = character(),
      task = character(),
      run = character(),
      type = character(),
      kind = character(),
      suffix = character(),
      extension = character(),
      file_size = numeric(),
      stringsAsFactors = FALSE
    )
  }
  
  # Get unique subjects, sessions, tasks, runs
  unique_subjects <- unique(na.omit(raw_data_df$subid))
  unique_sessions <- if (has_sessions) unique(na.omit(raw_data_df$session)) else character()
  unique_tasks <- unique(na.omit(raw_data_df$task))
  unique_runs <- unique(na.omit(raw_data_df$run))
  
  mock_project <- structure(
    list(
      name = project_name,
      part_df = part_df,
      desc = desc,
      bids_tree = bids_tree,
      event_data_store = event_data_store,
      confound_data_store = confound_data_store,
      path = if (create_stub) actual_stub_path else paste0("mock://", project_name), # Indicate mock path
      root = if (create_stub) actual_stub_path else paste0("mock://", project_name),
      has_sessions = has_sessions,
      has_fmriprep = has_fmriprep,
      prep_dir = prep_dir,
      # Add fields expected by plot functions
      tbl = raw_data_df,
      raw_data = raw_data_df,
      subjects = unique_subjects,
      sessions = unique_sessions,
      tasks = unique_tasks,
      runs = unique_runs,
      is_virtual = TRUE
    ),
    class = c("mock_bids_project", "list") # Inherit from list for basic access
  )

  return(mock_project)
}


# ---------------------------------------------------------------------------
# S3 Methods for mock_bids_project
# ---------------------------------------------------------------------------

#' Print Mock BIDS Project Summary
#'
#' Provides a console summary of the mock BIDS project, displaying key information
#' like participant count, tasks, sessions, derivatives status, and discovered
#' BIDS entities.
#'
#' @param x A `mock_bids_project` object.
#' @param ... Extra arguments (ignored).
#' @return The `mock_bids_project` object `x` invisibly.
#' @export
#' @examples
#' # Create a simple mock project
#' parts <- data.frame(participant_id = "01")
#' fs <- data.frame(subid = "01", datatype="func", suffix="bold.nii.gz", fmriprep=FALSE)
#' mock_proj <- create_mock_bids("SimpleMock", parts, fs)
#'
#' # Print the summary
#' print(mock_proj)
print.mock_bids_project <- function(x, ...) {
  # Check if crayon is available and use it
  has_crayon <- rlang::is_installed("crayon") && crayon::has_color()

  cat_col <- function(label, value, col_fn = crayon::cyan) {
      if (has_crayon) {
          cat(crayon::bold(label), col_fn(value), "\n")
      } else {
          cat(label, value, "\n")
      }
  }

  cat(if (has_crayon) crayon::bold("Mock BIDS Project Summary") else "Mock BIDS Project Summary", "\n")
  cat_col("Project Name: ", x$name)
  cat_col("Participants (n): ", nrow(x$part_df), col_fn = crayon::green)

  tasks_list <- tasks(x)
  tasks_str <- if (length(tasks_list) > 0) paste(tasks_list, collapse = ", ") else "(none)"
  cat_col("Tasks: ", tasks_str, col_fn = crayon::yellow)

  if (x$has_sessions) {
    sessions_list <- sessions(x)
    sessions_str <- if (length(sessions_list) > 0) paste(sessions_list, collapse = ", ") else "(none)"
    cat_col("Sessions: ", sessions_str, col_fn = crayon::yellow)
  }

  if (x$has_fmriprep) {
    cat_col("Derivatives: ", x$prep_dir, col_fn = crayon::magenta)
  }

  # Get unique datatypes from the tree structure
  datatypes <- unique(na.omit(x$bids_tree$Get("datatype", filterFun = data.tree::isLeaf)))
  dt_str <- if (length(datatypes) > 0) paste(datatypes, collapse = ", ") else "(none)"
  cat_col("Datatypes: ", dt_str, col_fn = crayon::green)

  # Get unique suffixes from leaf nodes
  suffixes <- unique(na.omit(x$bids_tree$Get("suffix", filterFun = data.tree::isLeaf)))
  suf_str <- if (length(suffixes) > 0) paste(suffixes, collapse = ", ") else "(none)"
  cat_col("Suffixes: ", suf_str, col_fn = crayon::green)

  # Get all unique keys stored in leaf nodes
  all_keys <- unique(unlist(x$bids_tree$Get(function(node) names(node$attributes), filterFun = data.tree::isLeaf)))
  # Filter out internal/common ones if desired
  internal_keys <- c("name", "relative_path", "children", "level", "parent", "path", "path_string", "position", "count", "is_leaf", "is_root", "root", "height")
  bids_keys <- sort(setdiff(all_keys, internal_keys))
  keys_str <- if (length(bids_keys) > 0) paste(bids_keys, collapse = ", ") else "(none)"
  cat_col("BIDS Keys: ", keys_str, col_fn = crayon::yellow)

  cat("Path: ", x$path, "\n") # Display the path (mock or stub)

  invisible(x)
}


#' Get Participants from Mock BIDS Project
#'
#' Extracts the unique participant IDs from the mock project definition.
#' Note: Returns IDs *without* the "sub-" prefix for consistency with `bids_project` methods.
#'
#' @param x A `mock_bids_project` object.
#' @param ... Extra arguments (ignored).
#' @return Character vector of unique participant IDs (e.g., c("01", "02")), sorted.
#' @export
#' @examples
#' # Create a mock project
#' parts <- data.frame(participant_id = c("sub-01", "sub-02"))
#' fs <- data.frame(subid=c("01", "02"), datatype="func", suffix="bold.nii.gz", fmriprep=FALSE)
#' mock_proj <- create_mock_bids("SimpleMock", parts, fs)
#'
#' # Get participant IDs
#' participants(mock_proj)
participants.mock_bids_project <- function(x, ...) {
  # Ensure participant_id is character, remove "sub-" prefix if present for consistency
  ids <- as.character(x$part_df$participant_id)
  ids <- stringr::str_remove(ids, "^sub-")
  # Return sorted unique IDs
  return(sort(unique(ids)))
}


#' Get Sessions from Mock BIDS Project
#'
#' Extracts the unique session IDs found in the mock project's file structure.
#' Note: Returns IDs *without* the "ses-" prefix.
#'
#' @param x A `mock_bids_project` object.
#' @param ... Extra arguments (ignored).
#' @return Character vector of unique session IDs (e.g., c("pre", "post")), sorted,
#'   or `NULL` if the project does not have sessions.
#' @export
#' @examples
#' # Create a mock project with sessions
#' parts <- data.frame(participant_id = "01")
#' fs <- data.frame(subid="01", session="test", datatype="func", suffix="bold.nii.gz", fmriprep=FALSE)
#' mock_proj <- create_mock_bids("SessionMock", parts, fs)
#'
#' # Get session IDs
#' sessions(mock_proj)
#'
#' # Project without sessions
#' fs_no_session <- data.frame(subid="01", datatype="func", suffix="bold.nii.gz", fmriprep=FALSE)
#' mock_proj_no_sess <- create_mock_bids("NoSessionMock", parts, fs_no_session)
#' sessions(mock_proj_no_sess) # Returns NULL
sessions.mock_bids_project <- function(x, ...) {
  if (!x$has_sessions) {
    return(NULL)
  }
  # Retrieve 'session' attribute from nodes that have it defined
  sessions_found <- unique(unlist(x$bids_tree$Get("session", filterFun = function(node) !is.null(node$session))))
  if (length(sessions_found) == 0) {
    return(NULL)
  }
  # Remove "ses-" prefix if present
  sessions_found <- stringr::str_remove(sessions_found, "^ses-")
  return(sort(sessions_found))
}


#' Get Tasks from Mock BIDS Project
#'
#' Extracts the unique task names found in the mock project's file structure.
#' Note: Returns names *without* the "task-" prefix.
#'
#' @param x A `mock_bids_project` object.
#' @param ... Extra arguments (ignored).
#' @return Character vector of unique task names (e.g., c("rest", "nback")), sorted.
#' @export
#' @examples
#' # Create a mock project with tasks
#' parts <- data.frame(participant_id = "01")
#' fs <- data.frame(subid="01", task="taskA", run="01", datatype="func", 
#'                  suffix="bold.nii.gz", fmriprep=FALSE)
#' fs <- rbind(fs, data.frame(subid="01", task="taskB", run="01", datatype="func", 
#'                           suffix="bold.nii.gz", fmriprep=FALSE))
#' mock_proj <- create_mock_bids("TaskMock", parts, fs)
#'
#' # Get task names
#' tasks(mock_proj)
tasks.mock_bids_project <- function(x, ...) {
  # Retrieve 'task' attribute from nodes where it's defined
  tasks_found <- unique(unlist(x$bids_tree$Get("task", filterFun = function(node) !is.null(node$task) && !is.na(node$task))))
  # Remove "task-" prefix if present
  tasks_found <- stringr::str_remove(tasks_found, "^task-")
  return(sort(tasks_found))
}

#' Search Files in Mock BIDS Structure
#'
#' Finds files in the mock BIDS tree by matching file names and BIDS entities.
#'
#' @param x A `mock_bids_project` object.
#' @param regex A regular expression to match filenames (node names). Default `".*"`.
#' @param full_path If `TRUE`, return full paths (prefixed with `x$path`).
#'        If `FALSE`, return relative paths within the BIDS structure. Default `FALSE`.
#' @param strict If `TRUE` (default), queries for a BIDS entity (e.g., `task="X"`)
#'        require the entity to exist on the file node and match the pattern.
#'        If `FALSE`, files lacking the queried entity are not automatically excluded
#'        (though they won't match if the pattern isn't `.*`).
#' @param ... Additional BIDS entities to match (e.g., `subid = "01"`, `task = "rest"`).
#'        Values are treated as regex patterns unless they are simple strings without regex characters.
#' @return A character vector of matching file paths, or `NULL` if no matches.
#' @rdname search_files
#' @export
search_files.mock_bids_project <- function(x, regex = ".*", full_path = FALSE, strict = TRUE, ...) {
  # Extract fmriprep parameter if provided
  dots <- list(...)
  fmriprep_filter <- NULL
  
  # Handle parameter name conversion
  # Map 'sub' to 'subid' and vice versa to handle inconsistencies in storage vs search
  # Only duplicate if not already present to avoid confusion
  if("subid" %in% names(dots) && !("sub" %in% names(dots))) { 
    dots$sub <- dots$subid  # When user passes subid, also check sub
  } else if("sub" %in% names(dots) && !("subid" %in% names(dots))) {
    dots$subid <- dots$sub  # When user passes sub, also check subid
  }
  
  # Use `ses` for consistency if provided as `session`
  if("session" %in% names(dots) && !("ses" %in% names(dots))) { 
    dots$ses <- dots$session 
  }
  # Also ensure 'session' exists if 'ses' is provided
  if("ses" %in% names(dots) && !("session" %in% names(dots))) { 
    dots$session <- dots$ses 
  }

  if ("fmriprep" %in% names(dots)) {
    fmriprep_filter <- dots$fmriprep
    # Remove fmriprep from dots before passing to mock_key_match
    dots <- dots[names(dots) != "fmriprep"]
    if (!is.logical(fmriprep_filter) || length(fmriprep_filter) != 1) {
       rlang::warn("'fmriprep' filter must be TRUE or FALSE. Ignoring.")
       fmriprep_filter <- NULL
    }
  } else if (x$has_fmriprep) {
    # Default behaviour: when derivatives exist but the caller did not specify
    # the `fmriprep` argument, search raw files only.  This mirrors the
    # behaviour expected in unit tests where strict=FALSE should still return
    # raw files even if derivatives are present.
    fmriprep_filter <- FALSE
  }

  # Define the filter function
  filterNodes <- function(node) {
    if (!node$isLeaf) return(FALSE)

    # Check filename regex
    if (!stringr::str_detect(node$name, regex)) {
      # cat("DEBUG search_files: Node", node$name, "rejected - filename doesn't match regex\n")
      return(FALSE)
    }

    # Determine if node is raw or derivative based on its path
    is_in_deriv <- FALSE
    node_path_str_parts <- node$path # Get path components from root
    prep_dir_parts <- strsplit(x$prep_dir, "/")[[1]]
    if (x$has_fmriprep && 
        length(node_path_str_parts) > (1 + length(prep_dir_parts)) &&
        all(node_path_str_parts[2:(1 + length(prep_dir_parts))] == prep_dir_parts)) {
         is_in_deriv <- TRUE
    }

    # Apply fmriprep filter if specified
    if (!is.null(fmriprep_filter)) {
      # Filter based on fmriprep value
      if (fmriprep_filter && !is_in_deriv) {
        # cat("DEBUG search_files: Node", node$name, "rejected - want deriv but not in deriv\n")
        return(FALSE) # Want deriv, but not in deriv
      }
      if (!fmriprep_filter && is_in_deriv) {
        # cat("DEBUG search_files: Node", node$name, "rejected - want raw but in deriv\n")
        return(FALSE) # Want raw, but in deriv
      }
    }

    # Regular entity filtering using mock_key_match
    if (!mock_key_match(node_attrs = node, filters = dots, default = !strict)) {
      # Debugging for specific node failures can go here if needed, carefully accessing variables
      return(FALSE)
    }

    return(TRUE)
  }

  # Get the relative paths of matching leaf nodes
  relative_paths <- x$bids_tree$Get(function(node) node$relative_path, filterFun = filterNodes, simplify = FALSE)

  # Filter out any NULL paths that might have resulted
  relative_paths <- Filter(Negate(is.null), relative_paths)

  if (length(relative_paths) == 0) {
    return(NULL)
  }

  # Convert list to vector and ensure uniqueness
  relative_paths <- unique(unlist(relative_paths))

  # Add full path prefix if requested
  if (full_path) {
    if (startsWith(x$path, "mock://")) {
      proj_prefix <- sub("^mock://", "", x$path)
      final_paths <- file.path(proj_prefix, relative_paths)
    } else {
      final_paths <- file.path(x$path, relative_paths)
    }
  } else {
    final_paths <- relative_paths
  }

  return(final_paths)
}


#' Get Functional Scans from Mock BIDS Project
#' @export
#' @rdname func_scans
#' @param x A mock_bids_project object
#' @param subid Regex to match subject IDs (default: ".*")
#' @param task Regex to match tasks (default: ".*")
#' @param run Regex to match runs (default: ".*") 
#' @param session Regex to match sessions (default: ".*")
#' @param kind Type of functional data (default: "bold")
#' @param suffix Regex pattern for file suffix (default: "nii(\\.gz)?$")
#' @param full_path If TRUE, return full file paths (default: TRUE)
#' @param ... Additional arguments passed to search_files
func_scans.mock_bids_project <- function(x, subid = ".*", task = ".*", run = ".*", session = ".*",
                                         kind = "bold", suffix = "nii(\\.gz)?$", full_path = TRUE, ...) {
  # Use search_files, filtering for raw files (fmriprep=FALSE),
  # and matching the provided kind. Use suffix arg for regex matching filename end.

  filter_entities <- list(
      kind = kind,
      sub = subid,  # Both parameter names for consistent matching 
      subid = subid,
      task = task,
      run = run,
      ses = session, # Both parameter names for consistent matching 
      session = session,
      ...
  )
  filter_entities <- Filter(Negate(is.null), filter_entities)

  search_args <- list(
      x = x,
      regex = paste0(".*\\.", suffix), # Regex for filename ending
      full_path = full_path,
      fmriprep = FALSE # Explicitly search only raw data
  )

  final_args <- c(search_args, filter_entities)
  rlang::exec(search_files, !!!final_args)
}



#' Get Event Files from Mock BIDS Project
#' @export
#' @rdname event_files-method
#' @param x A mock_bids_project object
#' @param subid Regex to match subject IDs (default: ".*")
#' @param task Regex to match tasks (default: ".*")
#' @param run Regex to match runs (default: ".*")
#' @param session Regex to match sessions (default: ".*")
#' @param full_path If TRUE, return full paths of files (default: TRUE)
#' @param ... Additional arguments passed to internal functions
event_files.mock_bids_project <- function(x, subid = ".*", task = ".*", run = ".*", session = ".*", full_path = TRUE, ...) {
  
  # Search specifically for events.tsv files, assuming they are raw data
  result <- search_files(
    x,
    regex = "events\\.tsv$",
    full_path = full_path,
    sub = subid,
    subid = subid, # Pass both parameter names for consistent matching
    task = task,
    run = run,
    ses = session, # Both parameter names for consistent matching 
    session = session,
    fmriprep = FALSE, # Events usually associated with raw data
    kind = "events", # Pass kind="events" if encode() produces it reliably
    ...
  )
  
  return(result)
}




#' @export
preproc_scans.mock_bids_project <- function(x, subid = ".*", task = ".*", run = ".*", session = ".*",
                                           variant = NULL, space = ".*", modality = "bold",
                                           kind = ".*", full_path = FALSE, ...) {
  if (!x$has_fmriprep) {
    rlang::inform("Mock project does not have derivatives enabled.")
    return(NULL)
  }

  # Build entity list for filtering
  filter_entities <- list(
      kind = kind,       # Usually the BIDS type like 'bold' or 'T1w'
      desc = "preproc",  # Fixed desc for preprocessed files
      space = space,
      sub = subid,       # Pass both parameter names for consistent matching
      subid = subid,
      task = task,
      run = run,
      ses = session, # Both parameter names for consistent matching 
      session = session,
      variant = variant, # Will be NULL if not provided, handled by Filter
      ...
  )
  filter_entities <- Filter(Negate(is.null), filter_entities)

  # Build arguments for search_files call
  search_args <- list(
      x = x,
      regex = ".*\\.nii(\\.gz)?$", # Fixed regex for nifti files
      full_path = full_path,
      fmriprep = TRUE # Explicitly search only derivatives
  )

  final_args <- c(search_args, filter_entities)
  rlang::exec(search_files, !!!final_args)
}


#' Read Event Files from Mock BIDS Project
#'
#' Retrieves and formats event data stored within the mock project object.
#'
#' @param x A `mock_bids_project` object.
#' @param subid Regex pattern for subject IDs. Default `".*"`.
#' @param task Regex pattern for task names. Default `".*"`.
#' @param run Regex pattern for run indices. Default `".*"`.
#' @param session Regex pattern for session IDs. Default `".*"`.
#' @param ... Additional arguments passed to `event_files`.
#' @return A nested tibble with columns `.subid`, `.task`, `.run`, `.session` (if applicable),
#'   and `data` (containing the event tibbles), or an empty tibble if no matching data.
#' @rdname read_events-method
#' @export
read_events.mock_bids_project <- function(x, subid = ".*", task = ".*", run = ".*", session = ".*", ...) {

  # Find the relative paths of the relevant event files using the S3 method
  relative_event_paths <- event_files.mock_bids_project(
      x,
      subid = subid, # Pass subid here
      task = task,
      run = run,
      session = session, # Pass session here
      full_path = FALSE, # Need relative paths to key into event_data_store
      ...
  )

  if (is.null(relative_event_paths) || length(relative_event_paths) == 0) {
    # inform is noisy, return empty tibble quietly unless verbose option added
    # rlang::inform("No matching event files found in the mock project.")
    return(tibble::tibble(
        .subid = character(),
        .task = character(),
        .run = character(),
        .session = character(),
        data = list()
        ))
  }

  all_event_data <- list()

  # Retrieve data for each found path
  for (rel_path in relative_event_paths) {
    if (rel_path %in% names(x$event_data_store)) {
      event_df <- x$event_data_store[[rel_path]]

      # Find the corresponding node in the tree to get metadata reliably
      target_node <- NULL
      # Use data.tree::FindNode which might be cleaner if node names are unique path components
      # Or stick with Traverse if nodes might have same name but different attrs/path
      node_list <- data.tree::Traverse(x$bids_tree, filterFun = function(n) {
           n$isLeaf && !is.null(n$relative_path) && n$relative_path == rel_path
      })
      if (!is.null(node_list) && length(node_list) > 0) {
          target_node <- node_list[[1]] # Assume first match is correct
      }

      if (!is.null(target_node)) {
          # --- Extract metadata --- Ensure these attribute names are correct!
          meta <- list(
              .subid = target_node$subid %||% target_node$sub, # Try both attribute names
              .task = target_node$task,
              .run = target_node$run,
              .session = target_node$ses %||% target_node$session # Try both attribute names
          )
          # Remove NULLs and ensure required columns exist, even if NA
          meta$.subid <- meta$.subid %||% NA_character_
          meta$.task <- meta$.task %||% NA_character_
          meta$.run <- meta$.run %||% NA_character_
          meta$.session <- meta$.session %||% NA_character_

          # Combine with event_df
          if(nrow(event_df) > 0) {
              meta_df <- tibble::as_tibble(meta)
              combined_df <- dplyr::bind_cols(meta_df, event_df)
              all_event_data[[rel_path]] <- combined_df
          } else {
              # Handle empty event files - create tibble with just metadata
               meta_df <- tibble::as_tibble(meta)
               # Add necessary columns if event_df was empty but metadata exists
               if(nrow(meta_df)>0){
                    meta_df <- meta_df %>% dplyr::mutate(onset = NA_real_, duration = NA_real_) # Example cols
               }
               # If meta is also empty, this results in an empty tibble, handled by bind_rows
               all_event_data[[rel_path]] <- meta_df
          }
      } else {
        warn(paste("Could not find node in bids_tree corresponding to event file path:", rel_path))
      }
    } else {
      warn(paste("Event data not found in store for path:", rel_path))
    }
  }

  if (length(all_event_data) == 0) {
     # inform is noisy
     # rlang::inform("No event data could be loaded for the matching files.")
     return(tibble::tibble(
         .subid = character(), .task = character(), .run = character(),
         .session = character(), data = list()
       ))
  }

  # Combine all data frames
  final_df <- dplyr::bind_rows(all_event_data)

  # Check if essential grouping vars were created
  if (!all(c(".subid", ".task", ".run", ".session") %in% names(final_df))) {
       rlang::warn("Missing essential metadata columns (.subid, .task, .run, .session) after combining event data. Cannot nest correctly.")
       # Return flat frame or try partial nesting? Return flat for now.
       return(final_df)
   }


  # Create the nested structure
  # Ensure grouping vars actually exist before grouping
  grouping_vars <- intersect(c(".subid", ".task", ".run", ".session"), names(final_df))

  # Remove grouping vars with all NAs (like .session if none exist)
  # grouping_vars <- grouping_vars[sapply(grouping_vars, function(v) !all(is.na(final_df[[v]])))]

  if (length(grouping_vars) > 0) {
      # Handle potential NA grouping vars if needed by group_by
      nested_df <- final_df %>%
          # Convert NA sessions/runs to a specific value if group_by drops NAs?
          # Or ensure they are character NAs
          dplyr::group_by(!!!rlang::syms(grouping_vars)) %>%
          tidyr::nest()

      # Ensure standard columns exist
       std_cols <- c(".subid", ".task", ".run", ".session", "data")
       for(col in std_cols) {
           if (!col %in% names(nested_df)) {
               # Add missing column - decide default type carefully
               if(col=="data") nested_df[[col]] <- list() else nested_df[[col]] <- NA_character_
           }
       }
       # Reorder columns
       nested_df <- nested_df %>% dplyr::select(dplyr::all_of(std_cols))

  } else {
       rlang::warn("Could not determine grouping variables for nesting event data.")
       nested_df <- tibble::tibble(data = list(final_df)) # Fallback
  }

  return(nested_df)
}


#' Find Confound Files in Mock BIDS Project
#'
#' Searches the mock BIDS structure for files matching typical confound file patterns
#' (e.g., `*_confounds*.tsv`, `*_regressors*.tsv`, `*_timeseries*.tsv`)
#' within the derivatives directory.
#'
#' @details This function assumes confound files reside in the derivatives path
#'   specified by `x$prep_dir` and were defined in the `file_structure`
#'   passed to `create_mock_bids` with `fmriprep=TRUE`.
#'
#' @param x A `mock_bids_project` object.
#' @param subid Regex pattern for subject IDs. Default `".*"`.
#' @param task Regex pattern for task names. Default `".*"`.
#' @param session Regex pattern for session IDs. Default `".*"`.
#' @param run Regex pattern for run indices. Default `".*"`.
#' @param full_path If `TRUE`, return full paths (prefixed with `x$path`). 
#'        If `FALSE` (default), return relative paths.
#' @param ... Additional arguments passed to `search_files`.
#' @return A character vector of relative or full paths to potential confound files,
#'   or `NULL` if none are found.
#' @export
#' @rdname confound_files-method
#' @examples
#' # Setup mock project with a derivative confound file
#' participants_df <- tibble::tibble(participant_id = "01")
#' file_structure_df <- tibble::tribble(
#'   ~subid, ~session, ~datatype, ~task, ~run, ~suffix, ~fmriprep, ~desc,
#'   "01",   NA,       "func",    "taskA", "01", 
#'   "desc-confounds_timeseries.tsv", TRUE, "confounds"
#' )
#' mock_proj <- create_mock_bids("ConfoundMock", participants_df, file_structure_df)
#' 
#' # Find confound files
#' confound_files(mock_proj)
#' 
#' # Find for specific subject
#' confound_files(mock_proj, subid="01")
confound_files.mock_bids_project <- function(x, subid = ".*", task = ".*", session = ".*", run = ".*", full_path = FALSE, ...) {
  if (!x$has_fmriprep) {
    inform("Mock project does not have derivatives enabled, cannot search for confound files.")
    return(NULL)
  }
  
  # Define common regex patterns for confound files
  confound_regex <- "(confounds|regressors|timeseries)\\.tsv$"
  
  search_files(
    x,
    regex = confound_regex,
    full_path = full_path,
    sub = subid,
    task = task,
    ses = session,
    run = run,
    fmriprep = TRUE, # Explicitly search only derivatives
    ...
  )
}


#' Read Confound Files (Mock Implementation)
#' @param x A `mock_bids_project` object.
#' @param subid Regex pattern for subject IDs. Default `".*"`.
#' @param task Regex pattern for task names. Default `".*"`.
#' @param session Regex pattern for session IDs. Default `".*"`.
#' @param run Regex pattern for run indices. Default `".*"`.
#' @param cvars Variables to select (ignored in mock).
#' @param npcs PCA components (ignored in mock).
#' @param perc_var PCA variance (ignored in mock).
#' @param nest If `TRUE`, returns a nested tibble keyed by subject, session and run.
#' @param ... Additional BIDS entities (passed to `search_files`).
#' @return A tibble of confound data (nested if `nest = TRUE`).
#' @rdname read_confounds-method
#' @export
read_confounds.mock_bids_project <- function(x, subid = ".*", task = ".*", session = ".*", run = ".*",
                                             cvars = NULL, npcs = -1, perc_var = -1, nest = TRUE, ...) {

  conf_paths <- confound_files.mock_bids_project(x, subid = subid, task = task,
                                                 session = session, run = run,
                                                 full_path = FALSE, ...)

  if (is.null(conf_paths) || length(conf_paths) == 0) {
    return(tibble::tibble(
      .subid = character(), .task = character(), .run = character(),
      .session = character(), data = list()
    ))
  }

  all_conf <- list()
  for (rel_path in conf_paths) {
    if (rel_path %in% names(x$confound_data_store)) {
      conf_df <- x$confound_data_store[[rel_path]]
    } else {
      conf_df <- tibble::tibble()
    }

    node_list <- data.tree::Traverse(x$bids_tree, filterFun = function(n) {
      n$isLeaf && !is.null(n$relative_path) && n$relative_path == rel_path
    })
    if (length(node_list) > 0) {
      nd <- node_list[[1]]
      meta <- list(
        .subid = nd$subid %||% nd$sub,
        .task = nd$task,
        .run = nd$run,
        .session = nd$ses %||% nd$session,
        .desc = nd$desc
      )
      meta$.subid <- meta$.subid %||% NA_character_
      meta$.task <- meta$.task %||% NA_character_
      meta$.run <- meta$.run %||% NA_character_
      meta$.session <- meta$.session %||% NA_character_
      meta$.desc <- meta$.desc %||% NA_character_
    } else {
      meta <- list(.subid = NA_character_, .task = NA_character_,
                   .run = NA_character_, .session = NA_character_,
                   .desc = NA_character_)
    }

    combined_df <- dplyr::bind_cols(tibble::as_tibble(meta), tibble::as_tibble(conf_df))
    all_conf[[rel_path]] <- combined_df
  }

  final_df <- dplyr::bind_rows(all_conf)

  if (!nest) {
    return(final_df)
  }

  grouping_vars <- intersect(c(".subid", ".task", ".run", ".session", ".desc"), names(final_df))
  final_df %>% dplyr::group_by(!!!rlang::syms(grouping_vars)) %>% tidyr::nest()
}


#' Create Preprocessing Mask (Mock Implementation)
#'
#' This function is not implemented for `mock_bids_project` objects as they do not
#' contain actual image data required to create a mask.
#'
#' @param x A `mock_bids_project` object.
#' @param ... Arguments (ignored).
#' @return Throws an error indicating the function is not applicable to mock objects.
#' @export
#' @rdname create_preproc_mask-method
create_preproc_mask.mock_bids_project <- function(x, ...) {
  abort("`create_preproc_mask` requires actual image data and cannot be used with `mock_bids_project` objects.")
}
