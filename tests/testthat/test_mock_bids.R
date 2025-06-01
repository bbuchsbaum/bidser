# tests/testthat/test_mock_bids.R

library(testthat)
library(tibble)
# Ensure bidser is loaded, or use bidser:: where needed if testing externally
# library(bidser) 

context("Mock BIDS Project Functionality")

# --- Test Data Setup ---
participants_df <- tibble::tibble(participant_id = c("01", "02"), age = c(25, 30))

# Define file structure more carefully, including 'kind' if used by encode/search
# Ensure attributes match what bidser::encode would produce
file_structure_df <- tibble::tribble(
  ~subid, ~session, ~datatype, ~task,   ~run, ~suffix,      ~fmriprep, ~desc,     ~space, # Suffix is BIDS suffix only now
  "01",   NA,       "anat",    NA,      NA,   "T1w",        FALSE,     NA,        NA,        
  "01",   NA,       "func",    "taskA", "01", "bold",       FALSE,     NA,        NA,        
  "01",   NA,       "func",    "taskA", "01", "events",     FALSE,     NA,        NA,        
  "02",   "test",   "anat",    NA,      NA,   "T1w",        FALSE,     NA,        NA,        
  "02",   "test",   "func",    "taskA", "01", "bold",       FALSE,     NA,        NA,        
  "02",   "test",   "func",    "taskA", "01", "events",     FALSE,     NA,        NA,        
  # Derivatives 
  "01",   NA,       "anat",    NA,      NA,   "T1w",        TRUE,      "preproc", "MNI",  
"01",   NA,       "func",    "taskA", "01", "bold",       TRUE,      "preproc", "MNI",
"01",   NA,       "func",    "taskA", "01", "desc-confounds_timeseries.tsv", TRUE, "confounds", NA,
)

# Define event data (paths must match generated structure)
# Use hardcoded paths based on what generate_bids_filename would likely produce
# Note: suffix in generate_bids_filename needs the extension!
event_filename_1 <- generate_bids_filename(subid = "01", task = "taskA", run = "01", suffix = "events.tsv")
event_filename_2 <- generate_bids_filename(subid = "02", session = "test", task = "taskA", run = "01", suffix = "events.tsv")
event_path_1 <- file.path("sub-01", "func", event_filename_1)
event_path_2 <- file.path("sub-02", "ses-test", "func", event_filename_2)

event_data_list <- list()
event_data_list[[event_path_1]] <- tibble::tibble(
  onset = c(1.0, 5.0), duration = c(0.5, 0.5), trial_type = c("condA", "condB")
)
event_data_list[[event_path_2]] <- tibble::tibble(
  onset = c(1.5, 5.5), duration = c(0.5, 0.5), trial_type = c("condC", "condD")
)

# Construct expected derivative filenames/patterns
# Filename generation helper needs extension in suffix arg
deriv_anat_filename <- generate_bids_filename(subid = "01", suffix = "T1w.nii.gz", space="MNI", desc="preproc")
deriv_func_filename <- generate_bids_filename(subid = "01", task = "taskA", run = "01", suffix = "bold.nii.gz", space="MNI", desc="preproc")

deriv_anat_relpath <- file.path("derivatives", "mockprep", "sub-01", "anat", deriv_anat_filename)
deriv_func_relpath <- file.path("derivatives", "mockprep", "sub-01", "func", deriv_func_filename)

# Define confound data (paths must match generated structure for derivatives)
confound_filename <- generate_bids_filename(subid = "01", task = "taskA", run = "01", suffix = "timeseries.tsv", desc = "confounds")
confound_relpath <- file.path("derivatives", "mockprep", "sub-01", "func", confound_filename)

confound_data_list <- list()
confound_data_list[[confound_relpath]] <- tibble::tibble(
  CSF = c(0.1, 0.2), WhiteMatter = c(0.3, 0.4), GlobalSignal = c(0.5, 0.6)
)

# --- Test Creation ---
test_that("Mock BIDS project can be created", {
  # Need to add .nii.gz back to suffix in file_structure for filename generation
  fs_for_create <- file_structure_df %>%
    mutate(suffix_ext = case_when(
      suffix == "T1w" ~ "T1w.nii.gz",
      suffix == "bold" ~ "bold.nii.gz",
      suffix == "events" ~ "events.tsv",
      TRUE ~ suffix # Fallback
    ))
    
  mock_proj <- create_mock_bids(
    project_name = "MockTaskA",
    participants = participants_df,
    file_structure = fs_for_create %>% select(-suffix) %>% rename(suffix=suffix_ext), # Pass suffix with extension
    event_data = event_data_list,
    confound_data = confound_data_list,
    prep_dir = "derivatives/mockprep" # Use a distinct prep_dir for testing
  )

  expect_s3_class(mock_proj, "mock_bids_project")
  expect_equal(mock_proj$name, "MockTaskA")
  expect_true(mock_proj$has_sessions)
  expect_true(mock_proj$has_fmriprep)
  expect_equal(mock_proj$prep_dir, "derivatives/mockprep")
})

# --- Test Basic Queries ---
test_that("Basic queries work on mock BIDS project", {
  fs_for_create <- file_structure_df %>%
    mutate(suffix_ext = case_when(
      suffix == "T1w" ~ "T1w.nii.gz",
      suffix == "bold" ~ "bold.nii.gz",
      suffix == "events" ~ "events.tsv",
      TRUE ~ suffix
    ))
  mock_proj <- create_mock_bids(
    project_name = "MockTaskA",
    participants = participants_df,
    file_structure = fs_for_create %>% select(-suffix) %>% rename(suffix=suffix_ext),
    event_data = event_data_list,
    confound_data = confound_data_list,
    prep_dir = "derivatives/mockprep"
  )

  expect_equal(sort(participants(mock_proj)), c("01", "02"))
  expect_equal(sessions(mock_proj), "test") # Only one session defined
  expect_equal(tasks(mock_proj), "taskA") # Only one task defined
})

# --- Test File Searching ---
test_that("File searching methods work on mock BIDS project", {
  fs_for_create <- file_structure_df %>%
    mutate(suffix_ext = case_when(
      suffix == "T1w" ~ "T1w.nii.gz",
      suffix == "bold" ~ "bold.nii.gz",
      suffix == "events" ~ "events.tsv",
      TRUE ~ suffix
    ))
  mock_proj <- create_mock_bids(
    project_name = "MockTaskA",
    participants = participants_df,
    file_structure = fs_for_create %>% select(-suffix) %>% rename(suffix=suffix_ext),
    event_data = event_data_list,
    confound_data = confound_data_list,
    prep_dir = "derivatives/mockprep"
  )

  # search_files for raw T1w
  # Filter by kind="T1w", fmriprep=FALSE, regex for extension
  t1w_files_raw <- search_files(mock_proj, kind = "T1w", regex = "\\.nii\\.gz$", fmriprep = FALSE, full_path = FALSE)
  expect_equal(length(t1w_files_raw), 2) # Should find 2 raw T1w files
  raw_t1w_f1 <- generate_bids_filename(subid="01", suffix="T1w.nii.gz")
  raw_t1w_f2 <- generate_bids_filename(subid="02", session="test", suffix="T1w.nii.gz")
  expect_true(all(sort(t1w_files_raw) %in% sort(c(file.path("sub-01","anat",raw_t1w_f1),
                                                  file.path("sub-02","ses-test","anat",raw_t1w_f2)))))

  # func_scans (should only return raw bold)
  # Pass suffix as the extension pattern, use 'sub'
  fscans_sub1 <- func_scans(mock_proj, sub = "01", suffix = "nii\\.gz$", full_path = FALSE)
  expect_equal(length(fscans_sub1), 1) # Should find 1 raw bold for sub-01
  raw_bold_f1 <- generate_bids_filename(subid="01", task="taskA", run="01", suffix="bold.nii.gz")
  expect_equal(fscans_sub1, file.path("sub-01", "func", raw_bold_f1))

  fscans_all <- func_scans(mock_proj, suffix = "nii\\.gz$", full_path = FALSE)
  expect_equal(length(fscans_all), 2) # Should find 2 raw bold files total

  # event_files
  # Use 'subid', 'session' instead of 'sub', 'ses'
  ev_files_sub2_ses <- event_files(mock_proj, subid = "02", session = "test", full_path = FALSE)
  expect_equal(length(ev_files_sub2_ses), 1) # Should find 1 event file
  expect_equal(ev_files_sub2_ses, event_path_2)

  ev_files_all <- event_files(mock_proj, full_path = FALSE)
  expect_equal(length(ev_files_all), 2) # Should find 2 event files total
  expect_true(all(sort(ev_files_all) %in% sort(c(event_path_1, event_path_2))))

  # preproc_scans (functional) - check path construction
  # Filter by BIDS entities: kind, desc, space. Use 'sub'. Pass suffix for extension regex.
  preproc_func <- preproc_scans(mock_proj, sub = "01", task = "taskA", kind="bold", space="MNI", desc="preproc", suffix = "nii\\.gz$", full_path = FALSE)
  expect_equal(length(preproc_func), 1) # Should find 1 derivative func
  expect_equal(preproc_func, deriv_func_relpath)

  # search_files for derivatives (anatomical)
  # Filter by kind="T1w", fmriprep=TRUE, regex for extension. Use 'sub'.
  preproc_anat <- search_files(mock_proj, sub = "01", kind="T1w", space="MNI", desc="preproc", regex = "\\.nii\\.gz$", fmriprep=TRUE, full_path = FALSE)
  expect_equal(length(preproc_anat), 1) # Should find 1 derivative anat
  expect_equal(preproc_anat, deriv_anat_relpath)
})


# --- Test Event Reading --- # (Check test content based on the expected output of the fixed read_events)
test_that("Event reading works on mock BIDS project", {
   fs_for_create <- file_structure_df %>%
    mutate(suffix_ext = case_when(
      suffix == "T1w" ~ "T1w.nii.gz",
      suffix == "bold" ~ "bold.nii.gz",
      suffix == "events" ~ "events.tsv",
      TRUE ~ suffix
    ))
   mock_proj <- create_mock_bids(
    project_name = "MockTaskA",
    participants = participants_df,
   file_structure = fs_for_create %>% select(-suffix) %>% rename(suffix=suffix_ext),
    event_data = event_data_list,
    confound_data = confound_data_list,
    prep_dir = "derivatives/mockprep"
  )

  expect_true(length(mock_proj$event_data_store) > 0) # Check data was stored

  # Read events for sub-01. Use 'sub'
  events_sub1 <- read_events(mock_proj, sub = "01")

  expect_s3_class(events_sub1, "tbl_df")
  expect_gt(nrow(events_sub1), 0) # Expect one row for the sub-01 run

  # Check metadata columns in the *outer* tibble
  expect_named(events_sub1, c(".subid", ".task", ".run", ".session", "data"), ignore.order = TRUE)
  expect_equal(events_sub1$.subid[[1]], "01") # Access first element
  expect_equal(events_sub1$.task[[1]], "taskA")
  expect_equal(events_sub1$.run[[1]], "01")
  # Check session is NA or correct value if applicable
  expect_true(is.na(events_sub1$.session[[1]]) || is.character(events_sub1$.session[[1]]))

  # Check the *nested* data
  expect_s3_class(events_sub1$data[[1]], "tbl_df") # Access first list element
  expect_equal(events_sub1$data[[1]]$trial_type, c("condA", "condB"))

  # Read all events
  events_all <- read_events(mock_proj)
  expect_equal(nrow(events_all), 2) # Expect two rows (one per event file)
})

# --- Test Unmatched Queries and strict=FALSE ---
test_that("Unmatched queries return NULL or empty tibble", {
  fs_for_create <- file_structure_df %>%
    mutate(suffix_ext = case_when(
      suffix == "T1w" ~ "T1w.nii.gz",
      suffix == "bold" ~ "bold.nii.gz",
      suffix == "events" ~ "events.tsv",
      TRUE ~ suffix
    ))
  mock_proj <- create_mock_bids(
    project_name = "MockTaskA",
    participants = participants_df,
    file_structure = fs_for_create %>% select(-suffix) %>% rename(suffix = suffix_ext),
    event_data = event_data_list,
    prep_dir = "derivatives/mockprep"
  )

  # No subject "99" exists
  expect_null(search_files(mock_proj, subid = "99")) # use subid
  expect_null(event_files(mock_proj, subid = "99"))
  expect_null(func_scans(mock_proj, subid = "99"))

  empty_events <- read_events(mock_proj, subid = "99") # use subid
  expect_s3_class(empty_events, "tbl_df")
  expect_equal(nrow(empty_events), 0)
})

test_that("search_files strict=FALSE matches files with missing entities", {
  fs_for_create <- file_structure_df %>%
    mutate(suffix_ext = case_when(
      suffix == "T1w" ~ "T1w.nii.gz",
      suffix == "bold" ~ "bold.nii.gz",
      suffix == "events" ~ "events.tsv",
      TRUE ~ suffix
    ))
  mock_proj <- create_mock_bids(
    project_name = "MockTaskA",
    participants = participants_df,
    file_structure = fs_for_create %>% select(-suffix) %>% rename(suffix = suffix_ext),
    event_data = event_data_list,
    prep_dir = "derivatives/mockprep"
  )

  # strict=TRUE should fail because T1w files lack a task attribute
  strict_res <- search_files(
    mock_proj,
    subid = "01", # use subid
    task = "taskA",
    kind = "T1w",
    regex = "\\.nii\\.gz$",
    # fmriprep = FALSE, # search_files does not have fmriprep argument directly
    strict = TRUE,
    full_path = FALSE
  )
  expect_null(strict_res)

  # To test strict=FALSE correctly, we expect it to find the T1w file for sub-01
  # even though we are querying for task="taskA" which T1w files don't have.
  # The search_files function should look in raw data by default unless fmriprep=TRUE is part of ...
  # and then specifically handled by the internal logic of search_files.
  # For this test, we're querying raw data (fmriprep=FALSE implied for T1w).
  lax_res <- search_files(
    mock_proj,
    subid = "01", # use subid
    task = "taskA", # T1w files won't have 'task'
    kind = "T1w",
    regex = "\\.nii\\.gz$",
    strict = FALSE, # This allows matching even if 'task' is not an attribute of T1w
    full_path = FALSE
  )
  # With strict=FALSE, search_files should find the T1w file even though we're searching for task="taskA"
  # T1w files don't have a task attribute, but strict=FALSE allows the match
  expect_equal(lax_res, "sub-01/anat/sub-01_T1w.nii.gz")
})

# --- Test Confound Utilities ---
test_that("Confound reading works on mock BIDS project", {
  fs_for_create <- file_structure_df %>%
    mutate(suffix_ext = case_when(
      suffix == "T1w" ~ "T1w.nii.gz",
      suffix == "bold" ~ "bold.nii.gz",
      suffix == "events" ~ "events.tsv",
      TRUE ~ suffix # Fallback
    ))
  
  # Ensure the file_structure_df includes a row for the confound file if create_mock_bids expects it
  # Or, ensure create_mock_bids can handle confound_data pointing to files not explicitly in file_structure
  # For this test, we assume create_mock_bids will use confound_data_list to populate the mock files
  
  mock_proj <- create_mock_bids(
    project_name = "ConfoundTest",
    participants = participants_df,
    file_structure = fs_for_create %>% select(-suffix) %>% rename(suffix=suffix_ext),
    event_data = event_data_list,
    confound_data = confound_data_list, # This will be used to create the mock confound file
    prep_dir = "derivatives/mockprep"
  )

  # Test confound_files
  # confound_files should find the mock confound file based on its path in confound_data_list keys
  cf_files <- confound_files(mock_proj, full_path = FALSE)
  expect_equal(length(cf_files), 1)
  expect_equal(cf_files, confound_relpath) # Check against the predefined relative path

  # Test read_confounds
  conf_tibble <- read_confounds(mock_proj) # Reads based on what confound_files finds
  expect_s3_class(conf_tibble, "tbl_df")
  expect_equal(nrow(conf_tibble), 1) # Expect one row for the single confound file
  
  # Check metadata columns
  expect_true(all(c(".subid", ".task", ".run", ".desc", "data") %in% names(conf_tibble)))
  expect_equal(conf_tibble$.subid[[1]], "01")
  expect_equal(conf_tibble$.task[[1]], "taskA")
  expect_equal(conf_tibble$.run[[1]], "01")
  expect_equal(conf_tibble$.desc[[1]], "confounds")

  # Check the nested data
  expect_s3_class(conf_tibble$data[[1]], "tbl_df")
  expect_equal(nrow(conf_tibble$data[[1]]), 2) # Based on our mock confound_data_list
  expect_true(all(c("CSF", "WhiteMatter", "GlobalSignal") %in% names(conf_tibble$data[[1]])))
  expect_equal(conf_tibble$data[[1]]$CSF, c(0.1, 0.2))
})


# --- Test Stub Creation (Optional) ---
# This test writes to disk, might be skipped on CI or needs cleanup
test_that("Mock BIDS stub creation works (writes to temp)", {
  skip_on_cran() # Don't run this on CRAN as it writes files
  # skip_if_not_installed("readr") # Ensure readr is available if used

  temp_stub_path <- tempfile(pattern = "mockbids_stub_")
  on.exit(unlink(temp_stub_path, recursive = TRUE, force = TRUE), add = TRUE)

  fs_for_create <- file_structure_df %>%
    mutate(suffix_ext = case_when(
      suffix == "T1w" ~ "T1w.nii.gz",
      suffix == "bold" ~ "bold.nii.gz",
      suffix == "events" ~ "events.tsv",
      TRUE ~ suffix
    ))
  mock_proj_stub <- create_mock_bids(
    project_name = "MockStub",
    participants = participants_df,
    file_structure = fs_for_create %>% select(-suffix) %>% rename(suffix=suffix_ext),
    event_data = event_data_list,
    confound_data = confound_data_list,
    create_stub = TRUE,
    stub_path = temp_stub_path,
    prep_dir = "derivatives/mockprep" # Match prep dir
  )

  expect_true(dir.exists(temp_stub_path))
  expect_true(file.exists(file.path(temp_stub_path, "participants.tsv")))
  expect_true(file.exists(file.path(temp_stub_path, "dataset_description.json")))
  expect_true(dir.exists(file.path(temp_stub_path, "sub-01", "func")))
  # Check for a raw file using its expected generated name
  raw_bold_f1 <- generate_bids_filename(subid="01", task="taskA", run="01", suffix="bold.nii.gz")
  expect_true(file.exists(file.path(temp_stub_path, "sub-01/func", raw_bold_f1)))
  expect_true(dir.exists(file.path(temp_stub_path, "derivatives", "mockprep", "sub-01", "func")))
  
  # Check for derivative files using exact paths now that we generate them
  expect_true(file.exists(file.path(temp_stub_path, deriv_func_relpath)))
  expect_true(file.exists(file.path(temp_stub_path, deriv_anat_relpath)))

  # Check event file content
  event_file_path_1_disk <- file.path(temp_stub_path, event_path_1)
  expect_true(file.exists(event_file_path_1_disk))
  
  # Use tryCatch for reading in case readr isn't available or file issue
  stub_event_data_1 <- tryCatch({
      readr::read_tsv(event_file_path_1_disk, show_col_types = FALSE)
  }, error = function(e) {
      warning("Could not read stub event file: ", e$message)
      NULL
  })
  
  if (!is.null(stub_event_data_1)) {
      expect_s3_class(stub_event_data_1, "tbl_df")
      expect_equal(nrow(stub_event_data_1), 2)
      expect_equal(stub_event_data_1$trial_type, c("condA", "condB"))
  }
})

# --- Test Debugging: Node Inspection ---
test_that("Node attributes inspection", {
  skip_on_cran() # Skip on CRAN, this is a debugging test
  
  fs_for_create <- file_structure_df %>%
    mutate(suffix_ext = case_when(
      suffix == "T1w" ~ "T1w.nii.gz",
      suffix == "bold" ~ "bold.nii.gz",
      suffix == "events" ~ "events.tsv",
      TRUE ~ suffix
    ))
  
  mock_proj <- create_mock_bids(
    project_name = "Debug",
    participants = participants_df,
    file_structure = fs_for_create %>% select(-suffix) %>% rename(suffix=suffix_ext),
    event_data = event_data_list,
    confound_data = confound_data_list,
    prep_dir = "derivatives/mockprep"
  )
  
  # Find a T1w file node 
  t1w_node <- NULL
  print("Getting T1w leaf nodes...")
  leaf_nodes <- data.tree::Traverse(mock_proj$bids_tree, 
                                   filterFun = function(n) n$isLeaf && stringr::str_detect(n$name, "T1w"))
  if (length(leaf_nodes) > 0) {
    t1w_node <- leaf_nodes[[1]]
    
    # Print tree structure first for debugging
    print("======== Tree Structure ========")
    print(mock_proj$bids_tree)
    print("=============================")
    
    # Print the node's attributes
    cat("Node name:", t1w_node$name, "\n")
    cat("Node path:", paste(t1w_node$path, collapse="/"), "\n")
    
    # Print all attributes in a way that handles environments
    cat("All node attributes (excluding complex types):\n")
    # Use attributesAll which is the non-deprecated way
    all_attrs <- t1w_node$attributesAll
    # Filter out complex data types that cause printing issues
    attrs_to_print <- names(all_attrs)[!sapply(all_attrs, function(x) is.environment(x) || is.list(x) || is.function(x) || inherits(x, "Node"))]
    
    for (attr_name in attrs_to_print) {
        cat("  -", attr_name, ":", toString(all_attrs[[attr_name]]), "\n")
    }
  }
  
  # Try direct use of search functions with verbose debugging
  cat("\n======== Direct Search Test ========\n")
  
  t1w_files_raw <- search_files(mock_proj, kind = "T1w", regex = "\\.nii\\.gz$", fmriprep = FALSE, full_path = FALSE)
  cat("Raw T1w search results:", length(t1w_files_raw), "\n")
  if (!is.null(t1w_files_raw)) {
    print(t1w_files_raw)
  }
  
  # Check if our filtering function would catch it
  cat("\nManually testing filter function on first T1w node...\n")
  # Get a T1w node
  if (!is.null(t1w_node)) {
    # Checks equivalent to those in filterNodes in search_files
    cat("Name match regex:", stringr::str_detect(t1w_node$name, "\\.nii\\.gz$"), "\n")
    
    # Check kind filter
    cat("Node has kind=T1w:", (!is.null(t1w_node$kind) && t1w_node$kind == "T1w"), "\n")
    
    # Check fmriprep filter
    is_in_deriv <- FALSE
    node_path_str_parts <- t1w_node$path
    prep_dir_parts <- strsplit(mock_proj$prep_dir, "/")[[1]]
    if (mock_proj$has_fmriprep && 
        length(node_path_str_parts) > (1 + length(prep_dir_parts)) &&
        all(node_path_str_parts[2:(1 + length(prep_dir_parts))] == prep_dir_parts)) {
      is_in_deriv <- TRUE
    }
    cat("Node is in derivatives:", is_in_deriv, "\n")
    
    # Manual mock_key_match equivalent
    filters <- list(kind = "T1w")
    node_attrs <- t1w_node
    
    for (k in names(filters)) {
      cat("Checking filter", k, "=", filters[[k]], "\n")
      cat("  Node has attribute:", k %in% names(node_attrs), "\n")
      if (k %in% names(node_attrs)) {
        cat("  Node value:", node_attrs[[k]], "\n")
        cat("  Values match:", node_attrs[[k]] == filters[[k]], "\n")
      } else {
        cat("  (Key missing)\n")
      }
    }
  }
  
  # Debugging assertions:
  expect_true(length(leaf_nodes) > 0, "No T1w nodes found")
  
  if (length(leaf_nodes) > 0) {
    expect_true(!is.null(t1w_node$relative_path), "T1w node is missing relative_path")
    
    # Check the specific attributes needed by search filters
    attrs_to_check <- c("kind", "sub", "suffix")
    has_needed_attrs <- attrs_to_check %in% names(t1w_node)
    
    if (!all(has_needed_attrs)) {
      missing <- attrs_to_check[!has_needed_attrs]
      warning("Missing attributes: ", paste(missing, collapse=", "))
    }
    
    for (attr in attrs_to_check) {
      # Only try to access attributes we've confirmed exist
      if (attr %in% names(t1w_node)) {
        # Safely check the attribute value
        val <- t1w_node[[attr]]
        expect_true(!is.null(val) && !is.environment(val),
                   paste("Attribute", attr, "is NULL or an environment"))
      }
    }
  }
})

# Add a dedicated inspect_create_mock_bids test to see what bidser::encode is returning
test_that("inspect create_mock_bids internals", {
  skip_on_cran() # Skip on CRAN, this is a debugging test

  # Create a simple test filename to encode
  test_filename <- "sub-01_task-rest_run-01_bold.nii.gz"
  
  # See what bidser::encode returns
  cat("\nTesting bidser::encode on", test_filename, "\n")
  enc <- bidser::encode(test_filename)
  
  # Print out what encode returns
  cat("encode() result fields:", paste(names(enc), collapse=", "), "\n")
  for (field in names(enc)) {
    cat("  - ", field, ": ", toString(enc[[field]]), "\n", sep="")
  }
  
  # Directly test the actual mapping from row in file_structure to node attributes
  cat("\nDemo parsing:\n")
  test_row <- data.frame(
    subid = "01",
    session = NA,
    datatype = "func",
    task = "rest",
    run = "01",
    suffix = "bold.nii.gz",
    fmriprep = FALSE,
    desc = NA,
    space = NA
  )
  
  print(test_row)
  
  # Manually generate filename
  filename <- generate_bids_filename(
    subid = test_row$subid,
    task = test_row$task,
    run = test_row$run,
    suffix = test_row$suffix
  )
  cat("Generated filename:", filename, "\n")
  
  # See what bidser::encode returns for this
  cat("\nEncoding result for generated filename:\n")
  enc <- bidser::encode(filename)
  print(enc)
})
