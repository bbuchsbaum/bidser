#' Helper functions for downloading BIDS example datasets
#' 
#' These functions download datasets from the official BIDS examples repository
#' at https://github.com/bids-standard/bids-examples for use in tests.

library(httr)
library(fs)
library(jsonlite)

# Version pinning for reproducibility
BIDS_EXAMPLES_REPO <- "https://github.com/bids-standard/bids-examples"
BIDS_EXAMPLES_VERSION <- "master"  # Could pin to specific tag like "v1.9.0"

# Session-level cache for test performance
.bids_cache <- new.env()

#' Check if we have internet connectivity
#' @return logical
has_internet <- function() {
  tryCatch({
    httr::GET("https://github.com", httr::timeout(5))
    TRUE
  }, error = function(e) {
    FALSE
  })
}

#' Skip test if offline
skip_if_offline <- function() {
  if (!has_internet()) {
    testthat::skip("Internet connection required for BIDS examples")
  }
}

#' Get the base cache directory for BIDS examples
#' @return character path to cache directory
get_bids_cache_dir <- function() {
  cache_dir <- file.path(tempdir(), "bidser_bids_examples", Sys.getpid())
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  return(cache_dir)
}

#' Download and extract BIDS examples repository
#' @param cache_dir Directory to store the downloaded repo
#' @param force_refresh Force re-download even if cached
#' @return Path to the extracted repository
download_bids_repo <- function(cache_dir = NULL, force_refresh = FALSE) {
  if (is.null(cache_dir)) {
    cache_dir <- get_bids_cache_dir()
  }
  
  repo_path <- file.path(cache_dir, "bids-examples-master")
  
  # Check if already cached
  if (dir.exists(repo_path) && !force_refresh) {
    return(repo_path)
  }
  
  # Skip if offline
  skip_if_offline()
  
  message("Downloading BIDS examples repository...")
  
  # Download the repository as a zip file
  zip_url <- paste0(BIDS_EXAMPLES_REPO, "/archive/refs/heads/", BIDS_EXAMPLES_VERSION, ".zip")
  zip_path <- file.path(cache_dir, "bids-examples.zip")
  
  # Create cache directory
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  # Download
  tryCatch({
    response <- httr::GET(zip_url, httr::write_disk(zip_path, overwrite = TRUE))
    httr::stop_for_status(response)
  }, error = function(e) {
    stop("Failed to download BIDS examples: ", e$message)
  })
  
  # Extract
  tryCatch({
    utils::unzip(zip_path, exdir = cache_dir)
    file.remove(zip_path)  # Clean up zip file
  }, error = function(e) {
    stop("Failed to extract BIDS examples: ", e$message)
  })
  
  if (!dir.exists(repo_path)) {
    stop("Expected directory not found after extraction: ", repo_path)
  }
  
  message("BIDS examples downloaded successfully")
  return(repo_path)
}

#' Create minimal participants.tsv file if missing
#' @param dataset_path Path to the dataset
#' @return logical indicating if file was created
create_participants_tsv_if_missing <- function(dataset_path) {
  participants_file <- file.path(dataset_path, "participants.tsv")
  
  # Check if file already exists
  if (file.exists(participants_file)) {
    return(FALSE)
  }
  
  # Find subject directories
  all_dirs <- list.dirs(dataset_path, full.names = FALSE, recursive = FALSE)
  subject_dirs <- all_dirs[grepl("^sub-", all_dirs)]
  
  if (length(subject_dirs) == 0) {
    return(FALSE)
  }
  
  # Extract subject IDs
  subject_ids <- gsub("^sub-", "", subject_dirs)
  
  # Create minimal participants.tsv
  participants_df <- data.frame(
    participant_id = paste0("sub-", subject_ids),
    stringsAsFactors = FALSE
  )
  
  # Write the file
  utils::write.table(participants_df, participants_file, 
                     sep = "\t", row.names = FALSE, quote = FALSE)
  
  message("Created minimal participants.tsv with ", length(subject_ids), " subjects")
  return(TRUE)
}

#' Restructure derivatives-only dataset to expected format
#' @param dataset_path Path to the dataset
#' @param pipeline_name Name of the pipeline (e.g., "fmriprep")
#' @return logical indicating if restructuring was done
restructure_derivatives_dataset <- function(dataset_path, pipeline_name = "fmriprep") {
  # Check if derivatives directory already exists
  derivatives_dir <- file.path(dataset_path, "derivatives", pipeline_name)
  if (dir.exists(derivatives_dir)) {
    return(FALSE)  # Already structured correctly
  }
  
  # Find subject directories in root
  all_dirs <- list.dirs(dataset_path, full.names = FALSE, recursive = FALSE)
  subject_dirs <- all_dirs[grepl("^sub-", all_dirs)]
  
  if (length(subject_dirs) == 0) {
    return(FALSE)  # No subjects to move
  }
  
  # Create derivatives directory structure
  dir.create(derivatives_dir, recursive = TRUE)
  
  # Move subject directories
  for (sub_dir in subject_dirs) {
    old_path <- file.path(dataset_path, sub_dir)
    new_path <- file.path(derivatives_dir, sub_dir)
    
    if (dir.exists(old_path)) {
      file.rename(old_path, new_path)
    }
  }
  
  # Create pipeline dataset_description.json
  pipeline_desc <- list(
    Name = pipeline_name,
    BIDSVersion = "1.4.0",
    PipelineDescription = list(
      Name = pipeline_name,
      Version = "unknown"
    )
  )
  
  desc_file <- file.path(derivatives_dir, "dataset_description.json")
  jsonlite::write_json(pipeline_desc, desc_file, pretty = TRUE, auto_unbox = TRUE)
  
  message("Restructured derivatives dataset for ", pipeline_name)
  return(TRUE)
}

#' Create a mock derivatives dataset compatible with preproc_scans
#' @param base_dataset_name A base dataset to use for raw data structure
#' @param cache_dir Directory to cache the result
#' @return Path to the mock derivatives dataset
create_mock_derivatives_dataset <- function(base_dataset_name = "ds001", cache_dir = NULL) {
  if (is.null(cache_dir)) {
    cache_dir <- get_bids_cache_dir()
  }
  
  mock_path <- file.path(cache_dir, "mock_derivatives")
  
  # Check if already exists
  if (dir.exists(mock_path)) {
    return(mock_path)
  }
  
  # Download base dataset
  base_path <- download_bids_example(base_dataset_name, cache_dir, force_refresh = FALSE)
  
  # Create mock derivatives structure
  dir.create(mock_path, recursive = TRUE)
  
  # Copy basic structure from base dataset
  file.copy(file.path(base_path, "participants.tsv"), mock_path, overwrite = TRUE)
  if (file.exists(file.path(base_path, "dataset_description.json"))) {
    file.copy(file.path(base_path, "dataset_description.json"), mock_path, overwrite = TRUE)
  }
  
  # Create derivatives/fmriprep structure
  fmriprep_dir <- file.path(mock_path, "derivatives", "fmriprep")
  dir.create(fmriprep_dir, recursive = TRUE)
  
  # Create fmriprep dataset_description.json
  fmriprep_desc <- list(
    Name = "fMRIPrep",
    BIDSVersion = "1.4.0",
    PipelineDescription = list(
      Name = "fMRIPrep",
      Version = "20.2.3",
      Description = "Mock fMRIPrep derivatives for testing"
    )
  )
  
  jsonlite::write_json(fmriprep_desc, file.path(fmriprep_dir, "dataset_description.json"), 
                       pretty = TRUE, auto_unbox = TRUE)
  
  # Find subjects in base dataset
  base_subjects <- list.dirs(base_path, full.names = FALSE, recursive = FALSE)
  subjects <- base_subjects[grepl("^sub-", base_subjects)]
  
  # Create mock preprocessed files for each subject
  for (subject in subjects[1:min(3, length(subjects))]) {  # Limit to first 3 subjects
    sub_fmriprep_dir <- file.path(fmriprep_dir, subject)
    func_dir <- file.path(sub_fmriprep_dir, "func")
    dir.create(func_dir, recursive = TRUE)
    
    # Create mock preproc files using proper BIDS naming convention
    # Pattern: sub-XX_task-YY_run-ZZ_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz
    tasks <- c("rest", "task1")
    runs <- c("01", "02")
    
    for (task in tasks) {
      for (run in runs) {
        # Create dummy preproc file with proper BIDS naming
        preproc_filename <- paste0(subject, "_task-", task, "_run-", run, 
                                 "_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz")
        preproc_path <- file.path(func_dir, preproc_filename)
        
        # Create an empty file (just for testing)
        file.create(preproc_path)
        
        # Also create confounds file
        confounds_filename <- paste0(subject, "_task-", task, "_run-", run, "_desc-confounds_timeseries.tsv")
        confounds_path <- file.path(func_dir, confounds_filename)
        file.create(confounds_path)
        
        # Create brain mask file for this run
        mask_filename <- paste0(subject, "_task-", task, "_run-", run, 
                               "_space-MNI152NLin2009cAsym_desc-brain_mask.nii.gz")
        mask_path <- file.path(func_dir, mask_filename)
        file.create(mask_path)
      }
    }
    
    # Create anatomical derivatives too
    anat_dir <- file.path(sub_fmriprep_dir, "anat")
    dir.create(anat_dir, recursive = TRUE)
    
    # Create T1w preproc file with proper naming
    t1w_filename <- paste0(subject, "_space-MNI152NLin2009cAsym_desc-preproc_T1w.nii.gz")
    t1w_path <- file.path(anat_dir, t1w_filename)
    file.create(t1w_path)
    
    # Create brain mask for anatomical
    brain_mask_filename <- paste0(subject, "_space-MNI152NLin2009cAsym_desc-brain_mask.nii.gz")
    brain_mask_path <- file.path(anat_dir, brain_mask_filename)
    file.create(brain_mask_path)
  }
  
  message("Created mock derivatives dataset with proper BIDS naming conventions")
  return(mock_path)
}

#' Download a specific BIDS example dataset
#' @param dataset_name Name of the dataset (e.g., "ds001", "7t_trt")
#' @param cache_dir Directory to cache downloads (default: tempdir)
#' @param force_refresh Force re-download even if cached
#' @return Path to the specific dataset directory
download_bids_example <- function(dataset_name, cache_dir = NULL, force_refresh = FALSE) {
  # Special case for mock derivatives
  if (dataset_name == "mock_derivatives") {
    return(create_mock_derivatives_dataset(cache_dir = cache_dir))
  }
  
  # Check session cache first
  cache_key <- paste0(dataset_name, "_", force_refresh)
  if (exists(cache_key, envir = .bids_cache) && !force_refresh) {
    dataset_path <- .bids_cache[[cache_key]]
    if (dir.exists(dataset_path)) {
      return(dataset_path)
    }
  }
  
  # Download the full repo
  repo_path <- download_bids_repo(cache_dir, force_refresh)
  
  # Check if the specific dataset exists
  dataset_path <- file.path(repo_path, dataset_name)
  if (!dir.exists(dataset_path)) {
    available_datasets <- list.dirs(repo_path, full.names = FALSE, recursive = FALSE)
    available_datasets <- available_datasets[available_datasets != ""]
    stop("Dataset '", dataset_name, "' not found. Available datasets: ", 
         paste(head(available_datasets, 20), collapse = ", "),
         if (length(available_datasets) > 20) "..." else "")
  }
  
  # Create participants.tsv if missing (common for derivatives datasets)
  create_participants_tsv_if_missing(dataset_path)
  
  # Restructure if it's a derivatives-only dataset
  if (grepl("fmriprep", dataset_name, ignore.case = TRUE)) {
    restructure_derivatives_dataset(dataset_path, "fmriprep")
  }
  
  # Cache the result
  .bids_cache[[cache_key]] <- dataset_path
  
  return(dataset_path)
}

#' Setup a test dataset with proper cleanup
#' @param dataset_name Name of the dataset to download
#' @param cleanup Whether to cleanup after test (default: TRUE)
#' @return Path to the dataset
setup_test_dataset <- function(dataset_name, cleanup = TRUE) {
  dataset_path <- download_bids_example(dataset_name)
  
  # Register cleanup if requested
  if (cleanup) {
    # Note: We don't actually delete here since we're using session cache
    # The temp directory will be cleaned up when R session ends
    # But we could add more aggressive cleanup if needed
  }
  
  return(dataset_path)
}

#' List available BIDS example datasets
#' @param cache_dir Directory to check for cached repo
#' @return Character vector of available dataset names
list_bids_examples <- function(cache_dir = NULL) {
  tryCatch({
    repo_path <- download_bids_repo(cache_dir, force_refresh = FALSE)
    available <- list.dirs(repo_path, full.names = FALSE, recursive = FALSE)
    available <- available[available != ""]
    
    # Filter out non-dataset directories
    datasets <- available[!available %in% c(".github", "docs", ".git")]
    
    # Add our mock dataset
    datasets <- c(datasets, "mock_derivatives")
    
    return(sort(datasets))
  }, error = function(e) {
    message("Could not list BIDS examples: ", e$message)
    return(character(0))
  })
}

#' Cleanup BIDS examples cache
#' @param cache_dir Directory to clean (default: all temp cache)
cleanup_bids_cache <- function(cache_dir = NULL) {
  if (is.null(cache_dir)) {
    cache_dir <- file.path(tempdir(), "bidser_bids_examples")
  }
  
  if (dir.exists(cache_dir)) {
    unlink(cache_dir, recursive = TRUE)
    message("BIDS examples cache cleaned")
  }
  
  # Clear session cache
  rm(list = ls(envir = .bids_cache), envir = .bids_cache)
} 