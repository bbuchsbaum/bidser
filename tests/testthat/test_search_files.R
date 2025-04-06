library(testthat)
library(bidser)

# Load the example BIDS project once for all tests
proj_path <- system.file("extdata/phoneme_stripped", package="bidser")
proj <- bids_project(proj_path)

# Helper to get expected relative paths (stripping the project name prefix)
# e.g., "phoneme_stripped/raw/sub-1001/anat/sub-1001_T1w.json" -> "raw/sub-1001/anat/sub-1001_T1w.json"
get_relative_paths <- function(full_paths, project_name) {
  sub_pattern <- paste0("^", project_name, "/")
  sub("^raw/", "", gsub(sub_pattern, "", full_paths)) # Remove leading raw/ too
}


test_that("search_files finds files by regex", {
  # Find all .tsv files (should be events files)
  event_files_rel <- search_files(proj, regex = "events\\.tsv$", full_path = FALSE)
  expect_true(length(event_files_rel) > 0)
  expect_true(all(grepl("events\\.tsv$", event_files_rel)))
  expect_false(any(grepl(proj$name, event_files_rel))) # Check they are relative
  
  # Find all .nii.gz files (anat and func)
  nifti_files_full <- search_files(proj, regex = "\\.nii\\.gz$", full_path = TRUE)
  expect_true(length(nifti_files_full) > 0)
  expect_true(all(grepl("\\.nii\\.gz$", nifti_files_full)))
  expect_true(all(startsWith(nifti_files_full, proj$path))) # Check they are full paths
  
  # Find files with 'T1w' in the name
  t1w_files <- search_files(proj, regex = "T1w", full_path = FALSE)
  expect_true(length(t1w_files) > 0)
  expect_true(all(grepl("T1w", basename(t1w_files))))
})

test_that("search_files filters by subid", {
  sub1001_files <- search_files(proj, regex = ".*", subid = "1001", full_path = FALSE)
  expect_true(length(sub1001_files) > 0)
  expect_true(all(grepl("sub-1001", sub1001_files)))
  
  # Check against another subject
  sub1002_files <- search_files(proj, regex = ".*", subid = "1002", full_path = FALSE)
  expect_true(length(sub1002_files) > 0)
  expect_true(all(grepl("sub-1002", sub1002_files)))
  
  # Ensure no overlap unless expected (e.g. root files - not applicable here)
  expect_false(any(sub1001_files %in% sub1002_files))
})

test_that("search_files filters by task", {
  # This dataset only has 'phoneme' and 'rest' tasks
  phoneme_files <- search_files(proj, regex = ".*", task = "phoneme", full_path = FALSE)
  expect_true(length(phoneme_files) > 0)
  expect_true(all(grepl("task-phoneme", phoneme_files)))
  
  rest_files <- search_files(proj, regex = ".*", task = "rest", full_path = FALSE)
  expect_true(length(rest_files) > 0)
  expect_true(all(grepl("task-rest", rest_files)))
  
  # No overlap expected
  expect_false(any(phoneme_files %in% rest_files))
})

test_that("search_files filters by run", {
  run01_files <- search_files(proj, regex = ".*", run = "01", full_path = FALSE)
  expect_true(length(run01_files) > 0)
  expect_true(all(grepl("run-01", run01_files)))
  
  run02_files <- search_files(proj, regex = ".*", run = "02", full_path = FALSE)
  expect_true(length(run02_files) > 0)
  expect_true(all(grepl("run-02", run02_files)))
})

test_that("search_files combines filters", {
  sub1001_task_phoneme_run01_bold <- search_files(proj, 
                                                  regex = "bold\\.nii\\.gz$", 
                                                  subid = "1001", 
                                                  task = "phoneme", 
                                                  run = "01", 
                                                  full_path = FALSE)
  expect_equal(length(sub1001_task_phoneme_run01_bold), 1)
  expect_equal(sub1001_task_phoneme_run01_bold, "sub-1001/func/sub-1001_task-phoneme_run-01_bold.nii.gz")

  sub1002_task_phoneme_run05_events <- search_files(proj, 
                                                    regex = "events\\.tsv$", 
                                                    subid = "1002", 
                                                    task = "phoneme", 
                                                    run = "05", 
                                                    full_path = TRUE)
  expect_equal(length(sub1002_task_phoneme_run05_events), 1)
  
})

test_that("search_files handles kind='bold' correctly", {
  # kind="bold" should find the functional nifti files
  bold_files <- search_files(proj, kind = "bold", full_path = FALSE, suffix="nii.gz")
  expect_true(length(bold_files) > 0)
  # Check they are all nifti files from func folders
  expect_true(all(grepl("sub-.*/func/.*_bold\\.nii\\.gz$", bold_files)))
  
  # Combine kind='bold' with other filters
  sub1001_bold_files <- search_files(proj, kind = "bold", subid = "1001", full_path = FALSE)
  expect_true(length(sub1001_bold_files) > 0)
  expect_true(all(grepl("sub-1001/func/.*_bold.*", sub1001_bold_files)))
  
  sub1001_task_phoneme_bold_files <- search_files(proj, kind = "bold", subid = "1001", task = "phoneme", full_path = FALSE)
  expect_true(length(sub1001_task_phoneme_bold_files) > 0)
  expect_true(all(grepl("sub-1001/func/sub-1001_task-phoneme.*_bold.*", sub1001_task_phoneme_bold_files)))
})

test_that("search_files handles no matches", {
  no_match_regex <- search_files(proj, regex = "nonexistent_file_pattern")
  expect_null(no_match_regex)
  
  no_match_filter <- search_files(proj, subid = "9999")
  expect_null(no_match_filter)
  
  no_match_combo <- search_files(proj, task = "nonexistent_task", run = "01")
  expect_null(no_match_combo)
})

# Note: Testing `strict` is difficult with the current structure as attributes 
# are usually present if the key exists in the filename. A more complex mock 
# object might be needed to test strict=FALSE effectively where an attribute is NULL.
# We can test strict=TRUE implicitly (default behavior).

test_that("search_files strict=TRUE works (implicitly)", {
  # This should only return the T1w file for sub-1001, which has kind=T1w implicitly
  t1w_anat_files <- search_files(proj, subid = "1001", type = "anat", kind = "T1w", regex = "T1w\\.nii\\.gz$", strict = TRUE)
  expect_equal(length(t1w_anat_files), 1)
  expect_true(grepl("sub-1001_T1w.nii.gz", t1w_anat_files))
  
  # This should return NULL because T1w files don't have a 'task' attribute
  no_match_strict <- search_files(proj, subid = "1001", task = "phoneme", kind = "T1w", regex = "T1w\\.nii\\.gz$", strict = TRUE)
  expect_null(no_match_strict)
})

# Add tests for derivatives if fmriprep is loaded
proj_fmriprep_path <- system.file("extdata/phoneme_fmriprep", package="bidser")
if (dir.exists(proj_fmriprep_path)) {
  proj_prep <- bids_project(proj_fmriprep_path, fmriprep = TRUE)

  test_that("search_files finds files in derivatives", {
    # Find preprocessed bold files
    prep_bold_files <- search_files(proj_prep, 
                                    regex = "preproc_bold\\.nii\\.gz$", 
                                    kind = "preproc", # Or use desc = "preproc"
                                    full_path = FALSE)
    expect_true(length(prep_bold_files) > 0)
    # Check paths include derivatives dir
    expect_true(all(grepl(paste0("^", proj_prep$prep_dir), prep_bold_files)))
    expect_true(all(grepl("desc-preproc_bold\\.nii\\.gz$", prep_bold_files)))
    
    # Find confounds files for a specific subject
    confounds_sub1001 <- search_files(proj_prep, 
                                      regex = "confounds_timeseries\\.tsv$", 
                                      subid = "1001", 
                                      full_path = FALSE)
    expect_true(length(confounds_sub1001) > 0)
    expect_true(all(grepl(paste0("^", proj_prep$prep_dir, "/sub-1001"), confounds_sub1001)))
    expect_true(all(grepl("desc-confounds_timeseries\\.tsv$", confounds_sub1001)))
  })
} else {
  message("Skipping derivative search tests: extdata/phoneme_fmriprep not found.")
}
