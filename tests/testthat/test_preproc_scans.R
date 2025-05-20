context("preproc_scans")
library(testthat)
library(bidser)

# Helper function to check if the phoneme_stripped dataset with fmriprep data is available
has_phoneme_data <- function() {
  test_path <- system.file("extdata/phoneme_stripped/derivatives/fmriprep", package="bidser")
  return(nchar(test_path) > 0 && dir.exists(test_path))
}

test_that("can extract preprocessed functional files from bids project", {
  skip_if_not(has_phoneme_data(), "Phoneme dataset with fmriprep derivatives not available")
  
  proj <- bids_project(system.file("extdata/phoneme_stripped", package="bidser"), fmriprep=TRUE)
  pscans <- preproc_scans(proj)
  expect_true(length(pscans) > 0)
  # Ensure there are preproc files for the dataset
  expect_true(any(grepl("preproc\\.nii\\.gz$", pscans)))
})

test_that("can extract preprocessed functional files for one subject", {
  skip_if_not(has_phoneme_data(), "Phoneme dataset with fmriprep derivatives not available")
  
  proj <- bids_project(system.file("extdata/phoneme_stripped", package="bidser"), fmriprep=TRUE)
  pscans <- preproc_scans(proj, subid="1001")
  expect_true(length(pscans) > 0)
  # Ensure all files are for subject 1001
  expect_true(all(grepl("sub-1001", pscans)))
})

test_that("can filter preprocessed files by run", {
  skip_if_not(has_phoneme_data(), "Phoneme dataset with fmriprep derivatives not available")
  
  proj <- bids_project(system.file("extdata/phoneme_stripped", package="bidser"), fmriprep=TRUE)
  pscans <- preproc_scans(proj, subid="1001", run="01")
  expect_true(length(pscans) > 0)
  # Ensure all files are for run 01
  expect_true(all(grepl("run-01", pscans)))
})

test_that("can filter preprocessed files by space", {
  skip_if_not(has_phoneme_data(), "Phoneme dataset with fmriprep derivatives not available")
  
  proj <- bids_project(system.file("extdata/phoneme_stripped", package="bidser"), fmriprep=TRUE)
  pscans <- preproc_scans(proj, space="MNI152NLin2009cAsym")
  expect_true(length(pscans) > 0)
  # Ensure all files are in MNI space
  expect_true(all(grepl("space-MNI152NLin2009cAsym", pscans)))
})

test_that("can get relative paths for preprocessed files", {
  skip_if_not(has_phoneme_data(), "Phoneme dataset with fmriprep derivatives not available")
  
  proj <- bids_project(system.file("extdata/phoneme_stripped", package="bidser"), fmriprep=TRUE)
  abs_pscans <- preproc_scans(proj, subid="1001", run="01", full_path=TRUE)
  rel_pscans <- preproc_scans(proj, subid="1001", run="01", full_path=FALSE)
  
  expect_true(length(abs_pscans) > 0)
  expect_true(length(rel_pscans) > 0)
  expect_equal(length(abs_pscans), length(rel_pscans))
  
  # Absolute paths should include the full system path
  expect_true(all(grepl("^/", abs_pscans)))
  
  # Relative paths should start with derivatives/fmriprep
  expect_true(all(grepl("^derivatives/fmriprep", rel_pscans)))
})

test_that("combining multiple filters works correctly", {
  skip_if_not(has_phoneme_data(), "Phoneme dataset with fmriprep derivatives not available")
  
  proj <- bids_project(system.file("extdata/phoneme_stripped", package="bidser"), fmriprep=TRUE)
  pscans <- preproc_scans(proj, 
                         subid="1001", 
                         task="phoneme", 
                         run="01", 
                         space="MNI152NLin2009cAsym")
  
  expect_true(length(pscans) > 0)
  # Ensure all files match all criteria
  expect_true(all(grepl("sub-1001", pscans)))
  expect_true(all(grepl("task-phoneme", pscans)))
  expect_true(all(grepl("run-01", pscans)))
  expect_true(all(grepl("space-MNI152NLin2009cAsym", pscans)))
})

test_that("attempt to find preproc scans with non-existent id returns NULL", {
  skip_if_not(has_phoneme_data(), "Phoneme dataset with fmriprep derivatives not available")

  proj <- bids_project(system.file("extdata/phoneme_stripped", package="bidser"), fmriprep=TRUE)
  pscans <- preproc_scans(proj, subid="nonexistent")
  expect_null(pscans)
})

test_that("returns NULL when project lacks fmriprep data", {
  proj <- bids_project(system.file("extdata/ds001", package="bidser"), fmriprep=FALSE)
  expect_message(pscans <- preproc_scans(proj), "fMRIPrep derivatives not found")
  expect_null(pscans)
})


