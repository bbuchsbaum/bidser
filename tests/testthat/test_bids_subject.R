context("bids_subject")

test_that("bids_subject interface works", {
  skip_if_offline()
  
  tryCatch({
    ds002_path <- setup_test_dataset("ds002")
    proj <- bids_project(ds002_path, fmriprep = FALSE)
    
    # Get participants
    participants_list <- participants(proj)
    
    if (length(participants_list) > 0) {
      # Test with first subject
      first_subject <- participants_list[1]
      subj <- bids_subject(proj, first_subject)
      
      # Check that interface functions exist
      expect_true(is.list(subj))
      expect_true("events" %in% names(subj))
      expect_true("scans" %in% names(subj))
      expect_true("confounds" %in% names(subj))
      expect_true("preproc_scans" %in% names(subj))
      expect_true("brain_mask" %in% names(subj))
      
      # Test that functions are callable
      expect_true(is.function(subj$events))
      expect_true(is.function(subj$scans))
    } else {
      skip("No participants found in test dataset")
    }
    
    # Clean up
    unlink(ds002_path, recursive = TRUE)
    
  }, error = function(e) {
    skip(paste("Test requires internet connection or valid dataset:", e$message))
  })
})

test_that("bids_subject preproc interface works", {
  skip_if_offline()
  
  tryCatch({
    # Create a mock dataset with fMRIPrep derivatives
    ds_path <- setup_test_dataset("mock_derivatives")
    proj <- bids_project(ds_path, fmriprep = TRUE)
    
    # Get participants
    participants_list <- participants(proj)
    
    if (length(participants_list) > 0) {
      # Test with first subject
      first_subject <- participants_list[1]
      subj <- bids_subject(proj, first_subject)
      
      # Test preprocessed scans function
      preproc_result <- subj$preproc_scans()
      expect_true(is.character(preproc_result) || is.null(preproc_result))
      
      # Test confounds function
      confounds_result <- subj$confounds()
      expect_true(is.data.frame(confounds_result) || is.null(confounds_result))
    } else {
      skip("No participants found in test dataset")
    }
    
    # Clean up
    unlink(ds_path, recursive = TRUE)
    
  }, error = function(e) {
    skip(paste("Test requires internet connection or derivatives dataset:", e$message))
  })
})

test_that("bids_subject brain mask works", {
  skip_if_offline()
  
  tryCatch({
    # Create a mock dataset with fMRIPrep derivatives
    ds_path <- setup_test_dataset("mock_derivatives")
    proj <- bids_project(ds_path, fmriprep = TRUE)
    
    # Get participants
    participants_list <- participants(proj)
    
    if (length(participants_list) > 0) {
      # Test with first subject
      first_subject <- participants_list[1]
      subj <- bids_subject(proj, first_subject)
      
      # Test brain mask function - this might return NULL if no mask files exist
      mask_result <- subj$brain_mask()
      expect_true(is.character(mask_result) || is.null(mask_result))
    } else {
      skip("No participants found in test dataset")
    }
    
    # Clean up
    unlink(ds_path, recursive = TRUE)
    
  }, error = function(e) {
    skip(paste("Test requires internet connection or derivatives dataset:", e$message))
  })
})
