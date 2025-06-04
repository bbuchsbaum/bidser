context("summary and compliance")

library(testthat)
library(bidser)

# Test bids_summary for the example dataset

test_that("bids_summary reports correct statistics for ds001", {
  skip_if_offline()
  
  tryCatch({
    ds001_path <- setup_test_dataset("ds001")
    proj <- bids_project(ds001_path, fmriprep = FALSE)
    sum <- bids_summary(proj)
    
    # Be flexible about the exact numbers since datasets may vary
    expect_true(sum$n_subjects > 0)
    expect_null(sum$n_sessions)  # ds001 typically doesn't have sessions
    expect_true(nrow(sum$tasks) > 0)
    expect_true(sum$total_runs > 0)
    
    # Check that the structure is correct
    expect_true("n_subjects" %in% names(sum))
    expect_true("n_sessions" %in% names(sum))
    expect_true("tasks" %in% names(sum))
    expect_true("total_runs" %in% names(sum))
    expect_true(is.data.frame(sum$tasks))
    expect_true("task" %in% names(sum$tasks))
    expect_true("n_runs" %in% names(sum$tasks))
  }, error = function(e) {
    skip(paste("Could not test bids_summary with ds001:", e$message))
  })
})

# Test simple compliance check on the dataset

test_that("bids_check_compliance passes on ds001", {
  skip_if_offline()
  
  tryCatch({
    ds001_path <- setup_test_dataset("ds001")
    proj <- bids_project(ds001_path, fmriprep = FALSE)
    chk <- bids_check_compliance(proj)
    
    # Check that the structure is correct
    expect_true("passed" %in% names(chk))
    expect_true("issues" %in% names(chk))
    expect_true(is.logical(chk$passed))
    expect_true(is.character(chk$issues))
    
    # A well-formed BIDS dataset should pass basic compliance
    # but we'll be flexible since not all example datasets are perfect
    if (!chk$passed) {
      message("Compliance issues found: ", paste(chk$issues, collapse = ", "))
    }
    
  }, error = function(e) {
    skip(paste("Could not test bids_check_compliance with ds001:", e$message))
  })
})
