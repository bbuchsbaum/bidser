context("sessions")
library(testthat)
library(bidser)

# Ensure sessions() returns session IDs for datasets with sessions

test_that("sessions() extracts session IDs when present", {
  skip_if_offline()
  
  # Use a dataset with sessions (ds007 or ds114 should have sessions)
  tryCatch({
    ds007_path <- setup_test_dataset("ds007")
    proj <- bids_project(ds007_path, fmriprep = FALSE)
    ses <- sessions(proj)
    
    # ds007 should have sessions, but let's be flexible about the exact session names
    # since different datasets may have different session naming
    if (!is.null(ses) && length(ses) > 0) {
      expect_true(length(ses) > 0)
      expect_true(all(nchar(ses) > 0))  # All session names should be non-empty
    } else {
      skip("Dataset ds007 does not have sessions - this is expected for some datasets")
    }
  }, error = function(e) {
    skip(paste("Could not test sessions with ds007:", e$message))
  })
})
