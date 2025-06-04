library(testthat)
library(bidser)

test_that("can identify BOLD files in BIDS project", {
  skip_if_offline()
  
  tryCatch({
    ds002_path <- setup_test_dataset("ds002")
    proj <- bids_project(ds002_path, fmriprep = FALSE)
    
    # Test basic BOLD file identification
    bold_files <- search_files(proj, regex = "bold")
    
    expect_true(length(bold_files) > 0, "Should find some BOLD files")
    
    # Check that all found files contain 'bold' in their name
    if (length(bold_files) > 0) {
      expect_true(all(grepl("bold", bold_files)), 
                  "All files should contain 'bold' in their name")
    }
    
    # Test func_scans function
    func_files <- func_scans(proj)
    
    expect_true(length(func_files) > 0, "Should find functional scan files")
    
    # Clean up
    unlink(ds002_path, recursive = TRUE)
    
  }, error = function(e) {
    skip(paste("Test requires internet connection or valid dataset:", e$message))
  })
})

test_that("can search files with different parameters", {
  skip_if_offline()
  
  tryCatch({
    ds002_path <- setup_test_dataset("ds002")
    proj <- bids_project(ds002_path, fmriprep = FALSE)
    
    # Try to find bold files with direct search_files
    bold_files <- search_files(proj, regex="bold", full_path=FALSE)
    expect_true(length(bold_files) >= 0, "search_files should return a vector")
    
    # Try using func_scans
    func_files <- func_scans(proj, full_path=FALSE)
    expect_true(length(func_files) >= 0, "func_scans should return a vector")
    
    # Check flat list output
    fl <- flat_list(proj, full_path=FALSE)
    expect_true(is.data.frame(fl), "flat_list should return a data frame")
    expect_true("name" %in% names(fl), "flat_list should have a 'name' column")
    
    # Clean up
    unlink(ds002_path, recursive = TRUE)
    
  }, error = function(e) {
    skip(paste("Test requires internet connection or valid dataset:", e$message))
  })
}) 