context("preproc_mask")
library(testthat)
library(bidser)

# Helper to check if phoneme_stripped dataset with fmriprep derivatives is available
has_phoneme_data <- function() {
  test_path <- system.file("extdata/phoneme_stripped/derivatives/fmriprep", package="bidser")
  nchar(test_path) > 0 && dir.exists(test_path)
}

# Error if thresh out of range

test_that("create_preproc_mask errors for invalid thresh", {
  skip_if_not(has_phoneme_data(), "Phoneme dataset with fmriprep derivatives not available")

  proj <- bids_project(system.file("extdata/phoneme_stripped", package="bidser"), fmriprep=TRUE)
  expect_error(create_preproc_mask(proj, subid="1001", thresh=1.5), "thresh")
  expect_error(create_preproc_mask(proj, subid="1001", thresh=-0.1), "thresh")
})

test_that("create_preproc_mask locates mask files", {
  skip_if_not(has_phoneme_data(), "Phoneme dataset with fmriprep derivatives not available")

  proj <- bids_project(system.file("extdata/phoneme_stripped", package="bidser"), fmriprep=TRUE)
  mask <- create_preproc_mask(proj, subid="1001")
  expect_true(inherits(mask, "LogicalNeuroVol"))
})

