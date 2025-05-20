context("bids_subject interface")
library(testthat)
library(bidser)

test_that("bids_subject exposes subject-level helpers", {
  proj <- bids_project(system.file("extdata/ds001", package="bidser"), fmriprep=FALSE)
  subj <- bids_subject(proj, "01")
  expect_true(is.list(subj))
  expect_true(all(c("events", "scans", "confounds", "preproc_scans", "brain_mask") %in% names(subj)))
  ev <- subj$events()
  sc <- subj$scans()
  expect_equal(nrow(ev), 3)
  expect_equal(length(sc), 3)
})

test_that("bids_subject works with fmriprep data", {
  test_path <- system.file("extdata/phoneme_stripped/derivatives/fmriprep", package="bidser")
  skip_if(!(nchar(test_path) > 0 && dir.exists(test_path)), "Phoneme dataset with fmriprep derivatives not available")
  proj <- bids_project(system.file("extdata/phoneme_stripped", package="bidser"), fmriprep=TRUE)
  subj <- bids_subject(proj, "1001")
  pscans <- subj$preproc_scans()
  expect_true(length(pscans) > 0)
  expect_true(all(grepl("sub-1001", pscans)))
})

test_that("bids_subject can return a brain mask", {
  test_path <- system.file("extdata/phoneme_stripped/derivatives/fmriprep", package="bidser")
  skip_if(!(nchar(test_path) > 0 && dir.exists(test_path)), "Phoneme dataset with fmriprep derivatives not available")
  proj <- bids_project(system.file("extdata/phoneme_stripped", package="bidser"), fmriprep=TRUE)
  subj <- bids_subject(proj, "1001")
  mask <- subj$brain_mask()
  expect_true(inherits(mask, "LogicalNeuroVol"))
})
