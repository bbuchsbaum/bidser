context("func_scans")
library(testthat)
library(bidser)

test_that("can extract functional files from bids project", {
  proj <- bids_project(system.file("extdata/ds001", package="bidser"), fmriprep=FALSE)
  fscans <- func_scans(proj)
  expect_equal(length(fscans), 48)
})

test_that("can extract functional files for one subject from bids project", {
  proj <- bids_project(system.file("extdata/ds001", package="bidser"), fmriprep=FALSE)
  fscans <- func_scans(proj, subid="01")
  expect_equal(length(fscans), 3)
})

test_that("attempt to find func_scan with non-existent id should return NULL", {
  proj <- bids_project(system.file("extdata/ds001", package="bidser"), fmriprep=FALSE)
  fscans <- func_scans(proj, subid="junk")
  expect_null(fscans)
})
