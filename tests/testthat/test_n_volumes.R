context("n_volumes")

library(testthat)
library(bidser)

test_that("n_volumes reads the 4th NIfTI dimension from paths", {
  skip_if_not_installed("RNifti")

  nii <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(nii), add = TRUE)

  RNifti::writeNifti(array(0, dim = c(2, 2, 2, 5)), nii)

  vols <- n_volumes(nii)
  expect_equal(unname(vols), 5L)
  expect_equal(names(vols), nii)
})

test_that("n_volumes.bids_project returns scan manifest rows when requested", {
  skip_if_not_installed("RNifti")

  root <- tempfile("bidser_nvols_")
  dir.create(file.path(root, "sub-01", "func"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  writeLines(
    '{"Name":"NVols","BIDSVersion":"1.8.0"}',
    file.path(root, "dataset_description.json")
  )
  writeLines("participant_id\nsub-01", file.path(root, "participants.tsv"))

  nii <- file.path(root, "sub-01", "func", "sub-01_task-rest_run-01_bold.nii.gz")
  RNifti::writeNifti(array(0, dim = c(2, 2, 2, 7)), nii)
  nii_norm <- normalizePath(nii, winslash = "/", mustWork = TRUE)

  proj <- bids_project(root, index = "none")
  vols <- n_volumes(proj, subid = "01", task = "rest", run = "01")
  expect_equal(unname(vols), 7L)
  expect_equal(names(vols), nii_norm)

  manifest <- n_volumes(proj, subid = "01", task = "rest", run = "01", as_tibble = TRUE)
  expect_equal(nrow(manifest), 1L)
  expect_equal(manifest$nvols, 7L)
  expect_equal(manifest$subid, "01")
  expect_equal(manifest$run, 1L)
  expect_equal(manifest$.path, nii_norm)
})
