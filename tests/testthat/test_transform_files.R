# tests/testthat/test_transform_files.R

library(testthat)
library(tibble)
library(dplyr)
library(bidser)

context("transform_files() Function")

# --- Test Data Setup ---
participants_df <- tibble::tibble(participant_id = c("01", "02"), age = c(25, 30))

# Define file structure with transform files
file_structure_df <- tibble::tribble(
  ~subid, ~session, ~datatype, ~task,   ~run, ~suffix,                                    ~fmriprep, ~desc,     ~space,    ~from,      ~to,
  # Raw data

  "01",   NA,       "anat",    NA,      NA,   "T1w.nii.gz",                               FALSE,     NA,        NA,        NA,         NA,
  "01",   NA,       "func",    "rest",  "01", "bold.nii.gz",                              FALSE,     NA,        NA,        NA,         NA,
  "02",   NA,       "anat",    NA,      NA,   "T1w.nii.gz",                               FALSE,     NA,        NA,        NA,         NA,
  # Preprocessed derivatives
  "01",   NA,       "anat",    NA,      NA,   "T1w.nii.gz",                               TRUE,      "preproc", "MNI152NLin2009cAsym", NA, NA,
  "01",   NA,       "func",    "rest",  "01", "bold.nii.gz",                              TRUE,      "preproc", "MNI152NLin2009cAsym", NA, NA,
  # Transform files
  "01",   NA,       "anat",    NA,      NA,   "xfm.h5",                                   TRUE,      NA,        NA,        "T1w",      "MNI152NLin2009cAsym",
  "01",   NA,       "anat",    NA,      NA,   "xfm.txt",                                  TRUE,      NA,        NA,        "fsnative", "T1w",
  "01",   NA,       "anat",    NA,      NA,   "warp.h5",                                  TRUE,      NA,        NA,        "MNI152NLin2009cAsym", "T1w",
  "02",   NA,       "anat",    NA,      NA,   "xfm.h5",                                   TRUE,      NA,        NA,        "T1w",      "MNI152NLin2009cAsym",
  "02",   NA,       "anat",    NA,      NA,   "affine.txt",                               TRUE,      NA,        NA,        "boldref",  "T1w"
)

# --- Tests ---
test_that("transform_files returns all transform files", {
  mock_proj <- create_mock_bids(
    project_name = "TransformTest",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep"
  )

  xfms <- transform_files(mock_proj, full_path = FALSE)

  expect_type(xfms, "character")
  expect_true(length(xfms) >= 4) # At least the transforms we defined
  expect_true(all(grepl("\\.(h5|txt)$", xfms)))
})

test_that("transform_files filters by subject", {
  mock_proj <- create_mock_bids(
    project_name = "TransformTest",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep"
  )

  xfms_01 <- transform_files(mock_proj, subid = "01", full_path = FALSE)
  xfms_02 <- transform_files(mock_proj, subid = "02", full_path = FALSE)

  expect_true(all(grepl("sub-01", xfms_01)))
  expect_true(all(grepl("sub-02", xfms_02)))
})

test_that("transform_files filters by from space", {
  mock_proj <- create_mock_bids(
    project_name = "TransformTest",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep"
  )

  xfms_t1w <- transform_files(mock_proj, from = "T1w", full_path = FALSE)

  expect_type(xfms_t1w, "character")
  expect_true(all(grepl("from-T1w", xfms_t1w)))
})

test_that("transform_files filters by to space", {
  mock_proj <- create_mock_bids(
    project_name = "TransformTest",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep"
  )

  xfms_mni <- transform_files(mock_proj, to = "MNI152NLin2009cAsym", full_path = FALSE)

  expect_type(xfms_mni, "character")
  expect_true(all(grepl("to-MNI152NLin2009cAsym", xfms_mni)))
})

test_that("transform_files filters by kind", {
  mock_proj <- create_mock_bids(
    project_name = "TransformTest",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep"
  )

  xfms_h5 <- transform_files(mock_proj, kind = "xfm", full_path = FALSE)

  expect_type(xfms_h5, "character")
  expect_true(all(grepl("_xfm\\.", xfms_h5)))
})

test_that("transform_files returns NULL for non-existent filters", {
  mock_proj <- create_mock_bids(
    project_name = "TransformTest",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep"
  )

  xfms <- transform_files(mock_proj, from = "nonexistent")

  expect_null(xfms)
})

test_that("transform_files returns full paths when requested", {
  mock_proj <- create_mock_bids(
    project_name = "TransformTest",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep",
    create_stub = TRUE,
    stub_path = tempdir()
  )

  xfms_full <- transform_files(mock_proj, full_path = TRUE)
  xfms_rel <- transform_files(mock_proj, full_path = FALSE)

  # Full paths should be longer
  if (!is.null(xfms_full) && !is.null(xfms_rel)) {
    expect_true(all(nchar(xfms_full) >= nchar(xfms_rel)))
  }

  # Clean up
  unlink(file.path(tempdir(), "TransformTest"), recursive = TRUE)
})
