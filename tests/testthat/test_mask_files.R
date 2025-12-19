# tests/testthat/test_mask_files.R

library(testthat)
library(tibble)
library(dplyr)
library(bidser)

context("mask_files() Function")

# --- Test Data Setup ---
participants_df <- tibble::tibble(participant_id = c("01", "02"), age = c(25, 30))

# Define file structure with mask files
file_structure_df <- tibble::tribble(
  ~subid, ~session, ~datatype, ~task,   ~run, ~suffix,                      ~fmriprep, ~desc,     ~space,               ~label,
  # Raw data
  "01",   NA,       "anat",    NA,      NA,   "T1w.nii.gz",                 FALSE,     NA,        NA,                   NA,
  "01",   NA,       "func",    "rest",  "01", "bold.nii.gz",                FALSE,     NA,        NA,                   NA,
  "02",   NA,       "anat",    NA,      NA,   "T1w.nii.gz",                 FALSE,     NA,        NA,                   NA,
  # Preprocessed files
  "01",   NA,       "anat",    NA,      NA,   "T1w.nii.gz",                 TRUE,      "preproc", "MNI152NLin2009cAsym", NA,
  # Mask files
  "01",   NA,       "anat",    NA,      NA,   "brainmask.nii.gz",           TRUE,      NA,        "MNI152NLin2009cAsym", NA,
  "01",   NA,       "anat",    NA,      NA,   "brainmask.nii.gz",           TRUE,      NA,        "T1w",                NA,
  "01",   NA,       "anat",    NA,      NA,   "mask.nii.gz",                TRUE,      NA,        "MNI152NLin2009cAsym", "GM",
  "01",   NA,       "anat",    NA,      NA,   "mask.nii.gz",                TRUE,      NA,        "MNI152NLin2009cAsym", "WM",
  "02",   NA,       "anat",    NA,      NA,   "brainmask.nii.gz",           TRUE,      NA,        "T1w",                NA
)

# --- Tests ---
test_that("mask_files returns all mask files", {
  mock_proj <- create_mock_bids(
    project_name = "MaskTest",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep"
  )

  masks <- mask_files(mock_proj, full_path = FALSE)

  expect_type(masks, "character")
  expect_true(length(masks) >= 4) # At least the masks we defined
  expect_true(all(grepl("_(mask|brainmask)\\.", masks)))
})

test_that("mask_files filters by subject", {
  mock_proj <- create_mock_bids(
    project_name = "MaskTest",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep"
  )

  masks_01 <- mask_files(mock_proj, subid = "01", full_path = FALSE)
  masks_02 <- mask_files(mock_proj, subid = "02", full_path = FALSE)

  expect_true(all(grepl("sub-01", masks_01)))
  expect_true(all(grepl("sub-02", masks_02)))
  expect_true(length(masks_01) > length(masks_02)) # Sub 01 has more masks
})

test_that("mask_files filters by space", {
  mock_proj <- create_mock_bids(
    project_name = "MaskTest",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep"
  )

  masks_mni <- mask_files(mock_proj, space = "MNI152NLin2009cAsym", full_path = FALSE)
  masks_t1w <- mask_files(mock_proj, space = "T1w", full_path = FALSE)

  expect_type(masks_mni, "character")
  expect_type(masks_t1w, "character")
  expect_true(all(grepl("space-MNI152NLin2009cAsym", masks_mni)))
  expect_true(all(grepl("space-T1w", masks_t1w)))
})

test_that("mask_files returns NULL for non-existent filters", {
  mock_proj <- create_mock_bids(
    project_name = "MaskTest",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep"
  )

  masks <- mask_files(mock_proj, space = "nonexistent")

  expect_null(masks)
})

test_that("mask_files returns full paths when requested", {
  mock_proj <- create_mock_bids(
    project_name = "MaskTest",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep",
    create_stub = TRUE,
    stub_path = tempdir()
  )

  masks_full <- mask_files(mock_proj, full_path = TRUE)
  masks_rel <- mask_files(mock_proj, full_path = FALSE)

  # Full paths should be longer
  if (!is.null(masks_full) && !is.null(masks_rel)) {
    expect_true(all(nchar(masks_full) >= nchar(masks_rel)))
  }

  # Clean up
  unlink(file.path(tempdir(), "MaskTest"), recursive = TRUE)
})

test_that("mask_files combines subject and space filters", {
  mock_proj <- create_mock_bids(
    project_name = "MaskTest",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep"
  )

  masks <- mask_files(mock_proj, subid = "01", space = "MNI152NLin2009cAsym", full_path = FALSE)

  expect_type(masks, "character")
  expect_true(all(grepl("sub-01", masks)))
  expect_true(all(grepl("space-MNI152NLin2009cAsym", masks)))
})
