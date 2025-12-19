# tests/testthat/test_surface_files.R

library(testthat)
library(tibble)
library(dplyr)
library(bidser)

context("surface_files() Function")

# --- Test Data Setup ---
participants_df <- tibble::tibble(participant_id = c("01", "02"), age = c(25, 30))

# Define file structure with surface files
file_structure_df <- tibble::tribble(
  ~subid, ~session, ~datatype, ~task,   ~run, ~suffix,                      ~fmriprep, ~desc,     ~space,
  # Raw data
  "01",   NA,       "anat",    NA,      NA,   "T1w.nii.gz",                 FALSE,     NA,        NA,
  "01",   NA,       "func",    "rest",  "01", "bold.nii.gz",                FALSE,     NA,        NA,
  "02",   NA,       "anat",    NA,      NA,   "T1w.nii.gz",                 FALSE,     NA,        NA,
  # Surface files - fsnative space
  "01",   NA,       "anat",    NA,      NA,   "pial.L.surf.gii",            TRUE,      NA,        "fsnative",
  "01",   NA,       "anat",    NA,      NA,   "pial.R.surf.gii",            TRUE,      NA,        "fsnative",
  "01",   NA,       "anat",    NA,      NA,   "white.L.surf.gii",           TRUE,      NA,        "fsnative",
  "01",   NA,       "anat",    NA,      NA,   "white.R.surf.gii",           TRUE,      NA,        "fsnative",
  "01",   NA,       "anat",    NA,      NA,   "inflated.L.surf.gii",        TRUE,      NA,        "fsnative",
  "01",   NA,       "anat",    NA,      NA,   "inflated.R.surf.gii",        TRUE,      NA,        "fsnative",
  "01",   NA,       "anat",    NA,      NA,   "midthickness.L.surf.gii",    TRUE,      NA,        "fsnative",
  "01",   NA,       "anat",    NA,      NA,   "midthickness.R.surf.gii",    TRUE,      NA,        "fsnative",
  # Surface files - fsaverage space
  "01",   NA,       "anat",    NA,      NA,   "pial.L.surf.gii",            TRUE,      NA,        "fsaverage",
  "01",   NA,       "anat",    NA,      NA,   "pial.R.surf.gii",            TRUE,      NA,        "fsaverage",
  # Subject 02 surfaces
  "02",   NA,       "anat",    NA,      NA,   "pial.L.surf.gii",            TRUE,      NA,        "fsnative",
  "02",   NA,       "anat",    NA,      NA,   "pial.R.surf.gii",            TRUE,      NA,        "fsnative"
)

# --- Tests ---
test_that("surface_files returns all surface files", {
  mock_proj <- create_mock_bids(
    project_name = "SurfaceTest",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep"
  )

  surfs <- surface_files(mock_proj, full_path = FALSE)

  expect_type(surfs, "character")
  expect_true(length(surfs) >= 10) # At least the surfaces we defined
  expect_true(all(grepl("\\.surf\\.gii$", surfs)))
})

test_that("surface_files filters by subject", {
  mock_proj <- create_mock_bids(
    project_name = "SurfaceTest",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep"
  )

  surfs_01 <- surface_files(mock_proj, subid = "01", full_path = FALSE)
  surfs_02 <- surface_files(mock_proj, subid = "02", full_path = FALSE)

  expect_true(all(grepl("sub-01", surfs_01)))
  expect_true(all(grepl("sub-02", surfs_02)))
  expect_true(length(surfs_01) > length(surfs_02)) # Sub 01 has more surfaces
})

test_that("surface_files filters by hemisphere", {
  mock_proj <- create_mock_bids(
    project_name = "SurfaceTest",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep"
  )

  surfs_left <- surface_files(mock_proj, hemi = "L", full_path = FALSE)
  surfs_right <- surface_files(mock_proj, hemi = "R", full_path = FALSE)

  expect_type(surfs_left, "character")
  expect_type(surfs_right, "character")
  expect_true(all(grepl("\\.L\\.surf\\.gii$", surfs_left)))
  expect_true(all(grepl("\\.R\\.surf\\.gii$", surfs_right)))
})

test_that("surface_files filters by surface type", {
  mock_proj <- create_mock_bids(
    project_name = "SurfaceTest",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep"
  )

  surfs_pial <- surface_files(mock_proj, surf_type = "pial", full_path = FALSE)
  surfs_white <- surface_files(mock_proj, surf_type = "white", full_path = FALSE)

  expect_type(surfs_pial, "character")
  expect_type(surfs_white, "character")
  expect_true(all(grepl("_pial\\.[LR]\\.surf\\.gii$", surfs_pial)))
  expect_true(all(grepl("_white\\.[LR]\\.surf\\.gii$", surfs_white)))
})

test_that("surface_files filters by space", {
  mock_proj <- create_mock_bids(
    project_name = "SurfaceTest",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep"
  )

  surfs_fsnative <- surface_files(mock_proj, space = "fsnative", full_path = FALSE)
  surfs_fsaverage <- surface_files(mock_proj, space = "fsaverage", full_path = FALSE)

  expect_type(surfs_fsnative, "character")
  expect_type(surfs_fsaverage, "character")
  expect_true(all(grepl("space-fsnative", surfs_fsnative)))
  expect_true(all(grepl("space-fsaverage", surfs_fsaverage)))
})

test_that("surface_files combines multiple filters", {
  mock_proj <- create_mock_bids(
    project_name = "SurfaceTest",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep"
  )

  surfs <- surface_files(mock_proj, subid = "01", hemi = "L", surf_type = "pial",
                         space = "fsnative", full_path = FALSE)

  expect_type(surfs, "character")
  expect_equal(length(surfs), 1)
  expect_true(grepl("sub-01.*space-fsnative.*_pial\\.L\\.surf\\.gii$", surfs))
})

test_that("surface_files returns NULL for non-existent filters", {
  mock_proj <- create_mock_bids(
    project_name = "SurfaceTest",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep"
  )

  surfs <- surface_files(mock_proj, surf_type = "nonexistent")

  expect_null(surfs)
})

test_that("surface_files handles case-insensitive hemisphere", {
  mock_proj <- create_mock_bids(
    project_name = "SurfaceTest",
    participants = participants_df,
    file_structure = file_structure_df,
    prep_dir = "derivatives/fmriprep"
  )

  surfs_upper <- surface_files(mock_proj, hemi = "L", full_path = FALSE)
  surfs_lower <- surface_files(mock_proj, hemi = "l", full_path = FALSE)

  expect_equal(surfs_upper, surfs_lower)
})
