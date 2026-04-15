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

test_that("create_preproc_mask ignores JSON sidecars for mask matches", {
  skip_if_not_installed("RNifti")

  tmp <- tempfile("preproc_mask_sidecar_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  readr::write_tsv(
    tibble::tibble(participant_id = "sub-1001"),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "MaskSidecar", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )
  dir.create(file.path(tmp, "sub-1001", "func"), recursive = TRUE)
  file.create(file.path(tmp, "sub-1001", "func",
                         "sub-1001_task-audio_run-01_bold.nii.gz"))

  deriv_root <- file.path(tmp, "derivatives", "fmriprep", "sub-1001", "func")
  dir.create(deriv_root, recursive = TRUE)
  jsonlite::write_json(
    list(Name = "fmriprep", BIDSVersion = "1.8.0", DatasetType = "derivative"),
    file.path(tmp, "derivatives", "fmriprep", "dataset_description.json"),
    auto_unbox = TRUE
  )

  mask_path <- file.path(
    deriv_root,
    "sub-1001_task-audio_run-01_space-MNI152NLin2009cAsym_res-2_desc-brain_mask.nii.gz"
  )
  RNifti::writeNifti(array(1, dim = c(2, 2, 2)), mask_path)
  writeLines("{}", sub("\\.nii\\.gz$", ".json", mask_path))

  proj <- bids_project(tmp, fmriprep = TRUE)
  mask <- create_preproc_mask(proj, subid = "1001")
  expect_true(inherits(mask, "LogicalNeuroVol"))
})
