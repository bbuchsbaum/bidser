context("read_preproc_scans.bids_project")
library(testthat)
library(bidser)

create_read_preproc_fixture <- function() {
  tmp <- tempfile("bidser_read_preproc_")
  dir.create(tmp, recursive = TRUE)

  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01"),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "ReadPreproc", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )

  dir.create(file.path(tmp, "sub-01", "func"), recursive = TRUE)
  file.create(file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-01_bold.nii.gz"))
  file.create(file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-02_bold.nii.gz"))

  deriv_root <- file.path(tmp, "derivatives", "fmriprep", "sub-01", "func")
  dir.create(deriv_root, recursive = TRUE)
  jsonlite::write_json(
    list(
      Name = "fmriprep",
      BIDSVersion = "1.8.0",
      DatasetType = "derivative",
      GeneratedBy = list(list(Name = "fmriprep", Version = "test"))
    ),
    file.path(tmp, "derivatives", "fmriprep", "dataset_description.json"),
    auto_unbox = TRUE
  )

  bold_arr <- array(seq_len(2 * 2 * 2 * 3), dim = c(2, 2, 2, 3))
  mask_arr <- array(1, dim = c(2, 2, 2))

  for (run in c("01", "02")) {
    RNifti::writeNifti(
      bold_arr,
      file.path(
        deriv_root,
        paste0("sub-01_task-rest_run-", run, "_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz")
      )
    )
    RNifti::writeNifti(
      mask_arr,
      file.path(
        deriv_root,
        paste0("sub-01_task-rest_run-", run, "_space-MNI152NLin2009cAsym_desc-brain_mask.nii.gz")
      )
    )
  }

  tmp
}

test_that("read_preproc_scans returns one NeuroVec per matched file", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("neuroim2")

  tmp <- create_read_preproc_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = TRUE, index = "none")
  fnames <- preproc_scans(proj, subid = "01", task = "rest", full_path = TRUE)
  scans <- bidser:::read_preproc_scans.bids_project(proj, subid = "01", task = "rest")

  expect_type(scans, "list")
  expect_length(scans, 2)
  expect_equal(names(scans), fnames)
  expect_true(all(vapply(scans, inherits, logical(1), "NeuroVec")))
})
