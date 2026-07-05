library(testthat)
library(bidser)

make_dwi_project <- function() {
  tmp <- tempfile("bidser_dwi_")
  dir.create(file.path(tmp, "sub-01", "dwi"), recursive = TRUE)
  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01"),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "DwiFixture", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )

  dwi_dir <- file.path(tmp, "sub-01", "dwi")
  invisible(file.create(file.path(dwi_dir, "sub-01_run-01_dwi.nii.gz")))
  invisible(file.create(file.path(dwi_dir, "sub-01_run-01_dwi.bval")))
  invisible(file.create(file.path(dwi_dir, "sub-01_run-01_dwi.bvec")))
  tmp
}

test_that("bids_project indexes raw DWI files by default", {
  fixture <- make_dwi_project()
  on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture, derivatives = "none")
  hits <- query_files(
    proj,
    datatype = "dwi",
    kind = "dwi",
    match_mode = "exact",
    scope = "raw",
    return = "tibble"
  )

  expect_equal(nrow(hits), 3L)
  expect_equal(unique(hits$type), "dwi")
  expect_equal(unique(hits$datatype), "dwi")
  expect_setequal(hits$extension, c(".nii.gz", ".bval", ".bvec"))
})
