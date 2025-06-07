context("Sidecar utilities")

library(testthat)
library(bidser)
library(tibble)

# create small mock dataset with JSON sidecar

test_that("read_sidecar and get_repetition_time work", {
  tmpdir <- tempfile("sidecar_proj_")
  on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)

  parts <- tibble(participant_id = "01")

  fs <- tibble::tribble(
    ~subid, ~datatype, ~task, ~run, ~suffix,       ~fmriprep,
    "01",   "func",    "rest", "01", "bold.nii.gz", FALSE,
    "01",   "func",    "rest", "01", "bold.json",   FALSE
  )

  mock_proj <- create_mock_bids(
    project_name = "SidecarProj",
    participants = parts,
    file_structure = fs,
    create_stub = TRUE,
    stub_path = tmpdir
  )

  json_name <- bidser:::generate_bids_filename(subid="01", task="rest", run="01", suffix="bold.json")
  json_path <- file.path(tmpdir, "sub-01", "func", json_name)
  jsonlite::write_json(list(RepetitionTime = 2.0), json_path, auto_unbox = TRUE)

  proj <- bids_project(tmpdir)

  meta <- read_sidecar(proj)
  expect_s3_class(meta, "tbl_df")
  expect_equal(nrow(meta), 1)
  expect_equal(meta$RepetitionTime, 2.0)
  expect_true(grepl("sub-01_task-rest_run-01_bold.json$", meta$file))

  tr <- get_repetition_time(proj, subid="01", task="rest", run="01")
  expect_equal(tr, 2.0)
})

