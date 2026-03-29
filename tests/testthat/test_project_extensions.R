library(testthat)
library(bidser)

create_project_extensions_fixture <- function(include_participants = TRUE) {
  tmp <- tempfile("bidser_ext_")
  dir.create(tmp, recursive = TRUE)

  if (isTRUE(include_participants)) {
    readr::write_tsv(
      tibble::tibble(participant_id = "sub-01"),
      file.path(tmp, "participants.tsv")
    )
  }

  jsonlite::write_json(
    list(Name = "ExtensionsFixture", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )

  dir.create(file.path(tmp, "sub-01", "func"), recursive = TRUE)
  file.create(file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-01_bold.nii.gz"))
  readr::write_tsv(
    tibble::tibble(onset = 0, duration = 1, trial_type = "go"),
    file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-01_events.tsv")
  )

  fmriprep_root <- file.path(tmp, "derivatives", "fmriprep")
  dir.create(file.path(fmriprep_root, "sub-01", "func"), recursive = TRUE)
  jsonlite::write_json(
    list(Name = "fmriprep", BIDSVersion = "1.8.0", DatasetType = "derivative"),
    file.path(fmriprep_root, "dataset_description.json"),
    auto_unbox = TRUE
  )
  file.create(
    file.path(
      fmriprep_root, "sub-01", "func",
      "sub-01_task-rest_run-01_space-MNI_desc-preproc_bold.nii.gz"
    )
  )
  readr::write_tsv(
    tibble::tibble(
      CSF = c(0.1, 0.2, 0.3),
      WhiteMatter = c(0.4, 0.5, 0.6),
      FramewiseDisplacement = c(0.01, 0.02, 0.03)
    ),
    file.path(
      fmriprep_root, "sub-01", "func",
      "sub-01_task-rest_run-01_desc-confounds_timeseries.tsv"
    )
  )

  qsiprep_root <- file.path(tmp, "derivatives", "qsiprep")
  dir.create(file.path(qsiprep_root, "sub-01", "anat"), recursive = TRUE)
  jsonlite::write_json(
    list(Name = "qsiprep", BIDSVersion = "1.8.0", DatasetType = "derivative"),
    file.path(qsiprep_root, "dataset_description.json"),
    auto_unbox = TRUE
  )
  file.create(
    file.path(
      qsiprep_root, "sub-01", "anat",
      "sub-01_space-MNI_desc-preproc_T1w.nii.gz"
    )
  )

  tmp
}

test_that("bids_project can infer participants when strict_participants is FALSE", {
  fixture <- create_project_extensions_fixture(include_participants = FALSE)
  on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

  expect_error(bids_project(fixture), "participants.tsv is missing")

  expect_warning(
    proj <- bids_project(fixture, strict_participants = FALSE, derivatives = "auto"),
    "inferring participants"
  )

  expect_equal(proj$participants_source, "filesystem")
  expect_equal(participants(proj), "01")
})

test_that("auto derivative discovery preserves legacy fmriprep helpers", {
  fixture <- create_project_extensions_fixture()
  on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture, derivatives = "auto")
  pipes <- derivative_pipelines(proj)

  expect_true(all(c("fmriprep", "qsiprep") %in% pipes$pipeline))
  expect_true(proj$has_derivatives)
  expect_true(proj$has_fmriprep)
  expect_equal(proj$prep_dir, "derivatives/fmriprep")

  preproc <- preproc_scans(proj, subid = "01", task = "rest", full_path = FALSE)
  expect_length(preproc, 1)
  expect_true(startsWith(preproc, "derivatives/fmriprep/"))
})

test_that("query_files supports pipeline filters, tibble return, and persisted indexes", {
  fixture <- create_project_extensions_fixture()
  on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture, derivatives = "auto")
  idx <- bids_index(proj, rebuild = TRUE, persist = TRUE)

  expect_true(file.exists(proj$index_path))
  expect_true(nrow(idx) > 0)

  deriv_tbl <- query_files(
    proj,
    regex = "bold\\.nii\\.gz$",
    scope = "derivatives",
    pipeline = "fmriprep",
    return = "tibble",
    use_index = "auto"
  )

  expect_s3_class(deriv_tbl, "tbl_df")
  expect_equal(nrow(deriv_tbl), 1)
  expect_equal(deriv_tbl$pipeline, "fmriprep")
  expect_equal(deriv_tbl$scope, "derivatives")
  expect_true(grepl("desc-preproc_bold\\.nii\\.gz$", deriv_tbl$path))
})

test_that("variables_table and bids_report_data provide run-level summaries", {
  fixture <- create_project_extensions_fixture()
  on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture, derivatives = "auto")
  vars <- variables_table(proj, scope = "all", pipeline = "fmriprep")

  expect_equal(nrow(vars), 1)
  expect_equal(vars$.subid, "01")
  expect_equal(vars$.task, "rest")
  expect_equal(vars$.run, "01")
  expect_equal(vars$n_scans, 1)
  expect_equal(vars$n_events, 1)
  expect_equal(vars$n_confound_rows, 3)
  expect_true(is.list(vars$scans))
  expect_true(is.list(vars$events))
  expect_true(is.list(vars$confounds))

  report <- bids_report_data(proj, scope = "all", pipeline = "fmriprep")
  expect_equal(report$project$participants_source, "file")
  expect_true(nrow(report$pipelines) >= 2)
  expect_equal(nrow(report$run_coverage), 1)
  expect_equal(report$run_coverage$n_confound_rows, 3)
})
