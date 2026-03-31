library(testthat)
library(bidser)

create_index_extensions_fixture <- function() {
  tmp <- tempfile("bidser_idx_ext_")
  dir.create(tmp, recursive = TRUE)

  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01"),
    file.path(tmp, "participants.tsv")
  )

  jsonlite::write_json(
    list(Name = "IndexFixture", BIDSVersion = "1.8.0"),
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

  tmp
}

create_index_inheritance_fixture <- function() {
  tmp <- tempfile("bidser_idx_inherit_")
  dir.create(tmp, recursive = TRUE)
  dir.create(file.path(tmp, "sub-01", "func"), recursive = TRUE)

  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01"),
    file.path(tmp, "participants.tsv")
  )

  jsonlite::write_json(
    list(Name = "inherit-fixture", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )

  run1_bold <- file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-01_bold.nii.gz")
  file.create(run1_bold)

  jsonlite::write_json(
    list(Manufacturer = "RootVendor", RepetitionTime = 2.5),
    file.path(tmp, "task-rest_bold.json"),
    auto_unbox = TRUE
  )

  jsonlite::write_json(
    list(RepetitionTime = 2.0, EchoTime = 0.03, RunLevel = TRUE),
    file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-01_bold.json"),
    auto_unbox = TRUE
  )

  list(path = tmp, run1 = run1_bold)
}

test_that("data.table-backed index state preserves query parity", {
  fixture <- create_index_extensions_fixture()
  on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture, derivatives = "auto")
  idx <- bids_index(proj, rebuild = TRUE, persist = TRUE)
  state <- bidser:::.bidser_load_cached_index_state(proj, refresh = FALSE, persist = FALSE)

  expect_s3_class(idx, "tbl_df")
  expect_true(data.table::is.data.table(state$manifest))
  expect_true(data.table::is.data.table(state$sidecars))

  exact_indexed <- query_files(
    proj,
    regex = "bold\\.nii\\.gz$",
    subid = "01",
    task = "rest",
    match_mode = "exact",
    scope = "raw",
    use_index = "auto",
    return = "tibble"
  )
  exact_tree <- query_files(
    proj,
    regex = "bold\\.nii\\.gz$",
    subid = "01",
    task = "rest",
    match_mode = "exact",
    scope = "raw",
    use_index = "never",
    return = "tibble"
  )
  expect_equal(exact_indexed, exact_tree)

  regex_indexed <- query_files(
    proj,
    regex = "\\.nii\\.gz$",
    task = "re.*",
    scope = "all",
    use_index = "auto",
    return = "tibble"
  )
  regex_tree <- query_files(
    proj,
    regex = "\\.nii\\.gz$",
    task = "re.*",
    scope = "all",
    use_index = "never",
    return = "tibble"
  )
  expect_equal(regex_indexed, regex_tree)
})

test_that("cached metadata is reused and invalidated when sidecars change", {
  fixture <- create_index_inheritance_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture$path)
  first <- get_metadata(proj, fixture$run1, inherit = TRUE)
  state1 <- bidser:::.bidser_get_session_index_state(proj)

  expect_equal(first$RepetitionTime, 2.0)
  expect_true(nrow(state1$resolved_meta) >= 1)

  second <- get_metadata(proj, fixture$run1, inherit = TRUE)
  state2 <- bidser:::.bidser_get_session_index_state(proj)
  expect_equal(second, first)
  expect_equal(nrow(state2$resolved_meta), nrow(state1$resolved_meta))

  Sys.sleep(1)
  jsonlite::write_json(
    list(RepetitionTime = 1.8, EchoTime = 0.03, RunLevel = TRUE),
    file.path(fixture$path, "sub-01", "func", "sub-01_task-rest_run-01_bold.json"),
    auto_unbox = TRUE
  )

  proj_refresh <- bids_project(fixture$path)
  refreshed <- get_metadata(proj_refresh, fixture$run1, inherit = TRUE)
  state_refresh <- bidser:::.bidser_get_session_index_state(proj_refresh)

  expect_equal(refreshed$RepetitionTime, 1.8)
  expect_true(nrow(state_refresh$resolved_meta) >= 1)
})
