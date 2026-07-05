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
    list(
      Name = "fmriprep",
      BIDSVersion = "1.8.0",
      DatasetType = "derivative",
      GeneratedBy = list(list(Name = "fmriprep"))
    ),
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

test_that("fast tree leaf extraction matches generic search surface", {
  fixture <- create_index_extensions_fixture()
  on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture, derivatives = "auto", index = "none")

  fast_paths <- sort(bidser:::.bidser_list_indexed_paths(proj))
  generic_paths <- sort(search_files(proj, regex = ".*", full_path = FALSE, strict = FALSE))

  expect_equal(fast_paths, generic_paths)
  expect_equal(nrow(proj$tbl), length(generic_paths))
  expect_setequal(
    names(proj$tbl),
    c("name", "type", "subid", "session", "task", "run", "modality", "suffix", "desc", "space")
  )
  expect_true(all(basename(generic_paths) %in% proj$tbl$name))
})

test_that("tree-derived manifest rows match path-derived manifest rows", {
  fixture <- create_index_extensions_fixture()
  on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture, derivatives = "auto", index = "none")
  paths <- bidser:::.bidser_list_indexed_paths(proj)

  path_rows <- bidser:::.bidser_finalize_manifest_dt(
    bidser:::.bidser_index_rows_from_paths(proj, paths)
  )
  tree_rows <- bidser:::.bidser_finalize_manifest_dt(
    bidser:::.bidser_index_rows_from_tree(proj)
  )

  data.table::setorder(path_rows, path)
  data.table::setorder(tree_rows, path)
  expect_equal(as.data.frame(tree_rows), as.data.frame(path_rows), ignore_attr = TRUE)
})

test_that("indexed full-path queries do not mutate the cached manifest", {
  fixture <- create_index_extensions_fixture()
  on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture, derivatives = "auto")
  before <- bidser:::.bidser_get_session_index_state(proj)$manifest$path

  full_hits <- query_files(
    proj,
    regex = "bold[.]nii[.]gz$",
    scope = "all",
    full_path = TRUE
  )
  after <- bidser:::.bidser_get_session_index_state(proj)$manifest$path

  expect_true(all(grepl(paste0("^", normalizePath(fixture)), full_hits)))
  expect_equal(after, before)
  expect_false(any(grepl(paste0("^", normalizePath(fixture)), after)))
})

test_that("index persistence failures warn with context and keep session cache", {
  fixture <- create_index_extensions_fixture()
  on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

  bad_index_path <- file.path(fixture, "missing-index-dir", "bidser-index.rds")
  proj <- bids_project(
    fixture,
    derivatives = "auto",
    index = "none",
    index_path = bad_index_path
  )

  expect_warning(
    idx <- bids_index(proj, rebuild = TRUE, persist = TRUE),
    regexp = paste(
      "Could not write the bidser file index cache",
      "Index path:",
      "Reason:",
      "index = \"none\"",
      sep = ".*"
    )
  )

  expect_true(nrow(idx) > 0)
  expect_false(file.exists(bad_index_path))
  expect_false(is.null(bidser:::.bidser_load_cached_index_state(
    proj,
    refresh = FALSE,
    persist = FALSE
  )))
})

test_that("query_files is a snapshot by default and refresh=TRUE re-scans", {
  fixture <- create_index_extensions_fixture()
  on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture, derivatives = "auto")

  bold <- file.path(
    fixture, "sub-01", "func", "sub-01_task-rest_run-01_bold.nii.gz"
  )
  before <- query_files(proj, regex = "bold\\.nii\\.gz$", task = "rest", scope = "raw")
  expect_equal(length(before), 1L)

  # Remove the raw run after construction.
  file.remove(bold)

  # Default query is a snapshot: it still reports the file known at construction.
  snapshot <- query_files(proj, regex = "bold\\.nii\\.gz$", task = "rest", scope = "raw")
  expect_equal(length(snapshot), 1L)

  # refresh = TRUE re-stats known files and drops the one removed from disk.
  refreshed <- query_files(
    proj, regex = "bold\\.nii\\.gz$", task = "rest", scope = "raw",
    refresh = TRUE
  )
  expect_equal(length(refreshed), 0L)

  # The refreshed state is retained in this project's session cache, so later
  # default queries do not resurrect the construction-time snapshot.
  after_refresh <- query_files(
    proj, regex = "bold\\.nii\\.gz$", task = "rest", scope = "raw"
  )
  expect_equal(length(after_refresh), 0L)
})

test_that("query_files uses the project's own snapshot, not a path-shared cache", {
  # Regression: two project objects on the same path must not share live query
  # state. A second project built with a narrower scope must not corrupt the
  # first project's query results.
  fixture <- create_index_extensions_fixture()
  on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

  proj_full <- bids_project(fixture, derivatives = "auto")
  # Constructing a derivatives-free project on the same path persists a
  # narrower manifest to the same RDS path.
  proj_none <- bids_project(fixture, derivatives = "none")
  invisible(proj_none)

  # proj_full must still see its derivative files.
  deriv_hits <- query_files(
    proj_full, regex = "bold\\.nii\\.gz$", scope = "derivatives"
  )
  expect_true(length(deriv_hits) >= 1L)
  expect_true(all(grepl("desc-preproc", deriv_hits)))

  deriv_refresh_hits <- query_files(
    proj_full, regex = "bold\\.nii\\.gz$", scope = "derivatives",
    refresh = TRUE
  )
  expect_true(length(deriv_refresh_hits) >= 1L)
  expect_true(all(grepl("desc-preproc", deriv_refresh_hits)))
})

test_that("query_files honours index = 'none' and ignores a stale index file", {
  # Regression: an index='none' project must query the live tree, not a leftover
  # RDS index written by an earlier index='auto' run on the same path.
  fixture <- create_index_extensions_fixture()
  on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

  # Build and persist an index (writes path/.bidser_index.rds).
  proj_auto <- bids_project(fixture, derivatives = "auto", index = "auto")
  invisible(query_files(proj_auto, regex = "bold\\.nii\\.gz$"))

  # Add a new raw run, then open the dataset with index = "none".
  new_run <- file.path(
    fixture, "sub-01", "func", "sub-01_task-rest_run-02_bold.nii.gz"
  )
  file.create(new_run)
  proj_none <- bids_project(fixture, derivatives = "auto", index = "none")

  hits <- query_files(proj_none, regex = "bold\\.nii\\.gz$", task = "rest", scope = "raw")
  # The live tree includes both runs; a stale RDS would report only one.
  expect_equal(length(hits), 2L)
})

test_that("cached metadata is reused and invalidated when sidecars change", {
  fixture <- create_index_inheritance_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture$path)
  state0 <- bidser:::.bidser_get_session_index_state(proj)
  expect_equal(nrow(state0$sidecars), 0L)

  first <- get_metadata(proj, fixture$run1, inherit = TRUE)
  state1 <- bidser:::.bidser_get_session_index_state(proj)

  expect_equal(first$RepetitionTime, 2.0)
  expect_true(nrow(state1$sidecars) >= 1)
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
