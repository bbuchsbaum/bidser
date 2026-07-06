library(testthat)
library(bidser)

make_query_mock_project <- function() {
  participants <- tibble::tibble(participant_id = "01")

  file_structure <- tibble::tribble(
    ~subid, ~session, ~datatype, ~task,   ~run, ~suffix,      ~fmriprep, ~desc,      ~space,
    "01",   NA,       "anat",    NA,      NA,   "T1w.nii.gz", FALSE,     NA,         NA,
    "01",   NA,       "func",    "taskA", "01", "bold.nii.gz", FALSE,    NA,         NA,
    "01",   NA,       "func",    "taskA", "01", "events.tsv",  FALSE,    NA,         NA,
    "01",   NA,       "func",    "taskA", "01", "bold.nii.gz", TRUE,     "preproc",  "MNI"
  )

  create_mock_bids(
    project_name = "QueryMock",
    participants = participants,
    file_structure = file_structure,
    prep_dir = "derivatives/mockprep"
  )
}

make_query_strict_project <- function() {
  tmp <- tempfile("bidser_query_strict_")
  dir.create(file.path(tmp, "sub-01", "anat"), recursive = TRUE)
  dir.create(file.path(tmp, "sub-01", "func"), recursive = TRUE)
  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01"),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "QueryStrict", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )
  invisible(file.create(file.path(tmp, "sub-01", "anat", "sub-01_T1w.nii.gz")))
  invisible(file.create(
    file.path(tmp, "sub-01", "func", "sub-01_task-rest_bold.nii.gz")
  ))
  tmp
}

test_that("query_files supports exact and regex entity matching", {
  mock_proj <- make_query_mock_project()

  exact_hits <- query_files(
    mock_proj,
    regex = "bold\\.nii\\.gz$",
    match_mode = "exact",
    scope = "raw",
    subid = "01",
    task = "taskA",
    full_path = FALSE
  )

  regex_hits <- query_files(
    mock_proj,
    regex = "bold\\.nii\\.gz$",
    match_mode = "regex",
    scope = "raw",
    subid = "0[1]",
    task = "task.*",
    full_path = FALSE
  )

  expect_equal(length(exact_hits), 1)
  expect_true(all(grepl("sub-01", exact_hits)))
  expect_true(all(grepl("task-taskA", exact_hits)))

  expect_equal(length(regex_hits), 1)
  expect_true(all(grepl("task-taskA", regex_hits)))
})

test_that("query_files validates entity filter names", {
  mock_proj <- make_query_mock_project()
  expect_error(
    query_files(mock_proj, not_an_entity = "x"),
    "Unknown entity filters"
  )
})

test_that("query_files require_entity enforces entity presence", {
  mock_proj <- make_query_mock_project()

  lax <- query_files(
    mock_proj,
    regex = "T1w\\.nii\\.gz$",
    kind = "T1w",
    task = ".*",
    scope = "raw",
    require_entity = FALSE,
    full_path = FALSE
  )

  strict_presence <- query_files(
    mock_proj,
    regex = "T1w\\.nii\\.gz$",
    kind = "T1w",
    task = ".*",
    scope = "raw",
    require_entity = TRUE,
    full_path = FALSE
  )

  expect_equal(length(lax), 1)
  expect_equal(lax, "sub-01/anat/sub-01_T1w.nii.gz")
  expect_null(strict_presence)
})

test_that("query_files require_entity + wildcard is honored on the indexed path", {
  # Regression: on a real on-disk project the indexed query path must respect
  # require_entity even when the entity value is the ".*" wildcard. Previously
  # the wildcard "do not require" branch overrode require_entity, leaking files
  # (e.g. anat T1w) that lack the requested entity.
  fixture <- make_query_strict_project()
  on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)
  proj <- bids_project(fixture, derivatives = "none")

  indexed <- query_files(
    proj,
    regex = "\\.nii\\.gz$",
    task = ".*",
    require_entity = TRUE,
    scope = "raw"
  )
  never <- query_files(
    proj,
    regex = "\\.nii\\.gz$",
    task = ".*",
    require_entity = TRUE,
    scope = "raw",
    use_index = "never"
  )

  # Only the func BOLD file carries a task entity; the anat T1w must be excluded.
  expect_equal(indexed, "sub-01/func/sub-01_task-rest_bold.nii.gz")
  # Indexed and non-indexed paths must agree.
  expect_equal(sort(indexed), sort(never))
})

test_that("query_files supports raw vs derivatives scope", {
  mock_proj <- make_query_mock_project()

  raw_hits <- query_files(
    mock_proj,
    regex = "bold\\.nii\\.gz$",
    scope = "raw",
    full_path = FALSE
  )
  deriv_hits <- query_files(
    mock_proj,
    regex = "bold\\.nii\\.gz$",
    scope = "derivatives",
    full_path = FALSE
  )

  expect_true(length(raw_hits) > 0)
  expect_true(length(deriv_hits) > 0)
  expect_true(all(!startsWith(raw_hits, paste0(mock_proj$prep_dir, "/"))))
  expect_true(all(startsWith(deriv_hits, paste0(mock_proj$prep_dir, "/"))))
})

test_that("query_files preserves positional strict argument for bids_project", {
  fixture <- make_query_strict_project()
  on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)
  proj <- bids_project(fixture, derivatives = "none", index = "none")

  named <- query_files(
    proj,
    regex = "\\.nii\\.gz$",
    task = "rest",
    use_index = "never",
    strict = FALSE
  )
  positional <- query_files(
    proj,
    "\\.nii\\.gz$",
    FALSE,
    "regex",
    FALSE,
    "all",
    NULL,
    "paths",
    "never",
    FALSE,
    task = "rest"
  )

  expect_equal(sort(positional), sort(named))
  expect_equal(length(positional), 2L)
})

test_that("query_files preserves positional strict argument for mock projects", {
  mock_proj <- make_query_mock_project()

  named <- query_files(
    mock_proj,
    regex = "\\.nii\\.gz$",
    task = "taskA",
    scope = "raw",
    strict = FALSE
  )
  positional <- query_files(
    mock_proj,
    "\\.nii\\.gz$",
    FALSE,
    "regex",
    FALSE,
    "raw",
    NULL,
    "paths",
    "auto",
    FALSE,
    task = "taskA"
  )

  expect_equal(sort(positional), sort(named))
  expect_equal(length(positional), 2L)
})
