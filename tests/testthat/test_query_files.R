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
