# tests/testthat/test_formula_filters.R

library(testthat)
library(bidser)

context("Formula-based entity filters in search_files / query_files")

# ---------------------------------------------------------------------------
# Shared fixture
# ---------------------------------------------------------------------------

make_proj <- function() {
  participants_df <- tibble::tibble(participant_id = c("01", "02"))

  file_structure_df <- tibble::tribble(
    ~subid, ~session, ~datatype, ~task,   ~run,  ~suffix,        ~fmriprep, ~desc, ~space,
    "01",   NA,       "func",    "taskA", "01",  "bold.nii.gz",  FALSE,     NA,    NA,
    "01",   NA,       "func",    "taskA", "02",  "bold.nii.gz",  FALSE,     NA,    NA,
    "01",   NA,       "func",    "taskA", "01",  "events.tsv",   FALSE,     NA,    NA,
    "01",   NA,       "func",    "taskA", "02",  "events.tsv",   FALSE,     NA,    NA,
    "01",   NA,       "anat",    NA,      NA,    "T1w.nii.gz",   FALSE,     NA,    NA,
    "02",   NA,       "func",    "taskA", "01",  "bold.nii.gz",  FALSE,     NA,    NA,
    "02",   NA,       "func",    "taskB", "01",  "bold.nii.gz",  FALSE,     NA,    NA
  )

  tmp <- tempfile("mock_bids_formula")
  proj <- create_mock_bids(
    project_name   = "FormulaTest",
    participants   = participants_df,
    file_structure = file_structure_df
  )
  proj
}

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

test_that("formula filter: run ~ as.integer(run) == 1 returns only run-01 files", {
  proj <- make_proj()
  res <- search_files(proj, run ~ as.integer(run) == 1)
  expect_true(!is.null(res) && length(res) > 0)
  # Every returned path must contain run-01
  expect_true(all(grepl("run-01", res)))
  # No run-02 paths
  expect_false(any(grepl("run-02", res)))
})

test_that("formula filter: run ~ as.integer(run) >= 2 returns only run-02 files", {
  proj <- make_proj()
  res <- search_files(proj, run ~ as.integer(run) >= 2)
  expect_true(!is.null(res) && length(res) > 0)
  expect_true(all(grepl("run-02", res)))
  expect_false(any(grepl("run-01", res)))
})

test_that("formula + string filter can be combined", {
  proj <- make_proj()
  # task filter restricts to taskA, formula restricts to run >= 1 (all)
  res_combined <- search_files(proj, task = "taskA", run ~ as.integer(run) >= 1)
  res_string_only <- search_files(proj, task = "taskA")
  # Combined result is a subset of string-only result
  expect_true(!is.null(res_combined))
  expect_true(all(res_combined %in% res_string_only))
})

test_that("node without run entity is excluded when run formula is active", {
  proj <- make_proj()
  # The T1w anat file has no run entity; formula filter should exclude it silently
  res <- search_files(proj, run ~ as.integer(run) >= 1)
  expect_false(any(grepl("T1w", res)))
})

test_that("formula filter captures variables from caller environment", {
  proj <- make_proj()
  x <- 1L
  res <- search_files(proj, run ~ as.integer(run) == x)
  expect_true(!is.null(res) && length(res) > 0)
  expect_true(all(grepl("run-01", res)))
})

test_that("two formula filters apply with AND semantics", {
  proj <- make_proj()
  res <- search_files(proj, run ~ as.integer(run) >= 1, run ~ as.integer(run) <= 1)
  expect_true(!is.null(res) && length(res) > 0)
  expect_true(all(grepl("run-01", res)))
  expect_false(any(grepl("run-02", res)))
})

test_that("bad formula LHS (non-symbol) emits a clear error", {
  proj <- make_proj()
  expect_error(
    search_files(proj, a + b ~ as.integer(run) >= 1),
    "Formula LHS must be a single entity name symbol"
  )
})

test_that("existing string-only search_files call still works (no regression)", {
  proj <- make_proj()
  res <- search_files(proj, task = "taskA")
  expect_true(!is.null(res) && length(res) > 0)
  expect_true(all(grepl("taskA", res)))
})

test_that("query_files falls back to tree traversal when formula filter present", {
  proj <- make_proj()
  res <- query_files(proj, run ~ as.integer(run) == 1)
  expect_true(!is.null(res) && length(res) > 0)
  expect_true(all(grepl("run-01", res)))
  expect_false(any(grepl("run-02", res)))
})
