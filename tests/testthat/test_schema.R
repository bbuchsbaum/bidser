context("BIDS schema validation (milestone 0.8)")

library(testthat)
library(bidser)

# ---------------------------------------------------------------------------
# Helper: create a minimal on-disk BIDS dataset for compliance tests
# ---------------------------------------------------------------------------
create_schema_test_bids <- function() {
  tmp <- tempfile("bidser_schema_")
  dir.create(tmp, recursive = TRUE)

  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01"),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "SchemaTest", BIDSVersion = "1.10.1"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )

  dir.create(file.path(tmp, "sub-01", "func"), recursive = TRUE)
  file.create(file.path(tmp, "sub-01", "func",
                         "sub-01_task-rest_run-01_bold.nii.gz"))
  file.create(file.path(tmp, "sub-01", "func",
                         "sub-01_task-rest_run-01_events.tsv"))

  tmp
}

# ===========================================================================
# bids_schema_versions()
# ===========================================================================
test_that("bids_schema_versions() returns a character vector", {
  vers <- bids_schema_versions()
  expect_type(vers, "character")
})

test_that("bids_schema_versions() includes 1.10.1", {
  vers <- bids_schema_versions()
  expect_true("1.10.1" %in% vers)
})

# ===========================================================================
# bids_schema()
# ===========================================================================
test_that("bids_schema() loads and returns a list", {
  s <- bids_schema()
  expect_type(s, "list")
  expect_true(!is.null(s$objects))
})

test_that("bids_schema() has expected top-level keys", {
  s <- bids_schema()
  expect_true("objects" %in% names(s))
  expect_true("rules"   %in% names(s))
})

test_that("bids_schema() caches on repeated calls", {
  s1 <- bids_schema()
  s2 <- bids_schema()
  expect_identical(s1, s2)
})

test_that("bids_schema() errors on unknown version", {
  expect_error(bids_schema("99.99.99"), "not found")
})

# ===========================================================================
# .bids_schema_entities() and .bids_schema_suffixes()
# ===========================================================================
test_that(".bids_schema_entities() returns known BIDS entity keys", {
  s    <- bids_schema()
  ents <- bidser:::.bids_schema_entities(s)
  expect_type(ents, "character")
  expect_true(length(ents) > 0L)
  expect_true("sub"  %in% ents)
  expect_true("task" %in% ents)
  expect_true("run"  %in% ents)
})

test_that(".bids_schema_suffixes() returns known BIDS suffixes", {
  s     <- bids_schema()
  suffs <- bidser:::.bids_schema_suffixes(s)
  expect_type(suffs, "character")
  expect_true(length(suffs) > 0L)
  expect_true("bold"   %in% suffs)
  expect_true("T1w"    %in% suffs)
  expect_true("events" %in% suffs)
})

# ===========================================================================
# .bids_schema_validate_filename()
# ===========================================================================
test_that(".bids_schema_validate_filename() accepts a valid BIDS filename", {
  s <- bids_schema()
  r <- bidser:::.bids_schema_validate_filename("sub-01_task-rest_bold.nii.gz", s)
  expect_true(r$valid)
  expect_type(r$warnings, "character")
  expect_type(r$issues,   "character")
  expect_length(r$issues, 0L)
})

test_that(".bids_schema_validate_filename() warns on unknown suffix", {
  s <- bids_schema()
  r <- bidser:::.bids_schema_validate_filename(
    "sub-01_task-rest_T99invalid.nii.gz", s
  )
  expect_true(any(grepl("T99invalid", r$warnings)))
})

test_that(".bids_schema_validate_filename() warns on unknown entity key", {
  s <- bids_schema()
  r <- bidser:::.bids_schema_validate_filename(
    "sub-01_foo-bar_bold.nii.gz", s
  )
  expect_true(any(grepl("foo", r$warnings)))
})

test_that(".bids_schema_validate_filename() is valid=TRUE with no issues", {
  s <- bids_schema()
  r <- bidser:::.bids_schema_validate_filename("sub-01_task-rest_bold.nii.gz", s)
  expect_true(r$valid)
})

# ===========================================================================
# bids_check_compliance() schema integration
# ===========================================================================
test_that("bids_check_compliance() has schema_checked field", {
  tmp  <- create_schema_test_bids()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj   <- bids_project(tmp, fmriprep = FALSE)
  result <- bids_check_compliance(proj)

  expect_true("schema_checked" %in% names(result))
})

test_that("bids_check_compliance() schema_check=TRUE sets schema_checked=TRUE", {
  tmp  <- create_schema_test_bids()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj   <- bids_project(tmp, fmriprep = FALSE)
  result <- bids_check_compliance(proj, schema_check = TRUE)

  expect_true(result$schema_checked)
})

test_that("bids_check_compliance() schema_check=FALSE sets schema_checked=FALSE", {
  tmp  <- create_schema_test_bids()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj   <- bids_project(tmp, fmriprep = FALSE)
  result <- bids_check_compliance(proj, schema_check = FALSE)

  expect_false(result$schema_checked)
})

test_that("bids_check_compliance() still returns passed, issues, warnings", {
  tmp  <- create_schema_test_bids()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj   <- bids_project(tmp, fmriprep = FALSE)
  result <- bids_check_compliance(proj)

  expect_true("passed"   %in% names(result))
  expect_true("issues"   %in% names(result))
  expect_true("warnings" %in% names(result))
  expect_type(result$passed,   "logical")
  expect_type(result$issues,   "character")
  expect_type(result$warnings, "character")
})

test_that("bids_check_compliance() warnings is still character with schema on", {
  tmp  <- create_schema_test_bids()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj   <- bids_project(tmp, fmriprep = FALSE)
  result <- bids_check_compliance(proj, schema_check = TRUE)

  expect_type(result$warnings, "character")
})
