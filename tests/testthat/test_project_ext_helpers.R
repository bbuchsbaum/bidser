library(testthat)
library(bidser)

context("Internal helpers in R/project_extensions.R")

# ---------------------------------------------------------------------------
# .bidser_empty_run_keys
# ---------------------------------------------------------------------------

test_that(".bidser_empty_run_keys returns a tibble with 0 rows and correct columns", {
  result <- bidser:::.bidser_empty_run_keys()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_true(all(c(".subid", ".session", ".task", ".run") %in% names(result)))
  expect_equal(ncol(result), 4)
})

test_that(".bidser_empty_run_keys column types are character", {
  result <- bidser:::.bidser_empty_run_keys()
  expect_type(result$.subid, "character")
  expect_type(result$.session, "character")
  expect_type(result$.task, "character")
  expect_type(result$.run, "character")
})

# ---------------------------------------------------------------------------
# .bidser_normalize_key_col
# ---------------------------------------------------------------------------

test_that(".bidser_normalize_key_col replaces NULL with empty strings", {
  result <- bidser:::.bidser_normalize_key_col(NULL, 3)
  expect_equal(result, c("", "", ""))
})

test_that(".bidser_normalize_key_col replaces NA with empty strings", {
  result <- bidser:::.bidser_normalize_key_col(c("a", NA, "b"), 3)
  expect_equal(result, c("a", "", "b"))
})

test_that(".bidser_normalize_key_col passes through normal strings", {
  result <- bidser:::.bidser_normalize_key_col(c("x", "y"), 2)
  expect_equal(result, c("x", "y"))
})

test_that(".bidser_normalize_key_col coerces numeric to character", {
  result <- bidser:::.bidser_normalize_key_col(c(1, 2), 2)
  expect_equal(result, c("1", "2"))
})

# ---------------------------------------------------------------------------
# .bidser_run_keys_from_df
# ---------------------------------------------------------------------------

test_that(".bidser_run_keys_from_df extracts keys from .subid/.task/.run/.session columns", {
  df <- tibble::tibble(
    .subid = c("01", "02"),
    .session = c("pre", "post"),
    .task = c("rest", "nback"),
    .run = c("01", "02"),
    extra = c(10, 20)
  )
  result <- bidser:::.bidser_run_keys_from_df(df, has_sessions = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$.subid, c("01", "02"))
  expect_equal(result$.session, c("pre", "post"))
  expect_equal(result$.task, c("rest", "nback"))
  expect_equal(result$.run, c("01", "02"))
})

test_that(".bidser_run_keys_from_df uses participant_id column with sub- prefix stripping", {
  df <- tibble::tibble(
    participant_id = c("sub-01", "sub-02"),
    task = c("rest", "nback"),
    run = c("01", "02")
  )
  result <- bidser:::.bidser_run_keys_from_df(df, has_sessions = TRUE)

  expect_equal(result$.subid, c("01", "02"))
  expect_equal(result$.task, c("rest", "nback"))
  expect_equal(result$.run, c("01", "02"))
  # No session columns present => empty strings
  expect_equal(result$.session, c("", ""))
})

test_that(".bidser_run_keys_from_df uses subid column as fallback", {
  df <- tibble::tibble(
    subid = c("03", "04"),
    session = c("A", "B"),
    task = c("motor", "visual"),
    run = c("01", "01")
  )
  result <- bidser:::.bidser_run_keys_from_df(df, has_sessions = TRUE)

  expect_equal(result$.subid, c("03", "04"))
  expect_equal(result$.session, c("A", "B"))
})

test_that(".bidser_run_keys_from_df converts session '1' to '' when has_sessions is FALSE", {
  df <- tibble::tibble(
    .subid = c("01", "02"),
    .session = c("1", "pre"),
    .task = c("rest", "rest"),
    .run = c("01", "01")
  )
  result <- bidser:::.bidser_run_keys_from_df(df, has_sessions = FALSE)

  expect_equal(result$.session[1], "")
  # Non-"1" sessions are preserved
  expect_equal(result$.session[2], "pre")
})

test_that(".bidser_run_keys_from_df returns empty run keys for NULL input", {
  result <- bidser:::.bidser_run_keys_from_df(NULL, has_sessions = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_true(all(c(".subid", ".session", ".task", ".run") %in% names(result)))
})

test_that(".bidser_run_keys_from_df returns empty run keys for 0-row df", {
  df <- tibble::tibble(.subid = character(0), .task = character(0))
  result <- bidser:::.bidser_run_keys_from_df(df, has_sessions = TRUE)

  expect_equal(nrow(result), 0)
})

test_that(".bidser_run_keys_from_df fills empty strings for missing columns", {
  df <- tibble::tibble(x = c(1, 2))
  result <- bidser:::.bidser_run_keys_from_df(df, has_sessions = TRUE)

  expect_equal(nrow(result), 2)
  expect_equal(result$.subid, c("", ""))
  expect_equal(result$.task, c("", ""))
  expect_equal(result$.run, c("", ""))
  expect_equal(result$.session, c("", ""))
})

# ---------------------------------------------------------------------------
# .bidser_nest_by_run
# ---------------------------------------------------------------------------

test_that(".bidser_nest_by_run groups and nests data by run keys", {
  df <- tibble::tibble(
    .subid = c("01", "01", "02"),
    .session = c("", "", ""),
    .task = c("rest", "rest", "rest"),
    .run = c("01", "01", "01"),
    onset = c(1, 2, 3),
    duration = c(0.5, 0.5, 0.5)
  )
  result <- bidser:::.bidser_nest_by_run(df, data_col = "events", count_col = "n_events")

  expect_s3_class(result, "tbl_df")
  # Two distinct groups: sub-01/rest/01 and sub-02/rest/01
  expect_equal(nrow(result), 2)
  expect_true("events" %in% names(result))
  expect_true("n_events" %in% names(result))
  expect_true(is.list(result$events))

  # sub-01 has 2 rows
  sub01 <- result[result$.subid == "01", ]
  expect_equal(sub01$n_events, 2)
  expect_equal(nrow(sub01$events[[1]]), 2)

  # sub-02 has 1 row
  sub02 <- result[result$.subid == "02", ]
  expect_equal(sub02$n_events, 1)
})

test_that(".bidser_nest_by_run returns empty run keys for NULL input", {
  result <- bidser:::.bidser_nest_by_run(NULL, data_col = "data", count_col = "n")
  expect_equal(nrow(result), 0)
  expect_true(all(c(".subid", ".session", ".task", ".run") %in% names(result)))
})

test_that(".bidser_nest_by_run returns empty run keys for 0-row df", {
  df <- tibble::tibble(
    .subid = character(0), .session = character(0),
    .task = character(0), .run = character(0),
    value = numeric(0)
  )
  result <- bidser:::.bidser_nest_by_run(df, data_col = "nested", count_col = "n")
  expect_equal(nrow(result), 0)
})

# ---------------------------------------------------------------------------
# derivative_pipelines
# ---------------------------------------------------------------------------

test_that("derivative_pipelines errors for non-bids_project input", {
  expect_error(derivative_pipelines("not_a_project"), "must be a `bids_project`")
  expect_error(derivative_pipelines(list()), "must be a `bids_project`")
})

# ---------------------------------------------------------------------------
# bids_report and print.bids_report with local fixture
# ---------------------------------------------------------------------------

create_report_fixture <- function() {
  tmp <- tempfile("bidser_report_")
  dir.create(tmp, recursive = TRUE)

  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01"),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "ReportFixture", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )

  dir.create(file.path(tmp, "sub-01", "func"), recursive = TRUE)
  file.create(file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-01_bold.nii.gz"))
  readr::write_tsv(
    tibble::tibble(onset = 0, duration = 1, trial_type = "go"),
    file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-01_events.tsv")
  )

  tmp
}

test_that("bids_report returns bids_report class object", {
  fixture <- create_report_fixture()
  on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture, fmriprep = FALSE)
  report <- bids_report(proj)

  expect_s3_class(report, "bids_report")
  expect_true("project" %in% names(report))
  expect_true("summary" %in% names(report))
  expect_true("compliance" %in% names(report))
  expect_true("pipelines" %in% names(report))
  expect_true("variables" %in% names(report))
  expect_true("run_coverage" %in% names(report))
})

test_that("print.bids_report produces expected output", {
  fixture <- create_report_fixture()
  on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture, fmriprep = FALSE)
  report <- bids_report(proj)

  output <- capture.output(print(report))
  output_text <- paste(output, collapse = "\n")

  expect_true(grepl("BIDS Report", output_text))
  expect_true(grepl("Project:", output_text))
  expect_true(grepl("Subjects:", output_text))
  expect_true(grepl("Tasks:", output_text))
  expect_true(grepl("Compliance:", output_text))
})

test_that("print.bids_report returns invisibly", {
  fixture <- create_report_fixture()
  on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(fixture, fmriprep = FALSE)
  report <- bids_report(proj)

  result <- withVisible(print(report))
  expect_false(result$visible)
  expect_s3_class(result$value, "bids_report")
})

# ---------------------------------------------------------------------------
# derivative_pipelines with a fixture that has derivatives
# ---------------------------------------------------------------------------

test_that("derivative_pipelines returns tibble with pipeline info", {
  tmp <- tempfile("bidser_deriv_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01"),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "DerivTest", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )

  dir.create(file.path(tmp, "sub-01", "func"), recursive = TRUE)
  file.create(file.path(tmp, "sub-01", "func", "sub-01_task-rest_bold.nii.gz"))

  fmriprep_root <- file.path(tmp, "derivatives", "fmriprep")
  dir.create(file.path(fmriprep_root, "sub-01", "func"), recursive = TRUE)
  jsonlite::write_json(
    list(Name = "fmriprep", BIDSVersion = "1.8.0", DatasetType = "derivative"),
    file.path(fmriprep_root, "dataset_description.json"),
    auto_unbox = TRUE
  )
  file.create(
    file.path(fmriprep_root, "sub-01", "func",
              "sub-01_task-rest_space-MNI_desc-preproc_bold.nii.gz")
  )

  proj <- bids_project(tmp, derivatives = "auto")
  pipes <- derivative_pipelines(proj)

  expect_s3_class(pipes, "tbl_df")
  expect_true("pipeline" %in% names(pipes))
  expect_true("root" %in% names(pipes))
  expect_true("fmriprep" %in% pipes$pipeline)
})
