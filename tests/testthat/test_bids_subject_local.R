library(testthat)
library(bidser)

context("bids_subject (local fixtures)")

# ---------------------------------------------------------------------------
# Fixture helper -- builds a minimal BIDS directory on disk so that
# bids_project() can parse it without any network access.
# ---------------------------------------------------------------------------
create_subject_fixture <- function() {
  tmp <- tempfile("bidser_subj_")
  dir.create(tmp, recursive = TRUE)

  readr::write_tsv(
    tibble::tibble(participant_id = c("sub-01", "sub-02")),
    file.path(tmp, "participants.tsv")
  )

  jsonlite::write_json(
    list(Name = "SubjectTest", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )

  # --- sub-01 ---
  dir.create(file.path(tmp, "sub-01", "func"), recursive = TRUE)
  file.create(file.path(tmp, "sub-01", "func",
                         "sub-01_task-rest_run-01_bold.nii.gz"))
  readr::write_tsv(
    tibble::tibble(onset = c(0, 5), duration = c(1, 1),
                   trial_type = c("go", "stop")),
    file.path(tmp, "sub-01", "func",
              "sub-01_task-rest_run-01_events.tsv")
  )

  # --- sub-02 ---
  dir.create(file.path(tmp, "sub-02", "func"), recursive = TRUE)
  file.create(file.path(tmp, "sub-02", "func",
                         "sub-02_task-rest_run-01_bold.nii.gz"))
  readr::write_tsv(
    tibble::tibble(onset = c(0, 5), duration = c(1, 1),
                   trial_type = c("go", "stop")),
    file.path(tmp, "sub-02", "func",
              "sub-02_task-rest_run-01_events.tsv")
  )

  tmp
}

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

test_that("bids_subject returns a list with expected function names", {
  tmp <- create_subject_fixture()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE)
  subj <- bids_subject(proj, "01")

  expect_true(is.list(subj))
  expected_names <- c("events", "event_files", "scans",
                      "confounds", "preproc_scans", "brain_mask")
  expect_true(all(expected_names %in% names(subj)))
  # Every element should be a function

  for (nm in expected_names) {
    expect_true(is.function(subj[[nm]]),
                info = paste(nm, "should be a function"))
  }
})

test_that("bids_subject works with 'sub-' prefix on subid", {
  tmp <- create_subject_fixture()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE)

  # Should work identically whether or not the prefix is supplied
  subj_plain  <- bids_subject(proj, "01")
  subj_prefix <- bids_subject(proj, "sub-01")

  expect_true(is.list(subj_plain))
  expect_true(is.list(subj_prefix))
  # Both should expose the same set of helpers

  expect_equal(sort(names(subj_plain)), sort(names(subj_prefix)))
})

test_that("bids_subject errors when subject is not found", {
  tmp <- create_subject_fixture()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE)

  expect_error(bids_subject(proj, "99"), "Subject not found")
  expect_error(bids_subject(proj, "sub-99"), "Subject not found")
})

test_that("bids_subject errors for non-bids_project input", {
  expect_error(bids_subject("not_a_project", "01"),
               "must be a.*bids_project")
  expect_error(bids_subject(list(a = 1), "01"),
               "must be a.*bids_project")
})

test_that("subj$scans() returns functional scan paths for the subject", {
  tmp <- create_subject_fixture()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE)
  subj <- bids_subject(proj, "01")

  scans <- subj$scans()
  expect_true(is.character(scans))
  expect_true(length(scans) >= 1)
  # All returned paths should reference sub-01
  expect_true(all(grepl("sub-01", scans)))
})

test_that("subj$event_files() returns event file paths for the subject", {
  tmp <- create_subject_fixture()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE)
  subj <- bids_subject(proj, "01")

  ef <- subj$event_files()
  expect_true(is.character(ef))
  expect_true(length(ef) >= 1)
  expect_true(all(grepl("events\\.tsv$", ef)))
  expect_true(all(grepl("sub-01", ef)))
})

test_that("subj$events() returns a tibble with event data", {
  tmp <- create_subject_fixture()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE)
  subj <- bids_subject(proj, "01")

  ev <- subj$events()
  expect_s3_class(ev, "tbl_df")
  expect_gt(nrow(ev), 0)
  # Should have a nested 'data' column
  expect_true("data" %in% names(ev))
})

test_that("subject-specific filtering: sub-01 scans differ from sub-02 scans", {
  tmp <- create_subject_fixture()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE)
  subj1 <- bids_subject(proj, "01")
  subj2 <- bids_subject(proj, "02")

  scans1 <- subj1$scans()
  scans2 <- subj2$scans()

  expect_true(is.character(scans1))
  expect_true(is.character(scans2))
  # The two sets should be disjoint (different subjects)
  expect_equal(length(intersect(scans1, scans2)), 0)
  # Each should reference only its own subject
  expect_true(all(grepl("sub-01", scans1)))
  expect_true(all(grepl("sub-02", scans2)))
})
