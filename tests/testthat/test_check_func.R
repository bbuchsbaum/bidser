context("check_func_scans and file_pairs")

library(testthat)
library(bidser)

# Helper to create a local BIDS fixture with functional scans
create_check_fixture <- function(tasks = c("rest", "nback")) {
  tmp <- tempfile("bidser_check_")
  dir.create(tmp, recursive = TRUE)
  readr::write_tsv(
    tibble::tibble(participant_id = c("sub-01", "sub-02")),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "CheckTest", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )
  for (sub in c("sub-01", "sub-02")) {
    dir.create(file.path(tmp, sub, "func"), recursive = TRUE)
    for (task in tasks) {
      bold_name <- paste0(sub, "_task-", task, "_run-01_bold.nii.gz")
      events_name <- paste0(sub, "_task-", task, "_run-01_events.tsv")
      # Write enough content so file_size is non-zero
      writeLines(rep("dummy-nifti-content", 10),
                 file.path(tmp, sub, "func", bold_name))
      readr::write_tsv(
        tibble::tibble(onset = c(1, 5), duration = c(0.5, 0.5),
                       trial_type = c("go", "stop")),
        file.path(tmp, sub, "func", events_name)
      )
    }
  }
  tmp
}

# ---------- check_func_scans: multiple tasks ----------

test_that("check_func_scans with multiple tasks returns scans_per_task", {
  tmp <- create_check_fixture(tasks = c("rest", "nback"))
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp)
  res <- check_func_scans(proj)

  # Basic structure

  expect_true(is.list(res))
  expect_true("scans" %in% names(res))
  expect_true("tasklist" %in% names(res))
  expect_true("scans_per_subject" %in% names(res))

  # Multiple-task branch should provide these extra elements

  expect_true("scans_per_task" %in% names(res))
  expect_true("scans_per_task_subject" %in% names(res))
  expect_true("size_per_task" %in% names(res))

  # scans tibble should have rows (2 subjects x 2 tasks = 4 bold files)
  expect_s3_class(res$scans, "tbl_df")
  expect_equal(nrow(res$scans), 4)

  # tasklist should contain both tasks
  expect_true(all(c("rest", "nback") %in% res$tasklist))

  # scans_per_subject should have one row per subject
  expect_equal(nrow(res$scans_per_subject), 2)
})

# ---------- check_func_scans: single task ----------

test_that("check_func_scans with single task returns size_per_subject", {
  tmp <- create_check_fixture(tasks = "rest")
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp)
  res <- check_func_scans(proj)

  # Single-task branch should provide size_per_subject instead of per_task
  expect_true("size_per_subject" %in% names(res))
  expect_false("scans_per_task" %in% names(res))
  expect_false("size_per_task" %in% names(res))

  # 2 subjects x 1 task = 2 bold files
  expect_equal(nrow(res$scans), 2)
  expect_equal(length(res$tasklist), 1)
})

# ---------- check_func_scans: class ----------

test_that("check_func_scans returns correct S3 class", {
  tmp <- create_check_fixture(tasks = "rest")
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp)
  res <- check_func_scans(proj)

  expect_s3_class(res, "check")
  expect_s3_class(res, "check_func_scans")
  expect_equal(class(res), c("check", "check_func_scans"))
})

# ---------- check_func_scans: non-bids_project errors ----------

test_that("check_func_scans errors on non-bids_project input", {
  expect_error(check_func_scans("not_a_project"),
               "must be a `bids_project` object")
  expect_error(check_func_scans(list(a = 1)),
               "must be a `bids_project` object")
  expect_error(check_func_scans(42),
               "must be a `bids_project` object")
})

# ---------- check_func_scans: scans tibble has size column ----------

test_that("check_func_scans scans tibble includes file size", {
  tmp <- create_check_fixture(tasks = "rest")
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp)
  res <- check_func_scans(proj)

  expect_true("size" %in% names(res$scans))
  # All sizes should be positive (files have content)
  expect_true(all(res$scans$size > 0))
})

# ---------- file_pairs: bold-events matching ----------

test_that("file_pairs matches bold and events files", {
  tmp <- create_check_fixture(tasks = c("rest", "nback"))
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp)

  pairs <- file_pairs(proj, pair = "bold-events")

  expect_s3_class(pairs, "tbl_df")
  expect_true("subid" %in% names(pairs))
  expect_true("bold" %in% names(pairs))
  expect_true("events" %in% names(pairs))

  # Each bold file should have a matched events file (no NAs)
  expect_true(all(!is.na(pairs$events)))
  # 2 subjects x 2 tasks = 4 rows
  expect_equal(nrow(pairs), 4)
})

# ---------- file_pairs: task filtering ----------

test_that("file_pairs respects task filter", {
  tmp <- create_check_fixture(tasks = c("rest", "nback"))
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp)

  pairs <- file_pairs(proj, pair = "bold-events", task = "rest")

  expect_s3_class(pairs, "tbl_df")
  # Only rest task should appear (2 subjects x 1 task = 2 rows)
  expect_equal(nrow(pairs), 2)
  expect_true(all(pairs$task == "rest"))
})

# ---------- file_pairs: non-bids_project errors ----------

test_that("file_pairs errors on non-bids_project input", {
  expect_error(file_pairs("not_a_project"), "bids_project")
})
