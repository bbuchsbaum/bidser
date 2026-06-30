context("regression fixes for parsing, query, plotting, and compliance")

library(testthat)
library(bidser)

make_regression_bids <- function(with_event = FALSE) {
  root <- tempfile("bidser_regression_")
  dir.create(file.path(root, "sub-01", "func"), recursive = TRUE)

  writeLines(
    '{"Name":"Regression","BIDSVersion":"1.8.0"}',
    file.path(root, "dataset_description.json")
  )
  writeLines("participant_id\nsub-01", file.path(root, "participants.tsv"))
  file.create(file.path(root, "sub-01", "func", "sub-01_task-rest_run-01_bold.nii.gz"))

  if (isTRUE(with_event)) {
    writeLines(
      c("onset\tduration\ttrial_type", "0\t1\tgo"),
      file.path(root, "sub-01", "func", "sub-01_task-rest_run-01_events.tsv")
    )
  }

  root
}

make_sparse_session_bids <- function() {
  root <- tempfile("bidser_sparse_sessions_")
  dir.create(file.path(root, "sub-01", "ses-A", "func"), recursive = TRUE)
  dir.create(file.path(root, "sub-02", "ses-B", "func"), recursive = TRUE)

  writeLines(
    '{"Name":"SparseSessions","BIDSVersion":"1.8.0"}',
    file.path(root, "dataset_description.json")
  )
  writeLines("participant_id\nsub-01\nsub-02", file.path(root, "participants.tsv"))
  file.create(file.path(root, "sub-01", "ses-A", "func", "sub-01_ses-A_task-rest_run-01_bold.nii.gz"))
  file.create(file.path(root, "sub-02", "ses-B", "func", "sub-02_ses-B_task-rest_run-01_bold.nii.gz"))

  root
}

test_that("encode strictly validates entity fragments and preserves raw type", {
  raw <- encode("sub-01_task-rest_run-01_bold.nii.gz")
  expect_equal(raw$type, "func")

  deriv <- encode("sub-01_task-rest_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz")
  expect_equal(deriv$type, "funcprep")

  expect_null(encode("sub-01_task-rest_bad-x_bold.nii.gz"))
  expect_null(encode("sub-01_run-01_task-rest_bold.nii.gz"))
  expect_null(encode("sub-01_task-rest_run-01_foo-bar_bold.nii.gz"))
})

test_that("decode_bids_entities round-trips valid BIDS filenames", {
  fname <- "sub-01_task-rest_run-01_bold.nii.gz"
  expect_equal(decode_bids_entities(encode(fname)), fname)

  acq_fname <- "sub-01_task-rest_acq-high_run-01_bold.nii.gz"
  expect_equal(decode_bids_entities(encode(acq_fname)), acq_fname)

  smoothed <- decode_bids_entities(utils::modifyList(encode(fname), list(desc = "smooth6mm")))
  expect_equal(smoothed, "sub-01_task-rest_run-01_desc-smooth6mm_bold.nii.gz")
})

test_that("plot_bids heatmap mode passes the project object to bids_heatmap", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridis")
  skip_if_not_installed("scales")

  root <- make_regression_bids()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  proj <- bids_project(root, index = "none")

  expect_warning(
    p <- plot_bids(proj, interactive = FALSE, visualization_mode = "heatmap"),
    NA
  )
  expect_s3_class(p, "ggplot")
})

test_that("query_files tibble fallback preserves full_path", {
  root <- make_regression_bids()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  proj <- bids_project(root, index = "none")

  rows <- query_files(
    proj,
    regex = "bold",
    full_path = TRUE,
    return = "tibble",
    use_index = "never"
  )

  expect_equal(nrow(rows), 1L)
  expect_true(startsWith(rows$path, normalizePath(root, winslash = "/", mustWork = TRUE)))
  expect_equal(rows$type, "func")
})

test_that("search_files falls back to filesystem for regex-only unmatched files", {
  root <- tempfile("bidser_regex_fallback_")
  dir.create(file.path(root, "sub-01", "func"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  writeLines(
    '{"Name":"RegexFallback","BIDSVersion":"1.8.0"}',
    file.path(root, "dataset_description.json")
  )
  writeLines("participant_id\nsub-01", file.path(root, "participants.tsv"))
  file.create(file.path(root, "sub-01", "func", "sub-01_task-rest_run-01_bold.nii.gz"))
  file.create(file.path(root, "sub-01", "func", "sub-01_task-rest_run-01_desc-brain_mask.nii.gz"))

  proj <- bids_project(root, index = "none")

  mask <- search_files(proj, regex = "mask", full_path = FALSE)
  expect_equal(mask, "sub-01/func/sub-01_task-rest_run-01_desc-brain_mask.nii.gz")
})

test_that("load_all_events reads relative labels from any working directory", {
  root <- make_regression_bids(with_event = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  proj <- bids_project(root, index = "none")

  old_wd <- getwd()
  setwd(tempdir())
  on.exit(setwd(old_wd), add = TRUE)

  events <- load_all_events(proj, full_path = FALSE)
  expect_equal(nrow(events), 1L)
  expect_equal(events$.file, "sub-01/func/sub-01_task-rest_run-01_events.tsv")
  expect_equal(events$trial_type, "go")
})

test_that("read_events accepts tab and whitespace-delimited event files", {
  root <- tempfile("bidser_event_delims_")
  dir.create(file.path(root, "sub-01", "func"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  writeLines(
    '{"Name":"EventDelimiters","BIDSVersion":"1.8.0"}',
    file.path(root, "dataset_description.json")
  )
  writeLines("participant_id\nsub-01", file.path(root, "participants.tsv"))

  event_lines <- list(
    tab = c("onset\tduration\ttrial_type", "0\t1\tgo", "2\t1\tstop"),
    space = c("onset duration trial_type", "0 1 go", "2 1 stop"),
    mixed = c("onset duration trial_type", "0\t1\tgo", "2 1 stop")
  )

  for (task in names(event_lines)) {
    file.create(file.path(
      root,
      "sub-01",
      "func",
      paste0("sub-01_task-", task, "_run-01_bold.nii.gz")
    ))
    writeLines(
      event_lines[[task]],
      file.path(root, "sub-01", "func",
                paste0("sub-01_task-", task, "_run-01_events.tsv"))
    )
  }

  proj <- bids_project(root, index = "none")

  for (task in names(event_lines)) {
    events <- read_events(proj, task = paste0("^", task, "$"))
    expect_equal(nrow(events), 1L)
    expect_true(all(c("participant_id", "task", "run", "session") %in% names(events)))
    expect_equal(events$participant_id, "01")
    expect_equal(events$task, task)
    expect_equal(events$run, "01")
    expect_true(all(c("onset", "duration", "trial_type") %in% names(events$data[[1]])))
    expect_equal(events$data[[1]]$onset, c(0, 2))
    expect_equal(events$data[[1]]$duration, c(1, 1))
    expect_equal(events$data[[1]]$trial_type, c("go", "stop"))
  }

  flat <- load_all_events(proj, task = "^space$", full_path = FALSE)
  expect_equal(nrow(flat), 2L)
  expect_true(all(c("onset", "duration", "trial_type") %in% names(flat)))
  expect_equal(flat$trial_type, c("go", "stop"))
})

test_that("bids_check_compliance allows sparse session coverage", {
  root <- make_sparse_session_bids()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  proj <- bids_project(root, index = "none")

  expect_true(proj$has_sessions)
  chk <- bids_check_compliance(proj, schema_check = FALSE)

  expect_false(any(grepl("Session directory not found", chk$issues)))
  expect_true(chk$passed)
})
