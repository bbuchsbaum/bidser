event_inheritance_fixture <- function(include_specific = FALSE, include_subject_level = FALSE) {
  root <- tempfile("bidser-event-inheritance-")
  dir.create(file.path(root, "sub-01", "func"), recursive = TRUE)
  dir.create(file.path(root, "sub-02", "func"), recursive = TRUE)
  dir.create(file.path(root, "derivatives", "other", "sub-01", "func"), recursive = TRUE)
  dir.create(file.path(root, "sourcedata", "sub-01", "func"), recursive = TRUE)
  dir.create(file.path(root, "code"), recursive = TRUE)
  dir.create(file.path(root, "stimuli"), recursive = TRUE)
  jsonlite::write_json(
    list(Name = "event-inheritance", BIDSVersion = "1.10.0"),
    file.path(root, "dataset_description.json"), auto_unbox = TRUE
  )
  jsonlite::write_json(
    list(
      Name = "other", BIDSVersion = "1.10.0", DatasetType = "derivative",
      GeneratedBy = list(list(Name = "bidser test fixture"))
    ),
    file.path(root, "derivatives", "other", "dataset_description.json"),
    auto_unbox = TRUE
  )
  writeLines(c("participant_id", "sub-01", "sub-02"), file.path(root, "participants.tsv"))
  writeLines(c("onset\\tduration\\ttrial_type", "0\\t1\\trest"), file.path(root, "task-rest_events.tsv"))
  writeLines(c("onset\\tduration\\ttrial_type", "0\\t1\\tother-subject"), file.path(
    root, "sub-02", "func", "sub-02_task-rest_run-01_events.tsv"
  ))
  writeLines(c("onset\\tduration\\ttrial_type", "0\\t1\\tother-task"), file.path(
    root, "sub-01", "func", "sub-01_task-motor_run-01_events.tsv"
  ))
  writeLines(c("onset\\tduration\\ttrial_type", "0\\t1\\tderivative"), file.path(
    root, "derivatives", "other", "sub-01", "func", "sub-01_task-rest_run-01_events.tsv"
  ))
  writeLines(c("onset\\tduration\\ttrial_type", "0\\t1\\tsource"), file.path(
    root, "sourcedata", "sub-01", "func", "sub-01_task-rest_run-01_events.tsv"
  ))
  writeLines(c("onset\\tduration\\ttrial_type", "0\\t1\\tcode"), file.path(
    root, "code", "task-rest_events.tsv"
  ))
  writeLines(c("onset\\tduration\\ttrial_type", "0\\t1\\tstimulus"), file.path(
    root, "stimuli", "task-rest_events.tsv"
  ))
  if (isTRUE(include_specific)) {
    writeLines(c("onset\\tduration\\ttrial_type", "0\\t1\\tspecific"), file.path(
      root, "sub-01", "func", "sub-01_task-rest_run-01_events.tsv"
    ))
  }
  if (isTRUE(include_subject_level)) {
    writeLines(c("onset\\tduration\\ttrial_type", "0\\t1\\tsubject-level"), file.path(
      root, "sub-01", "func", "sub-01_task-rest_events.tsv"
    ))
  }
  file.create(file.path(root, "sub-01", "func", "sub-01_task-rest_run-01_bold.nii.gz"))
  root
}

event_inheritance_project <- function(root) {
  bids_project(root, fmriprep = FALSE, index = "none", strict_participants = FALSE)
}

test_that("task-level events inherit onto a selected BOLD run", {
  root <- event_inheritance_fixture()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  result <- event_files(
    event_inheritance_project(root), subid = "01", task = "rest", run = "01",
    full_path = FALSE
  )

  expect_identical(result, "task-rest_events.tsv")
})

test_that("inherited event lookup excludes conflicts and non-raw trees", {
  root <- event_inheritance_fixture()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  result <- event_files(
    event_inheritance_project(root), subid = "01", task = "rest", run = "01",
    full_path = FALSE
  )

  expect_false(any(grepl("sub-02|motor|derivatives|sourcedata|code|stimuli", result)))
  expect_identical(result, "task-rest_events.tsv")
})

test_that("a run-specific event file remains preferred over inherited fallback", {
  root <- event_inheritance_fixture(include_specific = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  result <- event_files(
    event_inheritance_project(root), subid = "01", task = "rest", run = "01",
    full_path = FALSE
  )

  expect_identical(result, "sub-01/func/sub-01_task-rest_run-01_events.tsv")
})

test_that("the most specific compatible inherited event file takes precedence", {
  root <- event_inheritance_fixture(include_subject_level = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  result <- event_files(
    event_inheritance_project(root), subid = "01", task = "rest", run = "01",
    full_path = FALSE
  )

  expect_identical(result, "sub-01/func/sub-01_task-rest_events.tsv")
})

test_that("wildcard event queries retain all compatible inheritance candidates", {
  root <- event_inheritance_fixture(include_subject_level = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  result <- event_files(
    event_inheritance_project(root), subid = ".*", task = "rest", run = ".*",
    full_path = FALSE
  )

  expect_setequal(result, c(
    "task-rest_events.tsv",
    "sub-01/func/sub-01_task-rest_events.tsv",
    "sub-02/func/sub-02_task-rest_run-01_events.tsv"
  ))
})

test_that("read_events assigns a queried session to inherited events", {
  root <- event_inheritance_fixture()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(file.path(root, "sub-01", "ses-01", "func"), recursive = TRUE)
  file.create(file.path(
    root, "sub-01", "ses-01", "func", "sub-01_ses-01_task-rest_run-01_bold.nii.gz"
  ))

  result <- read_events(
    event_inheritance_project(root), subid = "01", task = "rest", run = "01",
    session = "^01$"
  )

  expect_true(nrow(result) > 0)
  expect_true(all(result$.session == "01"))
})
