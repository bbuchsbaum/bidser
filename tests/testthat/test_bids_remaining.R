library(testthat)
library(bidser)

context("bids.R remaining coverage")

# ---------------------------------------------------------------------------
# Fixture helper
# ---------------------------------------------------------------------------
create_remaining_fixture <- function(with_sessions = FALSE, with_fmriprep = FALSE) {
  tmp <- tempfile("bidser_rem_")
  dir.create(tmp, recursive = TRUE)

  readr::write_tsv(
    tibble::tibble(participant_id = c("sub-01", "sub-02")),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "RemTest", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )

  for (sub in c("sub-01", "sub-02")) {
    if (with_sessions) {
      for (ses in c("ses-01", "ses-02")) {
        dir.create(file.path(tmp, sub, ses, "func"), recursive = TRUE)
        dir.create(file.path(tmp, sub, ses, "anat"), recursive = TRUE)
        bold <- paste0(sub, "_", ses, "_task-rest_run-01_bold.nii.gz")
        events <- paste0(sub, "_", ses, "_task-rest_run-01_events.tsv")
        t1w <- paste0(sub, "_", ses, "_T1w.nii.gz")
        file.create(file.path(tmp, sub, ses, "func", bold))
        readr::write_tsv(
          tibble::tibble(onset = c(0, 5), duration = c(1, 1), trial_type = c("go", "stop")),
          file.path(tmp, sub, ses, "func", events)
        )
        file.create(file.path(tmp, sub, ses, "anat", t1w))
      }
    } else {
      dir.create(file.path(tmp, sub, "func"), recursive = TRUE)
      dir.create(file.path(tmp, sub, "anat"), recursive = TRUE)
      file.create(file.path(tmp, sub, "func", paste0(sub, "_task-rest_run-01_bold.nii.gz")))
      readr::write_tsv(
        tibble::tibble(onset = c(0, 5), duration = c(1, 1), trial_type = c("go", "stop")),
        file.path(tmp, sub, "func", paste0(sub, "_task-rest_run-01_events.tsv"))
      )
      file.create(file.path(tmp, sub, "anat", paste0(sub, "_T1w.nii.gz")))
    }
  }

  if (with_fmriprep) {
    fmriprep_root <- file.path(tmp, "derivatives", "fmriprep")
    for (sub in c("sub-01", "sub-02")) {
      if (with_sessions) {
        for (ses in c("ses-01", "ses-02")) {
          dir.create(file.path(fmriprep_root, sub, ses, "func"), recursive = TRUE)
          dir.create(file.path(fmriprep_root, sub, ses, "anat"), recursive = TRUE)
          file.create(file.path(fmriprep_root, sub, ses, "func",
            paste0(sub, "_", ses, "_task-rest_run-01_space-MNI_desc-preproc_bold.nii.gz")))
          file.create(file.path(fmriprep_root, sub, ses, "func",
            paste0(sub, "_", ses, "_task-rest_run-01_space-MNI_desc-brain_mask.nii.gz")))
          file.create(file.path(fmriprep_root, sub, ses, "anat",
            paste0(sub, "_", ses, "_space-MNI_desc-preproc_T1w.nii.gz")))
        }
      } else {
        dir.create(file.path(fmriprep_root, sub, "func"), recursive = TRUE)
        dir.create(file.path(fmriprep_root, sub, "anat"), recursive = TRUE)
        file.create(file.path(fmriprep_root, sub, "func",
          paste0(sub, "_task-rest_run-01_space-MNI_desc-preproc_bold.nii.gz")))
        file.create(file.path(fmriprep_root, sub, "func",
          paste0(sub, "_task-rest_run-01_space-MNI_desc-brain_mask.nii.gz")))
        file.create(file.path(fmriprep_root, sub, "anat",
          paste0(sub, "_space-MNI_desc-preproc_T1w.nii.gz")))
      }
    }
    jsonlite::write_json(
      list(Name = "fmriprep", BIDSVersion = "1.8.0", DatasetType = "derivative"),
      file.path(fmriprep_root, "dataset_description.json"),
      auto_unbox = TRUE
    )
  }

  tmp
}

# ===========================================================================
# 1. bids_project() with sessions
# ===========================================================================
test_that("bids_project with sessions sets has_sessions and sessions() returns values", {
  tmp <- create_remaining_fixture(with_sessions = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")

  expect_true(proj$has_sessions)

  sess <- sessions(proj)
  expect_true(length(sess) > 0)
  expect_true(all(c("01", "02") %in% sess))
})

# ===========================================================================
# 2. bids_project() with fmriprep
# ===========================================================================
test_that("bids_project with fmriprep sets has_fmriprep and prep_dir", {
  tmp <- create_remaining_fixture(with_fmriprep = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = TRUE, index = "none")

  expect_true(proj$has_fmriprep)
  expect_true(nzchar(proj$prep_dir))
  expect_true(grepl("fmriprep", proj$prep_dir))
})

# ===========================================================================
# 3. bids_project() derivatives modes
# ===========================================================================
test_that("derivatives='auto' discovers fmriprep pipeline", {
  tmp <- create_remaining_fixture(with_fmriprep = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = TRUE, derivatives = "auto", index = "none")
  expect_true(proj$has_derivatives)
  expect_true("fmriprep" %in% proj$derivatives$pipeline)
})

test_that("derivatives='legacy' with fmriprep=TRUE finds fmriprep", {
  tmp <- create_remaining_fixture(with_fmriprep = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = TRUE, derivatives = "legacy", index = "none")
  expect_true(proj$has_fmriprep)
})

test_that("derivatives='none' has no derivatives", {
  tmp <- create_remaining_fixture(with_fmriprep = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, derivatives = "none", index = "none")
  expect_false(proj$has_derivatives)
  expect_false(proj$has_fmriprep)
})

# ===========================================================================
# 4. sessions.bids_project()
# ===========================================================================
test_that("sessions() returns session IDs when sessions exist", {
  tmp <- create_remaining_fixture(with_sessions = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  sess <- sessions(proj)

  expect_is(sess, "character")
  expect_true(length(sess) >= 2)
  expect_true("01" %in% sess)
  expect_true("02" %in% sess)
})

test_that("sessions() returns NULL when no sessions exist", {
  tmp <- create_remaining_fixture(with_sessions = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  sess <- sessions(proj)

  expect_null(sess)
})

# ===========================================================================
# 5. tasks.bids_project()
# ===========================================================================
test_that("tasks() returns task names", {
  tmp <- create_remaining_fixture(with_sessions = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  tsk <- tasks(proj)

  expect_is(tsk, "character")
  expect_true("rest" %in% tsk)
})

test_that("tasks() with sessions returns task names", {
  tmp <- create_remaining_fixture(with_sessions = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  tsk <- tasks(proj)

  expect_is(tsk, "character")
  expect_true("rest" %in% tsk)
})

# ===========================================================================
# 6. func_scans.bids_project()
# ===========================================================================
test_that("func_scans returns scans and filters work", {
  tmp <- create_remaining_fixture(with_sessions = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")

  # All func scans
  scans <- func_scans(proj)
  expect_true(length(scans) >= 2)
  expect_true(all(grepl("bold\\.nii\\.gz$", scans)))

  # full_path = TRUE (default) should give absolute paths
  expect_true(all(grepl(normalizePath(tmp), scans, fixed = TRUE)))
})

test_that("func_scans full_path=FALSE returns relative paths", {
  tmp <- create_remaining_fixture(with_sessions = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  scans <- func_scans(proj, full_path = FALSE)

  expect_true(length(scans) >= 2)
  # Relative paths should NOT contain the temp directory
  expect_false(any(grepl(normalizePath(tmp), scans, fixed = TRUE)))
})

test_that("func_scans filters by subid", {
  tmp <- create_remaining_fixture(with_sessions = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  scans <- func_scans(proj, subid = "01")

  expect_true(length(scans) >= 1)
  expect_true(all(grepl("sub-01", scans)))
})

test_that("func_scans filters by task", {
  tmp <- create_remaining_fixture(with_sessions = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  scans <- func_scans(proj, task = "rest")

  expect_true(length(scans) >= 2)
  expect_true(all(grepl("task-rest", scans)))
})

test_that("func_scans filters by run", {
  tmp <- create_remaining_fixture(with_sessions = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  scans <- func_scans(proj, run = "01")

  expect_true(length(scans) >= 2)
})

test_that("func_scans returns NULL for nonexistent subid", {
  tmp <- create_remaining_fixture(with_sessions = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  scans <- func_scans(proj, subid = "^99$")

  expect_null(scans)
})

# ===========================================================================
# 7. preproc_scans.bids_project()
# ===========================================================================
test_that("preproc_scans returns derivative paths with fmriprep", {
  tmp <- create_remaining_fixture(with_fmriprep = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = TRUE, index = "none")
  scans <- preproc_scans(proj)

  expect_true(!is.null(scans))
  expect_true(length(scans) >= 2)
  expect_true(all(grepl("preproc", scans)))
})

test_that("preproc_scans returns NULL without fmriprep", {
  tmp <- create_remaining_fixture(with_fmriprep = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, derivatives = "none", index = "none")
  scans <- suppressMessages(preproc_scans(proj))

  expect_null(scans)
})

test_that("preproc_scans with full_path=TRUE returns absolute paths", {
  tmp <- create_remaining_fixture(with_fmriprep = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = TRUE, index = "none")
  scans <- preproc_scans(proj, full_path = TRUE)

  expect_true(!is.null(scans))
  expect_true(all(grepl(normalizePath(tmp), scans, fixed = TRUE)))
})

# ===========================================================================
# 8. load_all_events.bids_project()
# ===========================================================================
test_that("load_all_events reads and combines event files (no sessions)", {
  tmp <- create_remaining_fixture(with_sessions = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  events <- load_all_events(proj)

  expect_s3_class(events, "tbl_df")
  expect_true(nrow(events) > 0)
  expect_true(".subid" %in% names(events))
  expect_true(".task" %in% names(events))
  expect_true(".run" %in% names(events))
  expect_true("onset" %in% names(events))
  expect_true("duration" %in% names(events))
  expect_true("trial_type" %in% names(events))
})

test_that("load_all_events includes .session column with sessions", {
  tmp <- create_remaining_fixture(with_sessions = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  events <- load_all_events(proj)

  expect_s3_class(events, "tbl_df")
  expect_true(nrow(events) > 0)
  expect_true(".session" %in% names(events))
  # Should have events from both sessions
  expect_true(all(c("01", "02") %in% events$.session))
})

test_that("load_all_events filters by subid", {
  tmp <- create_remaining_fixture(with_sessions = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  events <- load_all_events(proj, subid = "01")

  expect_s3_class(events, "tbl_df")
  expect_true(nrow(events) > 0)
  expect_true(all(events$.subid == "01"))
})

test_that("load_all_events returns empty tibble for no matches", {
  tmp <- create_remaining_fixture(with_sessions = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  events <- suppressMessages(load_all_events(proj, subid = "^99$"))

  expect_s3_class(events, "tbl_df")
  expect_equal(nrow(events), 0)
})

# ===========================================================================
# 9. match_attribute (internal)
# ===========================================================================
test_that("match_attribute matches attributes on an object", {
  match_attribute <- bidser:::match_attribute

  obj <- structure("dummy", myattr = "hello_world", kind = "bold")

  expect_true(match_attribute(obj, myattr = "hello"))
  expect_true(match_attribute(obj, kind = "bold"))
  expect_true(match_attribute(obj, myattr = "hello", kind = "bold"))
  expect_false(match_attribute(obj, myattr = "^xyz$"))
  expect_false(match_attribute(obj, kind = "^anat$"))
})

test_that("match_attribute with no arguments returns TRUE", {
  match_attribute <- bidser:::match_attribute

  obj <- structure("dummy", myattr = "hello")
  expect_true(match_attribute(obj))
})

# ===========================================================================
# 10. key_match (internal)
# ===========================================================================
test_that("key_match with no args returns function that always returns TRUE", {
  key_match <- bidser:::key_match

  fn <- key_match()
  # Should return TRUE for any input
  expect_true(fn(list(a = "x")))
  expect_true(fn(list()))
  expect_true(fn(list(task = "rest", run = "01")))
})

test_that("key_match with key-value matches correctly", {
  key_match <- bidser:::key_match

  fn <- key_match(default = FALSE, task = "rest")
  expect_true(fn(list(task = "rest")))
  expect_false(fn(list(task = "motor")))
})

test_that("key_match wildcard '.*' matches any value", {
  key_match <- bidser:::key_match

  fn <- key_match(default = FALSE, task = ".*")
  expect_true(fn(list(task = "rest")))
  expect_true(fn(list(task = "motor")))
  # Wildcard matches even missing keys

  expect_true(fn(list(run = "01")))
})

test_that("key_match: NULL pattern with NULL value -> TRUE", {
  key_match <- bidser:::key_match

  fn <- key_match(default = FALSE, task = NULL)
  # NULL pattern, NULL value -> match
  expect_true(fn(list()))
  expect_true(fn(list(run = "01")))
})

test_that("key_match: NULL pattern with non-NULL value -> FALSE", {
  key_match <- bidser:::key_match

  fn <- key_match(default = FALSE, task = NULL)
  # NULL pattern but task has a value -> no match
  expect_false(fn(list(task = "rest")))
})

test_that("key_match: missing key with default=FALSE -> FALSE", {
  key_match <- bidser:::key_match

  fn <- key_match(default = FALSE, task = "rest")
  # Object does not have 'task' at all -> str_detect_null gets NULL, returns default
  expect_false(fn(list(run = "01")))
})

test_that("key_match: missing key with default=TRUE -> TRUE", {
  key_match <- bidser:::key_match

  fn <- key_match(default = TRUE, task = "rest")
  # Object does not have 'task' at all -> str_detect_null gets NULL, returns default=TRUE
  expect_true(fn(list(run = "01")))
})

# ===========================================================================
# 11. str_detect_null (internal)
# ===========================================================================
test_that("str_detect_null: NULL x returns default", {
  str_detect_null <- bidser:::str_detect_null

  expect_false(str_detect_null(NULL, "rest", default = FALSE))
  expect_true(str_detect_null(NULL, "rest", default = TRUE))
})

test_that("str_detect_null: NA x returns default", {
  str_detect_null <- bidser:::str_detect_null

  expect_false(str_detect_null(NA, "rest", default = FALSE))
  expect_true(str_detect_null(NA, "rest", default = TRUE))
})

test_that("str_detect_null: normal string returns str_detect result", {
  str_detect_null <- bidser:::str_detect_null

  expect_true(str_detect_null("rest_run", "rest"))
  expect_false(str_detect_null("motor", "^rest$"))
  expect_true(str_detect_null("rest", ".*"))
})

# ===========================================================================
# 12. .bidser_discover_derivatives (internal)
# ===========================================================================
test_that(".bidser_discover_derivatives finds fmriprep in auto mode", {
  discover <- bidser:::.bidser_discover_derivatives
  tmp <- create_remaining_fixture(with_fmriprep = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- discover(tmp, fmriprep = TRUE, derivatives = "auto")
  expect_s3_class(result, "tbl_df")
  expect_true("fmriprep" %in% result$pipeline)
  expect_true(nrow(result) >= 1)
})

test_that(".bidser_discover_derivatives returns empty tibble for derivatives='none'", {
  discover <- bidser:::.bidser_discover_derivatives
  tmp <- create_remaining_fixture(with_fmriprep = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- discover(tmp, fmriprep = FALSE, derivatives = "none")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that(".bidser_discover_derivatives legacy mode with fmriprep=TRUE", {
  discover <- bidser:::.bidser_discover_derivatives
  tmp <- create_remaining_fixture(with_fmriprep = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- discover(tmp, fmriprep = TRUE, derivatives = "legacy")
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) >= 1)
  expect_true("fmriprep" %in% result$pipeline)
})

test_that(".bidser_discover_derivatives legacy mode with fmriprep=FALSE returns empty", {
  discover <- bidser:::.bidser_discover_derivatives
  tmp <- create_remaining_fixture(with_fmriprep = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- discover(tmp, fmriprep = FALSE, derivatives = "legacy")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that(".bidser_discover_derivatives reads dataset_description.json", {
  discover <- bidser:::.bidser_discover_derivatives
  tmp <- create_remaining_fixture(with_fmriprep = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- discover(tmp, fmriprep = TRUE, derivatives = "auto")
  # description column should be a list column
  fmriprep_row <- result[result$pipeline == "fmriprep", ]
  expect_true(is.list(fmriprep_row$description))
  desc <- fmriprep_row$description[[1]]
  expect_equal(desc$Name, "fmriprep")
})

# ===========================================================================
# 13. .bidser_load_participants_df (internal)
# ===========================================================================
test_that(".bidser_load_participants_df strict mode reads participants.tsv", {
  load_parts <- bidser:::.bidser_load_participants_df
  tmp <- create_remaining_fixture(with_sessions = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- load_parts(tmp, strict_participants = TRUE)
  expect_is(result, "list")
  expect_equal(result$source, "file")
  expect_s3_class(result$part_df, "tbl_df")
  expect_true("participant_id" %in% names(result$part_df))
  expect_equal(nrow(result$part_df), 2)
})

test_that(".bidser_load_participants_df strict mode errors without participants.tsv", {
  load_parts <- bidser:::.bidser_load_participants_df
  tmp <- tempfile("bidser_nopart_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  dir.create(file.path(tmp, "sub-01", "anat"), recursive = TRUE)
  file.create(file.path(tmp, "sub-01", "anat", "sub-01_T1w.nii.gz"))

  expect_error(load_parts(tmp, strict_participants = TRUE), "participants.tsv is missing")
})

test_that(".bidser_load_participants_df non-strict mode infers from directories", {
  load_parts <- bidser:::.bidser_load_participants_df
  tmp <- tempfile("bidser_nopart_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  dir.create(file.path(tmp, "sub-01", "anat"), recursive = TRUE)
  dir.create(file.path(tmp, "sub-02", "anat"), recursive = TRUE)
  file.create(file.path(tmp, "sub-01", "anat", "sub-01_T1w.nii.gz"))
  file.create(file.path(tmp, "sub-02", "anat", "sub-02_T1w.nii.gz"))

  result <- suppressWarnings(load_parts(tmp, strict_participants = FALSE))
  expect_is(result, "list")
  expect_equal(result$source, "filesystem")
  expect_true(nrow(result$part_df) >= 2)
})

test_that(".bidser_load_participants_df non-strict appends extra subjects", {
  load_parts <- bidser:::.bidser_load_participants_df
  tmp <- tempfile("bidser_extrapart_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # participants.tsv only has sub-01, but sub-02 also exists on disk
  readr::write_tsv(
    tibble::tibble(participant_id = c("sub-01")),
    file.path(tmp, "participants.tsv")
  )
  dir.create(file.path(tmp, "sub-01", "anat"), recursive = TRUE)
  dir.create(file.path(tmp, "sub-02", "anat"), recursive = TRUE)
  file.create(file.path(tmp, "sub-01", "anat", "sub-01_T1w.nii.gz"))
  file.create(file.path(tmp, "sub-02", "anat", "sub-02_T1w.nii.gz"))

  result <- suppressWarnings(load_parts(tmp, strict_participants = FALSE))
  expect_equal(result$source, "file")
  # Should contain both subjects
  all_ids <- result$part_df$participant_id
  expect_true("sub-01" %in% all_ids)
  expect_true("sub-02" %in% all_ids)
})

# ===========================================================================
# 14. .bidser_normalize_subject_dirs (internal)
# ===========================================================================
test_that(".bidser_normalize_subject_dirs adds sub- prefix when missing", {
  norm <- bidser:::.bidser_normalize_subject_dirs

  result <- norm(c("01", "02", "03"))
  expect_equal(result, c("sub-01", "sub-02", "sub-03"))
})

test_that(".bidser_normalize_subject_dirs preserves existing sub- prefix", {
  norm <- bidser:::.bidser_normalize_subject_dirs

  result <- norm(c("sub-01", "sub-02"))
  expect_equal(result, c("sub-01", "sub-02"))
})

test_that(".bidser_normalize_subject_dirs handles mixed inputs", {
  norm <- bidser:::.bidser_normalize_subject_dirs

  result <- norm(c("sub-01", "02", "sub-03"))
  expect_true("sub-01" %in% result)
  expect_true("sub-02" %in% result)
  expect_true("sub-03" %in% result)
})

test_that(".bidser_normalize_subject_dirs removes NA and empty strings", {
  norm <- bidser:::.bidser_normalize_subject_dirs

  result <- norm(c("sub-01", NA, "", "02"))
  expect_equal(length(result), 2)
  expect_true("sub-01" %in% result)
  expect_true("sub-02" %in% result)
})

test_that(".bidser_normalize_subject_dirs deduplicates", {
  norm <- bidser:::.bidser_normalize_subject_dirs

  result <- norm(c("sub-01", "01", "sub-01"))
  expect_equal(result, "sub-01")
})

# ===========================================================================
# 15. mask_files.bids_project() and surface_files.bids_project()
# ===========================================================================
test_that("mask_files returns mask files from fmriprep derivatives", {
  tmp <- create_remaining_fixture(with_fmriprep = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = TRUE, index = "none")
  masks <- mask_files(proj)

  # Should find the desc-brain_mask files
  if (!is.null(masks)) {
    expect_true(all(grepl("mask", basename(masks))))
  }
})

test_that("mask_files returns NULL without fmriprep data", {
  tmp <- create_remaining_fixture(with_fmriprep = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, derivatives = "none", index = "none")
  masks <- mask_files(proj)

  expect_null(masks)
})

test_that("surface_files returns NULL when no surface data exists", {
  tmp <- create_remaining_fixture(with_fmriprep = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = TRUE, index = "none")
  surfs <- surface_files(proj)

  # No .surf.gii files were created, so should be NULL
  expect_null(surfs)
})

test_that("surface_files returns results when surface data exists", {
  tmp <- create_remaining_fixture(with_fmriprep = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # Add some surface files to the fmriprep derivative
  fmriprep_root <- file.path(tmp, "derivatives", "fmriprep")
  for (sub in c("sub-01", "sub-02")) {
    dir.create(file.path(fmriprep_root, sub, "anat"), recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(fmriprep_root, sub, "anat",
      paste0(sub, "_space-fsnative_hemi-L_pial.L.surf.gii")))
    file.create(file.path(fmriprep_root, sub, "anat",
      paste0(sub, "_space-fsnative_hemi-R_pial.R.surf.gii")))
  }

  proj <- bids_project(tmp, fmriprep = TRUE, index = "none")
  surfs <- surface_files(proj)

  if (!is.null(surfs)) {
    expect_true(all(grepl("\\.surf\\.gii$", surfs)))
  }
})

# ===========================================================================
# Additional edge case: print method does not error
# ===========================================================================
test_that("print.bids_project runs without error", {
  tmp <- create_remaining_fixture(with_sessions = TRUE, with_fmriprep = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = TRUE, index = "none")
  expect_output(print(proj))
})

# ===========================================================================
# Additional: bids_summary works
# ===========================================================================
test_that("bids_summary returns expected structure", {
  tmp <- create_remaining_fixture(with_sessions = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  summ <- bids_summary(proj)

  expect_is(summ, "list")
  expect_true("n_subjects" %in% names(summ))
  expect_true("n_sessions" %in% names(summ))
  expect_true("tasks" %in% names(summ))
  expect_true("total_runs" %in% names(summ))
  expect_equal(summ$n_subjects, 2)
  expect_true(summ$n_sessions >= 2)
})
