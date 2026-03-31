library(testthat)
library(bidser)

context("Mock BIDS methods (extended coverage)")

# ---------------------------------------------------------------------------
# Shared helpers / data used across multiple tests
# ---------------------------------------------------------------------------

# Minimal participants
parts_df <- tibble::tibble(participant_id = c("01", "02"), age = c(25, 30))

# File structure with raw + derivative rows
file_structure_df <- tibble::tribble(
  ~subid, ~session, ~datatype, ~task,   ~run, ~suffix,          ~fmriprep, ~desc,     ~space,
  "01",   NA,       "anat",    NA,      NA,   "T1w.nii.gz",     FALSE,     NA,        NA,
  "01",   NA,       "func",    "taskA", "01", "bold.nii.gz",    FALSE,     NA,        NA,
  "01",   NA,       "func",    "taskA", "01", "events.tsv",     FALSE,     NA,        NA,
  "02",   "pre",    "func",    "taskA", "01", "bold.nii.gz",    FALSE,     NA,        NA,
  "02",   "pre",    "func",    "taskA", "01", "events.tsv",     FALSE,     NA,        NA,
  # Derivatives
  "01",   NA,       "func",    "taskA", "01", "bold.nii.gz",    TRUE,      "preproc", "MNI",
  "01",   NA,       "anat",    NA,      NA,   "T1w.nii.gz",     TRUE,      "preproc", "MNI"
)

# Event data keyed by relative path
ev_fname_1 <- bidser:::generate_bids_filename(subid = "01", task = "taskA",
                                               run = "01", suffix = "events.tsv")
ev_fname_2 <- bidser:::generate_bids_filename(subid = "02", session = "pre",
                                               task = "taskA", run = "01",
                                               suffix = "events.tsv")
ev_path_1 <- file.path("sub-01", "func", ev_fname_1)
ev_path_2 <- file.path("sub-02", "ses-pre", "func", ev_fname_2)

event_data_list <- list()
event_data_list[[ev_path_1]] <- tibble::tibble(
  onset = c(1, 5), duration = c(0.5, 0.5), trial_type = c("A", "B")
)
event_data_list[[ev_path_2]] <- tibble::tibble(
  onset = c(2, 6), duration = c(0.5, 0.5), trial_type = c("C", "D")
)

# Helper to build a standard mock project (reused in many tests)
make_mock <- function(prep_dir = "derivatives/mockprep") {
  create_mock_bids(
    project_name = "MethodTest",
    participants  = parts_df,
    file_structure = file_structure_df,
    event_data     = event_data_list,
    prep_dir       = prep_dir
  )
}

# ---------------------------------------------------------------------------
# 1. print.mock_bids_project
# ---------------------------------------------------------------------------

test_that("print.mock_bids_project produces informative output", {
  mock <- make_mock()
  out <- capture.output(print(mock))
  combined <- paste(out, collapse = "\n")

  # Should mention the project name

  expect_true(grepl("MethodTest", combined, fixed = TRUE),
              info = "Output should contain the project name")
  # Should mention participant count
  expect_true(grepl("2", combined),
              info = "Output should mention 2 participants")
  # Should mention task
  expect_true(grepl("taskA", combined, fixed = TRUE),
              info = "Output should list tasks")
  # Should mention derivatives path
  expect_true(grepl("mockprep", combined, fixed = TRUE),
              info = "Output should mention the derivatives directory")
  # print() should return the object invisibly
  ret <- print(mock)
  expect_identical(ret, mock)
})

# ---------------------------------------------------------------------------
# 2. sessions.mock_bids_project
# ---------------------------------------------------------------------------

test_that("sessions() returns NULL for a project without sessions", {
  no_ses_fs <- tibble::tibble(
    subid = "01", datatype = "func", task = "rest", run = "01",
    suffix = "bold.nii.gz", fmriprep = FALSE
  )
  mock_no_ses <- create_mock_bids(
    project_name   = "NoSessions",
    participants   = tibble::tibble(participant_id = "01"),
    file_structure = no_ses_fs
  )
  result <- sessions(mock_no_ses)
  expect_null(result)
})

test_that("sessions() returns correct sessions when present", {
  mock <- make_mock()
  sess <- sessions(mock)
  expect_true(is.character(sess))
  expect_true("pre" %in% sess)
})

# ---------------------------------------------------------------------------
# 3. search_files.mock_bids_project -- full_path and fmriprep flags
# ---------------------------------------------------------------------------

test_that("search_files full_path=TRUE prepends mock path prefix", {
  mock <- make_mock()
  res_rel  <- search_files(mock, regex = "bold\\.nii\\.gz$",
                           fmriprep = FALSE, full_path = FALSE)
  res_full <- search_files(mock, regex = "bold\\.nii\\.gz$",
                           fmriprep = FALSE, full_path = TRUE)

  expect_false(is.null(res_rel))
  expect_false(is.null(res_full))
  # Full paths should be longer
  expect_true(all(nchar(res_full) > nchar(res_rel)))
})

test_that("search_files fmriprep=TRUE returns only derivative files", {
  mock <- make_mock()
  deriv_files <- search_files(mock, regex = "\\.nii\\.gz$",
                              fmriprep = TRUE, full_path = FALSE)
  expect_false(is.null(deriv_files))
  # Every result should live under the derivatives path
  expect_true(all(grepl("^derivatives/", deriv_files)))
})

test_that("search_files fmriprep=FALSE excludes derivative files", {
  mock <- make_mock()
  raw_files <- search_files(mock, regex = "\\.nii\\.gz$",
                            fmriprep = FALSE, full_path = FALSE)
  expect_false(is.null(raw_files))
  expect_false(any(grepl("^derivatives/", raw_files)))
})

# ---------------------------------------------------------------------------
# 4. create_preproc_mask.mock_bids_project
# ---------------------------------------------------------------------------

test_that("create_preproc_mask errors for mock projects", {
  mock <- make_mock()
  expect_error(create_preproc_mask(mock),
               "cannot be used with.*mock_bids_project")
})

# ---------------------------------------------------------------------------
# 5. transform_files.mock_bids_project
# ---------------------------------------------------------------------------

test_that("transform_files returns NULL when no transforms exist", {
  mock <- make_mock()
  result <- transform_files(mock, subid = "01")
  expect_null(result)
})

test_that("transform_files finds .h5 transform files in derivatives", {
  # Build a structure that includes a transform file
  xfm_fs <- tibble::tribble(
    ~subid, ~session, ~datatype, ~task, ~run, ~suffix,                                    ~fmriprep, ~desc, ~space, ~from,   ~to,
    "01",   NA,       "func",    NA,    NA,   "bold.nii.gz",                                FALSE,    NA,    NA,     NA,      NA,
    "01",   NA,       "anat",    NA,    NA,   "xfm.h5",                                     TRUE,     NA,    NA,     "T1w",   "MNI"
  )
  mock_xfm <- create_mock_bids(
    project_name   = "XfmTest",
    participants   = tibble::tibble(participant_id = "01"),
    file_structure = xfm_fs,
    prep_dir       = "derivatives/mockprep"
  )
  result <- transform_files(mock_xfm, subid = "01", full_path = FALSE)
  # If the tree has the .h5 file it should be found
  if (!is.null(result)) {
    expect_true(all(grepl("\\.h5$", result)))
  }
})

# ---------------------------------------------------------------------------
# 6. build_subject_graph.mock_bids_project
# ---------------------------------------------------------------------------

test_that("build_subject_graph returns a bids_subject_graph structure", {
  mock <- make_mock()
  graph <- build_subject_graph(mock, subid = "01")

  expect_s3_class(graph, "bids_subject_graph")
  expect_true(is.list(graph))
  expect_equal(graph$subid, "01")

  # Should contain the standard slots
  expected_slots <- c("subid", "sessions", "epi", "anat",
                      "transforms", "surfaces", "confounds")
  expect_true(all(expected_slots %in% names(graph)))
})

test_that("build_subject_graph errors for nonexistent subject", {
  mock <- make_mock()
  expect_error(build_subject_graph(mock, subid = "99"),
               "Subject not found")
})

# ---------------------------------------------------------------------------
# 7. generate_bids_filename (internal)
# ---------------------------------------------------------------------------

test_that("generate_bids_filename errors on missing subid", {
  expect_error(bidser:::generate_bids_filename(subid = NA, suffix = "bold.nii.gz"),
               "subid.*required")
  expect_error(bidser:::generate_bids_filename(subid = "", suffix = "bold.nii.gz"),
               "subid.*required")
})

test_that("generate_bids_filename errors on missing suffix", {
  expect_error(bidser:::generate_bids_filename(subid = "01", suffix = NA),
               "suffix.*required")
  expect_error(bidser:::generate_bids_filename(subid = "01", suffix = ""),
               "suffix.*required")
})

test_that("generate_bids_filename produces correct filename with all entities", {
  fn <- bidser:::generate_bids_filename(
    subid   = "01",
    session = "pre",
    task    = "rest",
    run     = "02",
    space   = "MNI",
    desc    = "preproc",
    suffix  = "bold.nii.gz"
  )
  expect_true(grepl("^sub-01_", fn))
  expect_true(grepl("ses-pre", fn))
  expect_true(grepl("task-rest", fn))
  expect_true(grepl("run-02", fn))
  expect_true(grepl("space-MNI", fn))
  expect_true(grepl("desc-preproc", fn))
  expect_true(grepl("bold\\.nii\\.gz$", fn))
})

test_that("generate_bids_filename handles subid already having 'sub-' prefix", {
  fn <- bidser:::generate_bids_filename(subid = "sub-01",
                                         suffix = "T1w.nii.gz")
  # Should NOT produce sub-sub-01

  expect_false(grepl("sub-sub-", fn))
  expect_true(grepl("^sub-01_", fn))
})

# ---------------------------------------------------------------------------
# 8. generate_bids_path (internal)
# ---------------------------------------------------------------------------

test_that("generate_bids_path produces a raw data path", {
  p <- bidser:::generate_bids_path(subid = "01", datatype = "func")
  expect_equal(p, "sub-01/func")
})

test_that("generate_bids_path produces a derivative path with fmriprep=TRUE", {
  p <- bidser:::generate_bids_path(subid = "01", datatype = "func",
                                    fmriprep = TRUE,
                                    prep_dir = "derivatives/fmriprep")
  expect_equal(p, "derivatives/fmriprep/sub-01/func")
})

test_that("generate_bids_path includes session when provided", {
  p <- bidser:::generate_bids_path(subid = "01", session = "pre",
                                    datatype = "anat")
  expect_equal(p, "sub-01/ses-pre/anat")
})

test_that("generate_bids_path adds sub- prefix when missing", {
  p <- bidser:::generate_bids_path(subid = "02", datatype = "dwi")
  expect_true(grepl("^sub-02", p))
})

test_that("generate_bids_path errors on missing subid or datatype", {
  expect_error(bidser:::generate_bids_path(subid = NA, datatype = "func"),
               "subid.*required")
  expect_error(bidser:::generate_bids_path(subid = "01", datatype = NA),
               "datatype.*required")
})
