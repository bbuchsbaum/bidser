# tests/testthat/test_build_subject_graph.R

library(testthat)
library(tibble)
library(dplyr)
library(bidser)

context("build_subject_graph() Function")

# --- Test Data Setup ---
participants_df <- tibble::tibble(participant_id = c("01", "02"), age = c(25, 30))

# Define comprehensive file structure for subject graph testing
file_structure_df <- tibble::tribble(
  ~subid, ~session, ~datatype, ~task,   ~run, ~suffix,                                    ~fmriprep, ~desc,       ~space,               ~from,      ~to,
  # Raw data
  "01",   NA,       "anat",    NA,      NA,   "T1w.nii.gz",                               FALSE,     NA,          NA,                   NA,         NA,
  "01",   NA,       "func",    "rest",  "01", "bold.nii.gz",                              FALSE,     NA,          NA,                   NA,         NA,
  "01",   NA,       "func",    "rest",  "02", "bold.nii.gz",                              FALSE,     NA,          NA,                   NA,         NA,
  "01",   NA,       "func",    "task",  "01", "bold.nii.gz",                              FALSE,     NA,          NA,                   NA,         NA,
  "02",   NA,       "anat",    NA,      NA,   "T1w.nii.gz",                               FALSE,     NA,          NA,                   NA,         NA,
  # Preprocessed EPI
  "01",   NA,       "func",    "rest",  "01", "bold.nii.gz",                              TRUE,      "preproc",   "MNI152NLin2009cAsym", NA,         NA,
  "01",   NA,       "func",    "rest",  "02", "bold.nii.gz",                              TRUE,      "preproc",   "MNI152NLin2009cAsym", NA,         NA,
  "01",   NA,       "func",    "task",  "01", "bold.nii.gz",                              TRUE,      "preproc",   "MNI152NLin2009cAsym", NA,         NA,
  # Confound files
  "01",   NA,       "func",    "rest",  "01", "timeseries.tsv",                           TRUE,      "confounds", NA,                   NA,         NA,
  "01",   NA,       "func",    "rest",  "02", "timeseries.tsv",                           TRUE,      "confounds", NA,                   NA,         NA,
  "01",   NA,       "func",    "task",  "01", "timeseries.tsv",                           TRUE,      "confounds", NA,                   NA,         NA,
  # Anatomical derivatives
  "01",   NA,       "anat",    NA,      NA,   "T1w.nii.gz",                               TRUE,      "preproc",   "MNI152NLin2009cAsym", NA,         NA,
  "01",   NA,       "anat",    NA,      NA,   "brainmask.nii.gz",                         TRUE,      NA,          "MNI152NLin2009cAsym", NA,         NA,
  "01",   NA,       "anat",    NA,      NA,   "brainmask.nii.gz",                         TRUE,      NA,          "T1w",                NA,         NA,
  # Transform files
  "01",   NA,       "anat",    NA,      NA,   "xfm.h5",                                   TRUE,      NA,          NA,                   "T1w",      "MNI152NLin2009cAsym",
  "01",   NA,       "anat",    NA,      NA,   "xfm.h5",                                   TRUE,      NA,          NA,                   "MNI152NLin2009cAsym", "T1w",
  "01",   NA,       "anat",    NA,      NA,   "xfm.txt",                                  TRUE,      NA,          NA,                   "fsnative", "T1w",
  # Surface files
  "01",   NA,       "anat",    NA,      NA,   "pial.L.surf.gii",                          TRUE,      NA,          "fsnative",           NA,         NA,
  "01",   NA,       "anat",    NA,      NA,   "pial.R.surf.gii",                          TRUE,      NA,          "fsnative",           NA,         NA,
  "01",   NA,       "anat",    NA,      NA,   "white.L.surf.gii",                         TRUE,      NA,          "fsnative",           NA,         NA,
  "01",   NA,       "anat",    NA,      NA,   "white.R.surf.gii",                         TRUE,      NA,          "fsnative",           NA,         NA,
  "01",   NA,       "anat",    NA,      NA,   "pial.L.surf.gii",                          TRUE,      NA,          "fsaverage",          NA,         NA,
  "01",   NA,       "anat",    NA,      NA,   "pial.R.surf.gii",                          TRUE,      NA,          "fsaverage",          NA,         NA
)

# Define confound data
confound_filename_1 <- bidser:::generate_bids_filename(subid = "01", task = "rest", run = "01", suffix = "timeseries.tsv", desc = "confounds")
confound_filename_2 <- bidser:::generate_bids_filename(subid = "01", task = "rest", run = "02", suffix = "timeseries.tsv", desc = "confounds")
confound_filename_3 <- bidser:::generate_bids_filename(subid = "01", task = "task", run = "01", suffix = "timeseries.tsv", desc = "confounds")

confound_relpath_1 <- file.path("derivatives", "fmriprep", "sub-01", "func", confound_filename_1)
confound_relpath_2 <- file.path("derivatives", "fmriprep", "sub-01", "func", confound_filename_2)
confound_relpath_3 <- file.path("derivatives", "fmriprep", "sub-01", "func", confound_filename_3)

confound_data_list <- list()
confound_data_list[[confound_relpath_1]] <- tibble::tibble(CSF = c(0.1, 0.2), WhiteMatter = c(0.3, 0.4))
confound_data_list[[confound_relpath_2]] <- tibble::tibble(CSF = c(0.1, 0.2), WhiteMatter = c(0.3, 0.4))
confound_data_list[[confound_relpath_3]] <- tibble::tibble(CSF = c(0.1, 0.2), WhiteMatter = c(0.3, 0.4))

# --- Tests ---
test_that("build_subject_graph returns correct structure", {
  mock_proj <- create_mock_bids(
    project_name = "GraphTest",
    participants = participants_df,
    file_structure = file_structure_df,
    confound_data = confound_data_list,
    prep_dir = "derivatives/fmriprep"
  )

  graph <- build_subject_graph(mock_proj, "01")

  expect_s3_class(graph, "bids_subject_graph")
  expect_type(graph, "list")
  expect_named(graph, c("subid", "sessions", "epi", "anat", "transforms", "surfaces", "confounds"))
})

test_that("build_subject_graph contains subject ID", {
  mock_proj <- create_mock_bids(
    project_name = "GraphTest",
    participants = participants_df,
    file_structure = file_structure_df,
    confound_data = confound_data_list,
    prep_dir = "derivatives/fmriprep"
  )

  graph <- build_subject_graph(mock_proj, "01")

  expect_equal(graph$subid, "01")
})

test_that("build_subject_graph handles sub- prefix", {
  mock_proj <- create_mock_bids(
    project_name = "GraphTest",
    participants = participants_df,
    file_structure = file_structure_df,
    confound_data = confound_data_list,
    prep_dir = "derivatives/fmriprep"
  )

  graph <- build_subject_graph(mock_proj, "sub-01")

  expect_equal(graph$subid, "01")
})

test_that("build_subject_graph organizes EPI by task.run", {
  mock_proj <- create_mock_bids(
    project_name = "GraphTest",
    participants = participants_df,
    file_structure = file_structure_df,
    confound_data = confound_data_list,
    prep_dir = "derivatives/fmriprep"
  )

  graph <- build_subject_graph(mock_proj, "01")

  expect_type(graph$epi, "list")
  # Should have rest.01, rest.02, task.01
  expect_true(length(graph$epi) >= 3)
  expect_true(any(grepl("rest", names(graph$epi))))
  expect_true(any(grepl("task", names(graph$epi))))
})

test_that("build_subject_graph contains anatomical data", {
  mock_proj <- create_mock_bids(
    project_name = "GraphTest",
    participants = participants_df,
    file_structure = file_structure_df,
    confound_data = confound_data_list,
    prep_dir = "derivatives/fmriprep"
  )

  graph <- build_subject_graph(mock_proj, "01")

  expect_type(graph$anat, "list")
  expect_named(graph$anat, c("t1w", "masks"))
})

test_that("build_subject_graph contains masks", {
  mock_proj <- create_mock_bids(
    project_name = "GraphTest",
    participants = participants_df,
    file_structure = file_structure_df,
    confound_data = confound_data_list,
    prep_dir = "derivatives/fmriprep"
  )

  graph <- build_subject_graph(mock_proj, "01")

  expect_true(length(graph$anat$masks) >= 2)
  expect_true(all(grepl("brainmask", graph$anat$masks)))
})

test_that("build_subject_graph organizes transforms by from_to", {
  mock_proj <- create_mock_bids(
    project_name = "GraphTest",
    participants = participants_df,
    file_structure = file_structure_df,
    confound_data = confound_data_list,
    prep_dir = "derivatives/fmriprep"
  )

  graph <- build_subject_graph(mock_proj, "01")

  expect_type(graph$transforms, "list")
  # Should have transforms keyed by from_to_to
  expect_true(length(graph$transforms) >= 1)
  expect_true(any(grepl("_to_", names(graph$transforms))))
})

test_that("build_subject_graph organizes surfaces by space and hemisphere", {
  mock_proj <- create_mock_bids(
    project_name = "GraphTest",
    participants = participants_df,
    file_structure = file_structure_df,
    confound_data = confound_data_list,
    prep_dir = "derivatives/fmriprep"
  )

  graph <- build_subject_graph(mock_proj, "01")

  expect_type(graph$surfaces, "list")
  # Should have fsnative and fsaverage
  expect_true(any(grepl("fsnative|fsaverage", names(graph$surfaces))))

  # Each space should have L and R hemispheres
  if (length(graph$surfaces) > 0) {
    first_space <- graph$surfaces[[1]]
    expect_type(first_space, "list")
    expect_true(any(c("L", "R") %in% names(first_space)))
  }
})

test_that("build_subject_graph contains confound files", {
  mock_proj <- create_mock_bids(
    project_name = "GraphTest",
    participants = participants_df,
    file_structure = file_structure_df,
    confound_data = confound_data_list,
    prep_dir = "derivatives/fmriprep"
  )

  graph <- build_subject_graph(mock_proj, "01")

  expect_type(graph$confounds, "character")
  expect_true(length(graph$confounds) >= 1)
})

test_that("build_subject_graph errors for non-existent subject", {
  mock_proj <- create_mock_bids(
    project_name = "GraphTest",
    participants = participants_df,
    file_structure = file_structure_df,
    confound_data = confound_data_list,
    prep_dir = "derivatives/fmriprep"
  )

  expect_error(build_subject_graph(mock_proj, "99"), "Subject not found")
})

test_that("build_subject_graph flatten returns tibble", {
  mock_proj <- create_mock_bids(
    project_name = "GraphTest",
    participants = participants_df,
    file_structure = file_structure_df,
    confound_data = confound_data_list,
    prep_dir = "derivatives/fmriprep"
  )

  flat <- build_subject_graph(mock_proj, "01", flatten = TRUE)

  expect_s3_class(flat, "tbl_df")
  expect_true("file_type" %in% names(flat))
  expect_true("path" %in% names(flat))
  expect_true("subid" %in% names(flat))
})

test_that("build_subject_graph flatten contains all file types", {
  mock_proj <- create_mock_bids(
    project_name = "GraphTest",
    participants = participants_df,
    file_structure = file_structure_df,
    confound_data = confound_data_list,
    prep_dir = "derivatives/fmriprep"
  )

  flat <- build_subject_graph(mock_proj, "01", flatten = TRUE)

  file_types <- unique(flat$file_type)
  expect_true("epi" %in% file_types)
  expect_true("mask" %in% file_types)
  expect_true("transform" %in% file_types)
  expect_true("surface" %in% file_types)
})

test_that("build_subject_graph flatten has correct metadata columns", {
  mock_proj <- create_mock_bids(
    project_name = "GraphTest",
    participants = participants_df,
    file_structure = file_structure_df,
    confound_data = confound_data_list,
    prep_dir = "derivatives/fmriprep"
  )

  flat <- build_subject_graph(mock_proj, "01", flatten = TRUE)

  expected_cols <- c("file_type", "path", "subid", "session", "task", "run", "space", "hemi", "from", "to")
  expect_true(all(expected_cols %in% names(flat)))
})

test_that("build_subject_graph works for subject with minimal data", {
  mock_proj <- create_mock_bids(
    project_name = "GraphTest",
    participants = participants_df,
    file_structure = file_structure_df,
    confound_data = confound_data_list,
    prep_dir = "derivatives/fmriprep"
  )

  # Subject 02 has minimal data
  graph <- build_subject_graph(mock_proj, "02")

  expect_s3_class(graph, "bids_subject_graph")
  expect_equal(graph$subid, "02")
  # Subject 02 has fewer files
  expect_true(length(graph$epi) == 0 || length(graph$epi) < 3)
})
