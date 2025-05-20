context("read_confounds.bids_project")
library(testthat)
library(bidser)
library(tibble)

# Helper to create a temporary BIDS project with a simple confounds file
create_confounds_proj <- function() {
  temp_dir <- tempfile("bids_conf_")
  dir.create(temp_dir)
  # participants and dataset description
  readr::write_tsv(tibble(participant_id = "01"),
                   file.path(temp_dir, "participants.tsv"))
  jsonlite::write_json(list(Name = "TestConfounds", BIDSVersion = "1.7.0"),
                       file.path(temp_dir, "dataset_description.json"),
                       auto_unbox = TRUE)
  # add minimal subject directory to satisfy bids_project parser
  dir.create(file.path(temp_dir, "sub-01"))
  # confounds file in derivatives
  conf_dir <- file.path(temp_dir, "derivatives", "fmriprep", "sub-01", "func")
  dir.create(conf_dir, recursive = TRUE)
  conf_data <- tibble(CSF = c(0.1, 0.2, 0.3),
                      WhiteMatter = c(0.5, 0.6, 0.7),
                      FramewiseDisplacement = c(0.01, 0.02, 0.03),
                      GlobalSignal = c(1.1, 1.2, 1.3))
  conf_file <- file.path(conf_dir,
                         "sub-01_task-test_run-01_desc-confounds_timeseries.tsv")
  readr::write_tsv(conf_data, conf_file)
  list(path = temp_dir,
       proj = bids_project(temp_dir, fmriprep = TRUE))
}


test_that("valid confound file is read and nested", {
  setup <- create_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)
  conf <- read_confounds(setup$proj)
  expect_s3_class(conf, "tbl_df")
  expect_equal(nrow(conf), 1)
  expect_equal(names(conf), c("participant_id", "run", "session", "data"))
  expect_equal(nrow(conf$data[[1]]), 3)
})


test_that("missing confound files return NULL", {
  setup <- create_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)
  res <- read_confounds(setup$proj, subid = "99")
  expect_null(res)
})


test_that("PCA reduction works and flat output returned", {
  setup <- create_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)
  flat <- read_confounds(setup$proj, npcs = 2, nest = FALSE)
  expect_true(all(c("PC1", "PC2") %in% names(flat)))
  expect_equal(nrow(flat), 3)
})


test_that("nest=FALSE returns flat tibble", {
  setup <- create_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)
  flat <- read_confounds(setup$proj, nest = FALSE)
  expect_equal(nrow(flat), 3)
  expect_true(all(c("participant_id", "run", "session") %in% names(flat)))
})

test_that("canonical names resolve to dataset columns", {
  setup <- create_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)
  conf <- read_confounds(setup$proj,
                         cvars = c("csf", "white_matter", "framewise_displacement", "global_signal"))
  cols <- names(conf$data[[1]])
  expect_true(all(c("CSF", "WhiteMatter", "FramewiseDisplacement", "GlobalSignal") %in% cols))
})

test_that("file without requested confounds is skipped", {
  setup <- create_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)
  expect_warning(res <- read_confounds(setup$proj, cvars = "junkvar"),
                 "No requested confounds")
  expect_null(res)
})
