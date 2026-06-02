context("read_confounds.bids_project")
library(testthat)
library(bidser)

write_fmriprep_description <- function(path) {
  deriv_root <- file.path(path, "derivatives", "fmriprep")
  dir.create(deriv_root, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    list(
      Name = "fMRIPrep",
      BIDSVersion = "1.7.0",
      GeneratedBy = list(list(Name = "fMRIPrep", Version = "21.0.0"))
    ),
    file.path(deriv_root, "dataset_description.json"),
    auto_unbox = TRUE
  )
}

# Helper to create a temporary BIDS project with a simple confounds file
create_confounds_proj <- function() {
  temp_dir <- tempfile("bids_conf_")
  dir.create(temp_dir)
  # participants and dataset description
  readr::write_tsv(tibble::tibble(participant_id = "01"),
                   file.path(temp_dir, "participants.tsv"))
  jsonlite::write_json(list(Name = "TestConfounds", BIDSVersion = "1.7.0"),
                       file.path(temp_dir, "dataset_description.json"),
                       auto_unbox = TRUE)
  # add minimal subject directory to satisfy bids_project parser
  dir.create(file.path(temp_dir, "sub-01"))
  # confounds file in derivatives
  conf_dir <- file.path(temp_dir, "derivatives", "fmriprep", "sub-01", "func")
  dir.create(conf_dir, recursive = TRUE)
  conf_data <- tibble::tibble(CSF = c(0.1, 0.2, 0.3),
                              WhiteMatter = c(0.5, 0.6, 0.7),
                              FramewiseDisplacement = c(0.01, 0.02, 0.03),
                              GlobalSignal = c(1.1, 1.2, 1.3))
  conf_file <- file.path(conf_dir,
                         "sub-01_task-test_run-01_desc-confounds_timeseries.tsv")
  readr::write_tsv(conf_data, conf_file)
  write_fmriprep_description(temp_dir)
  list(path = temp_dir,
       proj = bids_project(temp_dir, fmriprep = TRUE))
}


create_zero_variance_confounds_proj <- function() {
  temp_dir <- tempfile("bids_conf_zv_")
  dir.create(temp_dir)
  readr::write_tsv(tibble::tibble(participant_id = "01"),
                   file.path(temp_dir, "participants.tsv"))
  jsonlite::write_json(list(Name = "TestZeroVarianceConfounds", BIDSVersion = "1.7.0"),
                       file.path(temp_dir, "dataset_description.json"),
                       auto_unbox = TRUE)
  dir.create(file.path(temp_dir, "sub-01"))
  conf_dir <- file.path(temp_dir, "derivatives", "fmriprep", "sub-01", "func")
  dir.create(conf_dir, recursive = TRUE)
  conf_data <- tibble::tibble(
    cosine00 = c(-1, 0, 1, 2),
    cosine01 = c(0, 0, 0, 0),
    cosine02 = c(2, 1, 0, -1),
    t_comp_cor_02 = c(5, 5, 5, 5)
  )
  conf_file <- file.path(conf_dir,
                         "sub-01_task-test_run-01_desc-confounds_timeseries.tsv")
  readr::write_tsv(conf_data, conf_file)
  write_fmriprep_description(temp_dir)
  list(path = temp_dir,
       proj = bids_project(temp_dir, fmriprep = TRUE))
}


create_na_confounds_proj <- function() {
  temp_dir <- tempfile("bids_conf_na_")
  dir.create(temp_dir)
  readr::write_tsv(tibble::tibble(participant_id = "01"),
                   file.path(temp_dir, "participants.tsv"))
  jsonlite::write_json(list(Name = "TestNaConfounds", BIDSVersion = "1.7.0"),
                       file.path(temp_dir, "dataset_description.json"),
                       auto_unbox = TRUE)
  dir.create(file.path(temp_dir, "sub-01"))
  conf_dir <- file.path(temp_dir, "derivatives", "fmriprep", "sub-01", "func")
  dir.create(conf_dir, recursive = TRUE)
  conf_data <- tibble::tibble(
    framewise_displacement = c(NA, 0.2, 0.4),
    trans_x = c(1, 2, 3),
    trans_x_derivative1 = c(NA, 0.1, 0.3),
    cosine00 = c(-1, 0, 1)
  )
  conf_file <- file.path(conf_dir,
                         "sub-01_task-test_run-01_desc-confounds_timeseries.tsv")
  readr::write_tsv(conf_data, conf_file)
  write_fmriprep_description(temp_dir)
  list(path = temp_dir,
       proj = bids_project(temp_dir, fmriprep = TRUE))
}


test_that("valid confound file is read and nested", {
  setup <- create_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)
  conf <- read_confounds(setup$proj)
  expect_s3_class(conf, "bids_confounds")
  expect_s3_class(conf, "tbl_df")
  expect_equal(nrow(conf), 1)
  expect_true(all(c("participant_id", "task", "run", "session", "data") %in% names(conf)))
  expect_equal(nrow(conf$data[[1]]), 3)
})


test_that("missing confound files raise an informative error", {
  setup <- create_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)
  expect_error(
    read_confounds(setup$proj, subid = "99"),
    "found no participants matching the requested subject filter"
  )
})


test_that("PCA reduction works and flat output returned", {
  setup <- create_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)
  flat <- read_confounds(setup$proj, npcs = 2, nest = FALSE)
  expect_true(all(c("PC1", "PC2") %in% names(flat)))
  expect_equal(nrow(flat), 3)
  expect_false(is.null(attr(flat, "pca")))
})


test_that("nest=FALSE returns flat tibble", {
  setup <- create_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)
  flat <- read_confounds(setup$proj, nest = FALSE)
  expect_equal(nrow(flat), 3)
  expect_true(all(c("participant_id", "task", "run", "session") %in% names(flat)))
})

test_that("canonical names resolve to dataset columns", {
  setup <- create_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)
  conf <- read_confounds(setup$proj,
                         cvars = c("csf", "white_matter", "framewise_displacement", "global_signal"))
  cols <- names(conf$data[[1]])
  expect_true(all(c("CSF", "WhiteMatter", "FramewiseDisplacement", "GlobalSignal") %in% cols))
})

test_that("file without requested confounds raises an informative error", {
  setup <- create_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)
  expect_warning(
    expect_error(
      read_confounds(setup$proj, cvars = "junkvar"),
      "none contained usable confounds for the requested variables"
    ),
    "No requested confounds"
  )
})

test_that("read_confounds drops zero-variance columns by run and records diagnostics", {
  setup <- create_zero_variance_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)

  expect_message(
    conf <- read_confounds(
      setup$proj,
      cvars = c(confound_set("cosine"), "t_comp_cor_02")
    ),
    "Dropped zero-variance confounds"
  )

  inner_cols <- names(conf$data[[1]])
  expect_true("cosine00" %in% inner_cols)
  expect_true("cosine02" %in% inner_cols)
  expect_false("cosine01" %in% inner_cols)
  expect_false("t_comp_cor_02" %in% inner_cols)

  diag <- attr(conf, "confound_diagnostics")
  expect_s3_class(diag, "tbl_df")
  expect_equal(sort(diag$column), c("cosine01", "t_comp_cor_02"))
  expect_equal(unique(diag$reason), "zero_variance")
  expect_equal(unique(diag$participant_id), "01")
  expect_equal(unique(diag$run), "01")
})

test_that("read_confounds clean='none' preserves selected zero-variance columns", {
  setup <- create_zero_variance_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)

  conf <- read_confounds(
    setup$proj,
    cvars = c(confound_set("cosine"), "t_comp_cor_02"),
    clean = "none"
  )

  inner_cols <- names(conf$data[[1]])
  expect_true(all(c("cosine00", "cosine01", "cosine02", "t_comp_cor_02") %in% inner_cols))
  expect_equal(nrow(attr(conf, "confound_diagnostics")), 0)
})

test_that("read_confounds PCA path ignores zero-variance columns instead of failing", {
  setup <- create_zero_variance_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)

  expect_message(
    flat <- read_confounds(
      setup$proj,
      cvars = confound_set("cosine"),
      npcs = 1,
      nest = FALSE
    ),
    "Dropped zero-variance confounds"
  )

  expect_true("PC1" %in% names(flat))
  expect_false(anyNA(flat$PC1))
  expect_equal(attr(flat, "confound_diagnostics")$column, "cosine01")
})

test_that("read_confounds preserves raw confound NAs by default", {
  setup <- create_na_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)

  conf <- read_confounds(
    setup$proj,
    cvars = c("framewise_displacement", "trans_x_derivative1", "cosine00")
  )

  inner <- conf$data[[1]]
  expect_true(is.na(inner$framewise_displacement[1]))
  expect_true(is.na(inner$trans_x_derivative1[1]))
})

test_that("read_confounds na_action='zero' fills raw confound NAs with zero", {
  setup <- create_na_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)

  conf <- read_confounds(
    setup$proj,
    cvars = c("framewise_displacement", "trans_x_derivative1", "cosine00"),
    na_action = "zero"
  )

  inner <- conf$data[[1]]
  expect_false(anyNA(inner))
  expect_equal(inner$framewise_displacement[1], 0)
  expect_equal(inner$trans_x_derivative1[1], 0)
  expect_equal(inner$cosine00, c(-1, 0, 1))
})

test_that("read_confounds na_action='median' fills raw confound NAs with finite medians", {
  setup <- create_na_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)

  conf <- read_confounds(
    setup$proj,
    cvars = c("framewise_displacement", "trans_x_derivative1"),
    na_action = "median"
  )

  inner <- conf$data[[1]]
  expect_false(anyNA(inner))
  expect_equal(inner$framewise_displacement[1], 0.3)
  expect_equal(inner$trans_x_derivative1[1], 0.2)
})

test_that("read_confounds na_action applies to raw strategy columns", {
  setup <- create_na_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)

  strat <- confound_strategy(
    pca_vars = c("trans_x", "cosine00"),
    raw_vars = c("framewise_displacement", "trans_x_derivative1"),
    npcs = 1
  )
  conf <- read_confounds(setup$proj, cvars = strat, na_action = "zero")

  inner <- conf$data[[1]]
  expect_false(anyNA(inner))
  expect_true("PC1" %in% names(inner))
  expect_equal(inner$framewise_displacement[1], 0)
  expect_equal(inner$trans_x_derivative1[1], 0)
})

test_that("read_confounds mock project supports na_action for API parity", {
  parts <- c("01")
  fs <- tibble::tibble(
    subid = "01",
    datatype = "func",
    suffix = c("bold.nii.gz", "desc-confounds_timeseries.tsv"),
    task = "rest",
    fmriprep = c(TRUE, TRUE)
  )
  key <- "derivatives/fmriprep/sub-01/func/sub-01_task-rest_desc-confounds_timeseries.tsv"
  conf_data <- list()
  conf_data[[key]] <- tibble::tibble(
    framewise_displacement = c(NA, 0.2, 0.4),
    trans_x = c(1, 2, 3)
  )
  mock <- create_mock_bids("ConfNaTest", parts, fs, confound_data = conf_data)

  conf <- read_confounds(
    mock,
    cvars = c("framewise_displacement", "trans_x"),
    na_action = "zero"
  )

  expect_false(anyNA(conf$data[[1]]))
  expect_equal(conf$data[[1]]$framewise_displacement[1], 0)
})
