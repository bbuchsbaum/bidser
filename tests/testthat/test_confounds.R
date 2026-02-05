context("confound_set and confound_strategy")
library(testthat)
library(bidser)

# --- confound_set tests ---

test_that("confound_set returns correct motion6 variables", {
  m6 <- confound_set("motion6")
  expect_equal(length(m6), 6)
  expect_true(all(c("trans_x", "trans_y", "trans_z",
                     "rot_x", "rot_y", "rot_z") %in% m6))
})

test_that("confound_set returns 24 variables for motion24", {
  m24 <- confound_set("motion24")
  expect_equal(length(m24), 24)
  expect_true("trans_x_derivative1" %in% m24)
  expect_true("rot_z_power2" %in% m24)
  expect_true("trans_x_derivative1_power2" %in% m24)
})

test_that("confound_set 9p has 9 variables", {
  p9 <- confound_set("9p")
  expect_equal(length(p9), 9)
  expect_true(all(c("trans_x", "csf", "white_matter", "global_signal") %in% p9))
})

test_that("confound_set 36p has 36 variables", {
  p36 <- confound_set("36p")
  expect_equal(length(p36), 36)
})

test_that("confound_set is case-insensitive", {
  expect_equal(confound_set("Motion6"), confound_set("motion6"))
  expect_equal(confound_set("MOTION24"), confound_set("motion24"))
})

test_that("confound_set acompcor respects n", {
  acc <- confound_set("acompcor", n = 6)
  expect_equal(acc, "a_comp_cor_*[6]")
  acc_all <- confound_set("acompcor")
  expect_equal(acc_all, "a_comp_cor_*")
})

test_that("confound_set errors on unknown set", {
  expect_error(confound_set("nonexistent"), "Unknown confound set")
})

test_that("list_confound_sets returns a data.frame with expected columns", {
  df <- list_confound_sets()
  expect_s3_class(df, "data.frame")
  expect_true(all(c("set", "description") %in% names(df)))
  expect_true("9p" %in% df$set)
})

# --- confound_strategy tests ---

test_that("confound_strategy creates pcabasic80 object", {
  strat <- confound_strategy("pcabasic80")
  expect_s3_class(strat, "confound_strategy")
  expect_equal(strat$name, "pcabasic80")
  expect_equal(strat$perc_var, 80)
  expect_true(length(strat$pca_vars) > 0)
  expect_true(length(strat$raw_vars) > 0)
  # Should include motion vars and compcor wildcards
  expect_true("trans_x" %in% strat$pca_vars)
  expect_true("csf" %in% strat$pca_vars)
  # Cosine should be in raw_vars
  expect_true(any(grepl("cosine", strat$raw_vars)))
})

test_that("confound_strategy custom works", {
  strat <- confound_strategy(
    pca_vars = c("trans_x", "trans_y"),
    raw_vars = c("csf"),
    npcs = 2
  )
  expect_s3_class(strat, "confound_strategy")
  expect_equal(strat$name, "custom")
  expect_equal(strat$npcs, 2)
  expect_equal(strat$pca_vars, c("trans_x", "trans_y"))
  expect_equal(strat$raw_vars, "csf")
})

test_that("confound_strategy errors on unknown name", {
  expect_error(confound_strategy("nonexistent"), "Unknown confound strategy")
})

test_that("confound_strategy errors when pca_vars missing", {
  expect_error(confound_strategy(raw_vars = "csf"), "pca_vars must be specified")
})

test_that("list_confound_strategies returns a data.frame", {
  df <- list_confound_strategies()
  expect_s3_class(df, "data.frame")
  expect_true("pcabasic80" %in% df$strategy)
})


# --- read_confounds with strategy ---

# Helper to create a richer confounds file for strategy testing
create_rich_confounds_proj <- function() {
  temp_dir <- tempfile("bids_strat_")
  dir.create(temp_dir)
  readr::write_tsv(tibble::tibble(participant_id = "01"),
                   file.path(temp_dir, "participants.tsv"))
  jsonlite::write_json(list(Name = "TestStrategy", BIDSVersion = "1.7.0"),
                       file.path(temp_dir, "dataset_description.json"),
                       auto_unbox = TRUE)
  dir.create(file.path(temp_dir, "sub-01"))
  conf_dir <- file.path(temp_dir, "derivatives", "fmriprep", "sub-01", "func")
  dir.create(conf_dir, recursive = TRUE)

  n <- 20
  set.seed(42)
  conf_data <- tibble::tibble(
    CSF = rnorm(n),
    WhiteMatter = rnorm(n),
    GlobalSignal = rnorm(n),
    FramewiseDisplacement = abs(rnorm(n, 0, 0.1)),
    X = rnorm(n), Y = rnorm(n), Z = rnorm(n),
    RotX = rnorm(n), RotY = rnorm(n), RotZ = rnorm(n),
    X_derivative1 = rnorm(n), Y_derivative1 = rnorm(n), Z_derivative1 = rnorm(n),
    RotX_derivative1 = rnorm(n), RotY_derivative1 = rnorm(n), RotZ_derivative1 = rnorm(n),
    X_power2 = rnorm(n), Y_power2 = rnorm(n), Z_power2 = rnorm(n),
    RotX_power2 = rnorm(n), RotY_power2 = rnorm(n), RotZ_power2 = rnorm(n),
    X_derivative1_power2 = rnorm(n), Y_derivative1_power2 = rnorm(n),
    Z_derivative1_power2 = rnorm(n),
    RotX_derivative1_power2 = rnorm(n), RotY_derivative1_power2 = rnorm(n),
    RotZ_derivative1_power2 = rnorm(n),
    aCompCor00 = rnorm(n), aCompCor01 = rnorm(n), aCompCor02 = rnorm(n),
    tCompCor00 = rnorm(n), tCompCor01 = rnorm(n), tCompCor02 = rnorm(n),
    cosine_00 = rnorm(n), cosine_01 = rnorm(n), cosine_02 = rnorm(n)
  )

  conf_file <- file.path(conf_dir,
                         "sub-01_task-test_run-01_desc-confounds_timeseries.tsv")
  readr::write_tsv(conf_data, conf_file)
  list(path = temp_dir, proj = bids_project(temp_dir, fmriprep = TRUE), n = n)
}


test_that("read_confounds works with confound_strategy object", {
  setup <- create_rich_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)

  strat <- confound_strategy(
    pca_vars = c("csf", "white_matter", "trans_x", "trans_y", "trans_z",
                 "rot_x", "rot_y", "rot_z"),
    raw_vars = c("cosine_*"),
    perc_var = 80
  )
  conf <- read_confounds(setup$proj, cvars = strat, nest = FALSE)
  expect_s3_class(conf, "tbl_df")
  expect_equal(nrow(conf), setup$n)
  # Should have PC columns from PCA
  pc_cols <- grep("^PC", names(conf), value = TRUE)
  expect_true(length(pc_cols) > 0)
  # Should have cosine columns (raw)
  cos_cols <- grep("^cosine", names(conf), value = TRUE)
  expect_true(length(cos_cols) > 0)
  # Should have metadata columns
  expect_true(all(c("participant_id", "run", "session") %in% names(conf)))
})


test_that("read_confounds with strategy: PCA reduces columns", {
  setup <- create_rich_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)

  strat <- confound_strategy(
    pca_vars = c("csf", "white_matter", "trans_x", "trans_y", "trans_z",
                 "rot_x", "rot_y", "rot_z"),
    npcs = 3
  )
  conf <- read_confounds(setup$proj, cvars = strat, nest = FALSE)
  pc_cols <- grep("^PC", names(conf), value = TRUE)
  expect_equal(length(pc_cols), 3)
})


test_that("read_confounds with confound_set still works", {
  setup <- create_rich_confounds_proj()
  on.exit(unlink(setup$path, recursive = TRUE, force = TRUE), add = TRUE)

  conf <- read_confounds(setup$proj, cvars = confound_set("motion6"), nest = FALSE)
  expect_s3_class(conf, "tbl_df")
  expect_equal(nrow(conf), setup$n)
  # Should have 6 motion columns plus metadata
  data_cols <- setdiff(names(conf), c("participant_id", "run", "session"))
  expect_equal(length(data_cols), 6)
})
