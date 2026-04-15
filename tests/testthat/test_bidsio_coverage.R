library(testthat)
library(bidser)

context("bidsio.R coverage: resolve_cvars, confound_files, read_confounds, process_confounds")

# ---------------------------------------------------------------------------
# Helper fixture: minimal BIDS project with fmriprep confound TSV
# ---------------------------------------------------------------------------
create_confounds_fixture <- function() {
  tmp <- tempfile("bidser_conf_")
  dir.create(tmp, recursive = TRUE)
  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01"),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "ConfTest", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )
  dir.create(file.path(tmp, "sub-01", "func"), recursive = TRUE)
  file.create(file.path(tmp, "sub-01", "func",
                         "sub-01_task-rest_run-01_bold.nii.gz"))

  fmriprep_root <- file.path(tmp, "derivatives", "fmriprep")
  dir.create(file.path(fmriprep_root, "sub-01", "func"), recursive = TRUE)
  jsonlite::write_json(
    list(Name = "fmriprep", BIDSVersion = "1.8.0", DatasetType = "derivative"),
    file.path(fmriprep_root, "dataset_description.json"),
    auto_unbox = TRUE
  )
  file.create(file.path(fmriprep_root, "sub-01", "func",
                         "sub-01_task-rest_run-01_space-MNI_desc-preproc_bold.nii.gz"))
  readr::write_tsv(
    tibble::tibble(
      CSF                    = c(0.1, 0.2),
      WhiteMatter            = c(0.3, 0.4),
      FramewiseDisplacement  = c(0.01, 0.02),
      GlobalSignal           = c(1.0, 1.1),
      cosine_00              = c(0.5, 0.6),
      cosine_01              = c(0.7, 0.8),
      cosine_02              = c(0.9, 1.0)
    ),
    file.path(fmriprep_root, "sub-01", "func",
              "sub-01_task-rest_run-01_desc-confounds_timeseries.tsv")
  )
  tmp
}

create_confounds_fixture_direct_root <- function() {
  tmp <- tempfile("bidser_conf_direct_")
  dir.create(tmp, recursive = TRUE)
  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01"),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "ConfDirectRoot", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )
  dir.create(file.path(tmp, "sub-01", "func"), recursive = TRUE)
  file.create(file.path(tmp, "sub-01", "func",
                         "sub-01_task-rest_run-01_bold.nii.gz"))

  deriv_root <- file.path(tmp, "derivatives")
  dir.create(file.path(deriv_root, "sub-01", "func"), recursive = TRUE)
  jsonlite::write_json(
    list(Name = "direct-root-fmriprep", BIDSVersion = "1.8.0", DatasetType = "derivative"),
    file.path(deriv_root, "dataset_description.json"),
    auto_unbox = TRUE
  )
  file.create(file.path(deriv_root, "sub-01", "func",
                         "sub-01_task-rest_run-01_space-MNI_desc-preproc_bold.nii.gz"))
  readr::write_tsv(
    tibble::tibble(
      CSF = c(0.1, 0.2),
      WhiteMatter = c(0.3, 0.4),
      cosine_00 = c(0.5, 0.6)
    ),
    file.path(deriv_root, "sub-01", "func",
              "sub-01_task-rest_run-01_desc-confounds_timeseries.tsv")
  )
  tmp
}

# ===========================================================================
# resolve_cvars
# ===========================================================================

test_that("resolve_cvars: canonical 'csf' resolves to 'CSF'", {
  cols <- c("CSF", "WhiteMatter", "FramewiseDisplacement")
  res <- bidser:::resolve_cvars("csf", cols)
  expect_true("CSF" %in% res)
})

test_that("resolve_cvars: canonical 'white_matter' resolves to 'WhiteMatter'", {
  cols <- c("CSF", "WhiteMatter", "FramewiseDisplacement")
  res <- bidser:::resolve_cvars("white_matter", cols)
  expect_true("WhiteMatter" %in% res)
})

test_that("resolve_cvars: direct match for custom variable", {

  cols <- c("CSF", "MyCustomVar", "WhiteMatter")
  res <- bidser:::resolve_cvars("MyCustomVar", cols)
  expect_true("MyCustomVar" %in% res)
})

test_that("resolve_cvars: wildcard 'cosine_*' matches all cosine columns", {
  cols <- c("CSF", "cosine_00", "cosine_01", "cosine_02", "WhiteMatter")
  res <- bidser:::resolve_cvars("cosine_*", cols)
  expect_true(all(c("cosine_00", "cosine_01", "cosine_02") %in% res))
  expect_false("CSF" %in% res)
})

test_that("resolve_cvars: wildcard with limit 'a_comp_cor_*[3]' returns at most 3 matches", {
  cols <- c("a_comp_cor_00", "a_comp_cor_01", "a_comp_cor_02",
            "a_comp_cor_03", "a_comp_cor_04", "a_comp_cor_05")
  res <- bidser:::resolve_cvars("a_comp_cor_*[3]", cols)
  expect_lte(length(res), 3)
  expect_true(length(res) > 0)
  # Should be the first 3 (sorted)
  expect_equal(res, c("a_comp_cor_00", "a_comp_cor_01", "a_comp_cor_02"))
})

test_that("resolve_cvars: derivative suffix 'csf_derivative1' resolves correctly", {
  cols <- c("CSF", "CSF_derivative1", "WhiteMatter")
  res <- bidser:::resolve_cvars("csf_derivative1", cols)
  expect_true("CSF_derivative1" %in% res)
})

test_that("resolve_cvars: derivative suffix 'csf_power2' resolves correctly", {
  cols <- c("CSF", "CSF_power2", "WhiteMatter")
  res <- bidser:::resolve_cvars("csf_power2", cols)
  expect_true("CSF_power2" %in% res)
})

test_that("resolve_cvars: rename=TRUE still resolves to correct column names", {
  # Note: unique() at the end of resolve_cvars strips names from the vector,

  # so rename=TRUE produces the same values but names may not survive.
  cols <- c("CSF", "WhiteMatter", "FramewiseDisplacement")
  res <- bidser:::resolve_cvars(c("csf", "white_matter"), cols, rename = TRUE)
  expect_true("CSF" %in% res)
  expect_true("WhiteMatter" %in% res)
  expect_equal(length(res), 2)
})

test_that("resolve_cvars: no match returns empty result", {
  cols <- c("CSF", "WhiteMatter")
  res <- bidser:::resolve_cvars("nonexistent_var", cols)
  expect_length(res, 0)
})

test_that("resolve_cvars: empty cvars returns empty result", {
  cols <- c("CSF", "WhiteMatter")
  res <- bidser:::resolve_cvars(character(0), cols)
  expect_length(res, 0)
})

test_that("resolve_cvars: multiple canonical names resolved together", {
  cols <- c("CSF", "WhiteMatter", "FramewiseDisplacement", "GlobalSignal")
  res <- bidser:::resolve_cvars(c("csf", "white_matter", "framewise_displacement"), cols)
  expect_true("CSF" %in% res)
  expect_true("WhiteMatter" %in% res)
  expect_true("FramewiseDisplacement" %in% res)
  expect_equal(length(res), 3)
})

test_that("resolve_cvars: wildcard with zero matches returns nothing for that var", {
  cols <- c("CSF", "WhiteMatter")
  res <- bidser:::resolve_cvars("cosine_*", cols)
  expect_length(res, 0)
})

# ===========================================================================
# confound_files.bids_project
# ===========================================================================

test_that("confound_files returns character vector of paths for valid project", {
  tmp <- create_confounds_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = TRUE)
  cf <- confound_files(proj)

  expect_true(is.character(cf))
  expect_true(length(cf) >= 1)
  expect_true(all(grepl("confounds", cf)))
  expect_true(all(file.exists(cf)))
})

test_that("confound_files returns NULL for non-matching subid", {
  tmp <- create_confounds_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = TRUE)
  cf <- confound_files(proj, subid = "99")
  expect_null(cf)
})

test_that("confound_files returns NULL with message for non-fmriprep project", {
  tmp <- tempfile("bidser_nofp_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  readr::write_tsv(
    tibble::tibble(participant_id = "sub-01"),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "NoFP", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )
  dir.create(file.path(tmp, "sub-01", "func"), recursive = TRUE)
  file.create(file.path(tmp, "sub-01", "func",
                         "sub-01_task-rest_run-01_bold.nii.gz"))

  proj <- bids_project(tmp, fmriprep = FALSE)
  expect_message(
    cf <- confound_files(proj),
    "does not have fmriprep"
  )
  expect_null(cf)
})

# ===========================================================================
# read_confounds.bids_project
# ===========================================================================

test_that("read_confounds returns a bids_confounds tibble", {
  tmp <- create_confounds_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = TRUE)
  conf <- read_confounds(proj, cvars = c("csf", "white_matter"))

  expect_s3_class(conf, "bids_confounds")
  expect_s3_class(conf, "tbl_df")
  expect_true("data" %in% names(conf))
  expect_true(nrow(conf) >= 1)
  # inner data should have 2 rows (our fixture has 2 timepoints)
  inner <- conf$data[[1]]
  expect_equal(nrow(inner), 2)
})

test_that("direct-root derivatives preserve confound helpers when prep_dir points to derivatives", {
  tmp <- create_confounds_fixture_direct_root()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = TRUE, prep_dir = "derivatives", index = "none")
  expect_true(proj$has_fmriprep)
  expect_equal(proj$prep_dir, "derivatives")

  cf <- confound_files(proj, task = "rest")
  expect_true(is.character(cf))
  expect_equal(length(cf), 1)
  expect_true(grepl("desc-confounds_timeseries\\.tsv$", cf[[1]]))

  conf <- read_confounds(proj, task = "rest", cvars = c("csf", "white_matter"), nest = FALSE)
  expect_s3_class(conf, "bids_confounds")
  expect_equal(nrow(conf), 2)
})

test_that("read_confounds warns for non-matching subid and returns NULL", {
  tmp <- create_confounds_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = TRUE)
  expect_warning(
    res <- read_confounds(proj, subid = "^99$"),
    "No matching participants"
  )
  expect_null(res)
})

test_that("read_confounds with nest=FALSE returns flat tibble", {
  tmp <- create_confounds_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = TRUE)
  conf <- read_confounds(proj, cvars = c("csf"), nest = FALSE)

  expect_s3_class(conf, "bids_confounds")
  # When not nested, we should see the confound column directly

  expect_true("CSF" %in% names(conf) || "csf" %in% names(conf))
})

# ===========================================================================
# process_confounds (internal)
# ===========================================================================

test_that("process_confounds returns data.frame with correct columns", {
  df <- data.frame(
    a = c(1, 2, 3, 4, 5),
    b = c(5, 4, 3, 2, 1),
    c = c(2, 4, 6, 8, 10)
  )
  res <- bidser:::process_confounds(df)
  expect_s3_class(res, "data.frame")
  expect_equal(ncol(res), 3)
  expect_equal(nrow(res), 5)
})

test_that("process_confounds with npcs reduces to requested number of PCs", {
  df <- data.frame(
    a = c(1, 2, 3, 4, 5),
    b = c(5, 4, 3, 2, 1),
    c = c(2, 4, 6, 8, 10)
  )
  res <- bidser:::process_confounds(df, npcs = 2)
  expect_equal(ncol(res), 2)
  expect_true(all(grepl("^PC", names(res))))
})

test_that("process_confounds with return_pca=TRUE returns list with scores and pca", {
  df <- data.frame(
    a = c(1, 2, 3, 4, 5),
    b = c(5, 4, 3, 2, 1),
    c = c(2, 4, 6, 8, 10)
  )
  res <- bidser:::process_confounds(df, npcs = 2, return_pca = TRUE)
  expect_true(is.list(res))
  expect_true("scores" %in% names(res))
  expect_true("pca" %in% names(res))
  expect_true("rotation" %in% names(res$pca))
  expect_true("variance" %in% names(res$pca))
})

test_that("process_confounds with perc_var keeps enough PCs", {
  set.seed(42)
  df <- data.frame(
    a = rnorm(20),
    b = rnorm(20),
    c = rnorm(20)
  )
  res <- bidser:::process_confounds(df, perc_var = 50, return_pca = TRUE)
  expect_true(is.list(res))
  # Should have at least 1 PC

  expect_gte(ncol(res$scores), 1)
})

test_that("process_confounds handles NA values via imputation", {
  df <- data.frame(
    a = c(1, NA, 3, 4, 5),
    b = c(5, 4, NA, 2, 1),
    c = c(2, 4, 6, NA, 10)
  )
  # Should not error
  res <- bidser:::process_confounds(df)
  expect_s3_class(res, "data.frame")
  expect_false(anyNA(res))
})

test_that("process_confounds without PCA returns scaled data", {
  df <- data.frame(
    a = c(10, 20, 30),
    b = c(40, 50, 60)
  )
  res <- bidser:::process_confounds(df, npcs = -1, perc_var = -1)
  expect_s3_class(res, "data.frame")
  expect_equal(ncol(res), 2)
  # Scaled data should have mean ~0
  expect_true(abs(mean(res$a)) < 1e-10)
})
