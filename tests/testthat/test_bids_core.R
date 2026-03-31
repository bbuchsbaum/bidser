context("bids core functions (local fixtures)")

library(testthat)
library(bidser)

# ---------------------------------------------------------------------------
# Helper: create a minimal on-disk BIDS dataset
# ---------------------------------------------------------------------------
create_local_bids_fixture <- function() {
  tmp <- tempfile("bidser_core_")
  dir.create(tmp, recursive = TRUE)

  readr::write_tsv(
    tibble::tibble(participant_id = c("sub-01", "sub-02")),
    file.path(tmp, "participants.tsv")
  )
  jsonlite::write_json(
    list(Name = "TestProject", BIDSVersion = "1.8.0"),
    file.path(tmp, "dataset_description.json"),
    auto_unbox = TRUE
  )

  # sub-01: func + anat

  dir.create(file.path(tmp, "sub-01", "func"), recursive = TRUE)
  dir.create(file.path(tmp, "sub-01", "anat"), recursive = TRUE)
  file.create(file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-01_bold.nii.gz"))
  file.create(file.path(tmp, "sub-01", "func", "sub-01_task-rest_run-01_events.tsv"))
  file.create(file.path(tmp, "sub-01", "anat", "sub-01_T1w.nii.gz"))

  # sub-02: func only

  dir.create(file.path(tmp, "sub-02", "func"), recursive = TRUE)
  file.create(file.path(tmp, "sub-02", "func", "sub-02_task-rest_run-01_bold.nii.gz"))
  file.create(file.path(tmp, "sub-02", "func", "sub-02_task-rest_run-01_events.tsv"))

  tmp
}

# ===========================================================================
# 1. decode_bids_entities
# ===========================================================================
test_that("decode_bids_entities reconstructs a standard filename", {
  entities <- list(
    subid  = "01",
    task   = "rest",
    run    = "01",
    kind   = "bold",
    suffix = "nii.gz"
  )
  fname <- decode_bids_entities(entities)
  expect_true(is.character(fname))
  expect_equal(length(fname), 1L)

  # Must contain key BIDS parts
  expect_true(grepl("sub-01", fname))
  expect_true(grepl("task-rest", fname))
  expect_true(grepl("run-01", fname))
  expect_true(grepl("bold", fname))
  expect_true(grepl("nii\\.gz$", fname))
})

test_that("decode_bids_entities includes desc entity when added", {
  entities <- list(
    subid  = "01",
    task   = "rest",
    run    = "01",
    desc   = "smooth6mm",
    kind   = "bold",
    suffix = "nii.gz"
  )
  fname <- decode_bids_entities(entities)
  expect_true(grepl("desc-smooth6mm", fname))
  expect_true(grepl("sub-01", fname))
})

test_that("decode_bids_entities includes session entity", {
  entities <- list(
    subid   = "01",
    session = "pre",
    task    = "rest",
    kind    = "bold",
    suffix  = "nii.gz"
  )
  fname <- decode_bids_entities(entities)
  expect_true(grepl("ses-pre", fname))
  expect_true(grepl("sub-01", fname))
  # session should appear after sub
  expect_true(grepl("sub-01_ses-pre", fname))
})

test_that("decode_bids_entities errors on NULL or non-list input", {
  expect_error(decode_bids_entities(NULL))
  expect_error(decode_bids_entities("not_a_list"))
  expect_error(decode_bids_entities(42))
})

# ===========================================================================
# 2. set_key (internal)
# ===========================================================================
test_that("set_key sets a new key on a parsed filename", {
  result <- bidser:::set_key("sub-01_task-rest_bold.nii.gz", "desc", "smooth")
  expect_true(is.list(result))
  expect_equal(result$desc, "smooth")
  # Original entities should be preserved
  expect_equal(result$subid, "01")
  expect_equal(result$task, "rest")
})

test_that("set_key overwrites an existing key", {
  result <- bidser:::set_key("sub-01_task-rest_run-01_bold.nii.gz", "run", "99")
  expect_equal(result$run, "99")
  expect_equal(result$task, "rest")
})

# ===========================================================================
# 3. create_smooth_transformer
# ===========================================================================
test_that("create_smooth_transformer returns a function", {
  tr <- create_smooth_transformer(fwhm = 6)
  expect_true(is.function(tr))
})

test_that("create_smooth_transformer produces desc-smooth in output filename", {
  tr <- create_smooth_transformer(fwhm = 6)

  in_dir  <- tempfile("smooth_in_")
  out_dir <- tempfile("smooth_out_")
  dir.create(in_dir, recursive = TRUE)
  dir.create(out_dir, recursive = TRUE)
  on.exit({
    unlink(in_dir, recursive = TRUE, force = TRUE)
    unlink(out_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  infile <- file.path(in_dir, "sub-01_task-rest_bold.nii.gz")
  file.create(infile)

  outfile <- tr(infile, out_dir)
  expect_true(!is.null(outfile))
  expect_true(file.exists(outfile))
  expect_true(grepl("smooth6mm", basename(outfile)))
  expect_true(grepl("desc-smooth6mm", basename(outfile)))
})

test_that("create_smooth_transformer returns NULL for non-matching suffix", {
  tr <- create_smooth_transformer(fwhm = 8, suffix_pattern = "bold\\.nii")

  in_dir  <- tempfile("smooth_nomatch_")
  out_dir <- tempfile("smooth_nomatch_out_")
  dir.create(in_dir, recursive = TRUE)
  dir.create(out_dir, recursive = TRUE)
  on.exit({
    unlink(in_dir, recursive = TRUE, force = TRUE)
    unlink(out_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  # Create a file that does NOT match "bold\\.nii"
  infile <- file.path(in_dir, "sub-01_T1w.nii.gz")
  file.create(infile)

  result <- tr(infile, out_dir)
  expect_null(result)
})

# ===========================================================================
# 4. bids_project constructor
# ===========================================================================
test_that("bids_project creates a valid object from local fixture", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")

  expect_s3_class(proj, "bids_project")
  expect_true(!is.null(proj$bids_tree))
  expect_true(!is.null(proj$path))
  expect_equal(normalizePath(proj$path), normalizePath(tmp))
})

test_that("bids_project finds correct participants", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  parts <- participants(proj)
  expect_true(length(parts) >= 2)
  expect_true("01" %in% parts)
  expect_true("02" %in% parts)
})

test_that("bids_project finds correct tasks", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  t <- tasks(proj)
  expect_true(length(t) >= 1)
  expect_true("rest" %in% t)
})

test_that("bids_project populates bids_tree with leaves", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  # bids_tree should have descendant leaf nodes
  leaves <- proj$bids_tree$Get("name", filterFun = data.tree::isLeaf)
  expect_true(length(leaves) > 0)
})

# ===========================================================================
# 5. print.bids_project
# ===========================================================================
test_that("print.bids_project produces expected output", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")

  output <- capture.output(print(proj))
  output_str <- paste(output, collapse = "\n")

  # Should mention the project somehow and participant count
  expect_true(nchar(output_str) > 0)
  # The print method mentions "Project" or project name
  expect_true(grepl("roject|Participants|participants|Tasks|tasks", output_str, ignore.case = TRUE))
})

test_that("print.bids_project returns the object invisibly", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  ret <- withVisible(print(proj))
  expect_false(ret$visible)
  expect_s3_class(ret$value, "bids_project")
})

# ===========================================================================
# 6. participants.bids_project
# ===========================================================================
test_that("participants returns sorted character vector of IDs without sub- prefix", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  parts <- participants(proj)

  expect_true(is.character(parts))
  expect_true(length(parts) >= 2)
  # IDs should NOT have "sub-" prefix

  expect_false(any(grepl("^sub-", parts)))
  # Should be sorted
  expect_equal(parts, sort(parts))
})

test_that("participants with as_tibble returns a tibble", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  tbl <- participants(proj, as_tibble = TRUE)

  expect_true(tibble::is_tibble(tbl))
  expect_true("participant_id" %in% names(tbl))
  expect_true(nrow(tbl) >= 2)
})

# ===========================================================================
# 7. bids_summary
# ===========================================================================
test_that("bids_summary returns correct structure for local fixture", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  s <- bids_summary(proj)

  expect_true(is.list(s))
  expect_true("n_subjects" %in% names(s))
  expect_true("n_sessions" %in% names(s))
  expect_true("tasks" %in% names(s))
  expect_true("total_runs" %in% names(s))

  expect_equal(s$n_subjects, 2L)
  expect_null(s$n_sessions) # fixture has no sessions
  expect_true(is.data.frame(s$tasks))
  expect_true("rest" %in% s$tasks$task)
  expect_true(s$total_runs > 0)
})

# ===========================================================================
# 8. bids_check_compliance
# ===========================================================================
test_that("bids_check_compliance passes on well-formed local fixture", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  chk <- bids_check_compliance(proj)

  expect_true(is.list(chk))
  expect_true("passed" %in% names(chk))
  expect_true("issues" %in% names(chk))
  expect_true("warnings" %in% names(chk))
  expect_true(is.logical(chk$passed))
  expect_true(chk$passed)
  expect_equal(length(chk$issues), 0L)
})

test_that("bids_check_compliance detects missing dataset_description.json", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")

  # Remove the required file after project creation
  file.remove(file.path(tmp, "dataset_description.json"))

  chk <- bids_check_compliance(proj)
  expect_false(chk$passed)
  expect_true(any(grepl("dataset_description.json", chk$issues)))
})

test_that("bids_check_compliance warns about missing participants.tsv", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")

  # Remove the recommended file after project creation
  file.remove(file.path(tmp, "participants.tsv"))

  chk <- bids_check_compliance(proj)
  expect_true(any(grepl("participants.tsv", chk$warnings)))
})

test_that("bids_check_compliance warns about missing README", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  chk <- bids_check_compliance(proj)

  # Our fixture has no README, so this warning should be present
  expect_true(any(grepl("README", chk$warnings)))
})

test_that("bids_check_compliance detects missing subject directory", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")

  # Remove sub-02 directory after project creation
  unlink(file.path(tmp, "sub-02"), recursive = TRUE)

  chk <- bids_check_compliance(proj)
  expect_false(chk$passed)
  expect_true(any(grepl("Subject directory not found", chk$issues)))
})

# ===========================================================================
# 9. flat_list.bids_project
# ===========================================================================
test_that("flat_list returns a data.frame with full paths", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  fl <- flat_list(proj, full_path = TRUE)

  expect_true(is.data.frame(fl))
  expect_true("path" %in% names(fl))
  expect_true(nrow(fl) > 0)
})

test_that("flat_list with full_path=FALSE returns names", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  fl <- flat_list(proj, full_path = FALSE)

  expect_true(is.data.frame(fl))
  expect_true("name" %in% names(fl))
  expect_true(nrow(fl) > 0)
  # Names should include the actual filenames
  all_names <- fl$name
  expect_true(any(grepl("bold\\.nii\\.gz$", all_names)))
})

# ===========================================================================
# 10. search_files.bids_project
# ===========================================================================
test_that("search_files finds bold files by regex", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  bold_files <- search_files(proj, regex = "bold\\.nii\\.gz$", full_path = FALSE)

  expect_true(length(bold_files) > 0)
  expect_true(all(grepl("bold\\.nii\\.gz$", bold_files)))
})

test_that("search_files returns full paths when requested", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  bold_files <- search_files(proj, regex = "bold\\.nii\\.gz$", full_path = TRUE)

  expect_true(length(bold_files) > 0)
  # Full paths should start with the project path
  expect_true(all(startsWith(bold_files, normalizePath(tmp))))
})

test_that("search_files filters by subid", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  sub01_files <- search_files(proj, regex = ".*", subid = "01", full_path = FALSE)

  expect_true(length(sub01_files) > 0)
  expect_true(all(grepl("sub-01", sub01_files)))
  # Should not contain sub-02 files
  expect_false(any(grepl("sub-02", sub01_files)))
})

test_that("search_files filters by task", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  rest_files <- search_files(proj, regex = ".*", task = "rest", full_path = FALSE)

  expect_true(length(rest_files) > 0)
  expect_true(all(grepl("task-rest", rest_files)))
})

test_that("search_files returns NULL for no matches", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")
  result <- search_files(proj, regex = "nonexistent_file_xyz", full_path = FALSE)

  expect_null(result)
})

# ===========================================================================
# 11. bids_transform
# ===========================================================================
test_that("bids_transform applies a simple copy transformer", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")

  # A minimal transformer that copies the file into the output directory
  copy_transformer <- function(infile, outdir) {
    outfile <- file.path(outdir, basename(infile))
    file.copy(infile, outfile)
    outfile
  }

  new_files <- bids_transform(
    proj, copy_transformer, "test_pipeline",
    regex = "bold\\.nii\\.gz$", task = "rest"
  )

  expect_true(is.character(new_files))
  expect_true(length(new_files) > 0)
  # All output files should actually exist
  expect_true(all(file.exists(new_files)))
  # A derivatives directory should have been created
  expect_true(dir.exists(file.path(tmp, "derivatives", "test_pipeline")))
})

test_that("bids_transform returns empty character when no files match", {
  tmp <- create_local_bids_fixture()
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  proj <- bids_project(tmp, fmriprep = FALSE, index = "none")

  dummy_transformer <- function(infile, outdir) {
    outfile <- file.path(outdir, basename(infile))
    file.copy(infile, outfile)
    outfile
  }

  result <- bids_transform(
    proj, dummy_transformer, "empty_pipeline",
    regex = "absolutely_nothing_matches_this_xyz"
  )

  expect_equal(length(result), 0L)
  expect_true(is.character(result))
})

test_that("bids_transform errors on non-bids_project input", {
  expect_error(bids_transform("not_a_project", identity, "pipe"))
  expect_error(bids_transform(list(), identity, "pipe"))
})
