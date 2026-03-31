library(testthat)
library(bidser)

context("pack_bids.R coverage: is_bids_compliant, parse_file_size, add_resolution_tag")

# ===========================================================================
# is_bids_compliant (internal)
# ===========================================================================

test_that("is_bids_compliant: dataset_description.json is TRUE", {
  expect_true(bidser:::is_bids_compliant("dataset_description.json"))
})

test_that("is_bids_compliant: participants.tsv is TRUE", {
  expect_true(bidser:::is_bids_compliant("participants.tsv"))
})

test_that("is_bids_compliant: README is TRUE", {
  expect_true(bidser:::is_bids_compliant("README"))
})

test_that("is_bids_compliant: .bidsignore is TRUE", {
  expect_true(bidser:::is_bids_compliant(".bidsignore"))
})

test_that("is_bids_compliant: event file matches metadata_patterns", {
  expect_true(bidser:::is_bids_compliant("sub-01_task-rest_events.tsv"))
})

test_that("is_bids_compliant: NIfTI BIDS file parses correctly", {
  expect_true(bidser:::is_bids_compliant("sub-01_task-rest_bold.nii.gz"))
})

test_that("is_bids_compliant: JSON sidecar of valid BIDS file is TRUE", {
  expect_true(bidser:::is_bids_compliant("sub-01_task-rest_bold.json"))
})

test_that("is_bids_compliant: random non-BIDS file is FALSE", {
  expect_false(bidser:::is_bids_compliant("random_file.txt"))
})

test_that("is_bids_compliant: derivative confounds file is TRUE", {
  fp <- "derivatives/fmriprep/sub-01/func/sub-01_task-rest_desc-confounds_timeseries.tsv"
  expect_true(bidser:::is_bids_compliant(fp))
})

test_that("is_bids_compliant: derivative desc-preproc NIfTI is TRUE", {
  fp <- "derivatives/fmriprep/sub-01/func/sub-01_task-rest_desc-preproc_bold.nii.gz"
  expect_true(bidser:::is_bids_compliant(fp))
})

test_that("is_bids_compliant: derivative mask NIfTI is TRUE", {
  fp <- "derivatives/fmriprep/sub-01/anat/sub-01_desc-brain_mask.nii.gz"
  expect_true(bidser:::is_bids_compliant(fp))
})

test_that("is_bids_compliant: README.md is TRUE", {
  expect_true(bidser:::is_bids_compliant("README.md"))
})

test_that("is_bids_compliant: participants.json is TRUE", {
  expect_true(bidser:::is_bids_compliant("participants.json"))
})

test_that("is_bids_compliant: CHANGES is TRUE", {
  expect_true(bidser:::is_bids_compliant("CHANGES"))
})

test_that("is_bids_compliant: LICENSE is TRUE", {
  expect_true(bidser:::is_bids_compliant("LICENSE"))
})

test_that("is_bids_compliant: scans.tsv metadata file is TRUE", {
  expect_true(bidser:::is_bids_compliant("sub-01_scans.tsv"))
})

test_that("is_bids_compliant: non-BIDS JSON is FALSE", {
  # A JSON that does not have a BIDS-parsable base name
  expect_false(bidser:::is_bids_compliant("some_random_notes.json"))
})

test_that("is_bids_compliant: .DS_Store is FALSE", {
  expect_false(bidser:::is_bids_compliant(".DS_Store"))
})

test_that("is_bids_compliant: non-BIDS nii.gz is FALSE", {
  # Does not follow sub-XX_..._suffix.nii.gz pattern
  expect_false(bidser:::is_bids_compliant("myscript_output.nii.gz"))
})

# ===========================================================================
# parse_file_size (internal)
# ===========================================================================

test_that("parse_file_size: NULL returns NULL", {
  expect_null(bidser:::parse_file_size(NULL))
})

test_that("parse_file_size: numeric input returned as-is", {
  expect_equal(bidser:::parse_file_size(1024), 1024)
})

test_that("parse_file_size: '1MB' returns 1024^2", {
  expect_equal(bidser:::parse_file_size("1MB"), 1024^2)
})

test_that("parse_file_size: '500KB' returns 500 * 1024", {
  expect_equal(bidser:::parse_file_size("500KB"), 500 * 1024)
})

test_that("parse_file_size: '1.5GB' returns 1.5 * 1024^3", {
  expect_equal(bidser:::parse_file_size("1.5GB"), 1.5 * 1024^3)
})

test_that("parse_file_size: '100B' returns 100", {
  expect_equal(bidser:::parse_file_size("100B"), 100)
})

test_that("parse_file_size: '2TB' returns 2 * 1024^4", {
  expect_equal(bidser:::parse_file_size("2TB"), 2 * 1024^4)
})

test_that("parse_file_size: case insensitive ('1mb' treated as '1MB')", {
  expect_equal(bidser:::parse_file_size("1mb"), 1024^2)
})

test_that("parse_file_size: short unit 'M' works like 'MB'", {
  expect_equal(bidser:::parse_file_size("10M"), 10 * 1024^2)
})

test_that("parse_file_size: short unit 'K' works like 'KB'", {
  expect_equal(bidser:::parse_file_size("256K"), 256 * 1024)
})

test_that("parse_file_size: short unit 'G' works like 'GB'", {
  expect_equal(bidser:::parse_file_size("4G"), 4 * 1024^3)
})

test_that("parse_file_size: invalid format errors", {
  expect_error(bidser:::parse_file_size("abc"), "Invalid file size")
})

# ===========================================================================
# add_resolution_tag (internal)
# ===========================================================================

test_that("add_resolution_tag: factor 0.25 adds res-low4x", {
  input <- "sub-01_task-rest_bold.nii.gz"
  result <- bidser:::add_resolution_tag(input, 0.25)
  expect_true(grepl("_res-low4x_", result))
  expect_true(grepl("bold\\.nii\\.gz$", result))
})

test_that("add_resolution_tag: factor 0.5 adds res-low2x", {
  input <- "sub-01_task-rest_bold.nii.gz"
  result <- bidser:::add_resolution_tag(input, 0.5)
  expect_true(grepl("_res-low2x_", result))
})

test_that("add_resolution_tag: replaces existing res- tag", {
  input <- "sub-01_task-rest_res-native_bold.nii.gz"
  result <- bidser:::add_resolution_tag(input, 0.25)
  expect_true(grepl("_res-low4x_", result))
  expect_false(grepl("_res-native", result))
})

test_that("add_resolution_tag: preserves directory path", {
  input <- "/data/bids/sub-01/func/sub-01_task-rest_bold.nii.gz"
  result <- bidser:::add_resolution_tag(input, 0.5)
  expect_true(grepl("^/data/bids/sub-01/func/", result))
  expect_true(grepl("_res-low2x_", result))
})

test_that("add_resolution_tag: preserves all entities before suffix", {
  input <- "sub-01_ses-pre_task-rest_run-02_bold.nii.gz"
  result <- bidser:::add_resolution_tag(input, 0.25)
  expect_true(grepl("sub-01", result))
  expect_true(grepl("ses-pre", result))
  expect_true(grepl("task-rest", result))
  expect_true(grepl("run-02", result))
  expect_true(grepl("_res-low4x_", result))
  expect_true(grepl("bold\\.nii\\.gz$", result))
})

test_that("add_resolution_tag: works with anat file", {
  input <- "sub-01_T1w.nii.gz"
  result <- bidser:::add_resolution_tag(input, 0.5)
  expect_true(grepl("_res-low2x_", result))
  expect_true(grepl("T1w\\.nii\\.gz$", result))
})

test_that("add_resolution_tag: current directory path preserved for relative input", {
  input <- "sub-01_task-rest_bold.nii.gz"
  result <- bidser:::add_resolution_tag(input, 0.25)
  # dirname of bare filename is "."
  expect_equal(dirname(result), ".")
})
