context("utility helpers")
library(testthat)
library(bidser)

# Tests for str_detect_null
test_that("str_detect_null handles NULL and NA correctly", {
  expect_true(bidser:::str_detect_null("abc", "a"))
  expect_false(bidser:::str_detect_null(NULL, "a"))
  expect_true(bidser:::str_detect_null(NULL, "a", default = TRUE))
  expect_false(bidser:::str_detect_null(NA_character_, "a"))
  expect_true(bidser:::str_detect_null(NA_character_, "a", default = TRUE))
})

# Tests for key_match
test_that("key_match matcher works with various patterns", {
  matcher <- bidser:::key_match(default = FALSE, subid = "01", task = "task")
  expect_true(matcher(list(subid = "01", task = "task")))
  expect_false(matcher(list(subid = "02", task = "task")))
  expect_false(matcher(list(subid = "01")))

  matcher2 <- bidser:::key_match(default = TRUE, subid = "01", task = "task")
  expect_true(matcher2(list(subid = "01")))
  expect_true(matcher2(list(subid = "01", task = "task")))

  wildcard_matcher <- bidser:::key_match(subid = "01", task = ".*")
  expect_true(wildcard_matcher(list(subid = "01", task = "whatever")))
  expect_true(wildcard_matcher(list(subid = "01")))

  nullpattern_matcher <- bidser:::key_match(task = NULL)
  expect_true(nullpattern_matcher(list(task = NULL)))
  expect_false(nullpattern_matcher(list(task = "something")))
})

# Tests for generate_bids_path
library(data.tree)

test_that("generate_bids_path constructs expected directories", {
  p_raw <- bidser:::generate_bids_path("01", datatype = "func", fmriprep = FALSE)
  expect_equal(p_raw, "sub-01/func")

  p_prep <- bidser:::generate_bids_path("01", session = "test", datatype = "anat",
                                       fmriprep = TRUE, prep_dir = "derivatives/mockprep")
  expect_equal(p_prep, "derivatives/mockprep/sub-01/ses-test/anat")
})

# Tests for reconstruct_node_path

test_that("reconstruct_node_path handles raw and derivative nodes", {
  root <- Node$new("proj")
  raw <- root$AddChild("raw")
  sub <- raw$AddChild("sub-01")
  func <- sub$AddChild("func")
  leaf <- func$AddChild("file.nii.gz")
  expect_equal(bidser:::reconstruct_node_path(leaf), "sub-01/func/file.nii.gz")

  deriv <- root$AddChild("derivatives")$AddChild("fmriprep")
  subd <- deriv$AddChild("sub-01")
  funcd <- subd$AddChild("func")
  leafd <- funcd$AddChild("file_preproc.nii.gz")
  expect_equal(bidser:::reconstruct_node_path(leafd, prep_dir = "derivatives/fmriprep"),
               "derivatives/fmriprep/sub-01/func/file_preproc.nii.gz")

  root_file <- root$AddChild("dataset_description.json")
  expect_equal(bidser:::reconstruct_node_path(root_file), "dataset_description.json")
})
