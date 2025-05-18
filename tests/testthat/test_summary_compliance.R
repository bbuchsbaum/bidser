context("summary and compliance")

library(testthat)
library(bidser)

# Test bids_summary for the example dataset

test_that("bids_summary reports correct statistics for ds001", {
  proj <- bids_project(system.file("extdata/ds001", package = "bidser"),
                        fmriprep = FALSE)
  sum <- bids_summary(proj)
  expect_equal(sum$n_subjects, 16)
  expect_null(sum$n_sessions)
  expect_equal(nrow(sum$tasks), 1)
  expect_equal(sum$tasks$task, "balloonanalogrisktask")
  expect_equal(sum$tasks$n_runs, 3)
  expect_equal(sum$total_runs, 3)
})

# Test simple compliance check on the dataset

test_that("bids_check_compliance passes on ds001", {
  proj <- bids_project(system.file("extdata/ds001", package = "bidser"),
                        fmriprep = FALSE)
  chk <- bids_check_compliance(proj)
  expect_true(chk$passed)
  expect_equal(length(chk$issues), 0)
})
