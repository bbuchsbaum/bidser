context("sessions")
library(testthat)
library(bidser)

# Ensure sessions() returns session IDs for datasets with sessions

test_that("sessions() extracts session IDs when present", {
  proj <- bids_project(system.file("extdata/ds114", package = "bidser"), fmriprep = FALSE)
  ses <- sessions(proj)
  expect_true(!is.null(ses))
  expect_equal(sort(ses), c("retest", "test"))
})
