context("events")
library(testthat)
library(bidser)

test_that("can extract event files from bids project", {
  proj <- bids_project(system.file("extdata/ds001", package="bidser"), fmriprep=FALSE)
  evfiles <- event_files(proj)
  testthat::expect_true(!is.null(evfiles))
  testthat::expect_equal(length(evfiles), 48)
})

test_that("can search for event files from bids project", {
  proj <- bids_project(system.file("extdata/ds001", package="bidser"), fmriprep=FALSE)
  evfiles <- search_files(proj, regex=".*events.tsv$")
  testthat::expect_true(!is.null(evfiles))
  testthat::expect_equal(length(evfiles), 48)
})


test_that("can read in event files from bids project", {
  proj <- bids_project(system.file("extdata/ds001", package="bidser"), fmriprep=FALSE)
  ev <- read_events(proj)
  testthat::expect_equal(nrow(ev), 48)
})

test_that("can read in event files from a  single subject", {
  proj <- bids_project(system.file("extdata/ds001", package="bidser"), fmriprep=FALSE)
  ev <- read_events(proj, subid="01")
  testthat::expect_equal(nrow(ev), 3)
})

test_that("run filtering works", {
  proj <- bids_project(system.file("extdata/ds001", package="bidser"), fmriprep=FALSE)
  ev <- read_events(proj, run="02")
  testthat::expect_equal(nrow(ev), 16)
  testthat::expect_true(all(ev$.run == "02"))
})

test_that("session filtering works", {
  proj <- bids_project(system.file("extdata/ds114", package="bidser"), fmriprep=FALSE)
  ev <- read_events(proj, session="test")
  testthat::expect_equal(nrow(ev), 10)
  testthat::expect_true(all(ev$.session == "test"))
})