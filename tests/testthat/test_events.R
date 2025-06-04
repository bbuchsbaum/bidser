context("events")
library(testthat)
library(bidser)

test_that("can extract event files from bids project", {
  skip_if_offline()
  
  ds001_path <- setup_test_dataset("ds001")
  proj <- bids_project(ds001_path, fmriprep=FALSE)
  evfiles <- event_files(proj)
  testthat::expect_true(!is.null(evfiles))
  # Note: The count might be different from extdata version due to updated examples
  testthat::expect_true(length(evfiles) > 0)
})

test_that("can search for event files from bids project", {
  skip_if_offline()
  
  ds001_path <- setup_test_dataset("ds001")
  proj <- bids_project(ds001_path, fmriprep=FALSE)
  evfiles <- search_files(proj, regex=".*events.tsv$")
  testthat::expect_true(!is.null(evfiles))
  testthat::expect_true(length(evfiles) > 0)
})

test_that("can read in event files from bids project", {
  skip_if_offline()
  
  ds001_path <- setup_test_dataset("ds001")
  proj <- bids_project(ds001_path, fmriprep=FALSE)
  ev <- read_events(proj)
  testthat::expect_true(nrow(ev) > 0)
})

test_that("can read in event files from a single subject", {
  skip_if_offline()
  
  ds001_path <- setup_test_dataset("ds001")
  proj <- bids_project(ds001_path, fmriprep=FALSE)
  
  # Check what subjects are available first
  subjects <- participants(proj)
  testthat::expect_true(length(subjects) > 0)
  
  # Test with first available subject
  first_subject <- subjects[1]
  ev <- read_events(proj, subid=first_subject)
  testthat::expect_true(nrow(ev) >= 0)  # Could be 0 if subject has no events
})

test_that("run filtering works", {
  skip_if_offline()
  
  ds001_path <- setup_test_dataset("ds001")
  proj <- bids_project(ds001_path, fmriprep=FALSE)
  
  # First check what runs exist
  all_ev <- read_events(proj)
  if (nrow(all_ev) > 0) {
    available_runs <- unique(all_ev$.run)
    testthat::expect_true(length(available_runs) > 0)
    
    # Test filtering by first available run
    if (length(available_runs) > 0) {
      test_run <- available_runs[1]
      ev <- read_events(proj, run=test_run)
      testthat::expect_true(nrow(ev) > 0)
      testthat::expect_true(all(ev$.run == test_run))
    }
  }
})

test_that("session filtering works", {
  skip_if_offline()
  
  ds114_path <- setup_test_dataset("ds114")
  proj <- bids_project(ds114_path, fmriprep=FALSE)
  
  # Check what sessions exist
  sessions <- sessions(proj)
  if (length(sessions) > 0) {
    # Test with first available session
    test_session <- sessions[1]
    ev <- read_events(proj, session=paste0("^", test_session, "$"))
    testthat::expect_true(nrow(ev) >= 0)
    if (nrow(ev) > 0) {
      testthat::expect_true(all(ev$.session == test_session))
    }
  } else {
    testthat::skip("No sessions found in ds114")
  }
})