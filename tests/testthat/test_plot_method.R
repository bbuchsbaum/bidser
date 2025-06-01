context("plot.bids_project dendrogram")

library(testthat)
library(bidser)

make_proj <- function(...) {
  # Create a simple mock BIDS structure
  file_structure <- tibble::tribble(
    ~subid, ~session, ~datatype, ~task, ~run, ~suffix, ~fmriprep,
    "01", NA, "anat", NA, NA, "T1w.nii.gz", FALSE,
    "01", NA, "func", "rest", "01", "bold.nii.gz", FALSE,
    "02", NA, "anat", NA, NA, "T1w.nii.gz", FALSE,
    "02", NA, "func", "rest", "01", "bold.nii.gz", FALSE
  )
  
  participants <- tibble::tibble(
    participant_id = c("01", "02"),
    age = c(25, 30)
  )
  
  bidser::create_mock_bids(
    project_name = "test_proj",
    participants = participants,
    file_structure = file_structure,
    ...
  )
}

proj <- make_proj()


test_that("plot.bids_project returns invisibly", {
  expect_invisible(plot(proj, max_depth = 2))
})
