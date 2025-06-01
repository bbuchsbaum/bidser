context("plot_bids and bids_heatmap")

library(testthat)
library(bidser)

# Helper to make a tiny mock project
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

# Basic dataset
proj <- make_proj()

# dataset without run column to trigger plot_bids_heatmap
proj_missing_run <- make_proj()
proj_missing_run$raw_data$run <- NULL


test_that("plot_bids returns ggplot", {
  p_static <- plot_bids(proj, interactive = FALSE)
  expect_s3_class(p_static, "ggplot")

  p_interactive <- plot_bids(proj, interactive = TRUE)
  expect_s3_class(p_interactive, c("plotly", "ggplot"), exact = FALSE)
})


test_that("bids_heatmap returns ggplot", {
  p_static <- bids_heatmap(proj, interactive = FALSE)
  expect_s3_class(p_static, "ggplot")

  p_interactive <- bids_heatmap(proj, interactive = TRUE)
  expect_s3_class(p_interactive, c("plotly", "ggplot"), exact = FALSE)
})


test_that("bids_heatmap falls back when tasks or runs missing", {
  p <- bids_heatmap(proj_missing_run, interactive = FALSE)
  expect_s3_class(p, "ggplot")
})


